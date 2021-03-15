extern crate proc_macro;

use std::cell::RefCell;
use std::path::Path;
use std::{env, fs, mem, str};

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_hack::proc_macro_hack;
use quote::quote;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Ident, LitInt, LitStr, Token};

fn parse_and_compile<T: Parse + Source>(tokens: TokenStream) -> TokenStream {
    let args = parse_macro_input!(tokens with Arguments<T>::parse_with_usage_err);
    let CompiledGlsl {
        dependencies,
        spirv_words,
    } = match compile(args) {
        Ok(x) => x,
        Err(e) => return e.to_compile_error().into(),
    };
    let expanded = quote! {
        {
            #({ const _FORCE_DEP: &[u8] = include_bytes!(#dependencies); })*
            &[#(#spirv_words),*]
        }
    };
    TokenStream::from(expanded)
}

#[proc_macro_hack]
pub fn include_glsl(tokens: TokenStream) -> TokenStream {
    parse_and_compile::<PathSource>(tokens)
}

#[proc_macro_hack]
pub fn compile_glsl(tokens: TokenStream) -> TokenStream {
    parse_and_compile::<InlineSource>(tokens)
}

trait Source {
    fn span(&self) -> Span;
    fn source(&self) -> syn::Result<String>;
    fn path(&self) -> Option<String>;
    fn name(&self) -> String;
}

struct PathSource(pub LitStr);

impl Parse for PathSource {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(PathSource(input.parse::<LitStr>().map_err(|e| {
            syn::Error::new(e.span(), "expected path to shader source as literal string")
        })?))
    }
}

struct InlineSource(pub LitStr);

impl Parse for InlineSource {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(InlineSource(input.parse::<LitStr>().map_err(|e| {
            syn::Error::new(e.span(), "expected inline shader source as literal string")
        })?))
    }
}

impl Source for PathSource {
    fn span(&self) -> Span {
        self.0.span()
    }
    fn source(&self) -> syn::Result<String> {
        fs::read_to_string(&self.0.value()).map_err(|e| syn::Error::new(self.span(), e))
    }
    fn path(&self) -> Option<String> {
        let path = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join(&self.0.value());
        let path_str = path.to_string_lossy().to_string();
        Some(path_str)
    }
    fn name(&self) -> String {
        self.0.value()
    }
}

impl Source for InlineSource {
    fn span(&self) -> Span {
        self.0.span()
    }
    fn source(&self) -> syn::Result<String> {
        Ok(self.0.value())
    }
    fn path(&self) -> Option<String> {
        None
    }
    fn name(&self) -> String {
        format!("<inline{:?}>", self.span())
    }
}

struct Arguments<S> {
    source: S,
    kind: Option<shaderc::ShaderKind>,
    version: Option<u32>,
    debug: Option<bool>,
    define: HashMap<String, Option<String>>,
    optimize: Option<shaderc::OptimizationLevel>,
    target: Option<u32>,
}

const USAGE_STR: &str = concat!(
    "See `https://docs.rs/vk-shader-macros/",
    env!("CARGO_PKG_VERSION"),
    "' for help"
);

impl<S: Parse> Arguments<S> {
    pub fn parse_with_usage_err(input: ParseStream) -> Result<Self> {
        Self::parse(input).map_err(|e| syn::Error::new(e.span(), format!("{}\n{}", e, USAGE_STR)))
    }
}

impl<S: Parse> Parse for Arguments<S> {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = Arguments {
            source: input.parse::<S>()?,
            kind: None,
            version: None,
            debug: None,
            define: HashMap::new(),
            optimize: None,
            target: None,
        };

        while !input.is_empty() {
            input.parse::<Token![,]>()?;
            let key = input.parse::<Ident>()?;
            match key.to_string().as_str() {
                "kind" => {
                    input.parse::<Token![:]>()?;
                    let value = input.parse::<Ident>()?;
                    args.kind = Some(
                        extension_kind(&value.to_string())
                            .ok_or_else(|| syn::Error::new(value.span(), "unknown shader kind"))?,
                    );
                }
                "version" => {
                    input.parse::<Token![:]>()?;
                    args.version = Some(input.parse::<LitInt>()?.base10_parse()?);
                }
                "strip" => args.debug = Some(false),
                "debug" => args.debug = Some(true),
                "define" => {
                    input.parse::<Token![:]>()?;
                    let name = input.parse::<Ident>()?.to_string();
                    let value = if input.peek(Token![,]) {
                        None
                    } else {
                        Some(input.parse::<LitStr>()?.value())
                    };
                    args.define.insert(name, value);
                }
                "optimize" => {
                    input.parse::<Token![:]>()?;
                    let value = input.parse::<Ident>()?;
                    args.optimize =
                        Some(optimization_level(&value.to_string()).ok_or_else(|| {
                            syn::Error::new(value.span(), "unknown optimization level")
                        })?);
                }
                "target" => {
                    input.parse::<Token![:]>()?;
                    let value = input.parse::<Ident>()?;
                    args.target = Some(
                        target(&value.to_string())
                            .ok_or_else(|| syn::Error::new(value.span(), "unknown target"))?,
                    );
                }
                _ => {
                    return Err(syn::Error::new(
                        key.span(),
                        "unknown shader compile argument",
                    ))
                }
            }
        }
        Ok(args)
    }
}

struct CompiledGlsl {
    dependencies: Vec<String>,
    spirv_words: Vec<u32>,
}

fn compile<S: Source>(args: Arguments<S>) -> syn::Result<CompiledGlsl> {
    let dependencies: RefCell<Vec<String>> = RefCell::new(Vec::new());
    if let Some(src_path) = args.source.path() {
        dependencies.borrow_mut().push(src_path);
    }

    let mut options = shaderc::CompileOptions::new().unwrap();
    options.set_include_callback(|name, ty, src, _depth| {
        let path = match ty {
            shaderc::IncludeType::Relative => Path::new(src).parent().unwrap().join(name),
            shaderc::IncludeType::Standard => {
                Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join(name)
            }
        };
        let path_str = path.to_str().ok_or("non-unicode path")?.to_owned();
        dependencies.borrow_mut().push(path_str.clone());
        Ok(shaderc::ResolvedInclude {
            resolved_name: path_str,
            content: fs::read_to_string(path).map_err(|x| x.to_string())?,
        })
    });

    match args.debug {
        Some(true) => options.set_generate_debug_info(),
        None if !cfg!(feature = "strip") => options.set_generate_debug_info(),
        _ => {}
    };

    options.set_optimization_level(args.optimize.unwrap_or(
        if cfg!(feature = "default-optimize-zero") {
            shaderc::OptimizationLevel::Zero
        } else {
            shaderc::OptimizationLevel::Performance
        },
    ));
    options.set_target_env(shaderc::TargetEnv::Vulkan, args.target.unwrap_or(1 << 22));

    let kind = args
        .kind
        .or_else(|| {
            args.source.path().map(|path| {
                Path::new(&path)
                    .extension()
                    .and_then(|x| x.to_str().and_then(|x| extension_kind(x)))
                    .unwrap_or(shaderc::ShaderKind::InferFromSource)
            })
        })
        .unwrap_or(shaderc::ShaderKind::InferFromSource);

    if let Some(version) = args.version {
        options.set_forced_version_profile(version, shaderc::GlslProfile::None);
    }

    let src = args.source.source()?;

    let mut compiler = shaderc::Compiler::new().unwrap();
    let out = compiler
        .compile_into_spirv(&src, kind, &args.source.name(), "main", Some(&options))
        .map_err(|e| syn::Error::new(args.source.span(), e))?;
    if out.get_num_warnings() != 0 {
        return Err(syn::Error::new(
            args.source.span(),
            out.get_warning_messages(),
        ));
    }
    mem::drop(options);

    Ok(CompiledGlsl {
        dependencies: dependencies.into_inner(),
        spirv_words: out.as_binary().into(),
    })
}
fn extension_kind(ext: &str) -> Option<shaderc::ShaderKind> {
    use shaderc::ShaderKind::*;
    Some(match ext {
        "vert" => Vertex,
        "frag" => Fragment,
        "comp" => Compute,
        "geom" => Geometry,
        "tesc" => TessControl,
        "tese" => TessEvaluation,
        "spvasm" => SpirvAssembly,
        "rgen" => RayGeneration,
        "rahit" => AnyHit,
        "rchit" => ClosestHit,
        "rmiss" => Miss,
        "rint" => Intersection,
        "rcall" => Callable,
        "task" => Task,
        "mesh" => Mesh,
        _ => {
            return None;
        }
    })
}

fn optimization_level(level: &str) -> Option<shaderc::OptimizationLevel> {
    match level {
        "zero" => Some(shaderc::OptimizationLevel::Zero),
        "size" => Some(shaderc::OptimizationLevel::Size),
        "performance" => Some(shaderc::OptimizationLevel::Performance),
        _ => None,
    }
}

fn target(s: &str) -> Option<u32> {
    Some(match s {
        "vulkan" | "vulkan1_0" => 1 << 22,
        "vulkan1_1" => 1 << 22 | 1 << 12,
        "vulkan1_2" => 1 << 22 | 2 << 12,
        _ => return None,
    })
}
