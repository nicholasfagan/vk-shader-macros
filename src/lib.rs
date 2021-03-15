use proc_macro_hack::proc_macro_hack;

/// Compile a GLSL source file into a binary SPIR-V constant
///
/// ```
/// use vk_shader_macros::include_glsl;
/// const VERT: &[u32] = include_glsl!("example.vert");
/// ```
///
/// Due to limitations of proc macros, paths are resolved relative to the crate root.
///
/// # Options
///
/// Compile options may be specified as additional arguments. Supported options include:
/// - `kind: <kind>` - Specify shader kind. Valid kinds are the same as the recognized file
///    extensions: `vert`, `frag`, `comp`, `geom`, `tesc`, `tese`, `spvasm`, `rgen`, `rahit`,
///    `rchit`, `rmiss`, `rint`, `rcall`, `task`, and `mesh`. If omitted, kind is inferred from the
///    file's extension, or a pragma in the source.
/// - `version: <version>` - Specify GLSL version. If omitted, version must be specified in the
///    source with `#version`
/// - `strip` - Omit debug info (set as default by enabling the `strip` feature)
/// - `debug` - Force debug info, even with the `strip` feature enabled
/// - `define: <name> ["value"]` - Define the preprocessor macro `<name>` as `value`
/// - `optimize: <level>` - Specify optimization level. Supported values are: `zero`, `size`, and
///   `performance`.  If omitted, will default to `performance`.
/// - `target: <target>` - Specify target environment. Supported values: `vulkan1_0`, `vulkan1_1`,
///   `vulkan1_2`. Defaults to `vulkan1_0`.
#[proc_macro_hack]
pub use vk_shader_macros_impl::include_glsl;


/// Compile inline GLSL source into a binary SPIR-V constant
///
/// ```
/// use vk_shader_macros::compile_glsl;
/// const VERT: &[u32] = compile_glsl!("
/// #version 450
/// void main() {
///     gl_Position = vec4(0);
/// }
/// ", kind: vert);
/// ```
///
/// # Options
///
/// Compile options may be specified as additional arguments. Supported options include:
/// - `kind: <kind>` - Specify shader kind. Valid kinds are the same as the recognized file
///    extensions: `vert`, `frag`, `comp`, `geom`, `tesc`, `tese`, `spvasm`, `rgen`, `rahit`,
///    `rchit`, `rmiss`, `rint`, `rcall`, `task`, and `mesh`. If omitted, kind is inferred from the
///    file's extension, or a pragma in the source.
/// - `version: <version>` - Specify GLSL version. If omitted, version must be specified in the
///    source with `#version`
/// - `strip` - Omit debug info (set as default by enabling the `strip` feature)
/// - `debug` - Force debug info, even with the `strip` feature enabled
/// - `define: <name> ["value"]` - Define the preprocessor macro `<name>` as `value`
/// - `optimize: <level>` - Specify optimization level. Supported values are: `zero`, `size`, and
///   `performance`.  If omitted, will default to `performance`.
/// - `target: <target>` - Specify target environment. Supported values: `vulkan1_0`, `vulkan1_1`,
///   `vulkan1_2`. Defaults to `vulkan1_0`.
#[proc_macro_hack]
pub use vk_shader_macros_impl::compile_glsl;

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    const TEST: &[u32] = include_glsl!("example.vert", version: 450, optimize: size, target: vulkan1_1);
}
