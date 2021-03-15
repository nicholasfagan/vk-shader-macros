use vk_shader_macros::include_glsl;

const VERTEX: &[u32] = include_glsl!(r##"src"##, bad_arg: arg);

fn main() {}