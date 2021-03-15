use vk_shader_macros::compile_glsl;

const SHADER: &'static [u32] = compile_glsl!(r##"
#version 450
void main() {
    nonsense;
	gl_Position = vec4(0,0,0,0);
}
"##, kind: vert);

fn main() {}