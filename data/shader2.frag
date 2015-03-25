#version 150
uniform float time;
in vec2 Location;
out vec4 outColor;
void main()
{
  outColor = vec4(
    fract(Location.x*10) < 0.5 ? 1.0 : 0.0,
    fract(Location.y*10) < 0.5 ? 1.0 : 0.0,
    0.0, 1.0
  );
}
