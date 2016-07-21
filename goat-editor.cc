#include <iostream>

#include <inttypes.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>

static const char *vertex_shader =
"                               \
attribute vec3 pos;             \
varying vec2 coord;             \
void main(){                    \
  coord = pos.xy;               \
  gl_Position = vec4(pos, 0.0); \
}";

static const char *fragment_shader =
"                                           \
uniform sampler2D text;                     \
varying vec2 coord;                         \
void main() {                               \
  gl_FragColor = texture2D(text, coord.xy); \
}";

static const float32_t points[4][3] = {
  {-1.0, -1.0, 0.0},
  { 1.0, -1.0, 0.0},
  {-1.0,  1.0, 0.0},
  { 1.0,  1.0, 0.0}
};

static const uint8_t triangles[3][2] {
  {0, 1, 2},
  {2, 1, 3}
};

static void log_error(const int error, const char* description) {
  std::cerr << "Error:" << description;
}

int main() {
  GLFWwindow* window;
  glfwSetErrorCallback(log_error);
  if(!glfwInit()) { exit(EXIT_FAILURE); }

  window = glfwCreateWindow(640, 480, "Goat Editor", NULL, NULL);
  if(!window) {
    glfwTerminate();
    return EXIT_FAILURE;
  }

  glfwMakeContextCurrent(window);
  if(!gladLoadGLLoader((GLADloadproc) glfwGetProcAddress)) {
    log_error(0, "Couldn't load glad.");
    glfwDestroyWindow(window);
    glfwTerminate();
    return EXIT_FAILURE;
  }
  glfwSwapInterval(1);

  while(!glfwWindowShouldClose(window)) {
    int w, h;
    glfwGetFramebufferSize(window, &w, &h);
    glViewport(0, 0, w, h);
    glClear(GL_COLOR_BUFFER_BIT);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return EXIT_SUCCESS;
}
