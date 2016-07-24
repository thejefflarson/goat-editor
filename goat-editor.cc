#include <iostream>
#include <string>

#include <stddef.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <pango/pangocairo.h>

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

static const float points[4][3] = {
  {-1.0, -1.0, 0.0},
  { 1.0, -1.0, 0.0},
  {-1.0,  1.0, 0.0},
  { 1.0,  1.0, 0.0}
};

static const uint8_t triangles[2][3] {
  {0, 1, 2},
  {2, 1, 3}
};

static void delete_char(GLFWwindow* window, ssize_t offset, size_t num) {
  void *data = glfwGetWindowUserPointer(window);
  auto text = *static_cast<std::shared_ptr<std::wstring> *>(data);
  if(offset < 0) {
    offset = text->length() + offset;
    if(offset < 0) { return; }
  }
  text->erase(offset, num);
  std::wcout << *text << std::endl;
}

static void add_char(GLFWwindow* window, unsigned int codepoint) {
  void *data = glfwGetWindowUserPointer(window);
  auto text = *static_cast<std::shared_ptr<std::wstring> *>(data);
  text->push_back(codepoint);
  std::wcout << *text << std::endl;
}

static void control_key(GLFWwindow* window, int key,
                        int scancode, int action, int mods) {
  switch(key) {
  case GLFW_KEY_BACKSPACE:
    delete_char(window, -1, 1);
    break;
  case GLFW_KEY_ENTER:
    add_char(window, '\n');
    break;
  }
}

static void log_error(const int error, const char* description) {
  std::cerr << "Error:" << description << std::endl;
}

int main() {
  auto text = std::make_shared<std::wstring>();
  GLFWwindow* window;
  glfwSetErrorCallback(log_error);
  if(!glfwInit()) { exit(EXIT_FAILURE); }

  window = glfwCreateWindow(640, 480, "Goat Editor", NULL, NULL);
  if(!window) {
    glfwTerminate();
    return EXIT_FAILURE;
  }

  glfwSetWindowUserPointer(window, static_cast<void *>(&text));
  glfwSetCharCallback(window, add_char);
  glfwSetKeyCallback(window, control_key);

  glfwMakeContextCurrent(window);
  if(!gladLoadGLLoader((GLADloadproc) glfwGetProcAddress)) {
    log_error(0, "Couldn't load glad.");
    glfwDestroyWindow(window);
    glfwTerminate();
    return EXIT_FAILURE;
  }

  while(!glfwWindowShouldClose(window)) {
    int w, h;
    glfwGetFramebufferSize(window, &w, &h);

    cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
                                                          w, h);
    cairo_t *ctx = cairo_create(surface);
    PangoLayout *layout = pango_cairo_create_layout(ctx);
    cairo_set_source_rgb(ctx, 1.0, 1.0, 1.0);
    pango_layout_set_text(layout, (char *)text->c_str(), text->size());
    pango_cairo_show_layout(ctx, layout);
    g_object_unref(layout);
    cairo_surface_destroy(surface);
    glViewport(0, 0, w, h);
    glClear(GL_COLOR_BUFFER_BIT);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  glfwDestroyWindow(window);
  glfwTerminate();
  return EXIT_SUCCESS;
}
