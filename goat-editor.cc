#include <iostream>
#include <string>

#include <stddef.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <pango/pangocairo.h>

static const char *vertex =
"                               \
attribute vec2 pos;             \
varying vec2 coord;             \
void main(){                    \
  coord = pos;               \
  gl_Position = vec4(pos, 0.0, 1.0); \
}";

static const char *fragment =
"                                           \
uniform sampler2D text;                     \
varying vec2 coord;                         \
void main() {                               \
  gl_FragColor = vec4(1.);texture2D(text, coord); \
}";

static const float vertices[] = {
  -1.0, -1.0,
   1.0, -1.0,
  -1.0,  1.0,
   1.0,  1.0
};

static const uint8_t triangles[] {
  0, 1, 2,
  2, 1, 3
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
  case GLFW_KEY_TAB:
    add_char(window, ' ');
    add_char(window, ' ');
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

  // TODO: add error checks for this junk
  GLuint vertex_buffer;
  glGenBuffers(1, &vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  GLuint triangles_buffer;
  glGenBuffers(1, &triangles_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, triangles_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(triangles), triangles,
               GL_STATIC_DRAW);

  GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertex_shader, 1, &vertex, NULL);
  glCompileShader(vertex_shader);

  GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragment_shader, 1, &fragment, NULL);
  glCompileShader(fragment_shader);

  GLuint program = glCreateProgram();
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);

  GLuint text_id = glGetUniformLocation(program, "text");
  glGenTextures (1, &text_id);

  GLint pos = glGetAttribLocation(program, "pos");
  glEnableVertexAttribArray(pos);
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 2, NULL);
  glEnableVertexAttribArray(pos);

  while(!glfwWindowShouldClose(window)) {
    int w, h;
    // TODO: we should only alloc this on resize
    glfwGetFramebufferSize(window, &w, &h);
    uint8_t *data = (uint8_t *)calloc(w * h * 4, sizeof(uint8_t));
    cairo_surface_t *surface = cairo_image_surface_create_for_data(
      data, CAIRO_FORMAT_ARGB32, w, h, w * 4
    );

    cairo_t *ctx = cairo_create(surface);
    cairo_set_source_rgb(ctx, 1.0, 1.0, 1.0);
    cairo_paint(ctx);
    cairo_set_source_rgb (ctx, 0, 0, 0);
    PangoLayout *layout = pango_cairo_create_layout(ctx);
    pango_layout_set_text(layout, (char *)text->data(), text->size());
    pango_cairo_update_layout (ctx, layout);
    pango_cairo_show_layout(ctx, layout);
    g_object_unref(layout);
    cairo_destroy(ctx);
    cairo_surface_write_to_png(surface, "out.png");
    cairo_surface_destroy(surface);

    glViewport(0, 0, w, h);
    glClear(GL_COLOR_BUFFER_BIT);
    glUseProgram(program);

    glBindTexture(GL_TEXTURE_RECTANGLE_ARB, text_id);
    glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,
                  0, GL_RGBA, w, h,
                  0, GL_BGRA, GL_UNSIGNED_BYTE,	data);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, triangles_buffer);
    glPointSize(10.);
    glDrawElements(GL_POINTS, 6, GL_UNSIGNED_SHORT, NULL);

    glfwSwapBuffers(window);
    glfwPollEvents();
    free(data);
  }

  // todo clean up
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glDisableVertexAttribArray(pos);
  //glDeleteBuffers(1, &pos);
  //glDeleteBuffers(1, &)

  glfwDestroyWindow(window);
  glfwTerminate();
  return EXIT_SUCCESS;
}
