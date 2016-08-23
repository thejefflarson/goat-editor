#include <codecvt>
#include <iostream>
#include <locale>
#include <memory>
#include <string>
#include <vector>

#include <stddef.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <pango/pangocairo.h>


#pragma mark - text utils

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
  if(action == GLFW_RELEASE) return;
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
  std::cerr << "Error: " << description << std::endl;
}

static bool is_error() {
  return glGetError() != GL_NO_ERROR;
}


#pragma mark - GL utils

class GL {
public:
  virtual bool Init() = 0;
  virtual bool bind() = 0;
  virtual bool unbind() = 0;
};

class Vao : public GL {
public:
  Vao(std::vector<GLfloat> vertices,
      std::vector<GLuint> triangles) :
    vertices_(vertices),
    triangles_(triangles) {}

  ~Vao() {
    if(vbo_) glDeleteBuffers(1, &vbo_);
    if(ebo_) glDeleteBuffers(1, &ebo_);
    if(vao_) glDeleteVertexArrays(1, &vao_);
  }

  bool Init() {
    glGenVertexArrays(1, &vao_);

    bind();
    glGenBuffers(1, &vbo_);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_);
    glBufferData(GL_ARRAY_BUFFER, vertices_.size() * sizeof(GLfloat),
                 vertices_.data(), GL_STATIC_DRAW);
    glGenBuffers(1, &ebo_);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo_);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, triangles_.size() * sizeof(GLuint),
                 triangles_.data(), GL_STATIC_DRAW);
    unbind();
    return is_error();
  }

  bool bind() {
    glBindVertexArray(vao_);
    return is_error();
  }

  bool unbind() {
    glBindVertexArray(0);
    return is_error();
  }

private:
  std::vector<GLfloat> vertices_;
  std::vector<GLuint> triangles_;
  GLuint vao_;
  GLuint vbo_;
  GLuint ebo_;
};

class Shader : public GL {
public:
  Shader(std::string vertex, std::string fragment) :
    vertex_(vertex),
    fragment_(fragment) {}

  bool Init(){
    vertex_shader_ = glCreateShader(GL_VERTEX_SHADER);
    const char *v = vertex_.c_str();
    const int len = vertex_.length();
    glShaderSource(vertex_shader_, 1, &v, &len);
    glCompileShader(vertex_shader_);

    fragment_shader_ = glCreateShader(GL_FRAGMENT_SHADER);
    const char *f = fragment_.c_str();
    const int flen = fragment_.length();
    glShaderSource(fragment_shader_, 1, &f, &flen);
    glCompileShader(fragment_shader_);

    program_ = glCreateProgram();
    glAttachShader(program_, vertex_shader_);
    glAttachShader(program_, fragment_shader_);
    glBindFragDataLocation(program_, 0, "color");
    glLinkProgram(program_);

    return is_error();
  }

  GLuint get() {
    return program_;
  }

  bool bind() {
    glUseProgram(program_);
    return is_error();
  }

  bool unbind() {
    glUseProgram(0);
    return is_error();
  }

  ~Shader() {
    glDeleteProgram(program_);
    glDeleteShader(vertex_shader_);
    glDeleteShader(fragment_shader_);
  }

private:
  std::string vertex_;
  std::string fragment_;

  GLuint program_;
  GLuint vertex_shader_;
  GLuint fragment_shader_;
};


#pragma mark - main

int main() {
  auto text = std::make_shared<std::wstring>();
  GLFWwindow* window;
  glfwSetErrorCallback(log_error);
  if(!glfwInit()) { return EXIT_FAILURE; }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);

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

  Vao vao({
   -1.0f,  1.0f,
    1.0f,  1.0f,
    1.0f, -1.0f,
   -1.0f, -1.0f
  }, {
    0, 1, 2,
    2, 3, 0
  });

  if(vao.Init() || vao.bind()) {
    log_error(0, "Couldn't load vao.");
    glfwDestroyWindow(window);
    glfwTerminate();
    return EXIT_FAILURE;
  }

  Shader shader(
"                                    \
#version 150 core                    \
in vec2 pos;                         \
out vec2 coord;                      \
void main(){                         \
  coord = pos;                       \
  gl_Position = vec4(pos, 0.0, 1.0); \
}",
"                                                         \
#version 150 core                                         \
uniform sampler2D text;                                   \
in vec2 coord;                                            \
out vec4 color;                                           \
void main() {                                             \
  vec2 coord2 = (vec2(coord.x, 2. - coord.y) + 1.) / 2.;  \
  color = texture(text, coord2);                          \
}");

  if(shader.Init() || shader.bind()) {
    log_error(0, "Couldn't load shader.");
    glfwDestroyWindow(window);
    glfwTerminate();
    return EXIT_FAILURE;
  }

  GLint pos = glGetAttribLocation(shader.get(), "pos");
  glEnableVertexAttribArray(pos);
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);

  GLuint tex = glGetUniformLocation(shader.get(), "text");
  glGenTextures(1, &tex);
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  while(!glfwWindowShouldClose(window)) {
    int w, h;
    // TODO: we should only alloc this on resize
    glfwGetFramebufferSize(window, &w, &h);

    uint8_t *data = (uint8_t *)malloc(w * h * 4 * sizeof(uint8_t));

    cairo_surface_t *surface = cairo_image_surface_create_for_data(
      data, CAIRO_FORMAT_ARGB32, w, h, w * 4
    );

    cairo_t *ctx = cairo_create(surface);
    cairo_set_antialias(ctx, CAIRO_ANTIALIAS_BEST);
    float gray = 255.0 / 255.0;
    cairo_set_source_rgba(ctx, 0.0, 14.0 / 255.0, 47.0 / 255.0, 1.0f);
    cairo_paint(ctx);
    cairo_set_source_rgba(ctx, gray, gray, gray, 1.0);
    PangoLayout *layout = pango_cairo_create_layout(ctx);
    PangoFontDescription *desc = pango_font_description_from_string("Monaco 10");
    pango_layout_set_font_description(layout, desc);
    pango_font_description_free(desc);
    std::wstring_convert<std::codecvt_utf8<wchar_t>> conv;
    auto utf8 = conv.to_bytes(*text);
    pango_layout_set_text(layout, utf8.c_str(), utf8.size());
    pango_cairo_update_layout (ctx, layout);
    pango_cairo_show_layout(ctx, layout);

    g_object_unref(layout);
    cairo_surface_destroy(surface);
    cairo_destroy(ctx);

    glViewport(0, 0, w, h);
    glClearColor(0.0, 14.0 / 255.0, 47.0 / 255.0, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glTexImage2D(GL_TEXTURE_2D,
                  0, GL_RGBA, w, h,
                  0, GL_BGRA, GL_UNSIGNED_BYTE, data);

    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

    glfwSwapBuffers(window);
    glfwPollEvents();
    free(data);
  }
  vao.unbind();
  shader.unbind();

  glfwDestroyWindow(window);
  glfwTerminate();
  return EXIT_SUCCESS;
}
