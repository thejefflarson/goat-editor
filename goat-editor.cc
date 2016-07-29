#include <codecvt>
#include <iostream>
#include <string>
#include <vector>

#include <stddef.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <pango/pangocairo.h>

static const GLchar *vertex =
"                                    \
#version 150 core                    \
in vec2 pos;                         \
out vec2 coord;                      \
void main(){                         \
  coord = pos;                       \
  gl_Position = vec4(pos, 0.0, 1.0); \
}";

static const GLchar *fragment =
"                                                         \
#version 150 core                                         \
uniform sampler2D text;                                   \
in vec2 coord;                                            \
out vec4 color;                                           \
void main() {                                             \
  vec2 coord2 = (vec2(coord.x, 2. - coord.y) + 1.) / 2.;  \
  color = texture(text, coord2);                          \
}";

static const GLfloat vertices[] = {
  -1.0f,  1.0f,
   1.0f,  1.0f,
   1.0f, -1.0f,
  -1.0f, -1.0f
};

static const GLuint triangles[] = {
  0, 1, 2,
  2, 3, 0
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
  std::cerr << "Error:" << description << std::endl;
}

static bool is_error() {
  return glGetError() != GL_NO_ERROR;
}

class GL {
public:
  virtual bool Init();
  virtual bool bind();
  virtual bool unbind();
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
    glGenBuffers(1, &ebo_);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo_);
    glBufferData(GL_ARRAY_BUFFER, vertices_.size(),
                 vertices_.data(), GL_STATIC_DRAW);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, triangles_.size(),
                 triangles_.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
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
  GLuint vao_ = 0;
  GLuint vbo_ = 0;
  GLuint ebo_ = 0;
};

class Shader : public GL {
public:
  Shader(std::string fragment, std::string vertex) :
    fragment_(fragment),
    vertex_(vertex) {}

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
  std::string fragment_;
  std::string vertex_;
  GLuint program_ = 0;
  GLuint vertex_shader_ = 0;
  GLuint fragment_shader_ = 0;
};

int main() {
  auto text = std::make_shared<std::wstring>();
  GLFWwindow* window;
  glfwSetErrorCallback(log_error);
  if(!glfwInit()) { exit(EXIT_FAILURE); }

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

  // TODO: add error checks for this junk
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  GLuint vertex_buffer;
  glGenBuffers(1, &vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  GLuint element_buffer;
  glGenBuffers(1, &element_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element_buffer);
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
  glBindFragDataLocation(program, 0, "color");
  glLinkProgram(program);
  glUseProgram(program);

  GLint pos = glGetAttribLocation(program, "pos");
  glEnableVertexAttribArray(pos);
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);

  GLuint tex = glGetUniformLocation(program, "text");
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

  glDeleteProgram(program);
  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);
  glDeleteBuffers(1, &vertex_buffer);
  glDeleteBuffers(1, &element_buffer);
  glDeleteVertexArrays(1, &vao);
  glfwDestroyWindow(window);
  glfwTerminate();
  return EXIT_SUCCESS;
}
