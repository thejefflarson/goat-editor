#include <iostream>

#include <glad/glad.h>
#include <GLFW/glfw3.h>


static void log_error(int error, const char* description) {
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
