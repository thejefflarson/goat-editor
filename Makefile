CC = clang
CXX = clang++
INCLUDES = -I./lib/include/ $(shell pkg-config --cflags glfw3 pangocairo)
CFLAGS = $(INCLUDES) -std=c11
CXXFLAGS = $(INCLUDES) -std=c++14 -g -Wall
LDFLAGS = $(shell pkg-config --libs --static glfw3 pangocairo)
GLAD = lib/src/glad.c lib/include/glad/glad.h lib/include/KHR/khrplatform.h

all: build/goat-editor

lib/stamp:
	mkdir -p lib
	@rm -f data.tmp
	@touch data.tmp
	glad --generator=c-debug --out-path=lib
	@mv data.tmp $@

$(GLAD): lib/stamp
	@if test -f $@; then :; else \
          rm -f $^; \
          $(MAKE) $(AM_MAKEFLAGS) $^; \
	fi

build/glad.o: $(GLAD)
	mkdir -p build
	$(CC) $(CFLAGS) -o $@ -c $<

build/goat-editor.o: goat-editor.cc $(GLAD)
	mkdir -p build
	$(CXX) $(CXXFLAGS) -o $@ -c $<

build/goat-editor: build/goat-editor.o build/glad.o
	mkdir -p build
	$(CXX) $(LDFLAGS) $^ -o $@

clean:
	rm -rf build lib

.PHONY: clean
