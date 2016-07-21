CC = clang
CXX = clang++
INCLUDES = -I./lib/include/ $(shell pkg-config --cflags glfw3)
CFLAGS = $(INCLUDES)
CXXFLAGS = $(INCLUDES)
LDFLAGS = $(shell pkg-config --libs --static glfw3)
GLAD = lib/src/glad.c lib/include/glad/glad.h lib/include/KHR/khrplatform.h

all: build/goat-editor

build/stamp:
	mkdir -p build
	@rm -f data.tmp
	@touch data.tmp
	glad --generator=c --out-path=lib
	@mv data.tmp $@

$(GLAD): build/stamp
	@if test -f $@; then :; else \
          rm -f $^; \
          $(MAKE) $(AM_MAKEFLAGS) $^; \
	fi

build/glad.o: $(GLAD)
	$(CC) $(CFLAGS) -o $@ -c $<

build/goat-editor.o: goat-editor.cc $(GLAD)
	$(CXX) $(CFLAGS) -o $@ -c $<

build/goat-editor: build/goat-editor.o build/glad.o

clean:
	rm -rf build lib

.PHONY: clean
