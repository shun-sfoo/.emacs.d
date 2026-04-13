#include "plug.h"
#include <assert.h>
#include <complex.h>
#include <dlfcn.h>
#include <raylib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define ARRAY_LEN(xs) sizeof(xs) / sizeof(xs[0])

char *shift_args(int *argc, char ***argv) {
  assert(*argc > 0);
  char *result = (**argv);
  (*argv) += 1;
  (*argc) -= 1;
  return result;
}

plug_hello_t plug_hello = NULL;
plug_init_t plug_init = NULL;
plug_update_t plug_update = NULL;
plug_pre_reload_t plug_pre_reload = NULL;
plug_post_reload_t plug_post_reload = NULL;
Plug plug = {0};

const char *libplug_file_name = "./libplug.so";
void *libplug = NULL;

bool reload_libplug() {
  if (libplug != NULL) {
    dlclose(libplug);
  }

  libplug = dlopen(libplug_file_name, RTLD_NOW);
  if (libplug == NULL) {
    fprintf(stderr, "ERROR: could not load %s: %s\n", libplug_file_name,
            dlerror());
    return false;
  }

  plug_hello = dlsym(libplug, "plug_hello");
  if (plug_hello == NULL) {
    fprintf(stderr, "Error: could not find plug_hello symbol in %s: %s\n",
            libplug_file_name, dlerror());
    return false;
  }

  plug_init = dlsym(libplug, "plug_init");
  if (plug_init == NULL) {
    fprintf(stderr, "Error: could not find plug_init symbol in %s: %s\n",
            libplug_file_name, dlerror());
    return false;
  }

  plug_update = dlsym(libplug, "plug_update");
  if (plug_update == NULL) {
    fprintf(stderr, "Error: could not find plug_update symbol in %s: %s\n",
            libplug_file_name, dlerror());
    return false;
  }

  plug_pre_reload = dlsym(libplug, "plug_pre_reload");
  if (plug_pre_reload == NULL) {
    fprintf(stderr, "Error: could not find plug_pre_reload symbol in %s: %s\n",
            libplug_file_name, dlerror());
    return false;
  }

  plug_post_reload = dlsym(libplug, "plug_post_reload");
  if (plug_post_reload == NULL) {
    fprintf(stderr, "Error: could not find plug_post_reload symbol in %s: %s\n",
            libplug_file_name, dlerror());
    return false;
  }

  return true;
}

int main(int argc, char **argv) {
  if (!reload_libplug()) {
    return 1;
  };

  const char *program = shift_args(&argc, &argv);
  printf("grogram %s\n", program);

  if (argc == 0) {
    fprintf(stderr, "Usage: %s <input>\n", program);
    fprintf(stderr, "ERROR: no input file is provided\n");
    return 1;
  }

  const char *file_path = shift_args(&argc, &argv);

  InitWindow(800, 600, "Musializer");
  SetTargetFPS(60);

  InitAudioDevice();
  plug_init(&plug, file_path);

  while (!WindowShouldClose()) {
    if (IsKeyPressed(KEY_R)) {
      plug_pre_reload(&plug);
      if (!reload_libplug())
        return 1;
      plug_post_reload(&plug);
    }
    plug_update(&plug);
  }
}
