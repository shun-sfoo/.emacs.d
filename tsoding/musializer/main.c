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

#ifdef HOTRELOAD
#define PLUG(name, ...) name##_t *name = NULL;
#else
#define PLUG(name, ...) name##_t name;
#endif
LIST_OF_PLUGS
#undef PLUG

const char *libplug_file_name = "./libplug.so";
void *libplug = NULL;

#ifdef HOTRELOAD
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

#define PLUG(name, ...)                                                        \
  name = dlsym(libplug, #name);                                                \
  if (name == NULL) {                                                          \
    fprintf(stderr, "Error: could not %s symbol in %s: %s\n", #name,           \
            libplug_file_name, dlerror());                                     \
    return false;                                                              \
  }
  LIST_OF_PLUGS
#undef PLUG

  return true;
}
#else
#define reload_libplug() true
#endif

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
  plug_init(file_path);

  while (!WindowShouldClose()) {
    if (IsKeyPressed(KEY_R)) {
      Plug *plug = (Plug *)plug_pre_reload();
      if (!reload_libplug())
        return 1;
      plug_post_reload(plug);
    }
    plug_update();
  }
}
