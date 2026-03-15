#include <stdio.h>
#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "../nob.h"
#include <dlfcn.h>

char line[1024];

typedef void (*fn_t)(void);

int main(int argc, char **argv) {
  const char *program_name = shift(argv, argc);

  if (argc <= 0) {
    fprintf(stderr, "Usage: %s <input>\n", program_name);
    fprintf(stderr, "ERROR: no input is provided\n");
    return 1;
  }

  const char *dll_path = shift(argv, argc);

  void *dll = dlopen(dll_path, RTLD_NOW);
  if (dll == NULL) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    return 1;
  }

  for (;;) {
    printf("> ");
    if (fgets(line, sizeof(line), stdin) == NULL)
      break;
    fflush(stdout);
    String_View sv = sv_trim(sv_from_cstr(line));
    void *fn = dlsym(dll, temp_sv_to_cstr(sv));
    if (fn == NULL) {
      printf("ERROR: no function " SV_Fmt " found\n", SV_Arg(sv));
    } else {
      ((fn_t)fn)();
    }
    printf("echo: %s", line);
  }

  printf("OK\n");

  return 0;
}
