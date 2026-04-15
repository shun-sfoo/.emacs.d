#pragma once
#include <complex.h>
#include <raylib.h>

typedef struct {
  Music music;
} Plug;

#define LIST_OF_PLUGS                                                          \
  PLUG(plug_hello, void, void)                                                 \
  PLUG(plug_init, void, const char *file_path)                                 \
  PLUG(plug_pre_reload, void *, void)                                          \
  PLUG(plug_post_reload, void, Plug *plug)                                     \
  PLUG(plug_update, void, void)

#define PLUG(name, ret, ...) typedef ret(name##_t)(__VA_ARGS__);
LIST_OF_PLUGS
#undef PLUG
