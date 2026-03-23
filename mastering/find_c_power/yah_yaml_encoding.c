#include "yah_yaml.h"
#include <stdio.h>
#include <yaml.h>

int main() {
  printf("libyaml version : %s\n", yaml_get_version_string());

  FILE *yaml_file;
  YAH_OPEN_FILE(yaml_file, "foo.yaml", "w");

  yaml_emitter_t emitter;
  YAH_YAML_EMITTER_INIT(&emitter, yaml_file);

  yaml_event_t event;
  YAH_YAML_START(&emitter, &event, UTF8);

  YAH_YAML_DOC_START(&emitter, &event, 1);
  YAH_YAML_SCALAR(&emitter, &event, "Hello world!", PLAIN);
  YAH_YAML_DOC_END(&emitter, &event, 1);

  YAH_YAML_DOC_START(&emitter, &event, 1);
  const char *seq[] = {"a", "b", "c"};
  size_t n = sizeof(seq) / sizeof(const char *);
  YAH_YAML_SEQ_START(&emitter, &event, BLOCK);
  for (size_t i = 0; i < n; i++) {
    YAH_YAML_SCALAR(&emitter, &event, seq[i], PLAIN);
  }
  YAH_YAML_SEQ_END(&emitter, &event);
  YAH_YAML_DOC_END(&emitter, &event, 1);

  YAH_YAML_DOC_START(&emitter, &event, 1);
  int truth = 42;
  YAH_YAML_MAP_START(&emitter, &event, BLOCK);
  YAH_YAML_KEY(truth, PLAIN);
  YAH_YAML_INT(truth, PLAIN);
  YAH_YAML_MAP_END(&emitter, &event);
  YAH_YAML_DOC_END(&emitter, &event, 1);

  YAH_YAML_DOC_START(&emitter, &event, 1);
  struct foo {
    int a;
    double b;
    const char *c;
  } x = {42, 3.1415926, "Hello world!"};
  YAH_YAML_MAP_START(&emitter, &event, BLOCK);
  YAH_YAML_KEY(x, PLAIN);
  YAH_YAML_MAP_START(&emitter, &event, BLOCK);

  YAH_YAML_KEY(a, PLAIN);
  YAH_YAML_INT(x.a, PLAIN);

  YAH_YAML_KEY(b, PLAIN);
  YAH_YAML_REAL(x.b, PLAIN);

  YAH_YAML_KEY(c, PLAIN);
  YAH_YAML_STR(x.c, PLAIN);
  YAH_YAML_MAP_END(&emitter, &event);
  YAH_YAML_MAP_END(&emitter, &event);

  YAH_YAML_DOC_END(&emitter, &event, 1);

  YAH_YAML_END(&emitter, &event);

  yaml_event_delete(&event);
  yaml_emitter_delete(&emitter);
  fclose(yaml_file);
  return EXIT_SUCCESS;
}
