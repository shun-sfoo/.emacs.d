#pragma once

#define YAH_OPEN_FILE(file, name, mode)                                        \
  do {                                                                         \
    file = fopen(name, mode);                                                  \
    if (!file) {                                                               \
      fprintf(stderr, "failed to open %s", name);                              \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  } while (0)

#define YAH_YAML_EMITTER_INIT(emitter, file)                                   \
  do {                                                                         \
    if (!yaml_emitter_initialize(emitter)) {                                   \
      fprintf(stderr, "failed to initialize yaml emitter!\n");                 \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    yaml_emitter_set_output_file(emitter, file);                               \
  } while (0)

#define YAH_YAML_EMIT(emitter, event)                                          \
  do {                                                                         \
    if (!yaml_emitter_emit(emitter, event)) {                                  \
      fprintf(stderr, "failed to emit event %d because %s.\n", (event)->type,  \
              (emitter)->problem);                                             \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  } while (0)

#define YAH_YAML_START(emitter, event, encoding)                               \
  do {                                                                         \
    if (!yaml_stream_start_event_initialize(event,                             \
                                            YAML_##encoding##_ENCODING)) {     \
      fprintf(stderr, "failed to initialize *stream_start* event.\n");         \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_END(emitter, event)                                           \
  do {                                                                         \
    if (!yaml_stream_end_event_initialize(event)) {                            \
      fprintf(stderr, "failed to initialize *stream_end* event.\n");           \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_DOC_START(emitter, event, implicit)                           \
  do {                                                                         \
    if (!yaml_document_start_event_initialize(event, NULL, NULL, NULL,         \
                                              implicit)) {                     \
      fprintf(stderr, "failed to initialize *document_start* event.\n");       \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_DOC_END(emitter, event, implicit)                             \
  do {                                                                         \
    if (!yaml_document_end_event_initialize(event, implicit)) {                \
      fprintf(stderr, "failed to initialize *document_start* event.\n");       \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_SCALAR(emitter, event, string, style)                         \
  do {                                                                         \
    if (!yaml_scalar_event_initialize(                                         \
            event, NULL, NULL, (const yaml_char_t *)(string), strlen(string),  \
            1, 1, YAML_##style##_SCALAR_STYLE)) {                              \
      fprintf(stderr, "failed to initialize *scalar* event.\n");               \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_SEQ_START(emitter, event, style)                              \
  do {                                                                         \
    if (!yaml_sequence_start_event_initialize(                                 \
            event, NULL, NULL, 1, YAML_##style##_SEQUENCE_STYLE)) {            \
      fprintf(stderr, "failed to initialize *sequence_start* event.\n");       \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_SEQ_END(emitter, event)                                       \
  do {                                                                         \
    if (!yaml_sequence_end_event_initialize(event)) {                          \
      fprintf(stderr, "failed to initialize *sequence_end* event.\n");         \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_MAP_START(emitter, event, style)                              \
  do {                                                                         \
    if (!yaml_mapping_start_event_initialize(event, NULL, NULL, 1,             \
                                             YAML_##style##_MAPPING_STYLE)) {  \
      fprintf(stderr, "failed to initialize *mapping_start* event.\n");        \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_MAP_END(emitter, event)                                       \
  do {                                                                         \
    if (!yaml_mapping_end_event_initialize(event)) {                           \
      fprintf(stderr, "failed to initialize *mapping_end* event.\n");          \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
    YAH_YAML_EMIT(emitter, event);                                             \
  } while (0)

#define YAH_YAML_KEY(name, style)                                              \
  do {                                                                         \
    const char *key = #name;                                                   \
    YAH_YAML_SCALAR(&emitter, &event, key, style);                             \
  } while (0)

#define YAH_YAML_INT(integer, style)                                           \
  do {                                                                         \
    char value[1024];                                                          \
    snprintf(value, sizeof(value), "%d", integer);                             \
    YAH_YAML_SCALAR(&emitter, &event, value, style);                           \
  } while (0)

#define YAH_YAML_REAL(real, style)                                             \
  do {                                                                         \
    char value[1024];                                                          \
    snprintf(value, sizeof(value), "%f", real);                                \
    YAH_YAML_SCALAR(&emitter, &event, value, style);                           \
  } while (0)

#define YAH_YAML_STR(string, style)                                            \
  do {                                                                         \
    YAH_YAML_SCALAR(&emitter, &event, string, style);                          \
  } while (0)
