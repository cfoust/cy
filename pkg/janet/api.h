#include <janet.h>

void apply_env(JanetTable *env);
Janet access_argv(Janet *argv, int offset);
Janet wrap_result_value(Janet value);
Janet wrap_result_error(const char *message);
const char *cast_janet_string(const uint8_t *jstr);
const char *_pretty_print(Janet value);
