#include <janet.h>

Janet access_argv(Janet *argv, int offset);
Janet wrap_result_value(Janet value);
Janet wrap_result_error(const char *message);
const char *cast_janet_string(const uint8_t *jstr);
const char *_pretty_print(Janet value);
Janet wrap_keyword(const char *str);
int tuple_length(const Janet *t);
