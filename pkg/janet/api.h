#include <janet.h>

Janet access_argv(Janet *argv, int offset);
Janet wrap_result_value(Janet value);
Janet wrap_result_error(const char *message);
const char *cast_janet_string(const uint8_t *jstr);
const char *_pretty_print(Janet value);
Janet wrap_keyword(const char *str);
int tuple_length(const Janet *t);
int get_arity(JanetFunction *callee);
JANET_API JanetTable *go_janet_core_env();

// SourceLoc holds source location info extracted from a fiber's
// stack frame.
typedef struct {
    const char *source; // NULL if unavailable
    int32_t line;       // -1 if unavailable
    int32_t column;     // -1 if unavailable
} SourceLoc;

// fiber_source_loc extracts the source location from the topmost
// frame of a fiber. Returns a SourceLoc with NULLs/-1 if no source
// info is available.
SourceLoc fiber_source_loc(JanetFiber *fiber);
