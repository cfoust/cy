#include <janet.h>
#include <json.h>
#include <api.h>

Janet wrap_result_value(Janet value) {
    Janet parts[2] = {
        janet_ckeywordv("value"),
        value,
    };

    return janet_wrap_tuple(janet_tuple_n(parts, 2));
}

Janet wrap_result_error(const char *message) {
    Janet parts[2] = {
        janet_ckeywordv("error"),
        janet_wrap_string(janet_cstring(message)),
    };

    return janet_wrap_tuple(janet_tuple_n(parts, 2));
}

Janet access_argv(Janet *argv, int offset) {
    return argv[offset];
}

const char *cast_janet_string(const uint8_t *jstr) {
    return (const char *)jstr;
}

const char *_pretty_print(Janet value) {
    JanetBuffer *buffer = janet_buffer(32768);
    janet_pretty(buffer, 20, 0, value);
    return (const char *)buffer->data;
}

Janet wrap_keyword(const char *str) {
    return janet_ckeywordv(str);
}

int tuple_length(const Janet *t) {
    return janet_tuple_length(t);
}

int get_arity(JanetFunction *callee) {
    return callee->def->arity;
}

JANET_API JanetTable *go_janet_core_env() {
    JanetTable *env = janet_core_env(NULL);
    module_json(env);
    return env;
}

SourceLoc fiber_source_loc(JanetFiber *fiber) {
    SourceLoc loc = {NULL, -1, -1};
    if (fiber == NULL) return loc;

    int32_t i = fiber->frame;
    while (i > 0) {
        JanetStackFrame *frame = (JanetStackFrame *)(fiber->data + i - JANET_FRAME_SIZE);
        if (frame->func != NULL) {
            JanetFuncDef *def = frame->func->def;
            if (def->source != NULL) {
                loc.source = (const char *)def->source;
            }
            if (def->sourcemap != NULL) {
                int32_t off = (int32_t)(frame->pc - def->bytecode);
                if (off >= 0 && off < def->bytecode_length) {
                    loc.line = def->sourcemap[off].line;
                    loc.column = def->sourcemap[off].column;
                }
            }
            return loc;
        }
        i = frame->prevframe;
    }

    return loc;
}
