#include <janet.h>

extern Janet ExecGo(int32_t argc, Janet *argv);

static Janet cfun_go_exec(int32_t argc, Janet *argv) {
    return ExecGo(argc, argv);
}

static const JanetReg cfuns[] = {
    {"go/exec", cfun_go_exec, "(go/exec)\n\nExecute a Go callback."},
    {NULL, NULL, NULL}
};

void apply_env(JanetTable *env) {
    janet_cfuns(env, "go", cfuns);
}

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

Janet evaluate(Janet evaluate, const uint8_t *bytes, int32_t len, const char *sourcePath) {
    Janet args[2] = {
        janet_wrap_string(janet_string(bytes, len)),
        janet_cstringv(sourcePath),
    };

    JanetFiber *fiber = NULL;
    JanetFunction *evaluateFn = janet_unwrap_function(evaluate);
    Janet result;
    janet_pcall(evaluateFn, 2, args, &result, &fiber);
    return result;
}
