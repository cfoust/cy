#include <janet.h>

extern Janet ExecGo(int32_t argc, Janet *argv);

static Janet cfun_go_exec(int32_t argc, Janet *argv) {
    return ExecGo(argc, argv);
}

static const JanetReg cfuns[] = {
    {"exec", cfun_go_exec, "(go/exec)\n\nExecute a Go callback."},
    {NULL, NULL, NULL}
};

void apply_env(JanetTable *env) {
    janet_cfuns(env, "go", cfuns);
}

Janet wrap_error_result(const char *message) {
    Janet parts[2] = {
        janet_wrap_keyword(janet_cstring("error")),
        janet_wrap_string(janet_cstring(message)),
    };

    return janet_wrap_tuple(janet_tuple_n(parts, 2));
}
