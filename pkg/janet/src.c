#include <janet.h>

extern void ExecGo(char *callback);

static Janet cfun_go_exec(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 0);
    printf("hello from a module!\n");
    ExecGo("from c");
    return janet_wrap_nil();
}

static const JanetReg cfuns[] = {
    {"exec", cfun_go_exec, "(go/exec)\n\nExecute a Go callback."},
    {NULL, NULL, NULL}
};

void apply_env(JanetTable *env) {
    janet_cfuns(env, "go", cfuns);
}
