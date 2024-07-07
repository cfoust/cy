# janet

Package janet contains a Janet virtual machine for interoperation between Go and Janet code. Users of its API can register callbacks, define symbols in the Janet environment, execute Janet code, and convert between Go and Janet values.

## Updating Janet

To update the [janet](https://github.com/janet-lang/janet) version, clone the janet-lang/janet repository and run `make`, then copy `build/c/janet.c`, `src/include/janet.h`, and `src/conf/janetconf.h` to this directory.
