# janet

Package janet contains a Janet virtual machine for interoperation between Go and Janet code. Users of its API can register callbacks, define symbols in the Janet environment, execute Janet code, and convert between Go and Janet values.

This code is, admittedly, a little terrifying. Suffice it to say that Janet was not designed to be used from Go, and as a result there are a lot of silly tricks this library uses to ensure we only access memory used by the Janet VM in the goroutine where it was initialized.

## Updating Janet

To update the [janet](https://github.com/janet-lang/janet) version, clone the janet-lang/janet repository and run `make`, then copy `build/c/janet.c`, `src/include/janet.h`, and `src/conf/janetconf.h` to this directory.
