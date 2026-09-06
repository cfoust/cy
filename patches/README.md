# Vendor patches

cy carries a few small local modifications to vendored Go modules. `go mod
vendor` overwrites the vendored sources with pristine upstream copies, so the
modifications live here as patches (relative to the repository root) and are
re-applied by `just vendor`.

| Patch | Module | What it does |
| --- | --- | --- |
| `golang.design-x-clipboard.patch` | `golang.design/x/clipboard` | Adds `ReadErr` and makes `Write` return an `error` so clipboard failures reach the user instead of being swallowed. |
| `github.com-danielgatis-go-vte.patch` | `github.com/danielgatis/go-vte` | Colon (`:`) sub-parameter support in CSI sequences, used for styled underlines. |
| `github.com-sevlyar-go-daemon.patch` | `github.com/sevlyar/go-daemon` | Drops the string wrapping of lock-file errors so callers can inspect the underlying error. |

## Updating a patched module

1. Bump the version in `go.mod`.
2. Run `just vendor`. If a patch no longer applies cleanly, `git apply` will
   say so; fix up the vendored file by hand, then regenerate the patch:

   ```bash
   diff -u --label a/vendor/<mod>/<file> --label b/vendor/<mod>/<file> \
     "$(go env GOMODCACHE)/<mod>@<version>/<file>" vendor/<mod>/<file> \
     > patches/<name>.patch
   ```

3. Run `just vendor` again and confirm `git status` shows no unexpected
   changes under `vendor/`.
