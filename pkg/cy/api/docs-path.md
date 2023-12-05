# doc: Abs

(path/abs path)

Return the full absolute path for `path`. Calls Go's [`path/filepath.Abs`](https://pkg.go.dev/path/filepath#Abs).

# doc: Base

(path/base path)

Return the last element of `path`. Calls Go's [`path/filepath.Base`](https://pkg.go.dev/path/filepath#Base).

# doc: Join

(path/join paths)

Join the elements of the string array `paths` with the OS's file path separator. Calls Go's [`path/filepath.Join`](https://pkg.go.dev/path/filepath#Join).

# doc: Glob

(path/glob pattern)

Return an array of all files matching `pattern`. Calls Go's [`path/filepath.Glob`](https://pkg.go.dev/path/filepath#Glob).
