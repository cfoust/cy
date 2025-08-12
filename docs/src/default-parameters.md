# Default parameters

Note that this document omits the initial `:`. To change the value of a default parameter, you can do something like this:

```janet
(param/set :root :default-frame "big-hex")
```

Values stored to `:persist` are never interpreted as default parameters and do not affect cy's behavior.

{{gendoc params}}
