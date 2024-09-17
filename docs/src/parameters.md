# Parameters

`cy` has a key-value store referred to as **parameters**. In addition to being available for use from Janet for arbitrary purposes, parameters are also the primary means of configuring `cy`'s behavior.

Parameters are set with {{api param/set}} and retrieved with {{api param/get}}:

```janet
(param/set :root :some-parameter true)

(param/get :some-parameter)
# returns true
```

Parameters work just like bindings do in the [node tree](/groups-and-panes.md#the-node-tree) in that the parameter values in descendant nodes overrite those in their ancestors. This allows `cy`'s functionality to be configured on a per-group (or even per-pane) basis.

`cy`'s settings are known as **default parameters**. They are set and retrieved in the same way normal parameters are, but influence `cy`'s behavior in ways not directly controllable using the API. A complete list can be found in [the default parameters chapter](/default-parameters.md).
