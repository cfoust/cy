# Parameters

`cy` contains a key-value referred to as **parameters**. In addition to being available for use from Janet for arbitrary purposes, parameters are also the primary means of configuring `cy`'s behavior.

Parameters are set with {{api param/set}} and retrieved with {{api param/get}}:

```janet
(param/set :root :some-parameter true)

(param/get :some-parameter)
# returns true
```

Parameters work just like bindings do in the [node tree](./groups-and-panes.md#the-node-tree) in that the parameter values in descendant nodes overrite those in their ancestors when a client is attached to them. This allows `cy`'s functionality to be configured on a per-group (or even per-pane) basis using **default parameters**, which are described below.

`cy`'s settings are ordinary parameters known as **default parameters**. A complete list can be found below. Note that the initial `:` has been omitted. To change the value of a default parameter, you can do something like this:

```janet
(param/set :root :default-frame "big-hex")
```

## Default parameters

{{gendoc params}}
