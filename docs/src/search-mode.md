# Search mode

{{story cast search/query}}

Using **search mode** you can search all of the [terminal sessions you have recorded](/replay-mode.md#recording-to-disk) for a string literal or regex pattern. This works just like the [search functionality](/replay-mode/modes.md#searching) found in replay mode, but over more than one terminal session at once.

You can access search mode using the {{api action/search-borg}} action, which is bound by default to {{bind :root ctrl+a S}}. This action prompts you for a query string and searches for that pattern in all of the `.borg` files present in your {{param data-directory}}.

Unlike replay mode, search mode behaves like a pane in [the node tree](/groups-and-panes.md): you can attach to, detach from, and rename it just like any other pane. The {{api search/new}} API function can be used to create search mode nodes programmatically.

## Usage

After a query finishes executing, search mode will load the `.borg` file for the first result in an instance of replay mode and prepopulate it with the results of the search.

You can move between matches inside of a `.borg` file in the same way you do normally in replay mode, using {{bind :time n}} and {{bind :time N}}. To move between `.borg` files in search mode, use {{bind :search ctrl+n}} and {{bind :search ctrl+p}} to move to the next and previous results, respectively.

It can be convenient to run another search in an existing search mode instance. To do so, hit the {{bind :search :}} key, type a query string, and press enter.

Search mode is not very complicated, but it does have its own [isolated binding scope](/default-keys.md#search-mode) should you wish to rebind its controls.
