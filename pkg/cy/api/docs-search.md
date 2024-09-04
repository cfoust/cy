# doc: New

(search/new parent query &named files workers)

Create a new instance of search mode as a child of `parent` with the given string `query`. `parent` is a [NodeID](/api.md#nodeid). Calling this function will begin executing the search immediately, regardless of whether you attach to the new node.

This function supports a range of named parameters that adjust its functionality:

- `:files` (list of strings): All of the `.borg` files to search in. By default this is populated with all of the `.borg` files present in the user's [`:data-directory`](/default-parameters.md#data-directory).
- `:workers` (integer): The number of workers (threads) to use to perform the search. Defaults to [`:num-search-workers`](/default-parameters.md#num-search-workers).

# doc: Cancel

Cancel the current operation or the input of a query string.

# doc: Next

Move to the next `.borg` file in the search results.

# doc: Prev

Move to the previous `.borg` file in the search results.

# doc: First

Move to the first `.borg` file in the search results.

# doc: Last

Move to the last `.borg` file in the search results.

# doc: FocusInput

Focus search mode's input bar so you can enter a new query string.
