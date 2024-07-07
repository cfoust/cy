# bind

Package bind is a key binding engine. It checks incoming key events against all registered key bindings to determine whether an action should be fired. bind uses a trie data structure, implemented in the bind/trie package, to describe sequences of keys.

As distinct from a traditional trie, in which nodes have a fixed value, bind's trie also supports regex values. Key events are stringified and then compared against the regex pattern to determine if the state machine should transition to that node.
