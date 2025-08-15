# doc: Render

(style/render style text)

Apply styling effects to some text. This function generates a string containing ANSI escape sequences that will style the provided `text` according to `style`.

All `cy` API functions that render text to the screen, such as {{api layout/set}} and {{api input/find}} accept input styled with {{api style/render}}.

`style` is a [Style](/api.md#style) struct.

For example:

```janet
(style/render
    {:bg "4"
     :bold true
     :width 15
    } "some text")
```
