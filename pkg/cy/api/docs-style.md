# doc: Render

(style/render style text)

Apply styling effects to some text. This function generates a string containing ANSI escape sequences that will style the provided `text` according to `style`.

All `cy` API functions that render text to the screen, such as {{api layout/set}} and {{api input/find}} accept input styled with {{api style/render}}.

`style` is a struct with any of the following properties:

- `:fg`: The foreground [color](/api.md#color) of the text.
- `:bg`: The background [color](/api.md#color) of the text.
- `:width`: The number of horizontal cells the text should occupy. Padding is added if this value exceeds the length of `text`.
- `:height`: The number of vertical cells the text should occupy. Padding is added if this value exceeds the height of `text`.
- `:align-horizontal`: One of `:left`, `:center`, or `:right`. If `:width` is greater than the length of the text, the text will be aligned according to this property.
- `:align-vertical`: One of `:top`, `:center`, or `:bottom`. If `:height` is greater than the height of the text, the text will be aligned according to this property.
- `:bold`: A boolean indicating whether the text should be bolded.
- `:italic`: A boolean indicating whether the text should be italic.
- `:underline`: A boolean indicating whether the text should be underlined.
- `:strikethrough`: A boolean indicating whether the text should be struck through.
- `:reverse`: A boolean indicating whether the foreground and background colorshould be reversed.
- `:blink`: A boolean indicating whether the text should blink.
- `:faint`: A boolean indicating whether the text should be faint.

For example:

```janet
(style/render
    {:bg "4"
     :bold true
     :width 15
    } "some text")
```
