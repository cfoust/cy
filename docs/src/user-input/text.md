# Text

You can prompt the user for freeform text input using the {{api input/text}} API function. The `(input/text)`, like {{api input/find}}, also animates the background while the user manipulates the value in the text input.

`(input/text)` lets you provide both placeholder text (shown when the form is empty) and preset text (to pre-fill the text input.) See the API listing for more details.

Here are some examples:


### Preset text

```janet
(input/text "preset example" :preset "preset text")
```

{{story cast input/text/preset --width 40 --height 10}}

### Placeholder text

```janet
(input/text "placeholder example" :placeholder "placeholder")
```

{{story cast input/text/placeholder --width 40 --height 10}}
