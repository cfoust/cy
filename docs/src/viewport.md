# Viewport

{{story png placeholder}}

For the time being, `cy` only allows each user to view and interact with one pane at once, which is centered inside of the **viewport** and automatically resized as that user changes the bounds of their terminal. By default, the size of that pane is fixed at 80 columns, but you can change this using the {{api viewport/set-size}} function.

```janet
# Disable centering entirely
(viewport/set-size [0 0])

# "0" means "use the size of my actual terminal"
(viewport/set-size [20 0]) # 80 rows, any number of columns
(viewport/set-size [0 20]) # 20 columns, any number of rows

# Place the pane in a 20x20 box
(viewport/set-size [20 20])
```

The patterned background seen in the screenshot above is referred to as the **frame**. `cy` comes with a [range of different frames](/frames.md). You can choose between all of the available frames using the {{api action/choose-frame}} function, which is bound by default to {{bind :root ctrl+a F}}, and set the default frame on startup using the [`:default-frame`](/default-parameters.md#default-frame) parameter.
