# Viewport

{{story png placeholder}}

For the time being, `cy` only allows each user to view and interact with one pane at once, which is centered inside of the **viewport** and automatically resized as that user changes the bounds of their terminal.

By default, `cy` restricts that pane to a width of 80 columns or the width of your terminal window, whichever is smaller, but this is configurable. This is practical because `cy` makes it easy to switch between panes.

The patterned background seen in the screenshot above is referred to as the **frame**. `cy` comes with a [range of different frames](/frames.md). You can choose between all of the available frames using the {{api action/choose-frame}} function, which is bound by default to {{bind :root ctrl+a F}}, and set the default frame on startup using the [`:default-frame`](/default-parameters.md#default-frame) parameter.
