# Viewport

For the time being, `cy` only allows each user to view and interact with one pane at once, which is centered inside of the **viewport** and automatically resized as that user changes the bounds of their terminal.

By default, `cy` restricts that pane to a width of 80 columns or the width of your terminal window, whichever is smaller, but this is configurable. This is practical because `cy` makes it easy to switch between panes.

`cy` has two main abstractions for showing terminal art in the viewport:

- **frames**: static, configurable backgrounds that `cy` uses to fill the empty space in the viewport
- **animations:** shown on the splash screen and when [fuzzy finding](./fuzzy-finding.md)
