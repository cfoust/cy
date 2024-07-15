# Using the viewport

When you first connect, `cy` creates a new **pane** and attaches to it. By default, it runs your default shell. `cy` centers the pane and fills the rest of the horizontal space with a patterned background. `cy` refers to the state of your screen as the **viewport**.

{{story png placeholder}}

All actions in `cy`, such as creating panes and switching between them, are triggered by sequences of keys.

{{story cast cy/viewport --width 120 --height 26}}

Here are a few you can try:

1. To make the pane fill the entire viewport, type {{bind :root ctrl+a g}}. (Repeat to center it again.)
1. To set the width of the pane to 80 columns, type {{bind :root ctrl+a 1}}.
1. To set it to 160 columns, type {{bind :root ctrl+a 2}}.
1. To increase the width by 5 columns, type {{bind :root ctrl+a +}}.
1. To decrease the width by 5 columns, type {{bind :root ctrl+a -}}.
