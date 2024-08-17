# Changing the layout

When you first connect, `cy` creates a new **pane** and attaches to it.

By default, `cy` centers the pane and fills the rest of the horizontal space with a patterned background it calls the [**frame**](/frames.md). If your terminal is smaller than the minimum number of columns (80 by default), `cy` will not show a background.

{{story png placeholder}}

`cy` refers to the configuration of the content shown on your screen as the [**layout**](/layouts.md). Unlike `tmux`, every client on the `cy` server has their own layout that can be configured independently of other clients. The layout shown above has a single pane that has its horizontal sized fixed to 80 columns.

### Panes and splits

{{story cast quick-start/layout --width 120 --height 26}}

`cy`'s has pane and split functionality that should feel familiar to users of tmux. Like other terminal multiplexers, in `cy` you can divide your screen up into any number of panes. This can be done with {{bind :root ctrl+a -}} to split the current pane along a horizontal line and {{bind :root ctrl+a |}} to split along a vertical one.

After creating a few, you can move between them using directional keys:

- {{bind :root ctrl+a K}} or {{bind :root ctrl+a up}} to move up
- {{bind :root ctrl+a J}} or {{bind :root ctrl+a down}} to move down
- {{bind :root ctrl+a H}} or {{bind :root ctrl+a left}} to move left
- {{bind :root ctrl+a L}} or {{bind :root ctrl+a right}} to move right

### Tabs

{{story cast layout/tabs}}

`cy` also has tabs:

- {{bind :root ctrl+a t}} to create a new tab
- {{bind :root ctrl+a R}} to rename the current tab
- {{bind :root ctrl+a tab}} to move to the next tab
- {{bind :root ctrl+a shift+tab}} to move to the previous tab

### Removing layout nodes

You can also remove panes from your layout with {{bind :root ctrl+a x}}. However, it is important to note that unlike in other terminal multiplexers, removing a pane from your layout does not kill it; you can still attach to it again, such as by using {{bind :root ctrl+a ;}}. To remove it from your layout and kill the underlying process, you can use {{bind :root ctrl+a X}}.

### Margins

The outer margins can be adjusted or disabled with a few keybindings. Note that **these will only have an effect if your terminal is wider than 80 columns and the margins are visible.**

1. To make the inner pane fill the entire viewport, type {{bind :root ctrl+a g}}.
1. To set the width of the inner pane to 80 columns, type {{bind :root ctrl+a 1}}.
1. To set it to 160 columns, type {{bind :root ctrl+a 2}}.

{{story cast quick-start/margins --width 180 --height 40}}
