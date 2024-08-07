(defn
  style/text
  ````Style the provided text with the attributes provided. This function is a convenient wrapper around {{api style/render}}; instead of providing a struct, you may pass any of the attributes {{api style/render}} supports as named parameters.

For example:
```janet
(style/text "foobar" :bg "#00ff00")
(style/text "foobar" :italic true :bold true :width 15)
```
  ````
  [text
   &named
   fg
   bg
   width
   height
   align-horizontal
   align-vertical
   bold
   italic
   underline
   strikethrough
   reverse
   blink
   faint]

  (style/render
    {:fg fg
     :bg bg
     :width width
     :height height
     :align-horizontal align-horizontal
     :align-vertical align-vertical
     :bold bold
     :italic italic
     :underline underline
     :strikethrough strikethrough
     :reverse reverse
     :blink blink
     :faint faint}
    text))
