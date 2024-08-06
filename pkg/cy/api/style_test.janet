(test "render"
      (style/render {:fg "#0000ff"
                     :bg "#ff0000"
                     :width 15
                     :italic true
                     :align-horizontal :right} "test"))
