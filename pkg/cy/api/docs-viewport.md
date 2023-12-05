# doc: Size

Get the size of the inner viewport. Returns a tuple `[rows, columns]`.

# doc: SetSize

(viewport/set-size size)

Set the size of the inner viewport. `size` is a tuple in the form `[rows, columns]`.

# doc: SetFrame

(viewport/set-frame frame)

Set the frame to `frame`, which is an identifier for the desired frame.

You can get all of the available frames with [`(viewport/get-frames)`](api.md#viewportget-frames) and also browse them [here](./frames.md).

# doc: GetFrames

Get a list of all of the available [frames](./frames.md).
