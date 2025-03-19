# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "freetype-py",
#     "matplotlib",
#     "numpy",
#     "Pillow==10.0.0",
# ]
# ///
import freetype
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

def render_glyph(
    face,
    char,
    grid_size=(4, 8),
    render_size=(129, 69),
    baseline=30,
):
    """
    Renders a character into a fixed-size occupancy grid.

    :param face: A freetype Face.
    :param char: Character to render.
    :param grid_size: Target occupancy grid size (rows, cols).
    :param render_size: Fixed canvas size for rendering glyph (width, height).
    :return: Occupancy grid (numpy array) and rendered bitmap.
    """
    face.load_char(char)
    bitmap = face.glyph.bitmap

    # Create a blank fixed-size canvas
    fixed_canvas = np.zeros(render_size, dtype=np.uint8)

    # Get glyph bitmap dimensions
    glyph_w, glyph_h = bitmap.width, bitmap.rows

    # Align glyph with baseline
    y_offset = (render_size[0] - (baseline + face.glyph.bitmap_top))
    x_offset = (render_size[1] - glyph_w) // 2  # Center horizontally
    print(char, render_size, baseline, face.glyph.bitmap_top)

    # Insert the glyph into the fixed canvas (only if it has width/height)
    fixed_canvas[y_offset:y_offset+glyph_h, x_offset:x_offset+glyph_w] = \
        np.array(bitmap.buffer, dtype=np.uint8).reshape(glyph_h, glyph_w)

    # Resize to grid dimensions using PIL
    img = Image.fromarray(fixed_canvas)
    img_resized = img.resize(grid_size, Image.Resampling.BOX)

    # Convert to binary occupancy grid (threshold)
    occupancy_grid = np.array(img_resized) > 0  # Boolean mask
    occupancy_grid = occupancy_grid.astype(int)  # Convert to 0/1

    normal = Image.fromarray((occupancy_grid * 255).astype(np.uint8), mode='L').resize(
        (render_size[1], render_size[0]),
        Image.Resampling.BOX,
    )

    return occupancy_grid, fixed_canvas, np.array(normal)

def plot_grid(grid):
    plt.imshow(grid, cmap="gray_r", interpolation="nearest")
    plt.xticks([])
    plt.yticks([])
    plt.show()

symbols = [
    # "Standard" common characters
    ',', '_', '-', '"', ']', '[', '}', '{', 'v', 'V',
    '<', '>', '\'', 'Я', '⣳',

    # Light box-drawing characters
    '─', '│', '┌', '┐', '└', '┘', '├', '┤', '┬', '┴', '┼',
    '╭', '╮', '╯', '╰', '╱', '╲', '╳',

    # Legacy computing symbols
    # '🯐', '🯑', '🯒', '🯓', '🯔', '🯕', '🯖', '🯗', '🯘', '🯙', '🯚',
    # '🯛', '🯜', '🯝', '🯞', '🯟',
    # '🯠', '🯡', '🯢', '🯣',
    # '🭱','🭲','🭳','🭴','🭵',
]

face = freetype.Face("./CozetteVector.ttf")
face.set_char_size(128 * 64)  # Set font size in p

def compute_occupancies(
    face,
    symbols,
):
    if not symbols:
        return None

    # Compute the bounds
    max_width = 0
    max_height = 0
    max_descent = 0
    for symbol in symbols:
        face.load_char(symbol)
        bitmap = face.glyph.bitmap

        descent = bitmap.rows - face.glyph.bitmap_top

        max_width = max(max_width, bitmap.width)
        max_height = max(max_height, bitmap.rows)
        max_descent = max(max_descent, descent)

    size = (max_height, max_width)

    num_symbols = len(symbols)
    num_columns = 5
    num_rows = (num_symbols // num_columns) + 1
    preview_bounds = (max_height, max_width * 2)
    out_canvas = np.zeros(
        (num_rows*max_height, num_columns * max_width * 2),
        dtype=np.uint8,
    )

    for i, symbol in enumerate(symbols):
        y_offset = (i // num_columns) * max_height
        x_offset = ((i % num_columns) * max_width * 2)

        _, bitmap, occupancy = render_glyph(
            face,
            symbol,
            (4, 8),
            size,
            max_descent,
        )

        out_canvas[y_offset:y_offset+max_height, x_offset:x_offset+max_width] = \
            bitmap

        x_offset += max_width
        out_canvas[
            y_offset:y_offset+max_height, x_offset:x_offset+max_width
        ] = \
            occupancy

    plot_grid(out_canvas)

compute_occupancies(face, symbols)

# Show the occupancy grid
# plot_grid(occupancy_grid)
# plot_grid(occupancy_grid)
