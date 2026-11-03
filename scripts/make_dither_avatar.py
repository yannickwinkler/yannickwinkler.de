"""Make 1-bit dithered (Atkinson) versions of the avatar.

Writes two transparent PNGs at display size (shown with image-rendering:
pixelated so every dot stays crisp):
  img/yw_dither_light.png   ink dots for the light (paper) theme
  img/yw_dither_dark.png    cream dots for the dark theme (inverted, so it
                            doesn't look like a photo negative)

Run:  python3 scripts/make_dither_avatar.py
"""

from pathlib import Path

from PIL import Image, ImageEnhance, ImageFilter, ImageOps

SIZE = 160                       # CSS display size in px
INK = (31, 29, 26)               # $body-color, theme-light.scss
CREAM = (235, 230, 220)          # $body-color, theme-dark.scss

root = Path(__file__).resolve().parent.parent
src = Image.open(root / "img" / "yw_avatar.jpg").convert("L")
src = src.resize((SIZE, SIZE), Image.LANCZOS)
src = ImageOps.autocontrast(src, cutoff=1)
src = ImageEnhance.Contrast(src).enhance(1.25)
src = src.filter(ImageFilter.UnsharpMask(radius=1.2, percent=120, threshold=2))


def atkinson(img):
    """Atkinson dithering: returns a set of 'ink' pixels (dark areas)."""
    w, h = img.size
    px = [[float(img.getpixel((x, y))) for x in range(w)] for y in range(h)]
    ink = [[False] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            old = px[y][x]
            new = 255.0 if old > 127 else 0.0
            ink[y][x] = new == 0.0
            err = (old - new) / 8
            for dx, dy in ((1, 0), (2, 0), (-1, 1), (0, 1), (1, 1), (0, 2)):
                nx, ny = x + dx, y + dy
                if 0 <= nx < w and 0 <= ny < h:
                    px[ny][nx] += err
    return ink


def render(ink_map, colour, invert=False):
    out = Image.new("RGBA", (SIZE, SIZE), (0, 0, 0, 0))
    for y, row in enumerate(ink_map):
        for x, on in enumerate(row):
            if on != invert:
                out.putpixel((x, y), (*colour, 255))
    return out


render(atkinson(src), INK).save(root / "img" / "yw_dither_light.png", optimize=True)
render(atkinson(ImageOps.invert(src)), CREAM).save(root / "img" / "yw_dither_dark.png", optimize=True)
print("wrote img/yw_dither_light.png, img/yw_dither_dark.png")
