"""Make 1-bit dithered (Atkinson) versions of the avatar.

Writes transparent PNGs at display size (shown with image-rendering:
pixelated so every dot stays crisp), for the home tile (160 px) and the
about page (240 px):
  img/yw_dither_light[_240].png  ink dots for the light (paper) theme
  img/yw_dither_dark[_240].png   cream dots for the dark theme (inverted, so
                                 it doesn't look like a photo negative)
  img/favicon.png                64 px, ink dots on a round paper disc

Run:  python3 scripts/make_dither_avatar.py
"""

from pathlib import Path

from PIL import Image, ImageDraw, ImageEnhance, ImageFilter, ImageOps

SIZES = {160: "", 240: "_240"}   # CSS display size in px -> file suffix
INK = (31, 29, 26)               # $body-color, theme-light.scss
CREAM = (235, 230, 220)          # $body-color, theme-dark.scss
PAPER = (245, 242, 235)          # $body-bg, theme-light.scss

root = Path(__file__).resolve().parent.parent


def prepare(size):
    img = Image.open(root / "img" / "yw_avatar.jpg").convert("L")
    img = img.resize((size, size), Image.LANCZOS)
    img = ImageOps.autocontrast(img, cutoff=1)
    img = ImageEnhance.Contrast(img).enhance(1.25)
    return img.filter(ImageFilter.UnsharpMask(radius=1.2, percent=120, threshold=2))


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
    size = len(ink_map)
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    for y, row in enumerate(ink_map):
        for x, on in enumerate(row):
            if on != invert:
                out.putpixel((x, y), (*colour, 255))
    return out


def favicon(size=64):
    """Ink dots on a round paper disc, so it stays visible on dark browser tabs."""
    dots = render(atkinson(prepare(size)), INK)
    disc = Image.new("L", (size * 4, size * 4), 0)
    ImageDraw.Draw(disc).ellipse((0, 0, size * 4 - 1, size * 4 - 1), fill=255)
    out = Image.new("RGBA", (size, size), (*PAPER, 0))
    out.putalpha(disc.resize((size, size), Image.LANCZOS))
    out.alpha_composite(Image.composite(dots, Image.new("RGBA", (size, size)), out.getchannel("A")))
    return out


for size, suffix in SIZES.items():
    src = prepare(size)
    render(atkinson(src), INK).save(root / "img" / f"yw_dither_light{suffix}.png", optimize=True)
    render(atkinson(ImageOps.invert(src)), CREAM).save(root / "img" / f"yw_dither_dark{suffix}.png", optimize=True)
    print(f"wrote img/yw_dither_light{suffix}.png, img/yw_dither_dark{suffix}.png")

favicon().save(root / "img" / "favicon.png", optimize=True)
print("wrote img/favicon.png")
