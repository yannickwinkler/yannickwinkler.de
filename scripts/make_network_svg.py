"""Generate the stylized network graphics used as header/tile backgrounds.

Writes two single-colour SVGs that the site uses as CSS masks, so the actual
colours come from the theme (light/dark):
  img/network.svg         the full network (nodes + edges)
  img/network-accent.svg  one highlighted path through it ("the wanderer")

Run:  python3 scripts/make_network_svg.py   (deterministic, seeded)
"""

import math
import random
from pathlib import Path

W, H = 1600, 420
SEED = 7
N_CANDIDATES = 4000
MIN_DIST = 46        # spacing between nodes
K_NEIGHBOURS = 3
MAX_EDGE = 150

random.seed(SEED)
out_dir = Path(__file__).resolve().parent.parent / "img"


def density(x):
    """Sparse on the left (where text sits), denser to the right."""
    return 0.08 + 0.92 * (x / W) ** 1.4


def fade(x):
    """Opacity ramp from left to right."""
    return round(0.12 + 0.88 * (x / W) ** 1.2, 3)


# Poisson-disc-like sampling, weighted by density
nodes = []
for _ in range(N_CANDIDATES):
    x, y = random.uniform(10, W - 10), random.uniform(10, H - 10)
    if random.random() > density(x):
        continue
    if all((x - a) ** 2 + (y - b) ** 2 > MIN_DIST ** 2 for a, b in nodes):
        nodes.append((x, y))

# Connect each node to its nearest neighbours
edges = set()
for i, (x, y) in enumerate(nodes):
    near = sorted(
        (math.dist((x, y), p), j) for j, p in enumerate(nodes) if j != i
    )[:K_NEIGHBOURS]
    for d, j in near:
        if d < MAX_EDGE:
            edges.add((min(i, j), max(i, j)))

degree = [0] * len(nodes)
for i, j in edges:
    degree[i] += 1
    degree[j] += 1

adj = {i: [] for i in range(len(nodes))}
for i, j in edges:
    adj[i].append(j)
    adj[j].append(i)

# The wanderer's path: greedy walk from the left-centre towards the right edge
start = min(
    (i for i, (x, _) in enumerate(nodes) if 0.35 * W < x < 0.5 * W),
    key=lambda i: abs(nodes[i][1] - H * 0.62),
)
path = [start]
while nodes[path[-1]][0] < W - 120:
    options = [j for j in adj[path[-1]] if j not in path and nodes[j][0] > nodes[path[-1]][0]]
    if not options:
        break
    path.append(max(options, key=lambda j: nodes[j][0] - 0.6 * abs(nodes[j][1] - H * 0.45)))


def svg(body):
    return (
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" '
        f'preserveAspectRatio="xMaxYMid slice">\n{body}\n</svg>\n'
    )


def line(a, b, width, opacity):
    (x1, y1), (x2, y2) = nodes[a], nodes[b]
    return (
        f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
        f'stroke="#000" stroke-width="{width}" stroke-opacity="{opacity}"/>'
    )


def circle(i, r, opacity):
    x, y = nodes[i]
    return f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r:.1f}" fill="#000" fill-opacity="{opacity}"/>'


base = [line(i, j, 1, fade((nodes[i][0] + nodes[j][0]) / 2) * 0.7) for i, j in sorted(edges)]
base += [circle(i, 1.6 + 0.9 * degree[i], fade(nodes[i][0])) for i in range(len(nodes))]

accent = [line(a, b, 2, 0.9) for a, b in zip(path, path[1:])]
accent += [circle(i, 4 + 2.5 * (k == len(path) - 1), 1) for k, i in enumerate(path)]
# soft halo around the destination node
accent.append(circle(path[-1], 14, 0.18))

(out_dir / "network.svg").write_text(svg("\n".join(base)))
(out_dir / "network-accent.svg").write_text(svg("\n".join(accent)))
print(f"{len(nodes)} nodes, {len(edges)} edges, path of {len(path)} nodes")
