# Regenerates apple.ansi (the MOTD) from a reference image.
#
#   sips -s format bmp classic.png --out classic.bmp
#   python3 -I trace_logo.py classic.bmp 72 > apple.ansi
#
# Args: BMP path (32-bit, from sips), total width in columns, optional "plain" to preview
# without colour. Keeps only the colour stripes (apple) and near-black pixels (wordmark).
# The source image is not tracked in this repo.
import struct, sys
src, C = sys.argv[1], int(sys.argv[2])
plain = len(sys.argv) > 3 and sys.argv[3] == "plain"
d = open(src, "rb").read()
off = struct.unpack_from("<I", d, 10)[0]
w, h = struct.unpack_from("<ii", d, 18)
top_down = h < 0; h = abs(h)
def px(x, y):
    row = y if top_down else h-1-y
    b, g, r, a = d[off+(row*w+x)*4: off+(row*w+x)*4+4]
    return r, g, b
def kind(x, y):
    r, g, b = px(x, y); mx, mn = max(r, g, b), min(r, g, b)
    if mx > 60 and (mx-mn)/mx > 0.35: return 1      # apple stripe colours
    if mx < 100: return 2                            # black wordmark
    return 0
pts = [(x, y, kind(x, y)) for y in range(h) for x in range(w)]
ap = [(x, y) for x, y, k in pts if k == 1]
wd = [(x, y) for x, y, k in pts if k == 2]
x0 = min(p[0] for p in ap); x1 = max(p[0] for p in wd)
y0 = min(p[1] for p in ap); y1 = max(p[1] for p in ap)
bw, bh = x1-x0+1, y1-y0+1
sx = bw/C; R = max(2, round(bh/(2*sx))); sy = bh/R
def cell(cx, cy):
    xs = range(int(x0+cx*sx), max(int(x0+(cx+1)*sx), int(x0+cx*sx)+1))
    ys = range(int(y0+cy*sy), max(int(y0+(cy+1)*sy), int(y0+cy*sy)+1))
    n = a = k = 0
    for y in ys:
        for x in xs:
            n += 1; v = kind(x, y); a += v == 1; k += v == 2
    return a/n, k/n
RAMP = [(0.12," "),(0.30,"."),(0.50,":"),(0.72,"+"),(0.90,"#"),(9,"@")]
ch = lambda f: next(c for t, c in RAMP if f < t)
pal = {"97;189;79": (97,189,79), "253;184;39": (253,184,39), "245;130;31": (245,130,31),
       "224;58;62": (224,58,62), "150;61;151": (150,61,151), "0;157;220": (0,157,220)}
snap = lambda rgb: min(pal, key=lambda k: sum((a-b)**2 for a, b in zip(pal[k], rgb)))
lines = []
for r in range(R):
    ya, yb = int(y0+r*sy), int(y0+(r+1)*sy)
    samp = sorted((px(x, y) for y in range(ya, max(yb, ya+1)) for x in range(x0, x1+1) if kind(x, y) == 1),
                  key=sum)
    col = snap(samp[len(samp)//2]) if samp else None
    runs, cur, curc = [], "", None
    for c in range(C):
        a, k = cell(c, r)
        f, kd = (a, 1) if a >= k else (k, 2)
        s = ch(f)
        cc = (col if kd == 1 else "") if s != " " else curc
        if cc != curc and s != " ":
            if cur: runs.append((curc, cur))
            cur, curc = "", cc
        cur += s
    if cur: runs.append((curc, cur))
    if runs: runs[-1] = (runs[-1][0], runs[-1][1].rstrip())
    if plain:
        lines.append("".join(t for _, t in runs))
    else:
        lines.append("".join(f"\033[38;2;{c}m{t}\033[0m" if c else t for c, t in runs))
print(f"bbox {bw}x{bh} -> {C} cols x {R} rows", file=sys.stderr)
print("\n".join(lines))
