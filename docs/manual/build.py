#!/usr/bin/env python3
# Lays out the Fancy Pencil manual on 10 x 12 inch portrait pages and writes it as an
# OpenDocument presentation (.odp). Every line is measured with the real fonts so the text
# frames fit; long paragraphs and tables carry on to the next page.
import re, sys, os
from PIL import ImageFont, Image
from odf.opendocument import OpenDocumentPresentation
from odf.style import (Style, PageLayout, PageLayoutProperties, MasterPage, GraphicProperties,
                       ParagraphProperties, TextProperties, DrawingPageProperties, FontFace)
from odf.draw import Page, Frame, TextBox, Image as DImage, Rect, Ellipse, Line
from odf.text import P, Span, LineBreak, Tab, S as TSpace
import content

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "Fancy Pencil Manual.odp")

PW, PH = 10.0, 12.0                    # page, inches
ML, MR, MT, MB = 0.8, 0.8, 0.8, 0.9    # margins
CW = PW - ML - MR                      # content width
BOTTOM = PH - MB

NAVY, GOLD, INK, DIM, RULE, TINT, NOTE = "#1f2b3c", "#b8860b", "#1c2430", "#5d6878", "#c9d1dc", "#eef2f6", "#fff6dc"
SANS, MONO = "Liberation Sans", "Liberation Mono"
FF = {
    ("r", False): "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    ("b", False): "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf",
    ("i", False): "/usr/share/fonts/truetype/liberation/LiberationSans-Italic.ttf",
    ("m", False): "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
    ("bm", False): "/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf",
}
_fonts = {}
def font(kind, pt):
    k = (kind, pt)
    if k not in _fonts:
        _fonts[k] = ImageFont.truetype(FF[(kind, False)], int(round(pt * 10)))
    return _fonts[k]
def text_w(s, kind, pt):           # inches
    return font(kind, pt).getlength(s) / 720.0

# ---------------------------------------------------------------- inline markup -------------

def parse(s, base="r"):
    """'**bold** and `mono`' -> [(text, kind)]; kinds r b m bm i."""
    out = []
    for part in re.split(r"(\*\*.+?\*\*|`[^`]+`)", s):
        if not part: continue
        if part.startswith("**"):
            inner = part[2:-2]
            for q in re.split(r"(`[^`]+`)", inner):
                if not q: continue
                out.append((q[1:-1], "bm") if q.startswith("`") else (q, "b"))
        elif part.startswith("`"):
            out.append((part[1:-1], "m"))
        else:
            out.append((part, base))
    return out

def words(segs):
    """[(word, kind, space_before)] - words keep their style; a word may span segments."""
    toks = []
    for text, kind in segs:
        pieces = re.split(r"( +)", text)
        for pc in pieces:
            if pc == "": continue
            if pc.startswith(" "):
                toks.append((None, kind)); continue
            toks.append((pc, kind))
    # merge into words: list of list[(text,kind)] with space flags
    out, cur, sp = [], [], False
    for t, k in toks:
        if t is None:
            if cur: out.append((cur, sp)); cur = []
            sp = True
        else:
            if not cur and out == [] and sp is None: pass
            cur.append((t, k))
    if cur: out.append((cur, sp))
    # space flag means "space before this word"
    res = []
    for i, (w, s) in enumerate(out):
        res.append((w, s if i > 0 else False))
    return res

def size_for(kind, pt):            # mono runs a little smaller, like code in prose
    return pt * 0.92 if kind in ("m", "bm") else pt

def wrap(segs, width, pt, first_indent=0.0):
    """Lines as lists of words; uses the real font metrics."""
    ws = words(segs)
    lines, line, x = [], [], 0.0
    spw = text_w(" ", "r", pt)
    for w, sp in ws:
        wlen = sum(text_w(t, k, size_for(k, pt)) for t, k in w)
        avail = width - (first_indent if not lines else 0)
        add = (spw if (sp and line) else 0) + wlen
        if line and x + add > avail * 0.955:
            lines.append(line); line, x = [], 0.0
            add = wlen
        line.append((w, sp and bool(line)))
        x += add
    if line: lines.append(line)
    return lines

def line_segs(lines):
    """Lines -> flat [(text, kind)] for rendering one paragraph (LibreOffice re-wraps it the same)."""
    out = []
    for li, line in enumerate(lines):
        for wi, (w, sp) in enumerate(line):
            if (wi > 0 and sp) or (li > 0 and wi == 0):
                out.append((" ", w[0][1] if w[0][1] in ("r", "b", "i") else "r"))
            out.extend(w)
    # merge neighbours of the same kind
    m = []
    for t, k in out:
        if m and m[-1][1] == k: m[-1] = (m[-1][0] + t, k)
        else: m.append((t, k))
    return m

# ---------------------------------------------------------------- ODF document --------------

doc = OpenDocumentPresentation()
for name, fam in ((SANS, "swiss"), (MONO, "modern")):
    doc.fontfacedecls.addElement(FontFace(name=name, fontfamily=name, fontfamilygeneric=fam,
                                          fontpitch="fixed" if fam == "modern" else "variable"))
pl = PageLayout(name="PM1")
pl.addElement(PageLayoutProperties(margin="0cm", pagewidth="%.3fin" % PW, pageheight="%.3fin" % PH,
                                   printorientation="portrait"))
doc.automaticstyles.addElement(pl)
dps = Style(name="dp1", family="drawing-page")
dps.addElement(DrawingPageProperties(backgroundvisible="false", backgroundobjectsvisible="false",
                                     fill="solid", fillcolor="#ffffff"))
doc.automaticstyles.addElement(dps)
master = MasterPage(name="Manual", pagelayoutname=pl)
doc.masterstyles.addElement(master)

_gstyles, _pstyles, _tstyles = {}, {}, {}
def gstyle(fill=None, stroke=None, sw="0.01in", valign="top", pad="0in", radius=None):
    key = (fill, stroke, sw, valign, pad)
    if key in _gstyles: return _gstyles[key]
    st = Style(name="g%d" % len(_gstyles), family="graphic")
    kw = dict(fill="solid" if fill else "none", stroke="solid" if stroke else "none",
              padding=pad, paddingleft=pad, paddingright=pad, paddingtop=pad, paddingbottom=pad,
              autogrowheight="false", autogrowwidth="false",
              textareaverticalalign=valign, shadow="hidden")
    if fill: kw["fillcolor"] = fill
    if stroke: kw.update(strokecolor=stroke, strokewidth=sw)
    st.addElement(GraphicProperties(**kw))
    doc.automaticstyles.addElement(st); _gstyles[key] = st
    return st

def pstyle(pt, lh, align="start", ml=0.0, ti=0.0, sb=0.0, sa=0.0, color=INK, bold=False, italic=False,
           fam=SANS, letter=None):
    key = (pt, lh, align, ml, ti, sb, sa, color, bold, italic, fam, letter)
    if key in _pstyles: return _pstyles[key]
    st = Style(name="P%d" % len(_pstyles), family="paragraph")
    st.addElement(ParagraphProperties(lineheight="%.2fpt" % lh, textalign=align,
                                      marginleft="%.3fin" % ml, textindent="%.3fin" % ti,
                                      margintop="%.2fpt" % sb, marginbottom="%.2fpt" % sa))
    tp = dict(fontfamily=fam, fontname=fam, fontsize="%.1fpt" % pt, color=color,
              fontweight="bold" if bold else "normal", fontstyle="italic" if italic else "normal")
    if letter: tp["letterspacing"] = letter
    st.addElement(TextProperties(**tp))
    doc.automaticstyles.addElement(st); _pstyles[key] = st
    return st

def tstyle(kind, pt, color=None):
    key = (kind, pt, color)
    if key in _tstyles: return _tstyles[key]
    st = Style(name="T%d" % len(_tstyles), family="text")
    fam = MONO if kind in ("m", "bm") else SANS
    tp = dict(fontfamily=fam, fontname=fam, fontsize="%.1fpt" % size_for(kind, pt),
              fontweight="bold" if kind in ("b", "bm") else "normal",
              fontstyle="italic" if kind == "i" else "normal")
    if color: tp["color"] = color
    elif kind in ("m",): tp["color"] = "#2b4a6f"
    st.addElement(TextProperties(**tp))
    doc.automaticstyles.addElement(st); _tstyles[key] = st
    return st

def para(segs, ps, pt, color=None):
    p = P(stylename=ps)
    for t, k in segs:
        sp = Span(stylename=tstyle(k, pt, color))
        # keep runs of spaces and newlines
        parts = t.split("\n")
        for i, part in enumerate(parts):
            if i: sp.addElement(LineBreak())
            chunks = re.split(r"( {2,})", part)
            for c in chunks:
                if c.startswith("  "):
                    sp.addText(" "); sp.addElement(TSpace(c=len(c) - 1))
                elif c: sp.addText(c)
        p.addElement(sp)
    return p

class Sheet:
    def __init__(self, n):
        self.page = Page(masterpagename=master, stylename=dps, name="page%d" % n)
        doc.presentation.addElement(self.page)
    def text(self, x, y, w, h, paras, gs=None):
        fr = Frame(stylename=gs or gstyle(), width="%.3fin" % w, height="%.3fin" % h,
                   x="%.3fin" % x, y="%.3fin" % y)
        tb = TextBox()
        for p in paras: tb.addElement(p)
        fr.addElement(tb); self.page.addElement(fr)
    def rect(self, x, y, w, h, fill=None, stroke=None, sw="0.01in"):
        self.page.addElement(Rect(stylename=gstyle(fill, stroke, sw), width="%.3fin" % w, height="%.3fin" % h,
                                  x="%.3fin" % x, y="%.3fin" % y))
    def line(self, x1, y1, x2, y2, color=RULE, sw="0.01in"):
        self.page.addElement(Line(stylename=gstyle(None, color, sw), x1="%.3fin" % x1, y1="%.3fin" % y1,
                                  x2="%.3fin" % x2, y2="%.3fin" % y2))
    def image(self, path, x, y, w, h, border=True):
        href = doc.addPicture(path)
        fr = Frame(stylename=gstyle(None, "#9aa6b5" if border else None, "0.008in"), width="%.3fin" % w,
                   height="%.3fin" % h, x="%.3fin" % x, y="%.3fin" % y)
        fr.addElement(DImage(href=href)); self.page.addElement(fr)
    def badge(self, x, y, n, d=0.27):
        el = Ellipse(stylename=gstyle(GOLD, "#ffffff", "0.015in", valign="middle"), width="%.3fin" % d, height="%.3fin" % d,
                     x="%.3fin" % (x - d / 2), y="%.3fin" % (y - d / 2))
        el.addElement(para([(str(n), "b")], pstyle(9.5, 11, "center", color="#ffffff", bold=True), 9.5, "#ffffff"))
        self.page.addElement(el)

# ---------------------------------------------------------------- flow ----------------------

BODY, BODY_LH = 11.0, 15.2
pages = []          # list of Sheet
toc = []            # (chapter no, title, page no)
state = {"y": MT, "sheet": None, "chapter": None, "chno": 0}

def new_page():
    sh = Sheet(len(pages) + 1); pages.append(sh)
    state["sheet"], state["y"] = sh, MT + 0.35
    n = len(pages)
    # running head and footer
    if state["chapter"]:
        sh.text(ML, 0.42, CW, 0.25, [para([("%d  ·  %s" % (state["chno"], state["chapter"]), "r")],
                                         pstyle(8.5, 10, "end", color=DIM), 8.5, DIM)])
        sh.line(ML, 0.68, PW - MR, 0.68)
    sh.line(ML, PH - 0.62, PW - MR, PH - 0.62)
    sh.text(ML, PH - 0.55, CW / 2, 0.25, [para([("Fancy Pencil Manual", "r")], pstyle(8.5, 10, color=DIM), 8.5, DIM)])
    sh.text(ML + CW / 2, PH - 0.55, CW / 2, 0.25, [para([(str(n), "b")], pstyle(9, 10, "end", color=NAVY), 9, NAVY)])
    return sh

def room(): return BOTTOM - state["y"]
def ensure(h):
    if state["sheet"] is None or h > room(): new_page()

def text_block(segs, pt, lh, width, ml=0.0, ti=0.0, sb=0.0, sa=0.0, color=INK, bold=False, align="start",
               keep=False, min_lines=2):
    """Place a paragraph, carrying it on to the next page when it doesn't fit."""
    lines = wrap(segs, width - ml, pt, first_indent=ti)
    while lines:
        if state["sheet"] is None: new_page()
        avail = room() - (sb + sa) / 72.0
        fit = int(avail // (lh / 72.0)) if avail > 0 else 0
        if fit >= len(lines):
            take = len(lines)
        elif len(lines) - fit <= 2 and (len(lines) * lh + sb + sa) / 72.0 <= room() + 0.4:
            take = len(lines)          # a line or two over: let it run into the bottom margin
        elif fit >= min_lines and len(lines) - fit >= min_lines and not keep:
            take = fit
        else:
            new_page(); continue
        part = lines[:take]; lines = lines[take:]
        h = (take * lh + sb + sa) / 72.0 + 0.02
        ps = pstyle(pt, lh, align, ml, ti if part is not None else 0, sb, sa, color, bold)
        state["sheet"].text(ML, state["y"], width, h, [para(line_segs(part), ps, pt, color if color != INK else None)])
        state["y"] += h
        ti = 0.0  # continuation has no first-line indent / bullet

def gap(inches): state["y"] += inches

def h1(title, intro):
    state["chno"] += 1
    state["chapter"] = None
    sh = new_page()
    state["chapter"] = title
    n = state["chno"]
    toc.append((n, title, len(pages)))
    tl = wrap(parse(title), CW, 30)
    il = wrap(parse(intro, "i"), CW, 13)
    band = 1.62 + 0.5 * len(tl) + 0.24 * len(il) + 0.3
    sh.rect(0, 0, PW, band, fill=NAVY)
    sh.rect(0, band, PW, 0.06, fill=GOLD)
    sh.text(ML, 0.62, CW, 0.32, [para([("CHAPTER %d" % n, "b")], pstyle(10.5, 12, color="#f2b632", bold=True, letter="0.06em"), 10.5, "#f2b632")])
    sh.text(ML, 0.98, CW, 0.5 * len(tl) + 0.12, [para(parse(title), pstyle(30, 36, color="#ffffff", bold=True), 30, "#ffffff")])
    sh.text(ML, 1.12 + 0.5 * len(tl) + 0.08, CW, 0.24 * len(il) + 0.08, [para(parse(intro, "i"), pstyle(13, 17, color="#c9d3e0", italic=True), 13, "#c9d3e0")])
    state["y"] = band + 0.45

def h2(title, nxt_h=0.6):
    need = 0.5 + nxt_h
    if state["sheet"] is None or room() < need: new_page()
    state["y"] += 0.12
    text_block([(t, 'b') for t, k in parse(title)], 15, 19, CW, sb=4, sa=3, color=NAVY, bold=True, keep=True)
    state["sheet"].line(ML, state["y"] - 0.02, ML + 0.6, state["y"] - 0.02, GOLD, "0.025in")
    state["y"] += 0.06

def p_(t):
    text_block(parse(t), BODY, BODY_LH, CW, sa=6)

def items(lst, numbered=False):
    for i, it in enumerate(lst):
        mark = ("%d." % (i + 1)) if numbered else "•"
        segs = [(mark + " ", "b" if numbered else "r")] + parse(it)
        text_block(segs, BODY, BODY_LH, CW, ml=0.3, ti=-0.22, sa=3.5, min_lines=1)
    gap(0.05)

def code(t):
    lines = t.split("\n")
    lh = 13.5
    h = (len(lines) * lh) / 72.0 + 0.24
    ensure(h + 0.1)
    y = state["y"] + 0.03
    state["sheet"].rect(ML, y, CW, h, fill=TINT, stroke=RULE)
    ps = pstyle(9.8, lh, ml=0, fam=MONO)
    state["sheet"].text(ML + 0.18, y + 0.12, CW - 0.3, h - 0.2, [para([(l, "m")], ps, 10.6, INK) for l in lines])
    state["y"] = y + h + 0.14

def note(t):
    segs = [("Note  ", "b")] + parse(t)
    lines = wrap(segs, CW - 0.45, 10.5)
    lh = 14.2
    h = len(lines) * lh / 72.0 + 0.26
    ensure(h + 0.12)
    y = state["y"] + 0.04
    state["sheet"].rect(ML, y, CW, h, fill=NOTE)
    state["sheet"].rect(ML, y, 0.06, h, fill=GOLD)
    state["sheet"].text(ML + 0.24, y + 0.12, CW - 0.4, h - 0.18, [para(line_segs(lines), pstyle(10.5, lh), 10.5)])
    state["y"] = y + h + 0.16

def img_size(f):
    with Image.open(os.path.join(IMG, f)) as im: return im.size

def image(f, w, cap, after=None):
    pw, ph = img_size(f)
    w = min(w, CW)
    h = w * ph / pw
    caplines = wrap(parse(cap, "i"), CW, 9)
    ch_ = len(caplines) * 12 / 72.0 + 0.12
    need = h + ch_ + 0.15
    if state["sheet"] is None: new_page()
    if need > room():
        # shrink a little rather than leave a big gap; otherwise next page
        s = (room() - ch_ - 0.15) / h
        if s >= 0.62 and room() > 2.0: w, h = w * s, h * s
        else: new_page()
    x = ML + (CW - w) / 2
    y = state["y"] + 0.06
    state["sheet"].image(os.path.join(IMG, f), x, y, w, h)
    if after: after(x, y, w, h, pw, ph)
    state["sheet"].text(ML, y + h + 0.06, CW, ch_, [para(parse(cap, "i"), pstyle(9, 12, "center", color=DIM, italic=True), 9, DIM)])
    state["y"] = y + h + ch_ + 0.12

def callouts(f, w, pts, cap):
    def draw(x, y, w_, h_, pw, ph):
        for (px, py, n) in pts:
            state["sheet"].badge(x + px * w_ / pw, y + py * h_ / ph, n)
    image(f, w, cap, after=draw)

def table(widths, rows):
    tot = sum(widths); widths = [w * CW / tot for w in widths]
    pt, lh, pad = 10, 13.6, 0.07
    header = rows and all(c.startswith("**") and c.endswith("**") or c == "" for c in rows[0]) and len(rows) > 6
    def row_h(r):
        return max(len(wrap(parse(c), widths[i] - 2 * pad, pt)) for i, c in enumerate(r)) * lh / 72.0 + 2 * pad
    def draw_row(r, shade, hdr=False):
        h = row_h(r)
        y = state["y"]
        if hdr: state["sheet"].rect(ML, y, CW, h, fill=NAVY)
        elif shade: state["sheet"].rect(ML, y, CW, h, fill=TINT)
        x = ML
        for i, c in enumerate(r):
            col = "#ffffff" if hdr else None
            segs = parse(c)
            if hdr: segs = [(t, "b") for t, k in segs]
            state["sheet"].text(x + pad, y + pad, widths[i] - 2 * pad, h - pad,
                                [para(segs, pstyle(pt, lh, color=col or INK), pt, col)])
            x += widths[i]
        state["y"] += h
    if state["sheet"] is None: new_page()
    state["y"] += 0.04
    if room() < row_h(rows[0]) + (row_h(rows[1]) if len(rows) > 1 else 0) + 0.1: new_page()
    start = 0
    if header:
        draw_row(rows[0], False, True); start = 1
    for i, r in enumerate(rows[start:]):
        if row_h(r) > room():
            new_page()
            if header: draw_row(rows[0], False, True)
        draw_row(r, i % 2 == 0)
    state["sheet"].line(ML, state["y"], PW - MR, state["y"])
    state["y"] += 0.16

def cmdref():
    rows = []
    for l in open(os.path.join(HERE, "commands.tsv"), encoding="utf-8"):
        n, al, d = (l.rstrip("\n").split("\t") + ["", ""])[:3]
        rows.append((n, al, d))
    def key(r):
        n = r[0].lstrip("-'_.")
        return ("0" if n[:1].isdigit() else "1") + n.upper() + ("~" if r[0].startswith("-") else "")
    rows.sort(key=key)
    colw = (CW - 0.35) / 2
    pt, lh = 8.6, 11.0
    def entry_lines(n, al, d):
        segs = [(n, "bm")] + ([("  " + al.replace(" ", ", "), "m")] if al else []) + [("  " + d, "r")]
        return segs, wrap(segs, colw, pt)
    if state["sheet"] is None: new_page()
    col, top = 0, state["y"]
    y = top
    letter = None
    for n, al, d in rows:
        bare = n.lstrip("-'_.")
        first = "3D" if bare[:1].isdigit() else bare[:1].upper()
        segs, lines = entry_lines(n, al, d)
        h = len(lines) * lh / 72.0 + 0.055
        head = first != letter
        hh = 0.32 if head else 0
        if y + hh + h > BOTTOM:
            if col == 0: col, y = 1, top
            else:
                new_page(); col, top = 0, state["y"]; y = top
        x = ML + col * (colw + 0.35)
        if head:
            letter = first
            state["sheet"].text(x, y + 0.04, colw, 0.26, [para([(letter, "b")], pstyle(13, 15, color=GOLD, bold=True), 13, GOLD)])
            state["sheet"].line(x + 0.3, y + 0.19, x + colw, y + 0.19)
            y += hh
        state["sheet"].text(x, y, colw, h, [para(line_segs(lines), pstyle(pt, lh), pt)])
        y += h
    state["y"] = BOTTOM

# ---------------------------------------------------------------- cover & contents ----------

def cover():
    sh = Sheet(1); pages.append(sh)
    sh.rect(0, 0, PW, PH, fill=NAVY)
    sh.rect(0, 7.05, PW, 0.08, fill=GOLD)
    sh.image(os.path.join(HERE, "..", "lispdraft", "support", "icons", "fancypencil-512.png"), ML, 1.0, 1.9, 1.9, border=False)
    sh.text(ML, 3.15, CW, 1.0, [para([("Fancy Pencil", "b")], pstyle(54, 60, color="#ffffff", bold=True), 54, "#ffffff")])
    sh.text(ML, 4.15, CW, 0.55, [para([("User Manual", "r")], pstyle(26, 30, color="#f2b632"), 26, "#f2b632")])
    sh.text(ML, 4.95, CW, 1.2, [para([("Drafting with AutoCAD-style commands and AutoLISP, in 2D and 3D, on Linux.", "r")],
                                    pstyle(14, 19, color="#c9d3e0"), 14, "#c9d3e0")])
    sh.text(ML, 6.3, CW, 0.4, [para([("Version " + content.VERSION, "b")], pstyle(12, 14, color="#ffffff", bold=True), 12, "#ffffff")])
    sh.image(os.path.join(IMG, "soccer-crop.png"), PW - MR - 4.2, 7.55, 4.2, 4.2 * 700 / 900, border=False)
    sh.image(os.path.join(IMG, "plate-crop.png"), ML, 7.55, 4.0, 4.0 * 700 / 860, border=False)
    return sh

def contents_page(sh):
    sh.text(ML, 0.9, CW, 0.7, [para([("Contents", "b")], pstyle(30, 36, color=NAVY, bold=True), 30, NAVY)])
    sh.rect(ML, 1.62, 0.7, 0.04, fill=GOLD)
    y = 2.1
    for n, title, pg in toc:
        sh.text(ML, y, 0.5, 0.3, [para([(str(n), "b")], pstyle(12, 14, color=GOLD, bold=True), 12, GOLD)])
        sh.text(ML + 0.55, y, CW - 1.4, 0.3, [para([(title, "r")], pstyle(12, 14, color=INK), 12)])
        sh.text(PW - MR - 0.8, y, 0.8, 0.3, [para([(str(pg), "b")], pstyle(12, 14, "end", color=NAVY, bold=True), 12, NAVY)])
        sh.line(ML + 0.55, y + 0.3, PW - MR, y + 0.3, "#e1e6ec", "0.006in")
        y += 0.405
    sh.line(ML, PH - 0.62, PW - MR, PH - 0.62)
    sh.text(ML, PH - 0.55, CW / 2, 0.25, [para([("Fancy Pencil Manual", "r")], pstyle(8.5, 10, color=DIM), 8.5, DIM)])
    sh.text(ML + CW / 2, PH - 0.55, CW / 2, 0.25, [para([("2", "b")], pstyle(9, 10, "end", color=NAVY), 9, NAVY)])

# ---------------------------------------------------------------- go ------------------------

cover()
toc_sheet = Sheet(2); pages.append(toc_sheet)
for blocks in content.CHAPTERS:
    for i, b in enumerate(blocks):
        k = b[0]
        nxt = blocks[i + 1] if i + 1 < len(blocks) else None
        if k == "h1": h1(b[1], b[2])
        elif k == "h2":
            nh = 0.6
            if nxt and nxt[0] == "img":
                pw_, ph_ = img_size(nxt[1]); nh = min(nxt[2], CW) * ph_ / pw_ + 0.4
            elif nxt and nxt[0] == "table": nh = 0.9
            h2(b[1], min(nh, 5.0))
        elif k == "p": p_(b[1])
        elif k == "ul": items(b[1])
        elif k == "ol": items(b[1], True)
        elif k == "img": image(b[1], b[2], b[3])
        elif k == "callouts": callouts(b[1], b[2], b[3], b[4])
        elif k == "note": note(b[1])
        elif k == "code": code(b[1])
        elif k == "table": table(b[1], b[2])
        elif k == "cmdref": cmdref()
        elif k == "break": new_page()
contents_page(toc_sheet)
doc.save(OUT)
print("pages:", len(pages), "->", OUT)
for n, t, p in toc: print(" ", n, t, p)
