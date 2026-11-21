#!/usr/bin/env python3
# Makes the manual PDF clickable for reading on screen:
#   - each chapter on the Contents page jumps to its first page
#   - every page's "‹ Back" button goes back to where the reader came from (the viewer's Back),
#     and its "Contents" button goes to the Contents page
#   - bookmarks (the viewer's side panel) for every chapter and its sections
# build.py lays out the pages and writes NAME.odp and NAME.links.json; LibreOffice turns the .odp
# into NAME.pdf; this adds the links:
#   python3 links.py NAME.pdf NAME.links.json OUT.pdf
import json, sys
from pypdf import PdfReader, PdfWriter
from pypdf.generic import (ArrayObject, DictionaryObject, FloatObject, NameObject, NumberObject,
                           TextStringObject)
from pypdf.generic import Fit

src, data_file, out = sys.argv[1:4]
data = json.load(open(data_file))
PW, PH = data["page_size"]
reader = PdfReader(src)
writer = PdfWriter(clone_from=reader)
if len(writer.pages) != data["pages"]:
    sys.exit("links.py: the PDF has %d pages, the layout %d" % (len(writer.pages), data["pages"]))

def rect(x, y, w, h, page):
    """Inches from the top-left (as build.py lays out) -> PDF points from the bottom-left."""
    pw = float(page.mediabox.width) / PW        # points per inch (72, unless scaled)
    ph = float(page.mediabox.height)
    return (x * pw, ph - (y + h) * pw, (x + w) * pw, ph - y * pw)

def link(r, page_index=None, named=None):
    """A link annotation: to a page (by reference, as viewers need) or a named action like GoBack."""
    a = DictionaryObject({
        NameObject("/Type"): NameObject("/Annot"),
        NameObject("/Subtype"): NameObject("/Link"),
        NameObject("/Rect"): ArrayObject([FloatObject(v) for v in r]),
        NameObject("/Border"): ArrayObject([NumberObject(0), NumberObject(0), NumberObject(0)]),
    })
    if named:
        a[NameObject("/A")] = DictionaryObject({NameObject("/S"): NameObject("/Named"), NameObject("/N"): NameObject(named)})
    else:
        a[NameObject("/Dest")] = ArrayObject([writer.pages[page_index].indirect_reference, NameObject("/Fit")])
    return a

def put(page_index, annot):
    """Adds ANNOT to the page as it is (pypdf's add_annotation rewrites page links as numbers)."""
    page = writer.pages[page_index]
    if "/Annots" not in page:
        page[NameObject("/Annots")] = ArrayObject()
    page["/Annots"].append(writer._add_object(annot))

toc_index = data["toc_page"] - 1

# the Contents page: one link per chapter row
for pg, x, y, w, h in data["toc_rows"]:
    put(toc_index, link(rect(x, y, w, h, writer.pages[toc_index]), pg - 1))

# Back and Contents on every page after the cover
for i in range(1, len(writer.pages)):
    page = writer.pages[i]
    for name, x, y, w, h in data["buttons"]:
        r = rect(x, y, w, h, page)
        if name == "contents":
            put(i, link(r, toc_index))
        else:
            # the viewer's own Back (Firefox, Evince, Okular, Acrobat ...): back to the Contents
            # page after a jump from it, or to wherever the reader was
            put(i, link(r, named="/GoBack"))

# bookmarks: Contents, then each chapter with its sections
writer._root_object.pop("/Outlines", None)
writer.add_outline_item("Contents", toc_index)
by_chapter = {}
for n, title, pg, y in data["sections"]:
    by_chapter.setdefault(n, []).append((title, pg, y))
for n, title, pg in data["toc"]:
    parent = writer.add_outline_item("%d  %s" % (n, title), pg - 1)
    for t, spg, y in by_chapter.get(n, []):
        page = writer.pages[spg - 1]
        top = float(page.mediabox.height) - y * float(page.mediabox.width) / PW + 6
        writer.add_outline_item(t, spg - 1, parent=parent, fit=Fit.xyz(top=top))
writer.page_mode = "/UseOutlines"           # open with the bookmarks showing
writer.add_metadata({"/Title": "Fancy Pencil Manual", "/Subject": "User manual for Fancy Pencil"})
with open(out, "wb") as f:
    writer.write(f)
print("links: %d contents rows, Back and Contents on %d pages, %d bookmarks -> %s"
      % (len(data["toc_rows"]), len(writer.pages) - 1, len(data["toc"]) + len(data["sections"]) + 1, out))
