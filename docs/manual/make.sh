#!/bin/sh
# Builds docs/Fancy-Pencil-Manual.pdf: lay out the pages (build.py), let LibreOffice make the PDF,
# then add the clickable contents, Back/Contents buttons and bookmarks (links.py).
# Needs python3 with odfpy, Pillow and pypdf, and LibreOffice (soffice).
set -e
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
python3 "$here/build.py" "$work/manual.odp"
(cd "$work" && soffice --headless --convert-to pdf manual.odp >/dev/null)
python3 "$here/links.py" "$work/manual.pdf" "$work/manual.links.json" "$here/../Fancy-Pencil-Manual.pdf"
