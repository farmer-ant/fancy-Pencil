#!/bin/sh
# Builds the kit's single "install" file: a self-contained Tcl/Tk 9 program (no Tcl/Tk needed
# on the computer it runs on) with installer/main.tcl, the logo and fancypencil.tar.gz packed
# inside it.
#
#   sh build-installer.sh BASE-WISH TCLSH fancypencil.tar.gz VERSION OUT
#
# BASE-WISH and TCLSH come from build-base.sh (static Tcl/Tk 9 with Xft, glibc 2.34+).
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
WISH=$1; TCLSH=$2; TAR=$3; VER=$4; OUT=$5
TCLLIB=$(cd "$(dirname "$TCLSH")/../library" && pwd)
TKLIB=$(cd "$(dirname "$WISH")/../library" && pwd)
APP=$(mktemp -d)
cp "$HERE/installer/main.tcl" "$APP/"
cp "$HERE/../icons/fancypencil-128.png" "$HERE/../icons/fancypencil-64.png" "$APP/"
cp "$TAR" "$APP/fancypencil.tar.gz"
echo "$VER" > "$APP/VERSION.txt"
cp -r "$TCLLIB" "$APP/tcl_library"
cp -r "$TKLIB" "$APP/tk_library";   rm -rf "$APP/tk_library/demos" "$APP/tk_library/images"
echo "zipfs mkimg [list $OUT] [list $APP] [list $APP] {} [list $WISH]" | (cd / && "$TCLSH")
chmod +x "$OUT"
rm -rf "$APP"
ls -la "$OUT"
