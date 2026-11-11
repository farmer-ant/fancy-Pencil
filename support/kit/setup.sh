#!/bin/sh
# Fancy Pencil Kit - the text installer (the point-and-click one is "Install Fancy Pencil").
#
#   sh setup.sh                      installs into ~/Fancy Pencil
#   sh setup.sh "/some/other/folder" installs into that folder instead
#
# Running it again later updates Fancy Pencil to the copy in this kit.
# Your settings (in ~/.config/fancypencil) are kept.

set -e
KIT=$(cd "$(dirname "$0")" && pwd)
DEST=${1:-"$HOME/Fancy Pencil"}

if   command -v apt-get >/dev/null 2>&1; then INST="sudo apt-get install -y"; PICS="imagemagick poppler-utils"; ODA=deb
elif command -v dnf     >/dev/null 2>&1; then INST="sudo dnf install -y";     PICS="ImageMagick poppler-utils"; ODA=rpm
elif command -v zypper  >/dev/null 2>&1; then INST="sudo zypper install -y";  PICS="ImageMagick poppler-tools"; ODA=rpm
elif command -v pacman  >/dev/null 2>&1; then INST="sudo pacman -S --needed"; PICS="imagemagick poppler"; ODA=AppImage
else INST=; ODA=AppImage
fi

echo "== Fancy Pencil $(cat "$KIT/VERSION.txt" 2>/dev/null) =="
echo
echo "1. Installing what Fancy Pencil needs (SBCL Common Lisp and the Tk toolkit)."
if command -v sbcl >/dev/null 2>&1 && command -v wish >/dev/null 2>&1; then
    echo "   Already installed."
elif [ -n "$INST" ]; then
    echo "   You may be asked for your password."
    $INST sbcl tk
else
    echo "   Please install the packages sbcl and tk with your package manager, then run this again."; exit 1
fi
echo

if [ -n "$INST" ]; then
    printf "2. Also install picture/PDF support (%s)? [Y/n] " "$PICS"
    read a
    case "$a" in [nN]*) ;; *) $INST $PICS ;; esac
    echo
fi

echo "3. Unpacking Fancy Pencil into: $DEST"
mkdir -p "$DEST"
tar xzf "$KIT/fancypencil.tar.gz" -C "$DEST"
echo

echo "4. Adding Fancy Pencil to the app menu."
cd "$DEST/fancypencil"
sh install.sh || echo "   (couldn't add the menu entry; you can still start it from this folder)"
echo

echo "Done. Start Fancy Pencil from the app menu, or with:"
echo "   cd \"$DEST/fancypencil\" && ./fancypencil"
echo "(The first start takes a little longer while it compiles itself.)"
echo
echo "To open DWG files, also install the free ODA File Converter - the $ODA version from:"
echo "   https://www.opendesign.com/guestfiles/oda_file_converter"
case $ODA in
    deb) echo "then:  sudo apt install ./ODAFileConverter_*.deb" ;;
    rpm) echo "then:  sudo dnf install ./ODAFileConverter_*.rpm   (openSUSE: sudo zypper install ./ODAFileConverter_*.rpm)" ;;
    *)   echo "then:  mkdir -p ~/.local/bin && cp ODAFileConverter_*.AppImage ~/.local/bin/ODAFileConverter && chmod +x ~/.local/bin/ODAFileConverter" ;;
esac
