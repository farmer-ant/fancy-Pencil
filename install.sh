#!/bin/sh
# Adds Fancy Pencil to your desktop's application launcher (menu / dash / dock).
# Run it from wherever you keep the fancypencil folder:   sh install.sh
# Nothing outside your home folder is touched, so sudo is not needed.
# To remove it again:   sh install.sh --uninstall
set -e
DIR=$(cd "$(dirname "$0")" && pwd)
DATA=${XDG_DATA_HOME:-$HOME/.local/share}
APPS="$DATA/applications"
ICONS="$DATA/icons/hicolor"
DESKTOP="$APPS/fancypencil.desktop"

refresh() {
    command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database -q "$APPS" || true
    command -v gtk-update-icon-cache  >/dev/null 2>&1 && gtk-update-icon-cache -q -t "$ICONS" || true
    command -v xdg-desktop-menu >/dev/null 2>&1 && xdg-desktop-menu forceupdate >/dev/null 2>&1 || true
}

if [ "$1" = "--uninstall" ]; then
    rm -f "$DESKTOP" "$ICONS/scalable/apps/fancypencil.svg"
    for n in 16 24 32 48 64 128 256 512; do rm -f "$ICONS/${n}x${n}/apps/fancypencil.png"; done
    refresh
    echo "Fancy Pencil removed from the launcher."
    exit 0
fi

command -v sbcl >/dev/null 2>&1 || echo "Note: sbcl is not on your PATH; install it with: sudo apt install sbcl tk"

for n in 16 24 32 48 64 128 256 512; do
    mkdir -p "$ICONS/${n}x${n}/apps"
    cp "$DIR/support/icons/fancypencil-$n.png" "$ICONS/${n}x${n}/apps/fancypencil.png"
done
mkdir -p "$ICONS/scalable/apps" "$APPS"
cp "$DIR/support/icons/fancypencil.svg" "$ICONS/scalable/apps/fancypencil.svg"
# keep a hicolor index so icon caches work even on a fresh profile
[ -f "$ICONS/index.theme" ] || [ ! -f /usr/share/icons/hicolor/index.theme ] || cp /usr/share/icons/hicolor/index.theme "$ICONS/"

cat > "$DESKTOP" <<DESK
[Desktop Entry]
Type=Application
Version=1.0
Name=Fancy Pencil
GenericName=2D Drafting
Comment=2D drafting with AutoCAD-style commands and AutoLISP
Exec=sh "$DIR/fancypencil" %F
Path=$HOME
Icon=fancypencil
Terminal=false
Categories=Graphics;VectorGraphics;Engineering;
Keywords=CAD;drafting;drawing;DXF;AutoLISP;
MimeType=image/vnd.dxf;image/x-dxf;application/dxf;image/vnd.dwg;image/x-dwg;application/acad;
StartupWMClass=Fancypencil
StartupNotify=true
DESK
chmod +x "$DESKTOP" "$DIR/fancypencil"
command -v gio >/dev/null 2>&1 && gio set "$DESKTOP" metadata::trusted true 2>/dev/null || true
refresh
echo "Fancy Pencil added to the launcher (icon: fancypencil, entry: $DESKTOP)."
echo "If it doesn't show up straight away, log out and back in."
