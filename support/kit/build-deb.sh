#!/bin/sh
# Builds "Install Fancy Pencil.deb": Fancy Pencil as a Debian/Ubuntu/Mint package. Double-clicking
# it opens the system's package installer (App Center, Software Install, GDebi), which asks for
# the password and installs SBCL and Tk along with it - no "executable" mark needed, so it works
# from USB sticks and Windows-formatted drives.
#
#   sh build-deb.sh fancypencil.tar.gz VERSION OUT.deb
#
# Installs to /opt/fancypencil, with /usr/bin/fancypencil, menu entries for Fancy Pencil and
# "Get ODA File Converter", icons, and AppStream data (name, logo, description in App Center).
set -e
TAR=$1; VER=$2; OUT=$3
R=$(mktemp -d); chmod 755 "$R"
mkdir -p "$R/opt" "$R/usr/bin" "$R/usr/share/applications" "$R/usr/share/metainfo" "$R/DEBIAN"
tar xzf "$TAR" -C "$R/opt"
SRC="$R/opt/fancypencil"
chmod +x "$SRC/fancypencil"
rm -rf "$SRC/build" "$SRC/tests"

cat > "$R/usr/bin/fancypencil" <<'EOF'
#!/bin/sh
exec /opt/fancypencil/fancypencil "$@"
EOF
chmod 755 "$R/usr/bin/fancypencil"

for n in 16 24 32 48 64 128 256 512; do
    mkdir -p "$R/usr/share/icons/hicolor/${n}x${n}/apps"
    cp "$SRC/support/icons/fancypencil-$n.png" "$R/usr/share/icons/hicolor/${n}x${n}/apps/fancypencil.png"
done
mkdir -p "$R/usr/share/icons/hicolor/scalable/apps"
cp "$SRC/support/icons/fancypencil.svg" "$R/usr/share/icons/hicolor/scalable/apps/fancypencil.svg"

cat > "$R/usr/share/applications/fancypencil.desktop" <<'EOF'
[Desktop Entry]
Type=Application
Version=1.0
Name=Fancy Pencil
GenericName=2D and 3D Drafting
Comment=Drafting with AutoCAD-style commands and AutoLISP
Exec=fancypencil %F
Icon=fancypencil
Terminal=false
Categories=Graphics;VectorGraphics;Engineering;
Keywords=CAD;drafting;drawing;DXF;DWG;AutoLISP;
MimeType=image/vnd.dxf;image/x-dxf;application/dxf;image/vnd.dwg;image/x-dwg;application/acad;
StartupWMClass=Fancypencil
StartupNotify=true
EOF
cat > "$R/usr/share/applications/fancypencil-oda.desktop" <<'EOF'
[Desktop Entry]
Type=Application
Version=1.0
Name=Get ODA File Converter
Comment=Download and install the free ODA File Converter, so Fancy Pencil opens and saves .dwg drawings
Exec=wish /opt/fancypencil/support/oda-helper.tcl
Icon=fancypencil
Terminal=false
Categories=Graphics;Engineering;
EOF

cat > "$R/usr/share/metainfo/fancypencil.metainfo.xml" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<component type="desktop-application">
  <id>fancypencil</id>
  <metadata_license>CC0-1.0</metadata_license>
  <name>Fancy Pencil</name>
  <summary>2D and 3D drafting with AutoCAD-style commands</summary>
  <description>
    <p>Fancy Pencil is a drafting program with AutoCAD-style commands, AutoLISP, layers, blocks,
    dimensions, solids and plotting. It reads and writes DXF.</p>
    <p>To open and save AutoCAD .dwg drawings, install the free ODA File Converter: Help &gt; Get
    ODA File Converter in Fancy Pencil, or "Get ODA File Converter" in the app menu, fetches and
    installs it. Download page: https://www.opendesign.com/guestfiles/oda_file_converter</p>
  </description>
  <launchable type="desktop-id">fancypencil.desktop</launchable>
  <icon type="stock">fancypencil</icon>
  <url type="homepage">https://www.opendesign.com/guestfiles/oda_file_converter</url>
  <categories><category>Graphics</category><category>Engineering</category></categories>
  <releases><release version="$VER"/></releases>
</component>
EOF

SIZE=$(du -sk "$R" | cut -f1)
cat > "$R/DEBIAN/control" <<EOF
Package: fancypencil
Version: $VER
Section: graphics
Priority: optional
Architecture: all
Depends: sbcl, tk, fontconfig
Recommends: imagemagick, poppler-utils
Installed-Size: $SIZE
Maintainer: Fancy Pencil <fancypencil@localhost>
Homepage: https://www.opendesign.com/guestfiles/oda_file_converter
Description: Fancy Pencil - drafting with AutoCAD-style commands
 Fancy Pencil is a 2D and 3D drafting program with AutoCAD-style commands,
 AutoLISP, layers, blocks, dimensions, solids and plotting. It reads and
 writes DXF.
 .
 For AutoCAD .dwg drawings, install the free ODA File Converter: after
 installing, open "Get ODA File Converter" from the app menu (or Help > Get
 ODA File Converter in Fancy Pencil). Download page:
 https://www.opendesign.com/guestfiles/oda_file_converter
EOF
cat > "$R/DEBIAN/postinst" <<'EOF'
#!/bin/sh
# compile Fancy Pencil once, now, so it starts quickly for everyone
if [ "$1" = configure ] && command -v sbcl >/dev/null 2>&1; then
    (cd /opt/fancypencil && HOME=/tmp sbcl --noinform --non-interactive --load load.lisp >/dev/null 2>&1) || true
    chmod -R a+rX /opt/fancypencil/build 2>/dev/null || true
fi
command -v gtk-update-icon-cache >/dev/null 2>&1 && gtk-update-icon-cache -q -t /usr/share/icons/hicolor || true
command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database -q /usr/share/applications || true
exit 0
EOF
cat > "$R/DEBIAN/prerm" <<'EOF'
#!/bin/sh
rm -rf /opt/fancypencil/build
exit 0
EOF
chmod 755 "$R/DEBIAN/postinst" "$R/DEBIAN/prerm"
find "$R" -type d -exec chmod 755 {} +
find "$R/opt" -type f ! -perm -u+x -exec chmod 644 {} +
dpkg-deb --root-owner-group -Zxz --build "$R" "$OUT" >/dev/null
rm -rf "$R"
ls -la "$OUT"
