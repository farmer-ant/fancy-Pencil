#!/bin/sh
# Builds the self-contained Tcl/Tk 9 base that "install" is made from (see build-installer.sh):
# static Tcl and Tk with a static libXft, so the result needs nothing but what every Linux
# desktop has (libX11, libXrender, fontconfig, freetype, zlib, glibc 2.34+).
#
#   sh build-base.sh WORKDIR        -> WORKDIR/tk/unix/wish and WORKDIR/tcl/unix/tclsh
#
# Needs gcc, make, git and the X11 / Xrender / fontconfig / freetype development headers.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
W=$(mkdir -p "$1" && cd "$1" && pwd)
cd "$W"
[ -d tcl ] || git clone -q --depth 1 --branch core-9-0-2 https://github.com/tcltk/tcl.git tcl
[ -d tk ]  || git clone -q --depth 1 --branch core-9-0-2 https://github.com/tcltk/tk.git tk
[ -d xft2 ] || git clone -q --depth 1 https://github.com/ystk/debian-xft.git xft2
# old glibc symbol versions, so it runs on Ubuntu 22.04 / Debian 12 / Fedora 35 and newer
CF="-O2 -include $HERE/installer/compat.h"
(cd xft2 && CFLAGS="$CF -fPIC" ./configure --disable-shared --enable-static --prefix="$W/xftinst" && make && make install)
(cd tcl/unix && CFLAGS="$CF" ./configure --disable-shared --prefix="$W/inst" && make)
(cd tk/unix && PKG_CONFIG_PATH="$W/xftinst/lib/pkgconfig" CFLAGS="$CF -I$W/xftinst/include" LDFLAGS="-L$W/xftinst/lib" \
    ./configure --disable-shared --enable-xft --disable-xss --with-tcl="$W/tcl/unix" --prefix="$W/inst" && make)
ls -la "$W/tk/unix/wish" "$W/tcl/unix/tclsh"
