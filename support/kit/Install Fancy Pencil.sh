#!/bin/sh
# Fancy Pencil Kit - the point-and-click installer.
#
# Double-click "Install Fancy Pencil" in this folder (or right-click this file > Run as a Program,
# or in a terminal:  sh "Install Fancy Pencil.sh").
#
# It installs what Fancy Pencil needs (SBCL and Tk) with your distribution's package manager,
# unpacks Fancy Pencil, adds it to the app menu, and can fetch and install the free ODA File
# Converter for opening and saving .dwg drawings. Running it again updates Fancy Pencil; your
# settings in ~/.config/fancypencil are kept.
#
# Works with apt (Ubuntu, Debian, Mint), dnf (Fedora), zypper (openSUSE) and pacman (Arch).
# With no graphical session it falls back to the text installer, setup.sh.

KIT=$(cd "$(dirname "$0")" && pwd)
VERSION=$(cat "$KIT/VERSION.txt" 2>/dev/null)
TITLE="Fancy Pencil ${VERSION} Setup"
LOG="$HOME/.cache/fancypencil-install.log"
ODA_PAGE="https://www.opendesign.com/guestfiles/oda_file_converter"
ODA_GET="https://www.opendesign.com/guestfiles/get?filename=ODAFileConverter_QT6_lnxX64_11dll"
mkdir -p "$HOME/.cache"
echo "=== $(date) Fancy Pencil Kit $VERSION" >>"$LOG"

# ---------------------------------------------------------------- no dialogs? -----------------

text_installer() {
    # a terminal window running the text installer, or the text installer right here
    if [ -t 0 ] || [ -z "$DISPLAY$WAYLAND_DISPLAY" ]; then exec sh "$KIT/setup.sh"; fi
    S="cd \"$KIT\" && sh ./setup.sh; echo; echo 'Press Enter to close.'; read x"
    for T in x-terminal-emulator gnome-terminal konsole xfce4-terminal mate-terminal lxterminal kgx ptyxis xterm; do
        command -v $T >/dev/null 2>&1 || continue
        case $T in
            gnome-terminal|kgx|ptyxis) exec $T -- sh -c "$S" ;;
            xfce4-terminal|mate-terminal) exec $T -x sh -c "$S" ;;
            *) exec $T -e sh -c "$S" ;;
        esac
    done
    exec sh "$KIT/setup.sh"
}

# ---------------------------------------------------------------- package manager -------------

if   command -v apt-get >/dev/null 2>&1; then PM=apt;    ODA_EXT=deb
elif command -v dnf     >/dev/null 2>&1; then PM=dnf;    ODA_EXT=rpm
elif command -v zypper  >/dev/null 2>&1; then PM=zypper; ODA_EXT=rpm
elif command -v pacman  >/dev/null 2>&1; then PM=pacman; ODA_EXT=AppImage
else PM=none; ODA_EXT=AppImage
fi

pm_install_cmd() {   # the root command line that installs packages "$@"
    case $PM in
        apt)    echo "DEBIAN_FRONTEND=noninteractive apt-get install -y $*" ;;
        dnf)    echo "dnf install -y $*" ;;
        zypper) echo "zypper --non-interactive install $*" ;;
        pacman) echo "pacman -S --needed --noconfirm $*" ;;
    esac
}
pictures_pkgs() {
    case $PM in
        apt) echo "imagemagick poppler-utils" ;;
        dnf) echo "ImageMagick poppler-utils" ;;
        zypper) echo "ImageMagick poppler-tools" ;;
        pacman) echo "imagemagick poppler" ;;
    esac
}
fuse_pkg() {         # AppImages need FUSE 2
    case $PM in
        apt) if apt-cache show libfuse2t64 >/dev/null 2>&1; then echo libfuse2t64; else echo libfuse2; fi ;;
        dnf) echo fuse-libs ;;
        zypper) echo libfuse2 ;;
        pacman) echo fuse2 ;;
    esac
}

as_root() {          # run the shell script "$1" as root, asking for the password in a window
    if command -v pkexec >/dev/null 2>&1; then
        pkexec sh -c "$1"
    else
        PW=$(zenity --password --title="$TITLE" 2>/dev/null) || return 1
        printf '%s\n' "$PW" | sudo -S -p '' sh -c "$1"
    fi
}

# ---------------------------------------------------------------- zenity ----------------------

if [ -z "$DISPLAY$WAYLAND_DISPLAY" ]; then text_installer; fi
if ! command -v zenity >/dev/null 2>&1; then
    # the dialogs come from zenity; fetch it (the password window is the system's own)
    if [ "$PM" != none ] && command -v pkexec >/dev/null 2>&1; then
        pkexec sh -c "$(pm_install_cmd zenity)" >>"$LOG" 2>&1
    fi
    command -v zenity >/dev/null 2>&1 || text_installer
fi

Z() { zenity --title="$TITLE" "$@" 2>/dev/null; }

oda_installed() {
    command -v ODAFileConverter >/dev/null 2>&1 && return 0
    for f in /usr/bin/ODAFileConverter /usr/local/bin/ODAFileConverter /opt/ODAFileConverter*/ODAFileConverter \
             "$HOME/.local/bin/ODAFileConverter" "$HOME"/Applications/ODAFileConverter*.AppImage; do
        [ -e "$f" ] && return 0
    done
    return 1
}
have_pictures() { (command -v magick >/dev/null 2>&1 || command -v convert >/dev/null 2>&1) && command -v pdftoppm >/dev/null 2>&1; }

progress() {         # progress "message" command... : a pulsing bar while the command runs
    MSG=$1; shift
    ST=$(mktemp)
    ( "$@" >>"$LOG" 2>&1; echo $? >"$ST" ) | Z --progress --pulsate --auto-close --no-cancel --width=460 --text="$MSG"
    R=$(cat "$ST" 2>/dev/null); rm -f "$ST"
    return ${R:-1}
}

failed() {
    Z --error --width=460 --text="$1\n\nThe details are in $LOG"
    Z --text-info --width=720 --height=420 --filename="$LOG" >/dev/null
}

# ---------------------------------------------------------------- 1. welcome ------------------

[ -f "$KIT/fancypencil.tar.gz" ] || { Z --error --text="fancypencil.tar.gz is missing from the kit folder:\n$KIT"; exit 1; }

DEST="$HOME/Fancy Pencil"
UPDATE=; [ -f "$DEST/fancypencil/fancypencil" ] && UPDATE=1
Z --question --width=520 --ok-label="Next" --cancel-label="Cancel" \
  --text="<big><b>Fancy Pencil ${VERSION}</b></big>\n\n${UPDATE:+Fancy Pencil is already in $DEST - this will update it. Your settings are kept.\n\n}This will:\n  - install SBCL (Common Lisp) and Tk, which Fancy Pencil runs on\n  - put Fancy Pencil in a folder of your choice\n  - add it to your app menu\n  - help you install the free ODA File Converter, for .dwg drawings\n\nYou will be asked for your password once, to install the system packages." || exit 0

# ---------------------------------------------------------------- 2. where and what -----------

while :; do
    CH=$(Z --list --radiolist --width=560 --height=260 --ok-label="Next" \
        --text="Where should Fancy Pencil go?" --column="" --column="Folder" \
        TRUE "$DEST" FALSE "Another folder...") || exit 0
    if [ "$CH" = "Another folder..." ]; then
        D=$(Z --file-selection --directory --filename="$HOME/") || continue
        DEST="$D/Fancy Pencil"
    fi
    break
done

PIC=TRUE; have_pictures && PIC=FALSE
OPTS=$(Z --list --checklist --width=600 --height=330 --ok-label="Next" --separator="|" \
    --text="Choose what to install:" --column="" --column="Option" --column="What it is" \
    TRUE  "App menu"        "Fancy Pencil in your applications menu, with its icon" \
    FALSE "Desktop icon"    "A Fancy Pencil icon on the desktop too" \
    $PIC  "Pictures and PDF" "ImageMagick and Poppler, for attaching images and PDF pages$( have_pictures && echo ' (already installed)')" \
    ) || exit 0
case "|$OPTS|" in *"|App menu|"*) WANT_MENU=1 ;; esac
case "|$OPTS|" in *"|Desktop icon|"*) WANT_DESKTOP=1 ;; esac
case "|$OPTS|" in *"|Pictures and PDF|"*) WANT_PICS=1 ;; esac

# ---------------------------------------------------------------- 3. ODA File Converter -------

ODA_FILE=
if oda_installed; then
    ODA_NOTE="ODA File Converter is already installed - .dwg files will open."
else
    while :; do
        A=$(Z --list --radiolist --width=640 --height=360 --ok-label="Next" \
            --text="<b>ODA File Converter</b> (free, from the Open Design Alliance) lets Fancy Pencil open and save AutoCAD .dwg drawings. Everything else works without it.\n\nFor this computer the <b>.$ODA_EXT</b> version fits." \
            --column="" --column="Choice" \
            TRUE  "Download it now (opens your web browser)" \
            FALSE "I have already downloaded it - choose the file" \
            FALSE "Open the ODA download page" \
            FALSE "Skip - no .dwg files for now") || exit 0
        case "$A" in
            Download*)
                xdg-open "$ODA_GET.$ODA_EXT" >/dev/null 2>&1 &
                Z --info --width=520 --ok-label="It has downloaded" \
                  --text="Your browser is downloading <b>ODAFileConverter...$ODA_EXT</b>.\n(If it asks, accept ODA's terms and save the file.)\n\nWhen the download has finished, click the button and pick the file.\n\nIf no download starts, go back and choose \"Open the ODA download page\":\n$ODA_PAGE" || continue
                ;;
            "Open the ODA"*)
                xdg-open "$ODA_PAGE" >/dev/null 2>&1 &
                Z --info --width=520 --ok-label="It has downloaded" \
                  --text="On the page, under Linux, download the <b>$ODA_EXT</b> file\n(DEB for Ubuntu/Debian/Mint, RPM for Fedora/openSUSE, AppImage for anything else).\n\nThen click the button and pick the file." || continue
                ;;
            Skip*) ODA_NOTE="ODA File Converter skipped - run this installer again any time to add it."; break ;;
        esac
        DL=$(xdg-user-dir DOWNLOAD 2>/dev/null); [ -d "$DL" ] || DL="$HOME/Downloads"
        F=$(Z --file-selection --filename="$DL/" --file-filter="ODA File Converter | ODAFileConverter*.deb ODAFileConverter*.rpm ODAFileConverter*.AppImage" --file-filter="All files | *") || continue
        case "$F" in
            *.deb) [ "$PM" = apt ] && { ODA_FILE=$F; break; } ;;
            *.rpm) [ "$PM" = dnf ] || [ "$PM" = zypper ] && { ODA_FILE=$F; break; } ;;
            *.AppImage|*.appimage) ODA_FILE=$F; break ;;
        esac
        Z --warning --width=480 --text="That file doesn't suit this computer.\nPlease choose the <b>.$ODA_EXT</b> (or the AppImage, which runs anywhere)."
    done
fi

# ---------------------------------------------------------------- 4. system packages ----------

PKGS=
command -v sbcl >/dev/null 2>&1 || PKGS="$PKGS sbcl"
command -v wish >/dev/null 2>&1 || PKGS="$PKGS tk"
[ -n "$WANT_PICS" ] && ! have_pictures && PKGS="$PKGS $(pictures_pkgs)"
case "$ODA_FILE" in *.AppImage|*.appimage) PKGS="$PKGS $(fuse_pkg)" ;; esac
ROOT=
if [ "$PM" = none ]; then
    if [ -n "$PKGS" ]; then
        Z --warning --width=500 --text="This computer's package manager isn't one this installer knows.\nPlease install these yourself, then run the installer again:\n\n   $PKGS"
        command -v sbcl >/dev/null 2>&1 && command -v wish >/dev/null 2>&1 || exit 1
    fi
else
    [ -n "$PKGS" ] && ROOT="$(pm_install_cmd $PKGS)"
    case "$ODA_FILE" in
        *.deb) ROOT="${ROOT:+$ROOT && }$(pm_install_cmd "\"$ODA_FILE\"")" ;;
        *.rpm) if [ "$PM" = zypper ]; then ROOT="${ROOT:+$ROOT && }zypper --non-interactive install --allow-unsigned-rpm \"$ODA_FILE\""
               else ROOT="${ROOT:+$ROOT && }dnf install -y \"$ODA_FILE\""; fi ;;
    esac
    [ "$PM" = apt ] && [ -n "$ROOT" ] && ROOT="apt-get update -q; $ROOT"
fi
if [ -n "$ROOT" ]; then
    echo "as root: $ROOT" >>"$LOG"
    if ! progress "Installing system packages:$PKGS${ODA_FILE:+ + ODA File Converter}\n(enter your password in the window that opens)" as_root "$ROOT"; then
        failed "Installing the system packages didn't work."
        command -v sbcl >/dev/null 2>&1 && command -v wish >/dev/null 2>&1 || exit 1
    fi
fi
case "$ODA_FILE" in
    *.AppImage|*.appimage)
        mkdir -p "$HOME/.local/bin" && cp "$ODA_FILE" "$HOME/.local/bin/ODAFileConverter" && chmod +x "$HOME/.local/bin/ODAFileConverter" \
            || failed "Couldn't put the ODA File Converter AppImage in ~/.local/bin." ;;
esac
if [ -n "$ODA_FILE" ]; then
    if oda_installed; then ODA_NOTE="ODA File Converter is installed - .dwg files will open and save."
    else ODA_NOTE="ODA File Converter didn't install; see $LOG"; fi
fi

# ---------------------------------------------------------------- 5. Fancy Pencil -------------

unpack() {
    mkdir -p "$DEST" && tar xzf "$KIT/fancypencil.tar.gz" -C "$DEST" && chmod +x "$DEST/fancypencil/fancypencil" || return 1
    if [ -n "$WANT_MENU" ] || [ -n "$WANT_DESKTOP" ]; then (cd "$DEST/fancypencil" && sh install.sh) || return 1; fi
    if [ -n "$WANT_DESKTOP" ]; then
        DESK=$(xdg-user-dir DESKTOP 2>/dev/null); [ -d "$DESK" ] || DESK="$HOME/Desktop"
        mkdir -p "$DESK" && cp "${XDG_DATA_HOME:-$HOME/.local/share}/applications/fancypencil.desktop" "$DESK/" \
            && chmod +x "$DESK/fancypencil.desktop" \
            && { gio set "$DESK/fancypencil.desktop" metadata::trusted true 2>/dev/null; true; }
    fi
    if [ -z "$WANT_MENU" ]; then rm -f "${XDG_DATA_HOME:-$HOME/.local/share}/applications/fancypencil.desktop"; fi
    # compile now, so the first start is quick
    (cd "$DEST/fancypencil" && ./fancypencil --tty </dev/null >/dev/null 2>&1) || true
}
progress "Putting Fancy Pencil in $DEST\nand preparing it for its first start (a minute or so)..." unpack \
    || { failed "Fancy Pencil couldn't be unpacked into $DEST."; exit 1; }

# ---------------------------------------------------------------- 6. done ---------------------

if Z --question --width=540 --ok-label="Start Fancy Pencil" --cancel-label="Close" \
     --text="<big><b>Fancy Pencil ${VERSION} is installed.</b></big>\n\nFolder: $DEST/fancypencil\n${WANT_MENU:+It is in your app menu.\n}${ODA_NOTE}\n\nTo update later, put a newer kit in this folder and run the installer again."; then
    (cd "$HOME" && nohup sh "$DEST/fancypencil/fancypencil" >/dev/null 2>&1 &)
fi
exit 0
