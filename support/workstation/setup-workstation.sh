#!/bin/sh
# Fancy Pencil Workstation: makes an Ubuntu computer start straight into Fancy Pencil.
#
#   sudo sh setup-workstation.sh            set it up (GRUB menu: "Ubuntu" stays the first choice)
#   sudo sh setup-workstation.sh --default  ... and make "Fancy Pencil Workstation" the first choice
#   sudo sh setup-workstation.sh --remove   take it all out again (the fancypencil user and its
#                                           drawings are kept)
#   sudo sh setup-workstation.sh --cage     use the cage Wayland kiosk instead of Xorg + Openbox
#
# What it does:
#   - installs Fancy Pencil (Install Fancy Pencil.deb beside this script, unless it is installed),
#     and the ODA File Converter for .dwg files if its .deb or AppImage is beside this script
#   - installs Xorg and Openbox (set up as a kiosk: Fancy Pencil fills the screen, its dialogs on
#     top), and udiskie (USB sticks); or, with --cage, the cage Wayland kiosk and Xwayland
#   - makes a user "fancypencil" that is logged in automatically, with ~/Drawings, and lets
#     Fancy Pencil use four fifths of the computer's memory
#   - adds the GRUB menu entry "Fancy Pencil Workstation" and shows the menu for 5 seconds at start-up
#   - puts in your own settings if a "Fancy Pencil Settings ....tar.gz" (File > Save My Settings...)
#     is beside this script
# Network shares (a folder of drawings on another computer): add-share.sh, after this.
# The normal "Ubuntu" entry is unchanged: it starts the desktop as before.
set -e

here=$(cd "$(dirname "$0")" && pwd)
files="$here/files"
lib=/usr/local/lib/fancypencil-workstation
user=fancypencil
backup=/etc/default/grub.before-fancypencil-workstation

say() { printf '\n== %s\n' "$*"; }
die() { printf '\nSTOPPED: %s\n' "$*" >&2; exit 1; }

[ "$(id -u)" = 0 ] || die "run it with sudo:  sudo sh $0 $*"
command -v apt-get >/dev/null || die "this is for Ubuntu (or Debian): apt-get wasn't found"
[ -d "$files" ] || die "the files/ folder isn't beside this script ($files)"
if [ ! -d /etc/grub.d ] || [ ! -f /etc/default/grub ] || ! command -v update-grub >/dev/null; then
    die "this computer doesn't start with GRUB (no /etc/grub.d, /etc/default/grub or update-grub), so nothing was changed"
fi

mode=install; make_default=no; display=x11
for a in "$@"; do
    case "$a" in
        --remove) mode=remove ;;
        --default) make_default=yes ;;
        --cage) display=cage ;;
        --x11) display=x11 ;;
        -h|--help) sed -n '2,23p' "$0"; exit 0 ;;
        *) die "unknown option $a (use --default, --cage or --remove)" ;;
    esac
done

set_grub() {  # set_grub KEY VALUE: in /etc/default/grub
    if grep -q "^$1=" /etc/default/grub; then
        sed -i "s|^$1=.*|$1=$2|" /etc/default/grub
    else
        printf '%s=%s\n' "$1" "$2" >> /etc/default/grub
    fi
}

if [ "$mode" = remove ]; then
    say "Taking out the Fancy Pencil Workstation"
    rm -f /etc/grub.d/41_fancypencil-workstation
    if [ -f "$backup" ]; then cp "$backup" /etc/default/grub && rm -f "$backup"; fi
    update-grub
    rm -f /etc/systemd/system/fancypencil-workstation.service /etc/systemd/system/fancypencil-workstation.target
    rm -f /etc/pam.d/fancypencil-workstation /etc/fancypencil-workstation.conf
    if grep -q '^# fancypencil-share ' /etc/fstab 2>/dev/null; then
        echo "   (network shares connected with add-share.sh stay: sudo sh add-share.sh --list / --remove NAME)"
    fi
    rm -rf "$lib"
    systemctl daemon-reload
    say "Done. The GRUB menu is as it was. The user '$user' and /home/$user (its drawings) are still there;"
    echo "   to delete them too:  sudo deluser --remove-home $user"
    exit 0
fi

. /etc/os-release 2>/dev/null || true
say "Setting up a Fancy Pencil Workstation on ${PRETTY_NAME:-this computer}"

# 1. Fancy Pencil
deb=$(ls "$here"/*Fancy*Pencil*.deb "$here"/../*Fancy*Pencil*.deb 2>/dev/null | head -n 1 || true)
if [ -n "$deb" ]; then
    say "Installing Fancy Pencil from $(basename "$deb")"
    cp "$deb" /tmp/fancypencil-install.deb      # apt reads it more happily from /tmp than from a USB stick
    apt-get update
    apt-get install -y /tmp/fancypencil-install.deb
    rm -f /tmp/fancypencil-install.deb
elif [ -x /usr/bin/fancypencil ]; then
    say "Fancy Pencil is installed already ($(readlink -f /usr/bin/fancypencil))"
    apt-get update
else
    die "put 'Install Fancy Pencil.deb' beside this script (or install it first)"
fi

# 2. the display: Xorg + Openbox (default) or cage; udiskie for USB sticks
if [ "$display" = cage ]; then
    say "Installing cage (a full-screen Wayland display server), Xwayland and udiskie (USB sticks)"
    apt-get install -y cage xwayland udiskie
else
    say "Installing Xorg, Openbox (the window manager, set up as a kiosk) and udiskie (USB sticks)"
    apt-get install -y xserver-xorg xinit x11-xserver-utils x11-utils openbox udiskie
fi

# 3. ODA File Converter (.dwg), if it came along
oda=$(ls "$here"/ODAFileConverter*.deb "$here"/../ODAFileConverter*.deb 2>/dev/null | head -n 1 || true)
odaimg=$(ls "$here"/ODAFileConverter*.AppImage "$here"/../ODAFileConverter*.AppImage 2>/dev/null | head -n 1 || true)
if [ -n "$oda" ]; then
    say "Installing the ODA File Converter (.dwg files) from $(basename "$oda")"
    cp "$oda" /tmp/oda-install.deb && apt-get install -y /tmp/oda-install.deb; rm -f /tmp/oda-install.deb
elif [ -n "$odaimg" ]; then
    say "Installing the ODA File Converter AppImage (.dwg files)"
    apt-get install -y libfuse2t64 || apt-get install -y libfuse2 || true
    install -m 755 "$odaimg" /usr/local/bin/ODAFileConverter
elif command -v ODAFileConverter >/dev/null 2>&1; then
    say "ODA File Converter is installed already"
else
    echo "   (No ODA File Converter beside this script: the workstation opens and saves .dxf;"
    echo "    for .dwg put ODAFileConverter_*.deb beside this script and run it again.)"
fi

# 4. the user that is logged in automatically
if ! id "$user" >/dev/null 2>&1; then
    say "Making the user '$user'"
    useradd --create-home --comment "Fancy Pencil" --shell /bin/bash "$user"
    passwd --lock "$user" >/dev/null
fi
for g in video render input plugdev audio; do
    getent group "$g" >/dev/null && usermod -a -G "$g" "$user"
done
home=$(getent passwd "$user" | cut -d: -f6)
ram=$(awk '/MemTotal/ { print int($2 / 1024) }' /proc/meminfo)
mkdir -p "$home/Drawings" "$home/.config/fancypencil"
if [ ! -f "$home/.config/fancypencil/memory.conf" ]; then
    # a dedicated machine: Fancy Pencil may use four fifths of the memory, cleaning up rarely
    printf '# Fancy Pencil memory settings (Options > Memory and speed)\nheap=%s\nspeed=0\n' "$((ram * 4 / 5))" \
        > "$home/.config/fancypencil/memory.conf"
fi
# your own Fancy Pencil (File > Save My Settings... on your computer), if its file came along
settings=$(ls -t "$here"/*"Fancy Pencil Settings"*.tar.gz "$here"/../*"Fancy Pencil Settings"*.tar.gz 2>/dev/null | head -n 1 || true)
stamp="$home/.config/fancypencil/.settings-from"
if [ -n "$settings" ] && [ "$(cat "$stamp" 2>/dev/null)" = "$(basename "$settings")" ]; then
    echo "   (your settings from $(basename "$settings") are in already; a newer file is put in when it's here)"
elif [ -n "$settings" ]; then
    say "Putting in your settings from $(basename "$settings")"
    t=$(mktemp -d)
    # only Fancy Pencil's own files come out of it (not the other computer's memory setting)
    tar tzf "$settings" 2>/dev/null | grep -E '^(\./)?(\.config/fancypencil/.*[^/]|\.fancypencil\.lsp)$' \
        | grep -v -e '\.\./' -e '/memory\.conf$' -e '/recent\.lisp$' > "$t.list" || true
    if [ -s "$t.list" ] && tar xzf "$settings" -C "$t" --no-same-owner --no-same-permissions -T "$t.list"; then
        (cd "$t" && find . -type f) | while read -r f; do
            install -D -m 644 "$t/$f" "$home/${f#./}"
        done
        basename "$settings" > "$stamp"
        echo "   $(wc -l < "$t.list") files: options, aliases, toolbars, ribbon, plot settings, fonts..."
    else
        echo "   (couldn't read it - the workstation starts with Fancy Pencil's standard settings)"
    fi
    rm -rf "$t" "$t.list"
fi
chown -R "$user:$user" "$home/Drawings" "$home/.config"
if [ -f "$home/.fancypencil.lsp" ]; then chown "$user:$user" "$home/.fancypencil.lsp"; fi

# 5. the session
say "Installing the workstation session"
install -d "$lib"
install -m 755 "$files/start" "$files/session" "$files/xsession" "$lib/"
install -m 644 "$files/openbox-rc.xml" "$lib/"
printf '# Fancy Pencil Workstation (setup-workstation.sh): x11 (Xorg + Openbox) or cage\nDISPLAY_SERVER=%s\n' "$display" \
    > /etc/fancypencil-workstation.conf
install -m 644 "$files/fancypencil-workstation.service" "$files/fancypencil-workstation.target" /etc/systemd/system/
install -m 644 "$files/pam-fancypencil-workstation" /etc/pam.d/fancypencil-workstation
systemctl daemon-reload

# 6. the GRUB menu
say "Adding 'Fancy Pencil Workstation' to the GRUB menu"
[ -f "$backup" ] || cp /etc/default/grub "$backup"
install -m 755 "$files/41_fancypencil-workstation" /etc/grub.d/
set_grub GRUB_TIMEOUT_STYLE menu
set_grub GRUB_TIMEOUT 5
set_grub GRUB_RECORDFAIL_TIMEOUT 5
if [ "$make_default" = yes ]; then set_grub GRUB_DEFAULT fancypencil-workstation; fi
update-grub

say "Done. (Display: $([ "$display" = cage ] && echo 'cage, Wayland' || echo 'Xorg with Openbox'))"
echo "   Restart the computer. The GRUB menu shows for 5 seconds:"
echo "     Ubuntu                     - the desktop, as before"
echo "     Fancy Pencil Workstation   - straight into Fancy Pencil (File > Shut Down / Restart Computer)"
[ "$make_default" = yes ] && echo "   Fancy Pencil Workstation is the first choice."
echo "   Drawings are kept in $home/Drawings; USB sticks appear under /media/$user/."
echo "   Shared folders on the network:  sudo sh add-share.sh //COMPUTER/SHARE   (see README)"
echo "   On the workstation, Ctrl+Alt+F3 gives a text login for maintenance (Ctrl+Alt+F1 goes back)."
echo "   To take it out again:  sudo sh $0 --remove"
