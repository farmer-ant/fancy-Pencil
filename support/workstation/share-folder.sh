#!/bin/sh
# Share a folder of drawings on the network, so Fancy Pencil Workstations (and other computers)
# can open and save drawings in it. Run it on the computer that has the drawings (an Ubuntu
# desktop like the Lenovo, or a shop file server):
#
#   sudo sh share-folder.sh "/run/media/jack/more/Fancy Pencil/drawings"          share it (name: drawings)
#   sudo sh share-folder.sh "/run/media/jack/more/Fancy Pencil/drawings" shop     ... as "shop"
#   sudo sh share-folder.sh --list                                                what is shared
#   sudo sh share-folder.sh --remove shop                                         stop sharing it
#
# It uses Samba (Windows file sharing), which Windows computers and network drives (NAS) speak too.
# The folder is shared for your own account: the workstations sign in with your user name and a
# Samba password you choose the first time (it can differ from your login password), and the files
# they save belong to you. Then, on each workstation:
#   sudo sh add-share.sh //THIS-COMPUTER'S-ADDRESS/NAME
set -e

conf=/etc/samba/smb.conf
say() { printf '\n== %s\n' "$*"; }
die() { printf '\nSTOPPED: %s\n' "$*" >&2; exit 1; }

[ "$(id -u)" = 0 ] || die "run it with sudo:  sudo sh $0 $*"
owner=${SUDO_USER:-}
[ -n "$owner" ] && [ "$owner" != root ] || die "run it with sudo from your own account (not as root), so the folder is shared for you"

clean_name() { printf '%s' "$1" | tr -c 'A-Za-z0-9._-' '_' | cut -c1-40; }

list_shares() {
    [ -f "$conf" ] || { echo "(Samba isn't set up here)"; return; }
    awk '/^# fancypencil-share .* begin$/ { n = $3; next }
         n != "" && $1 == "path" { sub(/^[ \t]*path = /, ""); printf "  %-16s %s\n", n, $0; n = "" }' "$conf"
}
remove_share() {  # remove_share NAME: takes our block for NAME out of smb.conf
    [ -f "$conf" ] || return 0
    awk -v n="$1" '$0 == "# fancypencil-share " n " begin" { skip = 1; next }
                   skip && $0 == "# fancypencil-share " n " end" { skip = 0; next }
                   !skip' "$conf" > "$conf.fp-new" && cat "$conf.fp-new" > "$conf" && rm -f "$conf.fp-new"
}

case "${1:-}" in
    ""|-h|--help) sed -n '2,17p' "$0"; exit 0 ;;
    --list) echo "Folders shared by share-folder.sh:"; list_shares; exit 0 ;;
    --remove)
        [ -n "${2:-}" ] || die "which one?  sudo sh $0 --remove NAME   (see --list)"
        n=$(clean_name "$2")
        grep -qx "# fancypencil-share $n begin" "$conf" 2>/dev/null || die "nothing called $n is shared (see --list)"
        remove_share "$n"
        systemctl reload smbd 2>/dev/null || systemctl restart smbd
        say "$n is no longer shared. (The folder and its drawings are untouched.)"
        exit 0 ;;
esac

folder=$1
[ -d "$folder" ] || die "there is no folder $folder"
folder=$(cd "$folder" && pwd)
case "$folder" in *'"'*|*'\'*) die "the folder's path has a \" or \\ in it, which Samba can't share - rename it first" ;; esac
name=$(clean_name "${2:-$(basename "$folder")}")
su -s /bin/sh "$owner" -c 'test -r "$1" && test -x "$1"' sh "$folder" || die "$owner can't open $folder"

if ! command -v smbd >/dev/null 2>&1; then
    say "Installing Samba (Windows file sharing)"
    apt-get update
    apt-get install -y samba
fi
[ -f "$conf" ] || die "Samba's settings ($conf) aren't there"
[ -f "$conf.before-fancypencil" ] || cp "$conf" "$conf.before-fancypencil"

say "Sharing $folder as \"$name\""
remove_share "$name"
cat >> "$conf" <<EOF
# fancypencil-share $name begin
[$name]
   path = $folder
   comment = Fancy Pencil drawings
   read only = no
   browseable = yes
   valid users = $owner
   force user = $owner
   create mask = 0664
   directory mask = 0775
# fancypencil-share $name end
EOF
if command -v testparm >/dev/null 2>&1 && ! testparm -s "$conf" >/dev/null 2>&1; then
    testparm -s "$conf" 2>&1 | tail -n 5
    die "Samba doesn't accept its settings now; look at $conf (the old one is $conf.before-fancypencil)"
fi

# the Samba password: once per account
if ! pdbedit -L 2>/dev/null | cut -d: -f1 | grep -qx "$owner"; then
    say "Choose a Samba password for $owner: the workstations sign in to this share with it"
    echo "   (type it twice; nothing shows while you type)"
    smbpasswd -a "$owner"
fi

systemctl enable smbd >/dev/null 2>&1 || true
systemctl restart smbd

# the firewall, if it's switched on
if command -v ufw >/dev/null 2>&1 && ufw status 2>/dev/null | grep -q "^Status: active"; then
    say "Letting Samba through the firewall"
    ufw allow samba
fi

addr=$(hostname -I 2>/dev/null | tr ' ' '\n' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | head -n 1)
say "Done. \"$name\" is shared from this computer (${addr:-its address: see Settings > Network})."
echo "   On each Fancy Pencil Workstation (in a terminal there, or over ssh), run:"
echo "       sudo sh add-share.sh //${addr:-ADDRESS}/$name"
echo "   and sign in as $owner with the Samba password. It shows in File > Open under Network."
case "$folder" in
    /run/media/*|/media/*)
        echo "   Note: $folder is on a drive that is opened when you log in here. It is shared while"
        echo "   that drive is plugged in and opened (log in on this computer after it starts)." ;;
esac
echo "   Stop sharing it:  sudo sh $0 --remove $name"
