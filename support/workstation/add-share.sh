#!/bin/sh
# Open drawings from a shared folder on the network in Fancy Pencil. Run it on the workstation
# (or any Ubuntu computer with Fancy Pencil):
#
#   sudo sh add-share.sh //10.0.0.20/drawings          connect it (asks the user name and password)
#   sudo sh add-share.sh //10.0.0.20/drawings shop     ... and call it "shop"
#   sudo sh add-share.sh --guest //nas/public          a share that needs no password
#   sudo sh add-share.sh --list                        the shares connected here
#   sudo sh add-share.sh --remove shop                 disconnect one
#
# The address is //COMPUTER/SHARE: the computer's address (like 10.0.0.20, or its name) and the
# shared folder's name - what share-folder.sh printed, or the share of a Windows computer or NAS.
# smb://10.0.0.20/drawings and \\10.0.0.20\drawings work too.
# The share appears in Fancy Pencil's File > Open and Save As under "Network" (it is the folder
# /mnt/NAME). It is connected when it is first opened, so the workstation starts even when the
# other computer is off, and it comes back by itself after every restart.
# The password is kept in /etc/fancypencil-workstation/shares/, readable by root only.
set -e

fstab=/etc/fstab
creds=/etc/fancypencil-workstation/shares
say() { printf '\n== %s\n' "$*"; }
die() { printf '\nSTOPPED: %s\n' "$*" >&2; exit 1; }

[ "$(id -u)" = 0 ] || die "run it with sudo:  sudo sh $0 $*"

clean_name() { printf '%s' "$1" | tr -c 'A-Za-z0-9._-' '_' | cut -c1-40; }

list_shares() {
    awk '/^# fancypencil-share / { n = $3; getline; s = $1; gsub(/\\040/, " ", s); printf "  %-22s %-34s /mnt/%s\n", n, s, n }' "$fstab"
}
remove_entry() {  # remove_entry NAME: our two fstab lines for NAME
    awk -v n="$1" '$0 == "# fancypencil-share " n { skip = 1; next } skip { skip = 0; next } { print }' "$fstab" \
        > "$fstab.fp-new" && cat "$fstab.fp-new" > "$fstab" && rm -f "$fstab.fp-new"
}
unit_of() { systemd-escape --path --suffix="$2" "/mnt/$1" 2>/dev/null || echo "mnt-$1.$2"; }

guest=no
case "${1:-}" in
    ""|-h|--help) sed -n '2,19p' "$0"; exit 0 ;;
    --list) echo "Network shares (add-share.sh):"; list_shares; exit 0 ;;
    --remove)
        [ -n "${2:-}" ] || die "which one?  sudo sh $0 --remove NAME   (see --list)"
        n=$(clean_name "$2")
        grep -qx "# fancypencil-share $n" "$fstab" || die "there is no share called $n (see --list)"
        systemctl stop "$(unit_of "$n" automount)" "$(unit_of "$n" mount)" 2>/dev/null || true
        umount -l "/mnt/$n" 2>/dev/null || true
        remove_entry "$n"
        rm -f "$creds/$n.cred"
        rmdir "/mnt/$n" 2>/dev/null || true
        systemctl daemon-reload
        say "$n is disconnected. (Nothing on the other computer was changed.)"
        exit 0 ;;
    --guest) guest=yes; shift ;;
esac

# //server/share, smb://server/share or \\server\share
addr=$(printf '%s' "${1:-}" | sed -e 's|^smb:||' -e 's|\\|/|g' -e 's|/*$||')
case "$addr" in
    //?*/?*) ;;
    *) die "give the share as //COMPUTER/SHARE (like //10.0.0.20/drawings)" ;;
esac
server=$(printf '%s' "$addr" | cut -d/ -f3)
share=$(printf '%s' "$addr" | cut -d/ -f4-)
name=$(clean_name "${2:-$(basename "$share")}")
[ -n "$name" ] || die "no name for it: add one, like  sudo sh $0 $1 drawings"

# who the files belong to here: the workstation's user, else the one running sudo
if id fancypencil >/dev/null 2>&1; then owner=fancypencil; else owner=${SUDO_USER:-root}; fi

if ! command -v mount.cifs >/dev/null 2>&1; then
    say "Installing cifs-utils (to connect to shared folders)"
    apt-get update
    apt-get install -y cifs-utils
fi

install -d -m 700 "$creds"
if [ "$guest" = yes ]; then
    auth=guest
else
    say "Signing in to //$server/$share"
    printf '   User name on %s: ' "$server"; IFS= read -r user
    [ -n "$user" ] || die "no user name (for a share without a password use --guest)"
    domain=
    case "$user" in *\\*) domain=${user%%\\*}; user=${user#*\\} ;; esac
    printf '   Password (nothing shows while you type): '
    stty -echo 2>/dev/null || true
    trap 'stty echo 2>/dev/null || true' EXIT
    trap 'stty echo 2>/dev/null; exit 1' INT
    IFS= read -r pass
    stty echo 2>/dev/null || true
    echo
    ( umask 077
      { printf 'username=%s\npassword=%s\n' "$user" "$pass"
        if [ -n "$domain" ]; then printf 'domain=%s\n' "$domain"; fi; } > "$creds/$name.cred" )
    chmod 600 "$creds/$name.cred"
    auth="credentials=$creds/$name.cred"
fi

say "Connecting it as /mnt/$name"
systemctl stop "$(unit_of "$name" automount)" "$(unit_of "$name" mount)" 2>/dev/null || true
umount -l "/mnt/$name" 2>/dev/null || true
remove_entry "$name"
mkdir -p "/mnt/$name"
src=$(printf '//%s/%s' "$server" "$share" | sed 's/ /\\040/g')
uid=$(id -u "$owner"); gid=$(id -g "$owner")
printf '# fancypencil-share %s\n%s /mnt/%s cifs %s,uid=%s,gid=%s,file_mode=0664,dir_mode=0775,iocharset=utf8,nofail,_netdev,x-systemd.automount,x-systemd.idle-timeout=600,x-systemd.mount-timeout=20 0 0\n' \
    "$name" "$src" "$name" "$auth" "$uid" "$gid" >> "$fstab"
systemctl daemon-reload
systemctl restart remote-fs.target 2>/dev/null || systemctl start "$(unit_of "$name" automount)" || true

# try it now
if timeout 40 ls "/mnt/$name" >/dev/null 2>&1; then
    n=$(ls "/mnt/$name" | wc -l)
    say "Done. //$server/$share is connected: $n things in it."
    echo "   In Fancy Pencil it is under Network in File > Open (the folder /mnt/$name)."
    echo "   Disconnect it:  sudo sh $0 --remove $name"
else
    say "It is set up, but connecting failed just now:"
    journalctl -u "$(unit_of "$name" mount)" -n 6 --no-pager 2>/dev/null | tail -n 4 || true
    echo "   - Is the other computer on, and is //$server/$share right? (share-folder.sh --list there)"
    echo "   - A wrong user name or password shows as \"Permission denied\" (error 13): run this again."
    echo "   - \"Host is down\" or a timeout: the address is wrong, or a firewall is in the way."
    echo "   It tries again each time the folder is opened. To take it out:  sudo sh $0 --remove $name"
    exit 1
fi
