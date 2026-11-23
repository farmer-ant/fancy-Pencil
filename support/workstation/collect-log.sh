#!/bin/sh
# Fancy Pencil Workstation: a report of what is on the screen and why, for finding problems.
# Run it while the problem is showing:
#   1. On the workstation press Ctrl+Alt+F3 and log in with your own account.
#   2. sudo sh /media/fancypencil/FANCYPENSIL/collect-log.sh
#      (the USB stick plugged in; if it isn't there, run this script from wherever it is)
#   3. Ctrl+Alt+F1 goes back to Fancy Pencil.
# The report is written beside this script as workstation-report.txt, and to
# /var/tmp/workstation-report.txt (copy that one from the Ubuntu desktop if the stick wasn't there).
u=fancypencil
h=$(getent passwd "$u" | cut -d: -f6)
here=$(cd "$(dirname "$0")" && pwd)
tmp=/var/tmp/workstation-report.txt
[ "$(id -u)" = 0 ] || { echo "run it with sudo:  sudo sh $0"; exit 1; }
if ! command -v xwininfo >/dev/null 2>&1; then
    apt-get install -y x11-utils >/dev/null 2>&1 || true     # xwininfo: the list of windows
fi
x() { sudo -u "$u" env DISPLAY=:0 XAUTHORITY="$h/.Xauthority" "$@" 2>&1; }
{
echo "== $(date)   $(. /etc/os-release 2>/dev/null; echo "$PRETTY_NAME")   kernel $(uname -r)"
echo "== setting:"; cat /etc/fancypencil-workstation.conf 2>&1
echo; echo "== the session, this start"; journalctl -b -u fancypencil-workstation --no-pager 2>&1 | tail -n 60
echo; echo "== logins"; loginctl list-sessions --no-pager 2>&1
for s in $(loginctl list-sessions --no-legend 2>/dev/null | awk -v u="$u" '$3 == u { print $1 }'); do
    loginctl show-session "$s" -p Type -p Class -p Active -p State -p Seat -p VTNr -p TTY --no-pager 2>&1
done
echo; echo "== its programs"; ps -u "$u" -o pid,etime,stat,pcpu,rss,args --forest 2>&1
echo; echo "== windows on the screen"
if command -v xwininfo >/dev/null 2>&1; then x xwininfo -root -tree | head -n 120; else echo "(xwininfo isn't installed)"; fi
echo; echo "== which window has the keyboard"
command -v xprop >/dev/null 2>&1 && x xprop -root _NET_ACTIVE_WINDOW
echo; echo "== Fancy Pencil's log"; tail -n 100 "$h/.cache/fancypencil.log" 2>&1
echo; echo "== Xorg's log (errors, warnings, input devices)"
for f in "$h/.local/share/xorg/Xorg.0.log" /var/log/Xorg.0.log; do
    [ -f "$f" ] || continue
    echo "-- $f"
    grep -E "\(EE\)|\(WW\)|Adding input device|Using input driver|systemd-logind|connected|modeset\(|Fatal" "$f" | tail -n 100
done
echo; echo "== safety copies"; ls -la "$h/.local/share/fancypencil/autosave/" 2>&1
echo; echo "== memory"; free -m 2>&1; cat "$h/.config/fancypencil/memory.conf" 2>&1
} > "$tmp" 2>&1
chmod 644 "$tmp"
cp "$tmp" "$here/workstation-report.txt" 2>/dev/null && echo "Written: $here/workstation-report.txt"
echo "Written: $tmp"
