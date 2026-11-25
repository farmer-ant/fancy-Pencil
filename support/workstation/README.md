# Fancy Pencil Workstation

Makes an Ubuntu computer start straight into Fancy Pencil, full screen, with no desktop: a
dedicated modeling workstation. The GRUB start-up menu gets a new entry:

    Ubuntu                      the desktop, exactly as before (for setting up and maintenance)
    Fancy Pencil Workstation    straight into Fancy Pencil

On the workstation, Fancy Pencil's **File** menu has **Restart Computer** and **Shut Down**
(both offer to save changed drawings first). If Fancy Pencil is closed, it starts again.

## What you need

- A computer with Ubuntu (made for Ubuntu 26.04; 24.04 and Debian work the same way) that starts
  with GRUB, and an internet connection while setting up.
- This folder: `setup-workstation.sh`, the `files` folder, and `Install Fancy Pencil.deb`.
- For .dwg files: the ODA File Converter for Ubuntu (`ODAFileConverter_*.deb`), downloaded from
  https://www.opendesign.com/guestfiles/oda_file_converter and put in this folder. (Its licence
  doesn't let us include it.) Without it the workstation opens and saves .dxf.

## Setting it up

1. Copy this folder onto a USB stick, and plug the stick into the computer.
2. In Ubuntu, connect to the network (Wi-Fi in Settings) - the workstation uses the same
   connection.
3. Open the folder on the stick, right-click in it and choose **Open in Terminal**, then type:

       sudo sh setup-workstation.sh

   or, to make the computer start in Fancy Pencil unless you choose Ubuntu:

       sudo sh setup-workstation.sh --default

   (Running it again is safe: it updates what's there. `--cage` uses the cage Wayland kiosk
   instead of Xorg + Openbox.)

   (Use `sudo sh ...`: a USB stick doesn't keep the "may run" mark on files.)
4. Restart, and choose **Fancy Pencil Workstation** in the menu (it shows for 5 seconds).

## Using it

- Drawings are kept in `/home/fancypencil/Drawings`.
- USB sticks are opened when they're plugged in: they appear under `/media/fancypencil/` in
  File > Open and Save As.
- Fancy Pencil may use four fifths of the computer's memory (Tools > Options > Memory and speed).
- File > Check for Updates works as usual.
- **Maintenance:** Ctrl+Alt+F3 gives a text login (use your own account; Ctrl+Alt+F1 goes back to
  Fancy Pencil), or restart and choose **Ubuntu** for the desktop.

## When something is wrong

`collect-log.sh` writes a report (the session, the windows on the screen, the logs of Fancy Pencil and
Xorg). Run it while the problem shows: Ctrl+Alt+F3, log in with your own account, and
`sudo sh collect-log.sh` from this folder - or over SSH from another computer. The report is
`workstation-report.txt` beside the script and in `/var/tmp`.

## Taking it out

    sudo sh setup-workstation.sh --remove

puts the GRUB menu back as it was and removes the workstation session. The `fancypencil` user and
its drawings are kept (`sudo deluser --remove-home fancypencil` deletes them).

## How it works

- `fancypencil-workstation.target`: a systemd target like the desktop's `graphical.target`, without
  the login screen; the GRUB entry starts the computer with `systemd.unit=fancypencil-workstation.target`.
- `fancypencil-workstation.service`: logs the `fancypencil` user in on the first screen (tty1)
  and starts the display (`start`, as `/etc/fancypencil-workstation.conf` says):
  - **x11** (the default): Xorg with Openbox as a kiosk window manager (`xsession`,
    `openbox-rc.xml`): no desktop menu or window-manager keys, Fancy Pencil's main window maximized
    without borders, dialogs on top with a title bar to move them by. Fancy Pencil is a Tk (X11)
    program, and this is the setting Tk is made for. (0.5.31's Openbox settings took every left
    click for themselves - nothing in Fancy Pencil responded to the mouse; 0.5.32 fixes that.)
  - **cage** (`--cage`): a Wayland display server that shows one program full screen; Fancy Pencil
    runs in it through Xwayland. The first trial of 0.5.30 froze when Options opened there; 0.5.31
    no longer waits for a dialog to be reported visible, and never grabs one that isn't.
  `udiskie` mounts USB sticks.
- `41_fancypencil-workstation` in `/etc/grub.d/`: makes the menu entry from the newest kernel each
  time `update-grub` runs (Ubuntu runs it after every kernel update). The old `/etc/default/grub` is
  kept as `/etc/default/grub.before-fancypencil-workstation`.
- If the workstation doesn't start (a black screen, or back at a text login), choose Ubuntu, and
  look at what happened with `journalctl -b -1 -u fancypencil-workstation`.
