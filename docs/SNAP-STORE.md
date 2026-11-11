# Putting Fancy Pencil in the Snap Store (Ubuntu's App Center)

App Center installs programs from the Snap Store. Fancy Pencil's Snap Store package is defined
in `snap/snapcraft.yaml`. These steps build it on your computer and publish it.

You do steps 1 and 2 once. Steps 3 to 6 are what you repeat for each new version.

## 1. Make a Snap Store developer account (once, in your web browser)

1. Go to **snapcraft.io** and click **Sign in**. Create an Ubuntu One account, or use the one you
   already have.
2. Accept the developer agreement, and choose your **publisher name**. App Center shows it on
   the listing, in place of "Unknown publisher".

## 2. Install the building tools (once, in a terminal)

```
sudo snap install snapcraft --classic
sudo snap install lxd
sudo lxd init --auto
sudo usermod -aG lxd $USER
```

Then **log out and back in**, so the last line takes effect. snapcraft uses LXD to build in a
clean Ubuntu container, so nothing on your own system is changed.

## 3. Unpack the Fancy Pencil source

```
mkdir -p ~/fancypencil-snap && cd ~/fancypencil-snap
tar xzf ~/Downloads/"fancy pencil project"/fancypencil-0.4.12.tar.gz
cd fancypencil
```

## 4. Build the package

```
snapcraft
```

The first build downloads Ubuntu's build container and packages, about 1 GB, and takes 10 to
20 minutes. Later builds are quicker. It ends with a file called
`fancypencil_0.4.12_amd64.snap` in this folder.

If it stops with an error, copy the last 30 lines or so and send them to Claude.

## 5. Try it before publishing

```
sudo snap install --dangerous ./fancypencil_0.4.12_amd64.snap
```

Start Fancy Pencil from the app menu and try it: open and save a drawing, plot to PDF, and
press F1 for the manual. To reach drives such as "more", connect the removable-media
permission, either in **App Center > Manage > Fancy Pencil > Permissions**, or with:

```
sudo snap connect fancypencil:removable-media
```

When you are done trying it, remove the test copy: `sudo snap remove fancypencil`

## 6. Publish

```
snapcraft login
snapcraft register fancypencil          # only the first time
snapcraft upload --release=stable fancypencil_0.4.12_amd64.snap
```

If the name `fancypencil` is taken, register another one, such as `fancypencil-cad`, and change
the `name:` line at the top of `snap/snapcraft.yaml` to match.

The store checks the upload automatically. A strictly confined snap like this one normally
passes within minutes. You can then install it like anything else: search **Fancy Pencil** in
App Center.

## 7. The listing page (once, in your web browser)

At **snapcraft.io > My snaps > fancypencil > Listing**, add:

- **Screenshots**: the five in `docs/store-screenshots/`
- **Category**: Graphics (and Productivity)
- **Contact** and **website**, if you have them
- **Visibility**: Public

Then click **Save**.

## What is different in the Snap Store edition

- It runs in Snap's safety box ("strict confinement"). It can open files in your home folder,
  and on drives (once removable-media is connected), and it can print.
- It opens and saves **DXF** only. It can't use an ODA File Converter installed on your
  computer, so for **.dwg** use the Kit edition (Install Fancy Pencil.deb).
- Its settings live in `~/snap/fancypencil/`, separate from the Kit edition's
  `~/.config/fancypencil`.
- Updates arrive automatically after you publish a new version (steps 3 to 6).
