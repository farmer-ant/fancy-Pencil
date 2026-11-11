# Fancy Pencil installer - the program inside the kit's single "install" file.
#
# This script, the Fancy Pencil logo and fancypencil.tar.gz are packed (zipfs) onto a
# self-contained Tcl/Tk 9 program, so "install" runs on a fresh computer with nothing else
# installed: double-click it and this window comes up. See support/kit/build-installer.sh.

package require Tk
set APP [file dirname [info script]]
set VERSION [string trim [read [set f [open $APP/VERSION.txt]]][close $f]]
set ODA_PAGE "https://www.opendesign.com/guestfiles/oda_file_converter"
set ODA_GET  "https://www.opendesign.com/guestfiles/get?filename=ODAFileConverter_QT6_lnxX64_11dll"
set HOME $::env(HOME)
set LOG $HOME/.cache/fancypencil-install.log
file mkdir $HOME/.cache

# ---------------------------------------------------------------- the computer --------------

proc have {cmd} { expr {[auto_execok $cmd] ne ""} }
if {[have apt-get]} {
    set PM apt; set ODA_EXT deb
} elseif {[have dnf]} {
    set PM dnf; set ODA_EXT rpm
} elseif {[have zypper]} {
    set PM zypper; set ODA_EXT rpm
} elseif {[have pacman]} {
    set PM pacman; set ODA_EXT AppImage
} else {
    set PM none; set ODA_EXT AppImage
}
proc shq {s} { return "'[string map {' '\\''} $s]'" }
proc pm_install {pkgs} {
    switch $::PM {
        apt    { return "DEBIAN_FRONTEND=noninteractive apt-get install -y $pkgs" }
        dnf    { return "dnf install -y $pkgs" }
        zypper { return "zypper --non-interactive install --allow-unsigned-rpm $pkgs" }
        pacman { return "pacman -S --needed --noconfirm $pkgs" }
    }
    return ""
}
proc pictures_pkgs {} {
    switch $::PM {
        apt { return "imagemagick poppler-utils" }
        dnf { return "ImageMagick poppler-utils" }
        zypper { return "ImageMagick poppler-tools" }
        pacman { return "imagemagick poppler" }
    }
    return ""
}
proc fuse_pkg {} {
    switch $::PM {
        apt { return [expr {[catch {exec apt-cache show libfuse2t64}] ? "libfuse2" : "libfuse2t64"}] }
        dnf { return fuse-libs }
        zypper { return libfuse2 }
        pacman { return fuse2 }
    }
    return ""
}
proc oda_installed {} {
    if {[have ODAFileConverter]} { return 1 }
    foreach f [list /usr/bin/ODAFileConverter /usr/local/bin/ODAFileConverter $::HOME/.local/bin/ODAFileConverter] {
        if {[file exists $f]} { return 1 }
    }
    if {[llength [glob -nocomplain /opt/ODAFileConverter*/ODAFileConverter $::HOME/Applications/ODAFileConverter*.AppImage]]} { return 1 }
    return 0
}
proc have_pictures {} { expr {([have magick] || [have convert]) && [have pdftoppm]} }
proc desktop_dir {} {
    if {![catch {exec xdg-user-dir DESKTOP} d] && [file isdirectory $d]} { return $d }
    return $::HOME/Desktop
}
proc downloads_dir {} {
    if {![catch {exec xdg-user-dir DOWNLOAD} d] && [file isdirectory $d]} { return $d }
    return $::HOME/Downloads
}
proc open_url {u} { catch {exec xdg-open $u >/dev/null 2>/dev/null &} }

# ---------------------------------------------------------------- look ----------------------

set C(bg) #f4f6f9; set C(head) #1f2b3c; set C(headfg) #ffffff; set C(sub) #aab6c8
set C(accent) #f2b632; set C(text) #1c2430; set C(dim) #5d6878; set C(card) #ffffff; set C(line) #d5dbe3
foreach {f size weight} {Body 11 normal Bold 11 bold Title 20 bold Head 14 bold Small 9 normal} {
    font create F$f -family sans-serif -size $size -weight $weight
}
ttk::style theme use clam
ttk::style configure . -background $C(bg) -foreground $C(text) -font FBody
ttk::style configure TFrame -background $C(bg)
ttk::style configure Card.TFrame -background $C(card)
ttk::style configure TLabel -background $C(bg)
ttk::style configure Card.TLabel -background $C(card)
ttk::style configure Dim.TLabel -foreground $C(dim) -font FSmall
ttk::style configure Head.TLabel -font FHead
ttk::style configure TCheckbutton -background $C(bg)
ttk::style configure TRadiobutton -background $C(bg)
ttk::style configure TButton -padding {14 6}
ttk::style configure Go.TButton -background $C(accent) -foreground #1c2430 -font FBold
ttk::style map Go.TButton -background [list active #ffc94d disabled #e3dccb]
ttk::style configure Horizontal.TProgressbar -background $C(accent) -troughcolor #e2e6ec

image create photo logo -file $APP/fancypencil-128.png
image create photo logo64 -file $APP/fancypencil-64.png
wm iconphoto . -default logo logo64
wm title . "Install Fancy Pencil $VERSION"
. configure -background $C(bg)
wm minsize . 680 540
wm geometry . 680x540

# the header band: logo, name, version
frame .head -background $C(head) -height 104
label .head.logo -image logo64 -background $C(head)
label .head.t -text "Fancy Pencil" -font FTitle -foreground $C(headfg) -background $C(head)
label .head.v -text "Version $VERSION  -  drafting with AutoCAD-style commands" -font FBody -foreground $C(sub) -background $C(head)
place .head.logo -x 22 -y 20
place .head.t -x 104 -y 22
place .head.v -x 106 -y 60
pack .head -side top -fill x
pack propagate .head 0

ttk::frame .page -padding {28 20 28 8}
pack .page -side top -fill both -expand 1
ttk::frame .foot -padding {20 10 20 16}
pack .foot -side bottom -fill x
ttk::button .foot.cancel -text Cancel -command {exit}
ttk::button .foot.back -text Back -command {go back}
ttk::button .foot.next -text Next -style Go.TButton -command {go next}
pack .foot.next -side right
pack .foot.back -side right -padx 8
pack .foot.cancel -side left

proc clear {} { foreach w [winfo children .page] { destroy $w } }
proc para {w text args} { ttk::label $w -text $text -wraplength 590 -justify left {*}$args; pack $w -anchor w -pady {0 10} }

# ---------------------------------------------------------------- pages ---------------------

set DEST $HOME/Fancy\ Pencil
set WANT(menu) 1; set WANT(desktop) 1; set WANT(pics) [expr {![have_pictures]}]
set ODA_CHOICE [expr {[oda_installed] ? "have" : "download"}]
set ODA_FILE ""
set PAGES {welcome options oda install done}
set PAGE 0

proc show {} {
    clear
    set p [lindex $::PAGES $::PAGE]
    .foot.back configure -state [expr {$::PAGE > 0 && $p ni {install done} ? "normal" : "disabled"}]
    .foot.next configure -state normal -text Next
    .foot.cancel configure -state normal -text Cancel
    page_$p
}
proc go {dir} {
    if {$dir eq "next"} {
        set p [lindex $::PAGES $::PAGE]
        if {$p eq "oda" && $::ODA_CHOICE eq "file" && $::ODA_FILE eq ""} {
            tk_messageBox -parent . -icon info -message "Choose the ODA File Converter file you downloaded first - or pick another option."
            return
        }
        incr ::PAGE
    } else { incr ::PAGE -1 }
    show
}

proc page_welcome {} {
    set upd [file exists $::DEST/fancypencil/fancypencil]
    ttk::label .page.h -text [expr {$upd ? "Update Fancy Pencil" : "Welcome"}] -style Head.TLabel
    pack .page.h -anchor w -pady {0 12}
    if {$upd} { para .page.u "Fancy Pencil is already installed in $::DEST. This brings it up to version $::VERSION. Your settings, toolbars and drawings are kept." }
    para .page.a "This puts Fancy Pencil on this computer:"
    foreach {n t} {1 "installs SBCL (Common Lisp) and Tk, which Fancy Pencil runs on"
                   2 "puts Fancy Pencil in a folder and adds it to your app menu"
                   3 "helps you get the free ODA File Converter, for AutoCAD .dwg drawings"} {
        ttk::label .page.b$n -text "    •  $t"; pack .page.b$n -anchor w -pady 2
    }
    para .page.p "You will be asked for your password once, to install the system packages." -style Dim.TLabel -padding {0 14 0 0}
}

proc page_options {} {
    ttk::label .page.h -text "Where and what" -style Head.TLabel; pack .page.h -anchor w -pady {0 12}
    ttk::label .page.fl -text "Install Fancy Pencil in:"; pack .page.fl -anchor w
    ttk::frame .page.f; pack .page.f -fill x -pady {4 16}
    ttk::entry .page.f.e -textvariable ::DEST
    ttk::button .page.f.b -text "Choose..." -command {
        set d [tk_chooseDirectory -parent . -initialdir $::HOME -title "Folder for Fancy Pencil"]
        if {$d ne ""} { set ::DEST [file join $d "Fancy Pencil"] }
    }
    pack .page.f.b -side right -padx {8 0}; pack .page.f.e -side left -fill x -expand 1
    ttk::checkbutton .page.m -text "Add Fancy Pencil to the app menu" -variable ::WANT(menu)
    ttk::checkbutton .page.d -text "Put a Fancy Pencil icon on the desktop" -variable ::WANT(desktop)
    ttk::checkbutton .page.p -text "Picture and PDF support (attach images and PDF pages)[expr {[have_pictures] ? {  - already installed} : {}}]" -variable ::WANT(pics)
    pack .page.m .page.d .page.p -anchor w -pady 3
    if {[have_pictures]} { .page.p configure -state disabled }
}

proc page_oda {} {
    ttk::label .page.h -text "ODA File Converter - for .dwg drawings" -style Head.TLabel; pack .page.h -anchor w -pady {0 10}
    if {$::ODA_CHOICE eq "have"} {
        para .page.a "ODA File Converter is already installed on this computer, so Fancy Pencil can open and save AutoCAD .dwg drawings. Nothing to do here."
        return
    }
    para .page.a "The free ODA File Converter, from the Open Design Alliance, lets Fancy Pencil open and save AutoCAD .dwg drawings. Everything else works without it. For this computer the .$::ODA_EXT version fits."
    ttk::radiobutton .page.r1 -text "Download it now - opens your web browser" -variable ::ODA_CHOICE -value download -command oda_refresh
    ttk::radiobutton .page.r2 -text "I have downloaded it - use that file" -variable ::ODA_CHOICE -value file -command oda_refresh
    ttk::radiobutton .page.r3 -text "Not now (run install again any time to add it)" -variable ::ODA_CHOICE -value skip -command oda_refresh
    pack .page.r1 .page.r2 .page.r3 -anchor w -pady 3
    ttk::frame .page.act -padding {24 10 0 0}; pack .page.act -fill x
    ttk::button .page.act.dl -text "Download .$::ODA_EXT" -command {
        open_url "$::ODA_GET.$::ODA_EXT"
        .page.act.note configure -text "When the download has finished (accept ODA's terms if your browser asks), click \"Choose the file...\". If no download starts, use \"Open ODA's page\"."
        set ::ODA_CHOICE file; oda_refresh
    }
    ttk::button .page.act.pg -text "Open ODA's page" -command { open_url $::ODA_PAGE }
    ttk::button .page.act.ch -text "Choose the file..." -command oda_choose
    grid .page.act.dl .page.act.pg .page.act.ch -sticky w -padx {0 8}
    ttk::label .page.act.note -text "" -wraplength 560 -style Dim.TLabel
    grid .page.act.note -columnspan 3 -sticky w -pady {10 0}
    ttk::label .page.act.file -text "" -wraplength 560 -font FBold
    grid .page.act.file -columnspan 3 -sticky w -pady {6 0}
    oda_refresh
}
proc oda_refresh {} {
    if {![winfo exists .page.act]} return
    set st [expr {$::ODA_CHOICE eq "skip" ? "disabled" : "normal"}]
    foreach b {dl pg ch} { .page.act.$b configure -state $st }
    .page.act.file configure -text [expr {$::ODA_FILE ne "" && $::ODA_CHOICE ne "skip" ? "Will install: [file tail $::ODA_FILE]" : ""}]
}
proc oda_choose {} {
    set types {{"ODA File Converter" {.deb .rpm .AppImage .appimage}} {"All files" *}}
    set f [tk_getOpenFile -parent . -initialdir [downloads_dir] -filetypes $types -title "The ODA File Converter you downloaded"]
    if {$f eq ""} return
    set ok [switch -glob -- [string tolower $f] {
        *.deb { expr {$::PM eq "apt"} }
        *.rpm { expr {$::PM in {dnf zypper}} }
        *.appimage { expr 1 }
        default { expr 0 }
    }]
    if {!$ok} {
        tk_messageBox -parent . -icon warning -message "That file doesn't suit this computer.\nPlease choose the .$::ODA_EXT (or the AppImage, which runs anywhere)."
        return
    }
    set ::ODA_FILE $f; set ::ODA_CHOICE file; oda_refresh
}

# ---------------------------------------------------------------- installing ----------------

proc page_install {} {
    .foot.next configure -state disabled -text Installing...
    .foot.cancel configure -state disabled
    ttk::label .page.h -text "Installing" -style Head.TLabel; pack .page.h -anchor w -pady {0 10}
    ttk::label .page.step -text "Getting ready..." -font FBold; pack .page.step -anchor w
    ttk::progressbar .page.bar -mode indeterminate; pack .page.bar -fill x -pady {8 12}
    .page.bar start 12
    ttk::frame .page.lf; pack .page.lf -fill both -expand 1
    text .page.lf.t -height 9 -font FSmall -background #ffffff -foreground $::C(dim) -relief solid -borderwidth 1 \
        -highlightthickness 0 -wrap word -yscrollcommand {.page.lf.s set}
    ttk::scrollbar .page.lf.s -command {.page.lf.t yview}
    pack .page.lf.s -side right -fill y; pack .page.lf.t -side left -fill both -expand 1
    after 100 run_install
}
proc logline {s} {
    set f [open $::LOG a]; puts $f $s; close $f
    if {[winfo exists .page.lf.t]} { .page.lf.t insert end "$s\n"; .page.lf.t see end }
}

proc install_script {tarball} {
    set s "#!/bin/sh\n"
    set pkgs {}
    if {![have sbcl]} { lappend pkgs sbcl }
    if {![have wish]} { lappend pkgs tk }
    if {$::WANT(pics) && ![have_pictures]} { lappend pkgs {*}[pictures_pkgs] }
    set oda [expr {$::ODA_CHOICE eq "file" ? $::ODA_FILE : ""}]
    if {[string match -nocase *.appimage $oda]} { lappend pkgs [fuse_pkg] }
    set root {}
    set ::NEEDROOT 0
    if {$::PM ne "none"} {
        if {[llength $pkgs]} { lappend root [pm_install [join $pkgs]] }
        if {[string match *.deb $oda] || [string match *.rpm $oda]} { lappend root [pm_install [shq $oda]] }
        if {[llength $root] && $::PM eq "apt"} { set root [linsert $root 0 "apt-get update -q || true"] }
    }
    if {[llength $root]} {
        set ::NEEDROOT 1
        set r [join $root " && "]
        append s "echo '@@Installing system packages - enter your password in the window that opens'\n"
        append s "echo \"as root: [string map {\" \\\"} $r]\"\n"
        if {[have pkexec]} {
            append s "pkexec sh -c [shq $r] || echo '@@WARN The system packages did not install (cancelled, or the password window was closed).'\n"
        } else {
            append s "sudo -S -p '' sh -c [shq $r] || echo '@@WARN The system packages did not install.'\n"
        }
    } elseif {$::PM eq "none" && [llength $pkgs]} {
        append s "echo '@@WARN Please install these with your package manager: [join $pkgs]'\n"
    }
    append s "command -v sbcl >/dev/null 2>&1 && command -v wish >/dev/null 2>&1 || { echo '@@FAIL SBCL and Tk are needed and are not installed.'; exit 1; }\n"
    if {[string match -nocase *.appimage $oda]} {
        append s "mkdir -p \"\$HOME/.local/bin\" && cp [shq $oda] \"\$HOME/.local/bin/ODAFileConverter\" && chmod +x \"\$HOME/.local/bin/ODAFileConverter\"\n"
    }
    set d [shq $::DEST]
    append s "echo '@@Putting Fancy Pencil in place'\n"
    append s "mkdir -p $d && tar xzf [shq $tarball] -C $d && chmod +x $d/fancypencil/fancypencil || { echo '@@FAIL Fancy Pencil could not be unpacked into the folder.'; exit 1; }\n"
    set apps "\${XDG_DATA_HOME:-\$HOME/.local/share}/applications"
    if {$::WANT(menu) || $::WANT(desktop)} {
        append s "echo '@@Adding Fancy Pencil to the app menu'\n(cd $d/fancypencil && sh install.sh)\n"
    }
    if {$::WANT(desktop)} {
        set dd [shq [desktop_dir]]
        append s "mkdir -p $dd && cp \"$apps/fancypencil.desktop\" $dd/ && chmod +x $dd/fancypencil.desktop && (gio set $dd/fancypencil.desktop metadata::trusted true 2>/dev/null || true)\n"
    }
    if {!$::WANT(menu)} { append s "rm -f \"$apps/fancypencil.desktop\"\n" }
    append s "echo '@@Preparing Fancy Pencil for its first start (a minute or so)'\n"
    append s "(cd $d/fancypencil && ./fancypencil --tty </dev/null >/dev/null 2>&1) || true\n"
    append s "echo '@@DONE'\n"
    return $s
}

proc run_install {} {
    logline "=== [clock format [clock seconds]] Fancy Pencil installer $::VERSION ($::PM)"
    set tmp [file join [expr {[info exists ::env(TMPDIR)] ? $::env(TMPDIR) : "/tmp"}] fancypencil-install-[pid]]
    file mkdir $tmp
    set tarball $tmp/fancypencil.tar.gz
    file copy -force $::APP/fancypencil.tar.gz $tarball
    set sf [open $tmp/install.sh w]; puts $sf [install_script $tarball]; close $sf
    set ::WARN {}; set ::FAIL {}; set ::DONE 0
    if {$::NEEDROOT && ![have pkexec]} {
        # no graphical password window on this computer: ask here and hand it to sudo
        set pw [password_dialog]
        if {$pw eq ""} { set ::FAIL "No password given, so nothing was installed."; install_failed; return }
        set ::PIPE [open "|[list sh $tmp/install.sh] 2>@1" r+]
        puts $::PIPE $pw; flush $::PIPE
        catch {chan close $::PIPE write}
    } else {
        set ::PIPE [open "|[list sh $tmp/install.sh] </dev/null 2>@1" r]
    }
    fconfigure $::PIPE -blocking 0 -buffering line
    fileevent $::PIPE readable [list install_read $tmp]
}
proc password_dialog {} {
    set w .pw; toplevel $w -background $::C(bg) -padx 20 -pady 16
    wm title $w "Password"; wm transient $w .
    ttk::label $w.l -text "Your password, to install the system packages:"
    ttk::entry $w.e -show \u2022 -textvariable ::PW -width 30
    ttk::button $w.ok -text OK -style Go.TButton -command {set ::PWDONE 1}
    ttk::button $w.c -text Cancel -command {set ::PW ""; set ::PWDONE 1}
    pack $w.l -anchor w; pack $w.e -fill x -pady 10; pack $w.ok $w.c -side right -padx 4
    bind $w.e <Return> {set ::PWDONE 1}
    set ::PW ""; focus $w.e; grab $w
    vwait ::PWDONE
    destroy $w
    return $::PW
}
proc install_read {tmp} {
    if {[gets $::PIPE line] >= 0} {
        if {[string match @@DONE* $line]} { set ::DONE 1
        } elseif {[string match @@FAIL* $line]} { set ::FAIL [string range $line 7 end]; logline $::FAIL
        } elseif {[string match @@WARN* $line]} { lappend ::WARN [string range $line 7 end]; logline [lindex $::WARN end]
        } elseif {[string match @@* $line]} { .page.step configure -text "[string range $line 2 end]..."; logline "-- [string range $line 2 end]"
        } else { logline $line }
        return
    }
    if {[eof $::PIPE]} {
        catch {close $::PIPE}
        file delete -force $tmp
        .page.bar stop
        if {$::DONE} { incr ::PAGE; show } else { install_failed }
    }
}
proc install_failed {} {
    .page.step configure -text [expr {$::FAIL ne "" ? $::FAIL : "Something went wrong."}] -foreground #b3261e
    .page.bar configure -mode determinate -value 0
    .foot.cancel configure -state normal -text Close
    .foot.next configure -state normal -text "Try again" -command {.foot.next configure -command {go next}; show}
    logline "(the whole record is in $::LOG)"
}

proc page_done {} {
    pack forget .foot.back
    .foot.cancel configure -text Close
    .foot.next configure -text "Start Fancy Pencil" -command start_fp
    ttk::frame .page.top; pack .page.top -fill x -pady {0 12}
    ttk::label .page.top.i -image logo; pack .page.top.i -side left -padx {0 18}
    ttk::frame .page.top.r; pack .page.top.r -side left -fill x
    ttk::label .page.top.r.h -text "Fancy Pencil $::VERSION is installed" -style Head.TLabel; pack .page.top.r.h -anchor w
    ttk::label .page.top.r.f -text "in $::DEST/fancypencil" -style Dim.TLabel -wraplength 420; pack .page.top.r.f -anchor w -pady {4 0}
    set lines {}
    if {$::WANT(menu)} { lappend lines "It is in your app menu." }
    if {$::WANT(desktop)} { lappend lines "There is a Fancy Pencil icon on your desktop." }
    if {[oda_installed]} { lappend lines "ODA File Converter is installed - .dwg drawings open and save."
    } else { lappend lines "No ODA File Converter yet - run install again any time to add it." }
    foreach w $::WARN { lappend lines $w }
    lappend lines "To update later, run a newer install file. Your settings are kept."
    set i 0
    foreach l $lines { ttk::label .page.l[incr i] -text "•  $l" -wraplength 590; pack .page.l$i -anchor w -pady 2 }
}
proc start_fp {} {
    catch {exec sh -c "cd \"\$HOME\" && nohup sh [shq $::DEST/fancypencil/fancypencil] >/dev/null 2>&1 &"}
    after 400 exit
}

# ---------------------------------------------------------------- the file's own icon -------

# Linux files carry no icon of their own; ask the file manager to show this one with the
# Fancy Pencil logo from now on (on this computer).
catch {
    set ic $HOME/.local/share/icons/fancypencil-installer.png
    file mkdir [file dirname $ic]
    file copy -force $APP/fancypencil-128.png $ic
    exec gio set [info nameofexecutable] metadata::custom-icon file://$ic
}

show
