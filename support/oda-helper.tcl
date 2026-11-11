# Get ODA File Converter - a small window that fetches and installs the free ODA File Converter,
# which Fancy Pencil uses to open and save AutoCAD .dwg drawings.
#   wish oda-helper.tcl          (Fancy Pencil: Help > Get ODA File Converter...)

package require Tk
set HERE [file dirname [file normalize [info script]]]
set ODA_PAGE "https://www.opendesign.com/guestfiles/oda_file_converter"
set ODA_GET  "https://www.opendesign.com/guestfiles/get?filename=ODAFileConverter_QT6_lnxX64_11dll"
set HOME $::env(HOME)

proc have {cmd} { expr {[auto_execok $cmd] ne ""} }
if {[have apt-get]} { set PM apt; set EXT deb
} elseif {[have dnf]} { set PM dnf; set EXT rpm
} elseif {[have zypper]} { set PM zypper; set EXT rpm
} else { set PM other; set EXT AppImage }

proc oda_installed {} {
    if {[have ODAFileConverter]} { return 1 }
    foreach f [list /usr/bin/ODAFileConverter /usr/local/bin/ODAFileConverter $::HOME/.local/bin/ODAFileConverter] {
        if {[file exists $f]} { return 1 }
    }
    expr {[llength [glob -nocomplain /opt/ODAFileConverter*/ODAFileConverter $::HOME/Applications/ODAFileConverter*.AppImage]] > 0}
}
proc open_url {u} { catch {exec xdg-open $u >/dev/null 2>/dev/null &} }
proc downloads_dir {} {
    if {![catch {exec xdg-user-dir DOWNLOAD} d] && [file isdirectory $d]} { return $d }
    return $::HOME/Downloads
}
proc shq {s} { return "'[string map {' '\\''} $s]'" }

# ---- look
foreach {f size weight} {Body 11 normal Bold 11 bold Head 15 bold Small 9 normal} {
    font create F$f -family sans-serif -size $size -weight $weight
}
set BG #f4f6f9
ttk::style theme use clam
ttk::style configure . -background $BG -font FBody
ttk::style configure TLabel -background $BG
ttk::style configure Dim.TLabel -foreground #5d6878 -font FSmall
ttk::style configure TButton -padding {12 5}
ttk::style configure Go.TButton -background #f2b632 -font FBold
ttk::style map Go.TButton -background {active #ffc94d disabled #e3dccb}
wm title . "Get ODA File Converter"
. configure -background $BG
catch {
    image create photo logo -file [file join $HERE icons fancypencil-64.png]
    wm iconphoto . -default logo
}
frame .head -background #1f2b3c
catch { label .head.i -image logo -background #1f2b3c; pack .head.i -side left -padx 18 -pady 14 }
label .head.t -text "ODA File Converter\nopens and saves .dwg drawings in Fancy Pencil" -justify left \
    -font FHead -foreground white -background #1f2b3c
pack .head.t -side left -pady 14
pack .head -fill x
ttk::frame .b -padding {24 16 24 18}; pack .b -fill both -expand 1

proc body {} {
    foreach w [winfo children .b] { destroy $w }
    if {[oda_installed]} {
        ttk::label .b.ok -text "ODA File Converter is installed.\nFancy Pencil can open and save .dwg drawings." -font FBold
        pack .b.ok -anchor w -pady {0 16}
        ttk::button .b.close -text Close -style Go.TButton -command exit; pack .b.close -anchor e
        return
    }
    ttk::label .b.t -wraplength 520 -justify left -text "The ODA File Converter is free, from the Open Design Alliance. For this computer the .$::EXT version fits.\n\n1.  Click Download (accept ODA's terms if your browser asks).\n2.  When it has downloaded, click Install the downloaded file and pick it."
    pack .b.t -anchor w -pady {0 14}
    ttk::frame .b.r; pack .b.r -anchor w
    ttk::button .b.r.dl -text "Download .$::EXT" -style Go.TButton -command { open_url "$::ODA_GET.$::EXT" }
    ttk::button .b.r.in -text "Install the downloaded file..." -command install_file
    ttk::button .b.r.pg -text "Open ODA's page" -command { open_url $::ODA_PAGE }
    pack .b.r.dl .b.r.in .b.r.pg -side left -padx {0 8}
    ttk::label .b.s -text "" -wraplength 520 -style Dim.TLabel; pack .b.s -anchor w -pady {14 0}
}

proc install_file {} {
    set f [tk_getOpenFile -parent . -initialdir [downloads_dir] -title "The ODA File Converter you downloaded" \
               -filetypes {{"ODA File Converter" {.deb .rpm .AppImage .appimage}} {"All files" *}}]
    if {$f eq ""} return
    .b.s configure -text "Installing [file tail $f] - enter your password if asked..."
    update
    switch -glob -- [string tolower $f] {
        *.appimage {
            file mkdir $::HOME/.local/bin
            file copy -force $f $::HOME/.local/bin/ODAFileConverter
            file attributes $::HOME/.local/bin/ODAFileConverter -permissions 0755
            set rc 0; set out ""
        }
        *.deb { set rc [catch {exec pkexec sh -c "DEBIAN_FRONTEND=noninteractive apt-get install -y [shq $f]" 2>@1} out] }
        *.rpm {
            if {$::PM eq "zypper"} { set c "zypper --non-interactive install --allow-unsigned-rpm [shq $f]" } else { set c "dnf install -y [shq $f]" }
            set rc [catch {exec pkexec sh -c $c 2>@1} out]
        }
        default { set rc 1; set out "That isn't an ODA File Converter package (.deb, .rpm or .AppImage)." }
    }
    if {[oda_installed]} {
        body
    } else {
        .b.s configure -text "It didn't install.\n[string range $out end-600 end]"
    }
}
body
