# gui.tcl - LispDraft's Tk front end. Started by the Lisp process as
#   wish gui.tcl
# Lisp sends Tcl commands on stdin; this script reports events on stdout,
# one per line (see `emit`).

package require Tk 8.5
catch {package require tile}

set ::buf ""
set ::allowspace 0
set ::grread 0
set ::cmdhist {}
set ::cmdhistpos -1

fconfigure stdin -blocking 0 -encoding utf-8
fconfigure stdout -encoding utf-8 -buffering full
proc emit {args} { puts [join $args " "]; flush stdout }

proc readcmd {} {
    # drain every complete line that is available, evaluating complete Tcl commands
    while {[gets stdin line] >= 0} {
        append ::buf $line "\n"
        if {[info complete $::buf]} {
            set c $::buf
            set ::buf ""
            if {[catch {uplevel #0 $c} err]} {
                puts stderr "LispDraft Tcl error: $err"
                puts stderr "  in: [string range $c 0 300]"
            }
        }
    }
    if {[eof stdin]} { exit }
}
fileevent stdin readable readcmd

# ---------------------------------------------------------------- look
set bg     "#212830"
set panel  "#2b313b"
set panel2 "#353c47"
set fg     "#d8dde4"
set dim    "#8a94a3"
set accent "#3b8ee8"

wm title . "LispDraft"
wm geometry . 1280x820
wm minsize . 800 500
. configure -background $panel
catch {
    ttk::style theme use clam
    ttk::style configure . -background $panel -foreground $fg -fieldbackground $panel2 \
        -bordercolor "#1b2027" -lightcolor $panel2 -darkcolor "#1b2027" -troughcolor $bg
    ttk::style configure TButton -padding {6 3} -background $panel2
    ttk::style map TButton -background [list active "#46505e" pressed $accent]
    ttk::style configure Tool.TButton -padding {5 1} -background $panel -relief flat -font {Helvetica 9}
    ttk::style map Tool.TButton -background [list active "#46505e" pressed $accent]
    ttk::style configure Toggle.TButton -padding {6 1} -background $panel -foreground $dim -relief flat
    ttk::style configure On.Toggle.TButton -background "#24496e" -foreground "#ffffff"
    ttk::style configure TCombobox -arrowcolor $fg -foreground $fg
    ttk::style map TCombobox -fieldbackground [list readonly $panel2] -foreground [list readonly $fg]
    ttk::style configure Treeview -background $panel2 -fieldbackground $panel2 -foreground $fg -rowheight 22
    ttk::style configure Treeview.Heading -background $panel -foreground $fg
    ttk::style map Treeview -background [list selected $accent]
    ttk::style configure TLabel -background $panel -foreground $fg
    ttk::style configure Dim.TLabel -foreground $dim
    ttk::style configure TFrame -background $panel
    ttk::style configure Group.TLabel -foreground $dim -font {Helvetica 8}
}
option add *Menu.background $panel2
option add *Menu.foreground $fg
option add *Menu.activeBackground $accent
option add *Menu.activeForeground white
option add *Menu.relief flat
option add *tearOff 0

# ---------------------------------------------------------------- menus
menu .mb
. configure -menu .mb
proc mcmd {m label cmd {accel ""}} {
    $m add command -label $label -accelerator $accel -command [list menucmd $cmd]
}
proc menucmd {cmd} { emit MENU $cmd; focus .cmd.e }

foreach {name label} {file File edit Edit view View draw Draw modify Modify format Format tools Tools help Help} {
    menu .mb.$name
    .mb add cascade -label $label -menu .mb.$name
}
mcmd .mb.file "New" NEW Ctrl+N
mcmd .mb.file "Open..." OPEN Ctrl+O
mcmd .mb.file "Save" QSAVE Ctrl+S
mcmd .mb.file "Save As..." SAVEAS
.mb.file add separator
mcmd .mb.file "Import DXF..." DXFIN
mcmd .mb.file "Export DXF..." DXFOUT
.mb.file add separator
mcmd .mb.file "Exit" QUIT

mcmd .mb.edit "Undo" U Ctrl+Z
mcmd .mb.edit "Redo" REDO Ctrl+Y
.mb.edit add separator
mcmd .mb.edit "Erase" ERASE Del
mcmd .mb.edit "Select All" SELECTALL Ctrl+A

mcmd .mb.view "Zoom Extents" "ZOOM E"
mcmd .mb.view "Zoom Window" "ZOOM W"
mcmd .mb.view "Zoom Previous" "ZOOM P"
mcmd .mb.view "Pan" PAN
.mb.view add separator
mcmd .mb.view "Regen" REGEN

mcmd .mb.draw "Line" LINE
mcmd .mb.draw "Polyline" PLINE
mcmd .mb.draw "Rectangle" RECTANG
mcmd .mb.draw "Polygon" POLYGON
.mb.draw add separator
mcmd .mb.draw "Circle: Center, Radius" CIRCLE
mcmd .mb.draw "Circle: 2 Points" "CIRCLE 2P"
mcmd .mb.draw "Circle: 3 Points" "CIRCLE 3P"
mcmd .mb.draw "Arc: 3 Points" ARC
mcmd .mb.draw "Arc: Center, Start, End" "ARC C"
mcmd .mb.draw "Ellipse (polyline)" ELLIPSE
.mb.draw add separator
mcmd .mb.draw "Point" POINT
mcmd .mb.draw "Single Line Text" TEXT
mcmd .mb.draw "Block: Make" BLOCK
mcmd .mb.draw "Block: Insert" INSERT

foreach {l c} {Move MOVE Copy COPY Rotate ROTATE Scale SCALE Mirror MIRROR Offset OFFSET Stretch STRETCH \
               Trim TRIM Extend EXTEND Fillet FILLET Chamfer CHAMFER Break BREAK Explode EXPLODE Join JOIN \
               "Array (rectangular)" ARRAYRECT "Array (polar)" ARRAYPOLAR Erase ERASE} {
    mcmd .mb.modify $l $c
}
mcmd .mb.format "Layer..." LAYER
mcmd .mb.format "Text Size" TEXTSIZE
mcmd .mb.format "Point Style" PDMODE
mcmd .mb.tools "Load AutoLISP..." APPLOAD
mcmd .mb.tools "Object Snap Settings" OSNAP
mcmd .mb.tools "Distance" DIST
mcmd .mb.tools "ID Point" ID
mcmd .mb.tools "List" LIST
mcmd .mb.tools "Area" AREA
mcmd .mb.help "Command Reference" HELP
mcmd .mb.help "About LispDraft" ABOUT

# ---------------------------------------------------------------- toolbar
ttk::frame .tb -padding {4 3}
pack .tb -side top -fill x
proc tbgroup {name title items {rows 2}} {
    set f [ttk::frame .tb.$name -padding {2 0 6 0}]
    set grid [ttk::frame $f.g]
    set n [expr {[llength $items] / 2}]
    set cols [expr {($n + $rows - 1) / $rows}]
    set i 0
    foreach {lbl cmd} $items {
        set w $grid.b$i
        ttk::button $w -text $lbl -style Tool.TButton -width 0 -takefocus 0 -command [list menucmd $cmd]
        grid $w -row [expr {$i / $cols}] -column [expr {$i % $cols}] -sticky ew -padx 1 -pady 1
        incr i
    }
    ttk::label $f.t -text $title -style Group.TLabel
    pack $grid -side top
    pack $f.t -side top
    pack $f -side left -anchor n
    ttk::separator .tb.s$name -orient vertical
    pack .tb.s$name -side left -fill y -padx 3
}
tbgroup draw Draw {Line LINE Polyline PLINE Circle CIRCLE Arc ARC Rectangle RECTANG Text TEXT}
tbgroup modify Modify {Move MOVE Copy COPY Rotate ROTATE Mirror MIRROR Scale SCALE Offset OFFSET Trim TRIM Extend EXTEND Fillet FILLET Erase ERASE Explode EXPLODE Array ARRAY}
tbgroup annot Annotation {Linear DIMLINEAR Aligned DIMALIGNED "Edit Text" TEXTEDIT Block BLOCK Insert INSERT List LIST}
set lf [ttk::frame .tb.layers -padding {2 0 6 0}]
set lr [ttk::frame $lf.r]
canvas $lr.sw -width 14 -height 14 -highlightthickness 0 -background $panel
$lr.sw create rectangle 1 1 13 13 -fill white -outline "#000000" -tags sw
ttk::combobox $lr.cb -width 16 -state readonly -takefocus 0
bind $lr.cb <<ComboboxSelected>> { emit LAYERSET [.tb.layers.r.cb get]; focus .cmd.e }
pack $lr.sw $lr.cb -side left -padx 2
ttk::button $lf.lp -text "Layer Properties" -style Tool.TButton -width 0 -takefocus 0 -command {menucmd LAYER}
ttk::label $lf.t -text Layers -style Group.TLabel
pack $lr -side top -pady 1
pack $lf.lp -side top -fill x
pack $lf.t -side top
pack $lf -side left -anchor n
ttk::separator .tb.slayers -orient vertical
pack .tb.slayers -side left -fill y -padx 3
tbgroup view View {Extents "ZOOM E" Window "ZOOM W" Undo U Redo REDO}

# ---------------------------------------------------------------- canvas
canvas .c -background $bg -highlightthickness 0 -cursor none -borderwidth 0
pack .c -side top -fill both -expand 1

bind .c <Motion> {emit M %x %y}
bind .c <ButtonPress-1> {emit B %x %y; focus .cmd.e}
bind .c <ButtonPress-3> {emit R %x %y}
bind .c <ButtonPress-2> {emit P %x %y}
bind .c <B2-Motion> {emit D %x %y}
bind .c <ButtonRelease-2> {emit Q %x %y}
bind .c <Double-Button-2> {emit MENU "'ZOOMEXT"}
set ::idleid ""
proc idle-later {} { after cancel $::idleid; set ::idleid [after 200 {emit IDLE}] }
proc wheel {x y d} { emit W $x $y $d; idle-later }
bind .c <Button-4> {wheel %x %y 1}
bind .c <Button-5> {wheel %x %y -1}
bind .c <MouseWheel> {wheel %x %y [expr {%D > 0 ? 1 : -1}]}
bind .c <Configure> {emit C %w %h}
bind .c <Leave> {emit X}

# ---------------------------------------------------------------- command line
ttk::frame .bottom
pack .bottom -side bottom -fill x
text .h -height 5 -background "#1a1f26" -foreground "#b9c2ce" -borderwidth 0 -highlightthickness 0 \
    -font {{DejaVu Sans Mono} 9} -wrap word -padx 6 -pady 3 -state disabled -takefocus 0
ttk::frame .cmd -padding {0 0}
label .cmd.p -text "Command:" -background "#14181e" -foreground "#ffffff" -font {{DejaVu Sans Mono} 10} -padx 6 -anchor w
entry .cmd.e -background "#14181e" -foreground "#ffffff" -insertbackground "#ffffff" -borderwidth 0 \
    -highlightthickness 0 -font {{DejaVu Sans Mono} 10} -relief flat
pack .cmd.p -side left -fill y
pack .cmd.e -side left -fill both -expand 1 -ipady 4

ttk::frame .st -padding {6 2}
label .st.xy -text "0.0000, 0.0000, 0.0000" -background $panel -foreground $fg -font {{DejaVu Sans Mono} 9} -width 34 -anchor w
pack .st.xy -side left
ttk::label .st.model -text "MODEL" -style Dim.TLabel
pack .st.model -side right -padx 6
foreach {t k} {OSNAP F3 ORTHO F8 SNAP F9 GRID F7} {
    ttk::button .st.[string tolower $t] -text $t -style Toggle.TButton -takefocus 0 -command [list togglekey $k]
    pack .st.[string tolower $t] -side right -padx 1
}
proc togglekey {k} { emit K $k; focus .cmd.e }

pack .st -in .bottom -side bottom -fill x
pack .cmd -in .bottom -side bottom -fill x
pack .h -in .bottom -side bottom -fill x
raise .h; raise .cmd; raise .st

bind .cmd.e <Return> {cmdsubmit; break}
bind .cmd.e <KP_Enter> {cmdsubmit; break}
bind .cmd.e <space> {if {!$::allowspace} {cmdsubmit; break}}
bind .cmd.e <Up> {histmove -1; break}
bind .cmd.e <Down> {histmove 1; break}
bind .cmd.e <Delete> {if {[.cmd.e get] eq ""} {emit MENU ERASE; break}}
bind .cmd.e <KeyPress> {if {$::grread} {if {"%A" ne ""} {emit G [scan "%A" %%c]}; break}}
proc cmdsubmit {} {
    set t [.cmd.e get]
    .cmd.e delete 0 end
    if {$t ne ""} { lappend ::cmdhist $t }
    set ::cmdhistpos [llength $::cmdhist]
    emit L $t
}
proc histmove {d} {
    set n [llength $::cmdhist]
    if {$n == 0} return
    set ::cmdhistpos [expr {max(0, min($n, $::cmdhistpos + $d))}]
    .cmd.e delete 0 end
    if {$::cmdhistpos < $n} { .cmd.e insert 0 [lindex $::cmdhist $::cmdhistpos] }
}

bind all <Escape> {.cmd.e delete 0 end; emit E; break}
bind all <F2> {emit K F2; break}
bind all <F3> {emit K F3; break}
bind all <F7> {emit K F7; break}
bind all <F8> {emit K F8; break}
bind all <F9> {emit K F9; break}
bind all <Control-z> {emit MENU U; break}
bind all <Control-y> {emit MENU REDO; break}
bind all <Control-s> {emit MENU QSAVE; break}
bind all <Control-o> {emit MENU OPEN; break}
bind all <Control-n> {emit MENU NEW; break}
bind all <Control-a> {emit MENU SELECTALL; break}
bind all <Control-c> {emit MENU COPYCLIP; break}
bind all <Control-v> {emit MENU PASTECLIP; break}
wm protocol . WM_DELETE_WINDOW {emit MENU QUIT}
focus .cmd.e

# ---------------------------------------------------------------- called from Lisp
proc hist {s} {
    .h configure -state normal
    .h insert end $s
    if {[.h count -lines 1.0 end] > 2000} { .h delete 1.0 500.0 }
    .h see end
    .h configure -state disabled
}
proc setprompt {s spaces} {
    .cmd.p configure -text $s
    set ::allowspace $spaces
    focus .cmd.e
}
proc setxy {s} { .st.xy configure -text $s }
proc settoggles {grid snap ortho osnap} {
    foreach {w v} [list grid $grid snap $snap ortho $ortho osnap $osnap] {
        .st.$w configure -style [expr {$v ? "On.Toggle.TButton" : "Toggle.TButton"}]
    }
}
proc setlayers {names current color} {
    .tb.layers.r.cb configure -values $names
    .tb.layers.r.cb set $current
    .tb.layers.r.sw itemconfigure sw -fill $color
}
proc settitle {s} { wm title . $s }
proc ask {id script} {
    set r [uplevel #0 $script]
    emit A $id [string map {"\n" " "} $r]
}
proc setgrread {v} { set ::grread $v }

# ---------------------------------------------------------------- layer dialog
proc layerdlg {rows} {
    set w .ld
    if {![winfo exists $w]} {
        toplevel $w -background $::panel
        wm title $w "Layer Properties Manager"
        wm geometry $w 640x380+[expr {[winfo rootx .] + 120}]+[expr {[winfo rooty .] + 140}]
        wm transient $w .
        ttk::frame $w.tb -padding 6
        foreach {lbl act} {"New Layer" new "Delete" delete "Set Current" current} {
            ttk::button $w.tb.[string map {" " ""} [string tolower $lbl]] -text $lbl -command [list layeract $act]
            pack $w.tb.[string map {" " ""} [string tolower $lbl]] -side left -padx 2
        }
        ttk::label $w.tb.cl -text "  Color (1-255):"
        ttk::entry $w.tb.ce -width 5
        ttk::button $w.tb.cs -text "Set Color" -command {layeract color [.ld.tb.ce get]}
        pack $w.tb.cl $w.tb.ce $w.tb.cs -side left -padx 2
        pack $w.tb -side top -fill x
        ttk::treeview $w.t -columns {status name on freeze lock color} -show headings -selectmode browse
        foreach {c t wd} {status "" 30 name Name 220 on On 60 freeze Freeze 70 lock Lock 60 color Color 70} {
            $w.t heading $c -text $t
            $w.t column $c -width $wd -anchor [expr {$c eq "name" ? "w" : "center"}]
        }
        pack $w.t -side top -fill both -expand 1 -padx 6
        ttk::label $w.hint -style Dim.TLabel -padding 6 \
            -text "Click On / Freeze / Lock cells to toggle. Double-click a layer to make it current."
        pack $w.hint -side top -fill x
        ttk::button $w.close -text Close -command {destroy .ld; focus .cmd.e}
        pack $w.close -side bottom -anchor e -padx 6 -pady 6
        bind $w.t <ButtonRelease-1> {layerclick %x %y}
        bind $w.t <Double-Button-1> {layeract current}
        bind $w <Escape> {destroy .ld; focus .cmd.e; break}
    }
    set sel [$w.t selection]
    set selname ""
    if {$sel ne ""} { set selname [lindex [$w.t item $sel -values] 1] }
    $w.t delete [$w.t children {}]
    foreach r $rows {
        lassign $r name on frz lck color cur
        if {$cur} { set mark "\u2713" } else { set mark "" }
        set id [$w.t insert {} end -values [list $mark $name \
                    [expr {$on ? "On" : "off"}] [expr {$frz ? "Frozen" : "-"}] [expr {$lck ? "Locked" : "-"}] $color]]
        if {$name eq $selname} { $w.t selection set $id }
    }
    raise $w
}
proc layersel {} {
    set s [.ld.t selection]
    if {$s eq ""} { return "" }
    return [lindex [.ld.t item $s -values] 1]
}
proc layerclick {x y} {
    set col [.ld.t identify column $x $y]
    set row [.ld.t identify row $x $y]
    if {$row eq ""} return
    .ld.t selection set $row
    set name [lindex [.ld.t item $row -values] 1]
    switch -- $col {
        "#3" { emit LAYERACT on $name }
        "#4" { emit LAYERACT freeze $name }
        "#5" { emit LAYERACT lock $name }
    }
}
proc layeract {act {arg ""}} {
    if {$act eq "color"} {
        emit LAYERACT color [expr {$arg eq "" ? 7 : $arg}] [layersel]
    } else {
        emit LAYERACT $act [layersel]
    }
}
emit READY
# test hook: LISPDRAFT_TEST_TCL=script.tcl is sourced after startup (used by the automated GUI tests)
if {[info exists env(LISPDRAFT_TEST_TCL)]} { after 1500 [list source $env(LISPDRAFT_TEST_TCL)] }
