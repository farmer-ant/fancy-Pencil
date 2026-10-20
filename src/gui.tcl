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

foreach {name label} {file File edit Edit view View draw Draw dim Dimension modify Modify format Format tools Tools express Express help Help} {
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
mcmd .mb.draw "Hatch..." HATCH
mcmd .mb.draw "Revision Cloud" REVCLOUD
mcmd .mb.draw "Point" POINT
mcmd .mb.draw "Single Line Text" TEXT
mcmd .mb.draw "Block: Make" BLOCK
mcmd .mb.draw "Block: Insert" INSERT

foreach {l c} {Move MOVE Copy COPY Rotate ROTATE Scale SCALE Mirror MIRROR Offset OFFSET Stretch STRETCH "Edit Polyline" PEDIT "Edit Hatch" HATCHEDIT \
               Trim TRIM Extend EXTEND Fillet FILLET Chamfer CHAMFER Break BREAK Explode EXPLODE Join JOIN \
               "Array (rectangular)" ARRAYRECT "Array (polar)" ARRAYPOLAR Erase ERASE} {
    mcmd .mb.modify $l $c
}
mcmd .mb.format "Layer..." LAYER
mcmd .mb.format "Linetype..." LINETYPE
mcmd .mb.format "Lineweight..." LWEIGHT
mcmd .mb.format "Dimension Style..." DIMSTYLE
mcmd .mb.format "Hatch Settings..." HPSET
mcmd .mb.format "Text Size" TEXTSIZE
mcmd .mb.format "Point Style" PDMODE
mcmd .mb.tools "Properties  (Ctrl+1)" PROPERTIES
mcmd .mb.tools "Load AutoLISP..." APPLOAD
mcmd .mb.tools "Edit Command Aliases..." ALIASEDIT
mcmd .mb.tools "Reload Aliases" REINIT
mcmd .mb.tools "Object Snap Settings" OSNAP
mcmd .mb.tools "Distance" DIST
mcmd .mb.tools "ID Point" ID
mcmd .mb.tools "List" LIST
mcmd .mb.tools "Area" AREA
foreach {l c} {Linear DIMLINEAR Aligned DIMALIGNED Radius DIMRADIUS Diameter DIMDIAMETER Angular DIMANGULAR \
               Continue DIMCONTINUE Baseline DIMBASELINE "Dimension Style..." DIMSTYLE} { mcmd .mb.dim $l $c }
foreach {l c} {"Layer Isolate" LAYISO "Layer Unisolate" LAYUNISO "Layer Off (pick)" LAYOFF "Layer Freeze (pick)" LAYFRZ
               "Layers On" LAYON "Layers Thaw" LAYTHW "Layer Lock (pick)" LAYLCK "Layer Unlock (pick)" LAYULK
               "Change to Current Layer" LAYCUR "Match Object Layer" LAYMCH "Merge Layers" LAYMRG "Delete Layer" LAYDEL
               "Overkill (delete duplicates)" OVERKILL "Break Line Symbol" BREAKLINE "Extended Trim" EXTRIM
               "Multiple Pedit" MPEDIT "Text Case" TCASE "Text Number (TCOUNT)" TCOUNT "Text to MText" TXT2MTXT
               "Move/Copy/Rotate (MOCORO)" MOCORO "Copy Multiple (COPYM)" COPYM "Flatten" FLATTEN
               "Select Similar" SELECTSIMILAR "List nested (XLIST)" XLIST "Burst" BURST} { mcmd .mb.express $l $c }
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
set pf [ttk::frame .tb.props -padding {2 0 6 0}]
foreach {w lbl key wd} {color Color COLOR 12 ltype Linetype LTYPE 14 lweight Lineweight LWEIGHT 10} {
    set r [ttk::frame $pf.$w]
    ttk::label $r.l -text $lbl -width 9 -style Dim.TLabel
    ttk::combobox $r.cb -width $wd -state readonly -takefocus 0
    bind $r.cb <<ComboboxSelected>> [list propcombo $key $r.cb]
    pack $r.l $r.cb -side left
    pack $r -side top -anchor w
}
ttk::label $pf.t -text Properties -style Group.TLabel
pack $pf.t -side top
pack $pf -side left -anchor n
ttk::separator .tb.sprops -orient vertical
pack .tb.sprops -side left -fill y -padx 3
proc propcombo {key cb} { emit CURPROP $key [$cb get]; focus .cmd.e }
proc setcurprops {colors color ltypes ltype lweights lweight} {
    .tb.props.color.cb configure -values $colors;   .tb.props.color.cb set $color
    .tb.props.ltype.cb configure -values $ltypes;   .tb.props.ltype.cb set $ltype
    .tb.props.lweight.cb configure -values $lweights; .tb.props.lweight.cb set $lweight
}
tbgroup view View {Extents "ZOOM E" Window "ZOOM W" Undo U Redo REDO}

# ---------------------------------------------------------------- canvas
ttk::frame .main
pack .main -side top -fill both -expand 1
canvas .c -background $bg -highlightthickness 0 -cursor none -borderwidth 0
pack .c -in .main -side left -fill both -expand 1

# Properties palette (PROPERTIES / Ctrl+1)
frame .pal -background $panel -width 290
pack propagate .pal 0
label .pal.title -text "Properties" -background $panel2 -foreground $fg -anchor w -padx 8 -pady 4 -font {Helvetica 10 bold}
label .pal.what -text "No selection" -background $panel -foreground $accent -anchor w -padx 8 -pady 3
frame .pal.rows -background $panel
pack .pal.title -side top -fill x
pack .pal.what -side top -fill x
pack .pal.rows -side top -fill both -expand 1 -padx 4
set ::palshown 0
proc palette_toggle {} {
    if {$::palshown} { pack forget .pal; set ::palshown 0 } else { pack .pal -in .main -side right -fill y -before .c; set ::palshown 1 }
    emit PALETTE $::palshown
}
proc palette_set {what rows} {
    .pal.what configure -text $what
    foreach w [winfo children .pal.rows] { destroy $w }
    set i 0
    foreach r $rows {
        lassign $r key label value kind choices
        if {$kind eq "head"} {
            label .pal.rows.h$i -text $label -background $::panel -foreground $::dim -anchor w -font {Helvetica 8 bold}
            grid .pal.rows.h$i -row $i -column 0 -columnspan 2 -sticky ew -pady {6 1}
            incr i; continue
        }
        label .pal.rows.l$i -text $label -background $::panel -foreground $::fg -anchor w -font {Helvetica 9}
        switch -- $kind {
            ro { label .pal.rows.v$i -text $value -background $::panel -foreground $::dim -anchor w -font {Helvetica 9} }
            combo {
                ttk::combobox .pal.rows.v$i -values $choices -width 16 -state readonly -takefocus 0
                .pal.rows.v$i set $value
                bind .pal.rows.v$i <<ComboboxSelected>> [list palemit $key .pal.rows.v$i]
            }
            default {
                entry .pal.rows.v$i -background $::panel2 -foreground $::fg -insertbackground $::fg -relief flat -width 16 -font {Helvetica 9}
                .pal.rows.v$i insert 0 $value
                bind .pal.rows.v$i <Return> [list palemit $key .pal.rows.v$i]
                bind .pal.rows.v$i <KP_Enter> [list palemit $key .pal.rows.v$i]
            }
        }
        grid .pal.rows.l$i -row $i -column 0 -sticky ew -padx {4 6} -pady 1
        grid .pal.rows.v$i -row $i -column 1 -sticky ew -pady 1
        incr i
    }
    grid columnconfigure .pal.rows 1 -weight 1
}
proc palemit {key w} { emit PROP $key [$w get]; focus .cmd.e }
bind all <Control-Key-1> {palette_toggle; break}

bind .c <Motion> {emit M %x %y}
bind .c <ButtonPress-1> {emit B %x %y; focus .cmd.e}
bind .c <ButtonRelease-1> {emit U %x %y}
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
foreach {t k} {LWT LWT OSNAP F3 ORTHO F8 SNAP F9 GRID F7} {
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
proc settoggles {grid snap ortho osnap {lwt 0}} {
    foreach {w v} [list grid $grid snap $snap ortho $ortho osnap $osnap lwt $lwt] {
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
proc layerdlg {rows {ltypes {}} {lweights {}}} {
    set w .ld
    if {![winfo exists $w]} {
        toplevel $w -background $::panel
        wm title $w "Layer Properties Manager"
        wm geometry $w 720x420+[expr {[winfo rootx .] + 120}]+[expr {[winfo rooty .] + 140}]
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
        ttk::frame $w.tb2 -padding {6 0 6 6}
        ttk::label $w.tb2.ll -text "Linetype:"
        ttk::combobox $w.tb2.lt -width 14 -state readonly
        ttk::button $w.tb2.ls -text "Set Linetype" -command {layeract ltype [.ld.tb2.lt get]}
        ttk::label $w.tb2.wl -text "   Lineweight:"
        ttk::combobox $w.tb2.lw -width 10 -state readonly
        ttk::button $w.tb2.ws -text "Set Lineweight" -command {layeract lweight [.ld.tb2.lw get]}
        pack $w.tb2.ll $w.tb2.lt $w.tb2.ls $w.tb2.wl $w.tb2.lw $w.tb2.ws -side left -padx 2
        pack $w.tb2 -side top -fill x
        ttk::treeview $w.t -columns {status name on freeze lock color ltype lweight} -show headings -selectmode browse
        foreach {c t wd} {status "" 26 name Name 170 on On 50 freeze Freeze 60 lock Lock 60 color Color 50 ltype Linetype 100 lweight Lineweight 80} {
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
    $w.tb2.lt configure -values $ltypes
    $w.tb2.lw configure -values $lweights
    set sel [$w.t selection]
    set selname ""
    if {$sel ne ""} { set selname [lindex [$w.t item $sel -values] 1] }
    $w.t delete [$w.t children {}]
    foreach r $rows {
        lassign $r name on frz lck color cur ltype lweight
        if {$cur} { set mark [format %c 0x2713] } else { set mark "" }
        set id [$w.t insert {} end -values [list $mark $name \
                    [expr {$on ? "On" : "off"}] [expr {$frz ? "Frozen" : "-"}] [expr {$lck ? "Locked" : "-"}] $color $ltype $lweight]]
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
    if {$act in {ltype lweight}} {
        if {$arg ne ""} { emit LAYERACT $act [string map {" " "_"} $arg] [layersel] }
    } elseif {$act eq "color"} {
        emit LAYERACT color [expr {$arg eq "" ? 7 : $arg}] [layersel]
    } else {
        emit LAYERACT $act [layersel]
    }
}
# ---------------------------------------------------------------- generic form dialog
# rows: {key label value kind choices}; returns key\x1evalue\x1f... or "" when cancelled
proc formdlg {title rows oklabel} {
    set w .form
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w $title
    wm transient $w .
    set f [ttk::frame $w.f -padding 10]
    pack $f -fill both -expand 1
    set i 0
    set ::formkeys {}
    foreach r $rows {
        lassign $r key label value kind choices
        ttk::label $f.l$i -text $label
        if {$kind eq "combo"} {
            ttk::combobox $f.v$i -values $choices -width 22
            $f.v$i set $value
        } else {
            ttk::entry $f.v$i -width 24
            $f.v$i insert 0 $value
        }
        grid $f.l$i -row $i -column 0 -sticky w -padx {0 10} -pady 2
        grid $f.v$i -row $i -column 1 -sticky ew -pady 2
        lappend ::formkeys [list $key $f.v$i]
        incr i
    }
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text $oklabel -command {formdone 1}
    ttk::button $b.cancel -text Cancel -command {formdone 0}
    pack $b.cancel $b.ok -side right -padx 3
    grid $b -row $i -column 0 -columnspan 2 -sticky e -pady {10 0}
    bind $w <Return> {formdone 1}
    bind $w <Escape> {formdone 0}
    set ::formresult ""
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    focus $f.v0
    grab $w
    tkwait window $w
    return $::formresult
}
proc formdone {ok} {
    set ::formresult ""
    if {$ok} {
        set parts {}
        foreach kv $::formkeys { lassign $kv key wdg; lappend parts "$key\x1e[$wdg get]" }
        set ::formresult [join $parts "\x1f"]
        if {$::formresult eq ""} { set ::formresult "\x1e" }
    }
    destroy .form
}

# ---------------------------------------------------------------- alias editor
# pairs: flat list alias command ...; returns the edited flat list joined by \x1f, or "" if cancelled
proc aliasdlg {pairs} {
    set w .alias
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Edit Command Aliases"
    wm transient $w .
    wm geometry $w 520x520+[expr {[winfo rootx .] + 160}]+[expr {[winfo rooty .] + 80}]
    set f [ttk::frame $w.f -padding 8]
    pack $f -fill both -expand 1
    ttk::treeview $f.t -columns {alias command} -show headings -selectmode browse
    $f.t heading alias -text Alias; $f.t column alias -width 120
    $f.t heading command -text "AutoCAD-style command"; $f.t column command -width 300
    ttk::scrollbar $f.s -command [list $f.t yview]
    $f.t configure -yscrollcommand [list $f.s set]
    foreach {a c} $pairs { $f.t insert {} end -values [list $a $c] }
    grid $f.t -row 0 -column 0 -columnspan 4 -sticky nsew
    grid $f.s -row 0 -column 4 -sticky ns
    ttk::label $f.la -text Alias:
    ttk::entry $f.a -width 12
    ttk::label $f.lc -text Command:
    ttk::entry $f.c -width 24
    grid $f.la -row 1 -column 0 -sticky w -pady 6
    grid $f.a -row 1 -column 1 -sticky w
    grid $f.lc -row 1 -column 2 -sticky e
    grid $f.c -row 1 -column 3 -sticky ew
    set b [ttk::frame $f.b]
    ttk::button $b.add -text "Add / Update" -command {
        set a [string toupper [string trim [.alias.f.a get]]]; set c [string toupper [string trim [.alias.f.c get]]]
        if {$a ne "" && $c ne ""} {
            foreach id [.alias.f.t children {}] { if {[lindex [.alias.f.t item $id -values] 0] eq $a} { .alias.f.t delete $id } }
            .alias.f.t insert {} 0 -values [list $a $c]
        }
    }
    ttk::button $b.del -text "Delete" -command { foreach id [.alias.f.t selection] { .alias.f.t delete $id } }
    ttk::button $b.ok -text "Save" -command {
        set out {}
        foreach id [.alias.f.t children {}] { lassign [.alias.f.t item $id -values] a c; lappend out "$a\x1e$c" }
        set ::aliasresult [join $out "\x1f"]; if {$::aliasresult eq ""} { set ::aliasresult "\x1e" }
        destroy .alias
    }
    ttk::button $b.cancel -text Cancel -command { set ::aliasresult ""; destroy .alias }
    pack $b.add $b.del -side left -padx 2
    pack $b.cancel $b.ok -side right -padx 2
    grid $b -row 2 -column 0 -columnspan 5 -sticky ew -pady {6 0}
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 3 -weight 1
    bind $f.t <<TreeviewSelect>> {
        set id [.alias.f.t selection]
        if {$id ne ""} { lassign [.alias.f.t item $id -values] a c
            .alias.f.a delete 0 end; .alias.f.a insert 0 $a; .alias.f.c delete 0 end; .alias.f.c insert 0 $c }
    }
    set ::aliasresult ""
    grab $w
    tkwait window $w
    return $::aliasresult
}
emit READY
# test hook: LISPDRAFT_TEST_TCL=script.tcl is sourced after startup (used by the automated GUI tests)
if {[info exists env(LISPDRAFT_TEST_TCL)]} { after 1500 [list source $env(LISPDRAFT_TEST_TCL)] }
