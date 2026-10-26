# gui.tcl - Fancy Pencil's Tk front end. Started by the Lisp process as
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
proc emit {args} {
    # while the in-place text editor is open, the drawing ignores clicks, keys and menu picks
    if {[info exists ::mted] && $::mted ne "" && [lindex $args 0] in {M B U R P D Q W L K MENU DBL G PROP CURPROP LAYERSET}} return
    puts [join $args " "]; flush stdout
}

proc readcmd {} {
    # drain every complete line that is available, evaluating complete Tcl commands
    while {[gets stdin line] >= 0} {
        append ::buf $line "\n"
        if {[info complete $::buf]} {
            set c $::buf
            set ::buf ""
            if {[catch {uplevel #0 $c} err]} {
                puts stderr "Fancy Pencil Tcl error: $err"
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

wm title . "Fancy Pencil"
# window / taskbar icon (the launcher icon lives in support/icons)
catch {
    set icondir [file join [file dirname [file normalize [info script]]] .. support icons]
    set icons {}
    foreach n {16 32 48 64 128} {
        lappend icons [image create photo -file [file join $icondir fancypencil-$n.png]]
    }
    wm iconphoto . -default {*}[lreverse $icons]
}
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
    ttk::style configure TNotebook -background $panel -bordercolor "#1b2027"
    ttk::style configure TNotebook.Tab -background $panel2 -foreground $fg -padding {10 3}
    ttk::style map TNotebook.Tab -background [list selected $accent active "#46505e"] -foreground [list selected "#ffffff"]
    ttk::style configure TCheckbutton -background $panel -foreground $fg
    ttk::style map TCheckbutton -background [list active $panel]
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

foreach {name label} {file File edit Edit view View draw Draw solid Solids dim Dimension modify Modify format Format tools Tools express Express window Window help Help} {
    menu .mb.$name
    .mb add cascade -label $label -menu .mb.$name
}
mcmd .mb.file "New" NEW Ctrl+N
mcmd .mb.file "Open..." OPEN Ctrl+O
menu .mb.file.recent -tearoff 0
.mb.file add cascade -label "Recent Files" -menu .mb.file.recent
mcmd .mb.file "Close" CLOSE Ctrl+F4
mcmd .mb.file "Close All" CLOSEALL
mcmd .mb.file "Save" QSAVE Ctrl+S
mcmd .mb.file "Save As..." SAVEAS
mcmd .mb.file "Plot..." PLOT Ctrl+P
.mb.file add separator
mcmd .mb.file "Import DXF..." DXFIN
mcmd .mb.file "Export DXF..." DXFOUT
.mb.file add separator
mcmd .mb.file "Exit" QUIT

mcmd .mb.edit "Undo" U Ctrl+Z
mcmd .mb.edit "Redo" REDO Ctrl+Y
.mb.edit add separator
mcmd .mb.edit "Cut" CUTCLIP Ctrl+X
mcmd .mb.edit "Copy" COPYCLIP Ctrl+C
mcmd .mb.edit "Copy with Base Point" COPYBASE Ctrl+Shift+C
mcmd .mb.edit "Paste" PASTECLIP Ctrl+V
mcmd .mb.edit "Paste as Block" PASTEBLOCK Ctrl+Shift+V
mcmd .mb.edit "Paste to Original Coordinates" PASTEORIG
mcmd .mb.edit "Create Block from Selection..." BLOCK
.mb.edit add separator
mcmd .mb.edit "Erase" ERASE Del
mcmd .mb.edit "Select All" SELECTALL Ctrl+A

menu .mb.view.v3 -tearoff 0
.mb.view add cascade -label "3D Views" -menu .mb.view.v3
foreach {label kw} {Top Top Bottom Bottom Left Left Right Right Front Front Back BAck - - \
                    "SW Isometric" SWiso "SE Isometric" SEiso "NE Isometric" NEiso "NW Isometric" NWiso} {
    if {$label eq "-"} { .mb.view.v3 add separator; continue }
    .mb.view.v3 add radiobutton -label $label -variable ::viewname -value $label -command [list menucmd "-VIEW $kw"]
}
set ::viewname Top
.mb.view add command -label "Plan View (Top)" -command {menucmd PLAN}
menu .mb.view.ucs -tearoff 0
.mb.view add cascade -label "Working Plane (UCS)" -menu .mb.view.ucs
mcmd .mb.view.ucs "World" "UCS World"
mcmd .mb.view.ucs "Object / Face (pick a panel)" "UCS OBject"
mcmd .mb.view.ucs "3 Point" "UCS 3point"
mcmd .mb.view.ucs "Origin" "UCS"
mcmd .mb.view.ucs "Facing the View" "UCS View"
.mb.view.ucs add separator
mcmd .mb.view.ucs "Rotate About X" "UCS X"
mcmd .mb.view.ucs "Rotate About Y" "UCS Y"
mcmd .mb.view.ucs "Rotate About Z" "UCS Z"
.mb.view.ucs add separator
mcmd .mb.view.ucs "Previous" "UCS Previous"
.mb.view add checkbutton -label "Show View Picker" -variable ::vc_shown -command {viewcube_show $::vc_shown}
.mb.view add separator
menu .mb.view.iso -tearoff 0
.mb.view add cascade -label "Isometric Drafting" -menu .mb.view.iso
mcmd .mb.view.iso "Off (Orthographic)" "ISODRAFT Orthographic"
mcmd .mb.view.iso "Isoplane Left" "ISODRAFT isoplaneLeft"
mcmd .mb.view.iso "Isoplane Top" "ISODRAFT isoplaneTop"
mcmd .mb.view.iso "Isoplane Right" "ISODRAFT isoplaneRight"
.mb.view.iso add separator
mcmd .mb.view.iso "Next Isoplane" ISOPLANE F5
.mb.view add separator
mcmd .mb.view "Zoom Extents" "ZOOM E"
mcmd .mb.view "Zoom Window" "ZOOM W"
mcmd .mb.view "Zoom Previous" "ZOOM P"
mcmd .mb.view "Pan" PAN
.mb.view add separator
mcmd .mb.view "Regen" REGEN

foreach {l c} {Box BOX Wedge WEDGE Cylinder CYLINDER Cone CONE Sphere SPHERE Torus TORUS} { mcmd .mb.solid $l $c }
.mb.solid add separator
foreach {l c} {Extrude EXTRUDE Revolve REVOLVE Sweep SWEEP Loft LOFT Presspull PRESSPULL} { mcmd .mb.solid $l $c }
.mb.solid add separator
foreach {l c} {Union UNION Subtract SUBTRACT Intersect INTERSECT Slice SLICE} { mcmd .mb.solid $l $c }
.mb.solid add separator
mcmd .mb.solid "Working Plane on a Face (UCS)" "UCS Face"
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
mcmd .mb.draw "Block: Make..." BLOCK
mcmd .mb.draw "Block: Edit..." BEDIT
mcmd .mb.draw "Block: Rename..." RENAME
mcmd .mb.draw "Block: Insert" INSERT

foreach {l c} {Move MOVE Copy COPY Rotate ROTATE Scale SCALE Mirror MIRROR Offset OFFSET Stretch STRETCH "Edit Polyline" PEDIT "Edit Hatch" HATCHEDIT \
               Trim TRIM Extend EXTEND Fillet FILLET Chamfer CHAMFER Break BREAK Explode EXPLODE Join JOIN \
               "Array (rectangular)" ARRAYRECT "Array (polar)" ARRAYPOLAR Erase ERASE} {
    mcmd .mb.modify $l $c
}
.mb.modify add separator
mcmd .mb.modify "Align" ALIGN
mcmd .mb.modify "Match Properties" MATCHPROP
mcmd .mb.modify "Properties" PROPERTIES
mcmd .mb.format "Layer..." LAYER
mcmd .mb.format "Make Object's Layer Current" LAYMCUR
mcmd .mb.format "Match Layer" LAYMCH
mcmd .mb.format "Change Layer..." CHANGELAYER
mcmd .mb.format "Change to Current Layer" LAYCUR
mcmd .mb.format "Linetype..." LINETYPE
mcmd .mb.format "Lineweight..." LWEIGHT
mcmd .mb.format "Text Style..." STYLE
mcmd .mb.format "Dimension Style..." DIMSTYLE
mcmd .mb.format "Hatch Settings..." HPSET
mcmd .mb.format "Units..." UNITS
mcmd .mb.format "Drawing Limits..." LIMITS
mcmd .mb.format "Text Size" TEXTSIZE
mcmd .mb.format "Point Style..." DDPTYPE
mcmd .mb.tools "Properties  (Ctrl+1)" PROPERTIES
mcmd .mb.tools "Quick Properties" QUICKPROPERTIES
mcmd .mb.tools "Load AutoLISP..." APPLOAD
menu .mb.tools.alias -tearoff 0
.mb.tools add cascade -label "Command Aliases" -menu .mb.tools.alias
mcmd .mb.tools.alias "Edit Aliases..." ALIASEDIT
mcmd .mb.tools.alias "Open Alias File (fancypencil.pgp)" ALIASFILE
mcmd .mb.tools.alias "Open Alias Folder" ALIASFOLDER
mcmd .mb.tools.alias "Reload Aliases" REINIT
mcmd .mb.tools "Object Snap Settings..." OSNAP
mcmd .mb.tools "Cursor, Selection & Background..." OPTIONS
mcmd .mb.tools "Spelling..." SPELL
mcmd .mb.tools "Distance" DIST
mcmd .mb.tools "ID Point" ID
mcmd .mb.tools "List" LIST
mcmd .mb.tools "Area" AREA
foreach {l c} {Linear DIMLINEAR Aligned DIMALIGNED Radius DIMRADIUS Diameter DIMDIAMETER Angular DIMANGULAR \
               Continue DIMCONTINUE Baseline DIMBASELINE Leader LEADER "Dimension Style..." DIMSTYLE} { mcmd .mb.dim $l $c }
foreach {l c} {"Layer Isolate" LAYISO "Layer Unisolate" LAYUNISO "Layer Off (pick)" LAYOFF "Layer Freeze (pick)" LAYFRZ
               "Layers On" LAYON "Layers Thaw" LAYTHW "Layer Lock (pick)" LAYLCK "Layer Unlock (pick)" LAYULK
               "Change to Current Layer" LAYCUR "Match Object Layer" LAYMCH "Merge Layers" LAYMRG "Delete Layer" LAYDEL
               "Overkill (delete duplicates)" OVERKILL "Break Line Symbol" BREAKLINE "Extended Trim" EXTRIM
               "Multiple Pedit" MPEDIT "Text Case" TCASE "Text Number (TCOUNT)" TCOUNT "Text to MText" TXT2MTXT
               "Move/Copy/Rotate (MOCORO)" MOCORO "Copy Multiple (COPYM)" COPYM "Flatten" FLATTEN
               "Select Similar" SELECTSIMILAR "List nested (XLIST)" XLIST "Burst" BURST} { mcmd .mb.express $l $c }
mcmd .mb.help "Command Reference" HELP
mcmd .mb.help "About Fancy Pencil" ABOUT

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
tbgroup annot Annotation {Linear DIMLINEAR Aligned DIMALIGNED Leader LEADER "Edit Text" TEXTEDIT Block BLOCK Insert INSERT List LIST}
set lf [ttk::frame .tb.layers -padding {2 0 6 0}]
set lr [ttk::frame $lf.r]
canvas $lr.sw -width 14 -height 14 -highlightthickness 0 -background $panel
$lr.sw create rectangle 1 1 13 13 -fill white -outline "#000000" -tags sw
ttk::combobox $lr.cb -width 16 -state readonly -takefocus 0
bind $lr.cb <<ComboboxSelected>> { emit LAYERSET [.tb.layers.r.cb get]; focus .cmd.e }
pack $lr.sw $lr.cb -side left -padx 2
ttk::button $lf.lp -text "Layer Properties" -style Tool.TButton -width 0 -takefocus 0 -command {menucmd LAYER}
set lb [ttk::frame $lf.b]
ttk::button $lb.mc -text "Make Current" -style Tool.TButton -width 0 -takefocus 0 -command {menucmd LAYMCUR}
ttk::button $lb.mh -text "Match" -style Tool.TButton -width 0 -takefocus 0 -command {menucmd LAYMCH}
ttk::button $lb.ch -text "Change" -style Tool.TButton -width 0 -takefocus 0 -command {menucmd CHANGELAYER}
pack $lb.mc $lb.mh $lb.ch -side left -expand 1 -fill x
ttk::label $lf.t -text Layers -style Group.TLabel
pack $lr -side top -pady 1
pack $lf.lp -side top -fill x
pack $lb -side top -fill x
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
# file tabs: one per open drawing, like AutoCAD's
frame .tabs -background "#1b2027" -height 28
pack .tabs -side top -fill x
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
# rows: {key label value kind choices}; edits are sent as PROP key value
proc fillrows {fr rows} {
    foreach w [winfo children $fr] { destroy $w }
    set i 0
    foreach r $rows {
        lassign $r key label value kind choices
        if {$kind eq "head"} {
            label $fr.h$i -text $label -background $::panel -foreground $::dim -anchor w -font {Helvetica 8 bold}
            grid $fr.h$i -row $i -column 0 -columnspan 2 -sticky ew -pady {6 1}
            incr i; continue
        }
        label $fr.l$i -text $label -background $::panel -foreground $::fg -anchor w -font {Helvetica 9}
        switch -- $kind {
            ro { label $fr.v$i -text $value -background $::panel -foreground $::dim -anchor w -font {Helvetica 9} }
            combo {
                ttk::combobox $fr.v$i -values $choices -width 16 -state readonly -takefocus 0
                $fr.v$i set $value
                bind $fr.v$i <<ComboboxSelected>> [list palemit $key $fr.v$i]
            }
            default {
                entry $fr.v$i -background $::panel2 -foreground $::fg -insertbackground $::fg -relief flat -width 16 -font {Helvetica 9}
                $fr.v$i insert 0 $value
                bind $fr.v$i <Return> [list palemit $key $fr.v$i]
                bind $fr.v$i <KP_Enter> [list palemit $key $fr.v$i]
            }
        }
        grid $fr.l$i -row $i -column 0 -sticky ew -padx {4 6} -pady 1
        grid $fr.v$i -row $i -column 1 -sticky ew -pady 1
        incr i
    }
    grid columnconfigure $fr 1 -weight 1
}
# the object-type list at the top ("All (15)", "Line (10)" ...): pick one to see and edit only those
proc settypes {cb types current} {
    set labels {}; set cur ""
    foreach t $types { lappend labels [lindex $t 1]; if {[lindex $t 0] eq $current} { set cur [lindex $t 1] } }
    $cb configure -values $labels
    $cb set $cur
    set ::typemap($cb) $types
}
proc typepicked {cb} {
    foreach t $::typemap($cb) { if {[lindex $t 1] eq [$cb get]} { emit PALTYPE [lindex $t 0] } }
    focus .cmd.e
}
proc palette_set {what rows {types {}} {current ALL}} {
    if {[llength $types] > 0} {
        pack forget .pal.what
        pack .pal.type -side top -fill x -padx 6 -pady 3 -after .pal.title
        settypes .pal.type $types $current
    } else {
        pack forget .pal.type
        pack .pal.what -side top -fill x -after .pal.title
        .pal.what configure -text $what
    }
    fillrows .pal.rows $rows
}
ttk::combobox .pal.type -state readonly -takefocus 0
bind .pal.type <<ComboboxSelected>> {typepicked .pal.type}

# ---- Quick Properties: a small panel by the cursor with the main properties of the selection ----
proc qpanel_set {rows types current} {
    set w .qp
    if {![winfo exists $w]} {
        toplevel $w -background $::panel
        wm title $w "Quick Properties"
        wm transient $w .
        wm resizable $w 0 0
        ttk::combobox $w.type -state readonly -takefocus 0 -width 26
        bind $w.type <<ComboboxSelected>> {typepicked .qp.type}
        frame $w.rows -background $::panel
        ttk::button $w.close -text "Close" -command qpanel_close
        pack $w.type -side top -fill x -padx 6 -pady {6 3}
        pack $w.rows -side top -fill both -expand 1 -padx 4
        pack $w.close -side bottom -anchor e -padx 6 -pady 6
        bind $w <Escape> qpanel_close
        wm protocol $w WM_DELETE_WINDOW qpanel_close
        lassign [winfo pointerxy .] px py
        wm geometry $w +[expr {$px + 24}]+[expr {$py + 16}]
    }
    settypes $w.type $types $current
    fillrows $w.rows $rows
    raise $w
}
proc qpanel_close {} { catch {destroy .qp}; emit QPCLOSE; focus .cmd.e }
proc qpanel_hide {} { catch {destroy .qp} }
proc palemit {key w} { emit PROP $key [$w get]; focus .cmd.e }
bind all <Control-Key-1> {palette_toggle; break}

bind .c <Motion> {emit M %x %y; rollover_arm %x %y}
bind .c <Leave> {rollover_hide}
bind .c <ButtonPress-1> {rollover_hide; if {$::mted ne ""} {mted_done ok; break}; emit B %x %y [expr {%s & 1}]; focus .cmd.e}
bind .c <Double-Button-1> {if {$::mted eq ""} {emit DBL %x %y}}
bind .c <ButtonRelease-1> {emit U %x %y}
bind .c <ButtonPress-3> {rollover_hide; emit R %x %y %X %Y [expr {%s & 1}]}

# rollover tooltips: rest the mouse on an object for a moment to see its sizes
set ::rollid ""
set ::rollpos {}
label .c.tip -background "#fffbe6" -foreground "#202020" -justify left -font {Helvetica 9} \
    -borderwidth 1 -relief solid -padx 6 -pady 3
proc rollover_arm {x y} {
    if {[winfo ismapped .c.tip]} { place forget .c.tip }
    if {$::rollid ne ""} { after cancel $::rollid }
    set ::rollpos [list $x $y]
    set ::rollid [after 450 [list emit H $x $y]]
}
proc rollover_hide {} {
    if {$::rollid ne ""} { after cancel $::rollid; set ::rollid "" }
    place forget .c.tip
}
proc rolltip {x y text} {
    # only if the mouse is still where it was when the tip was asked for
    if {$::rollpos ne [list $x $y]} return
    .c.tip configure -text $text
    update idletasks
    set w [winfo reqwidth .c.tip]; set h [winfo reqheight .c.tip]
    set tx [expr {$x + 18}]; set ty [expr {$y + 20}]
    if {$tx + $w > [winfo width .c]} { set tx [expr {$x - $w - 10}] }
    if {$ty + $h > [winfo height .c]} { set ty [expr {$y - $h - 10}] }
    place .c.tip -x $tx -y $ty
}
proc rolltip_hide {} { place forget .c.tip }
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
foreach {t k} {LWT LWT ISO ISO OSNAP F3 ORTHO F8 SNAP F9 GRID F7} {
    ttk::button .st.[string tolower $t] -text $t -style Toggle.TButton -takefocus 0 -command [list togglekey $k]
    pack .st.[string tolower $t] -side right -padx 1
}
proc togglekey {k} { emit K $k; focus .cmd.e }
# open on release: while the button is held the desktop keeps the mouse, so a dialog couldn't take it
bind .st.osnap <ButtonRelease-3> {emit MENU 'OSNAP}

pack .st -in .bottom -side bottom -fill x
pack .cmd -in .bottom -side bottom -fill x
pack .h -in .bottom -side bottom -fill x
raise .h; raise .cmd; raise .st

bind .cmd.e <Return> {cmdsubmit; break}
bind .cmd.e <KP_Enter> {cmdsubmit; break}
bind .cmd.e <space> {if {!$::allowspace} {cmdsubmit; break}}
bind .cmd.e <Up> {histmove -1; break}
bind .cmd.e <Button-3> {cmdline_popup %X %Y; break}
bind .h <Button-3> {hist_popup %X %Y; break}
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
bind all <F5> {emit K F5; break}
bind all <Control-e> {emit K F5; break}
bind all <F9> {emit K F9; break}
bind all <Control-z> {emit MENU U; break}
bind all <Control-y> {emit MENU REDO; break}
bind all <Control-s> {emit MENU QSAVE; break}
bind all <Control-o> {emit MENU OPEN; break}
bind all <Control-n> {emit MENU NEW; break}
bind all <Control-p> {emit MENU PLOT; break}
bind all <Control-Tab> {emit MENU DOCNEXT; break}
bind all <Control-ISO_Left_Tab> {emit MENU DOCPREV; break}
bind all <Control-Shift-Tab> {emit MENU DOCPREV; break}
bind all <Control-F4> {emit MENU CLOSE; break}
bind all <Control-a> {emit MENU SELECTALL; break}
# Ctrl+C/X/V copy, cut and paste drawing objects, except while typing in a text box:
# the text editor and dialog fields keep their own text copy/paste; on the command line,
# Ctrl+C with text selected copies the text, and Ctrl+V pastes text once you've started typing.
proc clipkey {cmd} {
    set w [focus]
    if {$w ne "" && [winfo exists $w] && $w ne ".cmd.e" &&
        [winfo class $w] in {Text Entry TEntry TCombobox Spinbox TSpinbox Listbox}} return
    if {$w eq ".cmd.e"} {
        # the command line's own text copy/paste already ran (see its bindings below)
        if {$cmd in {COPYCLIP CUTCLIP COPYBASE} && [.cmd.e selection present]} return
        if {$cmd in {PASTECLIP PASTEBLOCK} && [string trim [.cmd.e get]] ne ""} return
    }
    emit MENU $cmd
}
bind all <Control-c> {clipkey COPYCLIP; break}
bind all <Control-v> {clipkey PASTECLIP; break}
bind all <Control-x> {clipkey CUTCLIP; break}
bind all <Control-C> {clipkey COPYBASE; break}
bind all <Control-V> {clipkey PASTEBLOCK; break}
bind .cmd.e <Control-c> {if {[.cmd.e selection present]} { continue } { clipkey COPYCLIP; break }}
bind .cmd.e <Control-x> {if {[.cmd.e selection present]} { continue } { clipkey CUTCLIP; break }}
bind .cmd.e <Control-v> {if {[string trim [.cmd.e get]] ne ""} { continue } { clipkey PASTECLIP; break }}
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
proc settoggles {grid snap ortho osnap {lwt 0} {iso 0} {isolabel ISO}} {
    foreach {w v} [list grid $grid snap $snap ortho $ortho osnap $osnap lwt $lwt iso $iso] {
        .st.$w configure -style [expr {$v ? "On.Toggle.TButton" : "Toggle.TButton"}]
    }
    .st.iso configure -text $isolabel
}
# ---- the view picker: cube pictures for the ten standard views, top right of the drawing ----
set ::vc_layout {Top {1 1} Bottom {3 1} Left {1 0} Right {1 2} Front {2 1} Back {0 1}
                 "SW Isometric" {2 0} "SE Isometric" {2 2} "NE Isometric" {0 2} "NW Isometric" {0 0}}
set ::vc_shown 1
frame .c.vc -background "#1b1f24" -highlightthickness 1 -highlightbackground "#3a4049"
label .c.vc.h -text "Views   hide" -background "#1b1f24" -foreground "#9aa3ad" -font {Helvetica 8 bold} -cursor hand2
canvas .c.vc.c -width 102 -height 136 -background "#1b1f24" -highlightthickness 0 -cursor hand2
label .c.vc.t -text "" -background "#1b1f24" -foreground "#c9ced6" -font {Helvetica 8} -width 13
pack .c.vc.h -side top -fill x
pack .c.vc.c -side top
pack .c.vc.t -side top -fill x
place .c.vc -relx 1.0 -x -8 -y 30 -anchor ne
bind .c.vc.h <Button-1> viewcube_toggle
proc viewcube_toggle {} {
    if {[winfo ismapped .c.vc.c]} {
        pack forget .c.vc.c .c.vc.t
        .c.vc.h configure -text "Views   show"
    } else {
        pack .c.vc.c -side top; pack .c.vc.t -side top -fill x
        .c.vc.h configure -text "Views   hide"
    }
}
proc viewcube_show {on} {
    if {$on} { place .c.vc -relx 1.0 -x -8 -y 30 -anchor ne } else { place forget .c.vc }
}
proc viewcube_set {cells} {
    set c .c.vc.c
    $c delete all
    foreach cell $cells {
        lassign $cell kw name polys
        lassign [dict get $::vc_layout $name] row col
        set cx [expr {17 + $col * 34}]; set cy [expr {17 + $row * 34}]
        set tag "v[string map {" " _} $name]"
        $c create rectangle [expr {$cx - 16}] [expr {$cy - 16}] [expr {$cx + 16}] [expr {$cy + 16}] \
            -fill "#1b1f24" -outline "" -tags [list $tag bg bg$tag]
        foreach p $polys {
            set col [lindex $p 0]; set pts {}
            foreach {x y} [lrange $p 1 end] { lappend pts [expr {$cx + $x}] [expr {$cy + $y}] }
            $c create polygon {*}$pts -fill $col -outline "#2a3038" -tags $tag
        }
        $c bind $tag <Enter> [list viewcube_hover $tag $name 1]
        $c bind $tag <Leave> [list viewcube_hover $tag $name 0]
        $c bind $tag <Button-1> [list menucmd "-VIEW $kw"]
    }
    viewcube_highlight
}
proc viewcube_hover {tag name on} {
    .c.vc.c itemconfigure bg$tag -fill [expr {$on ? "#34507a" : ($name eq $::viewname ? "#2b4a73" : "#1b1f24")}]
    .c.vc.t configure -text [expr {$on ? $name : $::viewname}]
}
proc viewcube_highlight {} {
    if {![winfo exists .c.vc.c]} return
    foreach id [.c.vc.c find withtag bg] { .c.vc.c itemconfigure $id -fill "#1b1f24" }
    .c.vc.c itemconfigure "bgv[string map {" " _} $::viewname]" -fill "#2b4a73"
    .c.vc.t configure -text $::viewname
}

proc setviewname {name} {
    set ::viewname $name
    viewcube_highlight
    .st.model configure -text [expr {$name eq "Top" ? "MODEL" : "MODEL - [string toupper $name]"}]
}
proc setlayers {names current color} {
    .tb.layers.r.cb configure -values $names
    setlayerbox $current $color
}
# the drop-down shows the selected objects' layer (or VARIES) without changing the current layer
proc setlayerbox {shown color} {
    .tb.layers.r.cb set $shown
    .tb.layers.r.sw itemconfigure sw -fill $color
}
proc settitle {s} { wm title . $s }
proc ask {id script} {
    # always answer: the Lisp side is waiting for this reply, so an error must not leave it hanging
    if {[catch {uplevel #0 $script} r]} {
        puts stderr "Fancy Pencil Tcl error: $r\n$::errorInfo"
        # a half-opened text editor would keep blocking clicks and keys: clear it
        set ::mted ""
        catch {destroy .mtedf .mtedbar}
        catch {grab release [grab current]}
        catch {focus .cmd.e}
        emit A $id ""
        emit TCLERR [string map {"\n" " "} $r]
        return
    }
    emit A $id [string map {"\n" " "} $r]
}
# take the keyboard/mouse for a dialog once the desktop has actually shown it
proc safegrab {w} {
    if {![winfo exists $w]} return
    if {![winfo viewable $w]} { catch {tkwait visibility $w} }
    if {[catch {grab $w}]} {
        # refused (e.g. a mouse button is still held): try again shortly
        after 150 [list catch [list grab $w]]
        after 600 [list catch [list grab $w]]
    }
}
proc setgrread {v} { set ::grread $v }

# ---------------------------------------------------------------- document tabs
# items: list of {id label modified}; current: id of the active drawing
# File > Recent Files. items: list of {name folder}, most recent first
proc setrecent {items} {
    set m .mb.file.recent
    $m delete 0 end
    if {[llength $items] == 0} {
        $m add command -label "(no recent files)" -state disabled
        return
    }
    set i 0
    foreach it $items {
        incr i
        lassign $it name dir
        set label [expr {$i < 10 ? "$i  $name" : "   $name"}]
        $m add command -label $label -accelerator [file tail [string trimright $dir /]] \
            -underline [expr {$i < 10 ? 0 : -1}] -command [list menucmd "OPENRECENT $i"]
    }
    $m add separator
    $m add command -label "Clear Recent Files" -command [list menucmd CLEARRECENT]
}
setrecent {}

proc settabs {items current} {
    foreach w [winfo children .tabs] { destroy $w }
    foreach it $items {
        lassign $it id label modified
        set on [expr {$id == $current}]
        set bg [expr {$on ? $::bg : "#2b313b"}]
        set fg [expr {$on ? "#ffffff" : "#aab3bf"}]
        set f [frame .tabs.t$id -background $bg -borderwidth 0]
        label $f.l -text [expr {$modified ? "$label *" : $label}] -background $bg -foreground $fg \
            -font {Helvetica 10} -padx 10 -pady 4
        label $f.x -text "\u00D7" -background $bg -foreground "#8a94a3" -font {Helvetica 11} -padx 4
        if {$on} { frame $f.bar -background $::accent -height 2; pack $f.bar -side top -fill x }
        pack $f.l $f.x -side left
        foreach w [list $f $f.l] {
            bind $w <Button-1> [list emit MENU "DOCSWITCH $id"]
            bind $w <Button-2> [list emit MENU "DOCCLOSE $id"]
        }
        bind $f.x <Button-1> [list emit MENU "DOCCLOSE $id"]
        bind $f.x <Enter> [list $f.x configure -foreground "#ff8080"]
        bind $f.x <Leave> [list $f.x configure -foreground "#8a94a3"]
        pack $f -side left -padx {0 1} -pady {3 0}
    }
    label .tabs.plus -text "+" -background "#1b2027" -foreground "#aab3bf" -font {Helvetica 13} -padx 8
    bind .tabs.plus <Button-1> {emit MENU NEW}
    pack .tabs.plus -side left
    # Window menu: the same list
    set m .mb.window
    $m delete 0 end
    $m add command -label "Next Drawing" -accelerator Ctrl+Tab -command {menucmd DOCNEXT}
    $m add command -label "Previous Drawing" -accelerator Ctrl+Shift+Tab -command {menucmd DOCPREV}
    $m add command -label "Close" -accelerator Ctrl+F4 -command {menucmd CLOSE}
    $m add command -label "Close All" -command {menucmd CLOSEALL}
    $m add separator
    set n 1
    foreach it $items {
        lassign $it id label modified
        $m add radiobutton -label "$n  $label" -variable ::curdoc -value $id -command [list menucmd "DOCSWITCH $id"]
        incr n
    }
    set ::curdoc $current
}

# ---------------------------------------------------------------- in-place text editor
# MTEXT / DTEXT / TEXTEDIT type straight onto the drawing, like AutoCAD's in-place editor.
#   mode    mtext: multi-line, word-wrapped to WIDTH px (0 = no wrap), with a formatting bar.
#                  Enter = new line; Ctrl+Enter / OK / click outside = done.
#           line : one line (DTEXT, single-line TEXT); Enter = done.
#   X Y     screen position: top-left corner (mtext) or start of the baseline (line)
#   opts    {style family bold italic underline height k styles}
#           height in drawing units, k = font pixels per drawing unit,
#           styles = list of {name font bold italic height}
#   Returns "CANCEL", or "OK " + style \x1f font \x1f height \x1f bold \x1f italic \x1f underline \x1f justify \x1f text
#   (text MTEXT-encoded in mtext mode: \P new line, \\ \{ \} literal).
set ::mted ""
proc tkfamily {font} {
    # display family for a style font: AutoCAD .shx files get a look-alike system font,
    # and a family that isn't installed gets a metric-compatible one
    set f [string tolower [file tail $font]]
    if {[file extension $f] eq ".shx"} {
        if {[regexp {^(romanc|romant|romand|complex|italic|isoct)} $f]} { set fam Times } \
        elseif {[string match monotxt* $f]} { set fam Courier } \
        else { set fam Helvetica }
    } elseif {[file extension $f] in {.ttf .otf .ttc}} {
        set fam [file rootname [file tail $font]]
        if {[string match -nocase arial* $fam]} { set fam Arial }
        if {[string match -nocase times* $fam]} { set fam "Times New Roman" }
        if {[string match -nocase cour* $fam]} { set fam "Courier New" }
    } else {
        set fam $font
    }
    if {![info exists ::installed_fams]} { set ::installed_fams [font families] }
    set i [lsearch -exact -nocase $::installed_fams $fam]
    if {$i >= 0} { return [lindex $::installed_fams $i] }
    set lf [string tolower $fam]
    if {[regexp {mono|cour} $lf]} { set alts {"Liberation Mono" Cousine "DejaVu Sans Mono"}; set last Courier } \
    elseif {[regexp {serif|roman|times|cambria} $lf] && ![regexp {sans} $lf]} { set alts {"Liberation Serif" Tinos Caladea "DejaVu Serif"}; set last Times } \
    else { set alts {"Liberation Sans" Arimo Carlito "DejaVu Sans"}; set last Helvetica }
    foreach a $alts {
        set i [lsearch -exact -nocase $::installed_fams $a]
        if {$i >= 0} { return [lindex $::installed_fams $i] }
    }
    return $last
}
proc mted_font {} {
    set px [expr {max(9, min(160, round($::mt(height) * $::mt(k))))}]
    set f [list [tkfamily $::mt(font)] -$px]
    if {$::mt(bold)} { lappend f bold }
    if {$::mt(italic)} { lappend f italic }
    if {$::mt(underline)} { lappend f underline }
    return $f
}
proc mted_restyle {} {
    if {$::mted eq "" || ![winfo exists $::mted]} return
    $::mted configure -font [mted_font]
    $::mted tag configure all -justify $::mt(justify)
    mted_fit
}
proc mted_pickstyle {} {
    foreach s $::mt(styles) {
        lassign $s name font bold italic height
        if {[string equal -nocase $name $::mt(style)]} {
            set ::mt(font) $font; set ::mt(bold) $bold; set ::mt(italic) $italic
            if {$height > 0} { set ::mt(height) $height }
        }
    }
    mted_restyle
}
proc mted_families {} {
    if {![info exists ::mt_families]} {
        set fams [lsort -dictionary -unique [font families]]
        set ::mt_families [concat {txt.shx simplex.shx romans.shx romand.shx romanc.shx isocp.shx monotxt.shx} $fams]
    }
    return $::mt_families
}
proc mtedit {mode x y width fpx color init {justify left} {minh 0} {opts {}}} {
    lassign $opts style font bold italic underline height k styles
    if {$k eq "" || $k <= 0} { set k 1; set height $fpx }
    array set ::mt [list mode $mode style $style font $font bold [expr {$bold ne "" && $bold}] \
                        italic [expr {$italic ne "" && $italic}] underline [expr {$underline ne "" && $underline}] \
                        height $height k $k styles $styles justify $justify]
    if {$::mt(font) eq ""} { set ::mt(font) Arial }
    set font [mted_font]
    set ls [font metrics $font -linespace]
    set f .mtedf
    catch {destroy $f}
    frame $f -background $::accent -borderwidth 0
    set t $f.t
    text $t -font $font -foreground $color -background "#171c23" -insertbackground "#ffffff" \
        -insertwidth 2 -relief flat -borderwidth 0 -highlightthickness 0 -padx 3 -pady 1 \
        -undo 1 -selectbackground $::accent -wrap [expr {$mode eq "mtext" && $width > 0 ? "word" : "none"}]
    $t tag configure all -justify $justify
    $t tag configure misspelled -underline 1
    catch {$t tag configure misspelled -underlinefg "#ff5050"}
    $t insert end $init all
    $t edit reset
    pack $t -padx 1 -pady 1 -fill both -expand 1
    # only the text widget's own bindings: keep the program's global keys (Esc, Ctrl+Z, F8...) out
    bindtags $t [list $t Text]

    # ---- the bar above the box: formatting (MTEXT) and OK / Cancel
    set b .mtedbar
    catch {destroy $b}
    frame $b -background "#2b313b" -borderwidth 1 -relief solid
    if {$mode eq "mtext"} {
        set stylenames {}
        foreach s $styles { lappend stylenames [lindex $s 0] }
        ttk::label $b.sl -text "Style" -style Dim.TLabel
        ttk::combobox $b.style -values $stylenames -textvariable ::mt(style) -width 11 -state readonly
        bind $b.style <<ComboboxSelected>> {mted_pickstyle; focus $::mted}
        ttk::label $b.fl -text "Font" -style Dim.TLabel
        ttk::combobox $b.font -values [mted_families] -textvariable ::mt(font) -width 18
        bind $b.font <<ComboboxSelected>> {mted_restyle; focus $::mted}
        bind $b.font <Return> {mted_restyle; focus $::mted; break}
        ttk::label $b.hl -text "Height" -style Dim.TLabel
        ttk::entry $b.h -textvariable ::mt(hentry) -width 7
        set ::mt(hentry) [format %g $height]
        bind $b.h <Return> {mted_setheight; focus $::mted; break}
        bind $b.h <FocusOut> {mted_setheight}
        foreach {w key lbl} {bb bold B ii italic I uu underline U} {
            checkbutton $b.$w -text $lbl -variable ::mt($key) -indicatoron 0 -command {mted_restyle; focus $::mted} \
                -width 2 -background "#353c47" -foreground "#d8dde4" -selectcolor $::accent -relief flat \
                -activebackground "#46505e" -borderwidth 0 -font [list Helvetica 10 [dict get {bold bold italic italic underline underline} $key]]
        }
        foreach {w val lbl} {jl left L jc center C jr right R} {
            radiobutton $b.$w -text $lbl -value $val -variable ::mt(justify) -indicatoron 0 \
                -command {mted_restyle; focus $::mted} -width 2 -background "#353c47" -foreground "#d8dde4" \
                -selectcolor $::accent -relief flat -activebackground "#46505e" -borderwidth 0
        }
        menubutton $b.sym -text "Symbols" -menu $b.sym.m -background "#353c47" -foreground "#d8dde4" \
            -relief flat -activebackground "#46505e" -borderwidth 0 -padx 6
        menu $b.sym.m -tearoff 0
        mted_symbol_items $b.sym.m
        grid $b.sl $b.style $b.fl $b.font $b.hl $b.h $b.bb $b.ii $b.uu $b.jl $b.jc $b.jr $b.sym -padx 2 -pady 3 -row 0
    }
    ttk::button $b.ok -text OK -style Tool.TButton -command {mted_done ok}
    ttk::button $b.no -text Cancel -style Tool.TButton -command {mted_done cancel}
    set hint [expr {$mode eq "mtext" ? "Enter: new line   Ctrl+Enter or click outside: done   Esc: cancel   Right-click: menu"
                                      : "Enter: done   Esc: cancel   Right-click: menu"}]
    label $b.hint -text $hint -background "#2b313b" -foreground "#8a94a3" -font {Helvetica 8}
    if {$mode eq "mtext"} {
        grid $b.ok -row 0 -column 13 -padx {8 2}
        grid $b.no -row 0 -column 14 -padx {2 4}
        grid $b.hint -row 1 -column 0 -columnspan 15 -sticky w -padx 4
    } else {
        grid $b.hint $b.ok $b.no -row 0 -padx 3 -pady 2
    }

    if {$mode eq "mtext"} {
        bind $t <Control-Return> {mted_done ok; break}
        bind $t <KP_Enter> {mted_done ok; break}
    } else {
        bind $t <Return> {mted_done ok; break}
        bind $t <KP_Enter> {mted_done ok; break}
        set y [expr {$y - [font metrics $font -ascent] - 2}]
    }
    bind $t <Escape> {mted_done cancel; break}
    bind $t <Tab> {$::mted insert insert "    " all; break}
    bind $t <<Modified>> {mted_fit}
    bind $t <KeyRelease> {mted_fit; mted_spell_later}
    bind $t <<Paste>> {+after idle mted_spell_later}
    bind $t <Button-3> {$::mted mark set spellpt @%x,%y; mted_popup %X %Y; break}
    bind $t <Control-b> {set ::mt(bold) [expr {!$::mt(bold)}]; mted_restyle; break}
    bind $t <Control-i> {set ::mt(italic) [expr {!$::mt(italic)}]; mted_restyle; break}
    bind $t <Control-u> {set ::mt(underline) [expr {!$::mt(underline)}]; mted_restyle; break}
    bind $t <Control-a> {$::mted tag add sel 1.0 end; break}
    set ::mted $t
    set ::mted_geom [list $mode $x $y $width $ls $minh]
    mted_fit
    after 200 mted_spellcheck
    update idletasks
    set bw [winfo reqwidth $b]; set bh [winfo reqheight $b]
    set bx [expr {max(2, min($x - 4, [winfo width .c] - $bw - 2))}]
    set by [expr {$y - $bh - 4 >= 0 ? $y - $bh - 4 : $y + [winfo reqheight $f] + 4}]
    place $b -in .c -x $bx -y $by
    raise $b
    $t mark set insert end
    $t see insert
    focus $t
    set ::mted_result ""
    vwait ::mted_result
    set r $::mted_result
    destroy $b $f
    set ::mted ""
    focus .cmd.e
    return $r
}
proc mted_setheight {} {
    if {[string is double -strict $::mt(hentry)] && $::mt(hentry) > 0} {
        set ::mt(height) $::mt(hentry)
        mted_restyle
    } else {
        set ::mt(hentry) [format %g $::mt(height)]
    }
}
proc mted_symbol_items {m} {
    foreach {lbl ch} [list "Degree  \u00B0" \u00B0 "Plus/Minus  \u00B1" \u00B1 "Diameter  \u2300" \u2300 \
                          "Almost equal  \u2248" \u2248 "Angle  \u2220" \u2220 "Delta  \u0394" \u0394 \
                          "Centerline  \u2104" \u2104 "Not equal  \u2260" \u2260 "Ohm  \u03A9" \u03A9 \
                          "Squared  \u00B2" \u00B2 "Cubed  \u00B3" \u00B3 "Micro  \u00B5" \u00B5] {
        $m add command -label $lbl -command [list mted_insert $ch]
    }
}
proc mted_insert {s} {
    if {$::mted eq ""} return
    catch {$::mted delete sel.first sel.last}
    $::mted insert insert $s all
    mted_fit
    focus $::mted
}
proc mted_popup {X Y} {
    set m .mtedpop
    catch {destroy $m}
    menu $m -tearoff 0
    set t $::mted
    set hassel [expr {[llength [$t tag ranges sel]] > 0}]
    # a misspelled word under the mouse: suggestions first
    if {[lsearch [$t tag names spellpt] misspelled] >= 0} {
        set r [$t tag prevrange misspelled "spellpt + 1 char"]
        if {$r ne ""} {
            lassign $r a b
            set word [$t get $a $b]
            set sugs [spell_suggest $word]
            if {[llength $sugs] == 0} { $m add command -label "(no suggestions)" -state disabled }
            foreach sg $sugs {
                $m add command -label $sg -font {Helvetica 10 bold}                     -command [list apply {{a b sg} {$::mted delete $a $b; $::mted insert $a $sg all; mted_fit; mted_spellcheck}} $a $b $sg]
            }
            $m add command -label "Add \"$word\" to Dictionary" -command [list apply {{w} {spell_add $w; mted_spellcheck}} $word]
            $m add command -label "Ignore \"$word\"" -command [list apply {{w} {set ::spell_ignore([string tolower $w]) 1; mted_spellcheck}} $word]
            $m add separator
        }
    }
    $m add command -label "Undo" -accelerator Ctrl+Z -command {catch {$::mted edit undo}; mted_fit}
    $m add command -label "Redo" -accelerator Ctrl+Y -command {catch {$::mted edit redo}; mted_fit}
    $m add separator
    $m add command -label "Cut" -accelerator Ctrl+X -command {tk_textCut $::mted; mted_fit} -state [expr {$hassel ? "normal" : "disabled"}]
    $m add command -label "Copy" -accelerator Ctrl+C -command {tk_textCopy $::mted} -state [expr {$hassel ? "normal" : "disabled"}]
    $m add command -label "Paste" -accelerator Ctrl+V -command {tk_textPaste $::mted; $::mted tag add all 1.0 end; mted_fit}
    $m add command -label "Delete" -command {catch {$::mted delete sel.first sel.last}; mted_fit} -state [expr {$hassel ? "normal" : "disabled"}]
    $m add separator
    $m add command -label "Select All" -accelerator Ctrl+A -command {$::mted tag add sel 1.0 end}
    $m add cascade -label "Insert Symbol" -menu $m.sym
    menu $m.sym -tearoff 0
    mted_symbol_items $m.sym
    if {$::mt(mode) eq "mtext"} {
        $m add separator
        $m add command -label "Change Case: UPPER" -command {mted_case toupper}
        $m add command -label "Change Case: lower" -command {mted_case tolower}
    }
    $m add separator
    $m add command -label "OK" -command {mted_done ok}
    $m add command -label "Cancel" -command {mted_done cancel}
    tk_popup $m $X $Y
}
proc mted_case {fn} {
    set t $::mted
    if {[llength [$t tag ranges sel]]} { set a sel.first; set z sel.last } else { set a 1.0; set z end-1c }
    set s [string $fn [$t get $a $z]]
    set ia [$t index $a]
    $t delete $a $z
    $t insert $ia $s all
    mted_fit
}
proc mted_fit {} {
    set t $::mted
    if {$t eq "" || ![winfo exists $t]} return
    $t edit modified 0
    $t tag add all 1.0 end
    lassign $::mted_geom mode x y width ls minh
    set ls [font metrics [$t cget -font] -linespace]
    if {$mode eq "mtext" && $width > 0} {
        set w [expr {$width + 8}]
    } else {
        set w 40
        foreach line [split [$t get 1.0 end-1c] "\n"] {
            set w [expr {max($w, [font measure [$t cget -font] $line] + $ls)}]
        }
    }
    place .mtedf -in .c -x [expr {$x - 4}] -y [expr {$y - 2}] -width $w -height 400
    update idletasks
    set n [$t count -displaylines 1.0 end]
    if {$n eq "" || $n < 1} { set n 1 }
    place .mtedf -in .c -x [expr {$x - 4}] -y [expr {$y - 2}] -width $w -height [expr {max($n * $ls + 6, $minh)}]
    raise .mtedf
    catch {raise .mtedbar}
}
proc mted_done {how} {
    if {$::mted eq ""} return
    if {$how eq "cancel"} { set ::mted_result CANCEL; return }
    if {$::mt(mode) eq "mtext"} { mted_setheight }
    set s [$::mted get 1.0 end-1c]
    if {$::mt(mode) eq "mtext"} {
        set s [string map [list "\\" "\\\\" "\{" "\\\{" "\}" "\\\}" "\n" "\\P"] [string trimright $s "\n"]]
    } else {
        set s [string map {"\n" " "} $s]
    }
    set fields [list $::mt(style) $::mt(font) $::mt(height) $::mt(bold) $::mt(italic) $::mt(underline) $::mt(justify) $s]
    set ::mted_result "OK [join $fields \x1f]"
}

# right-click menu for the command line
proc cmdline_popup {X Y} {
    set m .cmdpop
    catch {destroy $m}
    menu $m -tearoff 0
    set e .cmd.e
    set hassel [$e selection present]
    $m add command -label "Cut" -accelerator Ctrl+X -command {event generate .cmd.e <<Cut>>} -state [expr {$hassel ? "normal" : "disabled"}]
    $m add command -label "Copy" -accelerator Ctrl+C -command {event generate .cmd.e <<Copy>>} -state [expr {$hassel ? "normal" : "disabled"}]
    $m add command -label "Paste" -accelerator Ctrl+V -command {event generate .cmd.e <<Paste>>}
    $m add separator
    $m add command -label "Select All" -command {.cmd.e selection range 0 end; .cmd.e icursor end}
    $m add command -label "Clear" -command {.cmd.e delete 0 end}
    if {[info exists ::cmdhist] && [llength $::cmdhist]} {
        $m add separator
        $m add cascade -label "Recent Input" -menu $m.recent
        menu $m.recent -tearoff 0
        foreach h [lrange [lreverse [lsort -unique $::cmdhist]] 0 11] {
            $m.recent add command -label $h -command [list apply {{h} {.cmd.e delete 0 end; .cmd.e insert 0 $h; focus .cmd.e}} $h]
        }
    }
    tk_popup $m $X $Y
}
# right-click menu for the command history: copy
proc hist_popup {X Y} {
    set m .histpop
    catch {destroy $m}
    menu $m -tearoff 0
    $m add command -label "Copy" -command {catch {clipboard clear; clipboard append [.h get sel.first sel.last]}}
    $m add command -label "Copy All" -command {clipboard clear; clipboard append [.h get 1.0 end-1c]}
    $m add command -label "Select All" -command {.h tag add sel 1.0 end}
    tk_popup $m $X $Y
}

# ---------------------------------------------------------------- text style dialog (STYLE)
# records: list of {name font bold italic height width oblique}; current: style name.
# Returns "CANCEL" or  current \x1e rec \x1e rec ...  with each rec's fields joined by \x1f.
proc styledlg {records current} {
    set w .sty
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Text Style"
    wm transient $w .
    array unset ::sty
    array unset ::stydata
    set ::sty(order) {}
    foreach r $records {
        lassign $r name font bold italic height width oblique
        lappend ::sty(order) $name
        set ::stydata($name) [list $font $bold $italic $height $width $oblique]
    }
    set ::sty(current) $current
    set ::sty(sel) ""
    set f [ttk::frame $w.f -padding 10]
    pack $f -fill both -expand 1
    ttk::label $f.cur -text "Current text style: $current" -style Dim.TLabel
    grid $f.cur -row 0 -column 0 -columnspan 3 -sticky w -pady {0 6}
    # styles list
    ttk::label $f.sl -text "Styles:"
    listbox $f.list -height 12 -width 20 -exportselection 0 -background $::panel2 -foreground $::fg \
        -selectbackground $::accent -borderwidth 0 -highlightthickness 0 -activestyle none
    grid $f.sl -row 1 -column 0 -sticky w
    grid $f.list -row 2 -column 0 -rowspan 8 -sticky nsew -padx {0 12}
    bind $f.list <<ListboxSelect>> {styledlg_select [.sty.f.list get [.sty.f.list curselection]]}
    # font
    ttk::label $f.fl -text "Font name:"
    ttk::combobox $f.font -values [mted_families] -textvariable ::sty(font) -width 26
    bind $f.font <<ComboboxSelected>> styledlg_preview
    bind $f.font <KeyRelease> styledlg_preview
    ttk::label $f.stl -text "Font style:"
    set fs [ttk::frame $f.fs]
    ttk::checkbutton $fs.b -text Bold -variable ::sty(bold) -command styledlg_preview
    ttk::checkbutton $fs.i -text Italic -variable ::sty(italic) -command styledlg_preview
    pack $fs.b $fs.i -side left -padx {0 10}
    ttk::label $f.hl -text "Height (0 = ask when placing):"
    ttk::entry $f.h -textvariable ::sty(height) -width 10
    ttk::label $f.wl -text "Width factor:"
    ttk::entry $f.wf -textvariable ::sty(width) -width 10
    ttk::label $f.ol -text "Oblique angle:"
    ttk::entry $f.ob -textvariable ::sty(oblique) -width 10
    canvas $f.pv -width 320 -height 80 -background "#171c23" -highlightthickness 0
    set row 1
    foreach {l v} [list $f.fl $f.font $f.stl $f.fs $f.hl $f.h $f.wl $f.wf $f.ol $f.ob] {
        grid $l -row $row -column 1 -sticky w -pady 2
        grid $v -row $row -column 2 -sticky w -pady 2
        incr row
    }
    grid $f.pv -row $row -column 1 -columnspan 2 -sticky ew -pady {8 0}
    # buttons
    set lb [ttk::frame $f.lb]
    ttk::button $lb.cur -text "Set Current" -command styledlg_setcurrent
    ttk::button $lb.new -text "New..." -command styledlg_new
    ttk::button $lb.del -text "Delete" -command styledlg_delete
    pack $lb.cur $lb.new $lb.del -side left -padx {0 4}
    grid $lb -row 10 -column 0 -columnspan 2 -sticky w -pady {10 0}
    set bb [ttk::frame $f.bb]
    ttk::button $bb.ok -text "OK" -command {styledlg_done 1}
    ttk::button $bb.cancel -text "Cancel" -command {styledlg_done 0}
    pack $bb.cancel $bb.ok -side right -padx 3
    grid $bb -row 10 -column 2 -sticky e -pady {10 0}
    bind $w <Escape> {styledlg_done 0}
    styledlg_fill
    styledlg_select $current
    set ::sty(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 180}]+[expr {[winfo rooty .] + 100}]
    safegrab $w
    tkwait window $w
    return $::sty(result)
}
proc styledlg_fill {} {
    set l .sty.f.list
    $l delete 0 end
    foreach n $::sty(order) { $l insert end $n }
}
proc styledlg_store {} {
    # keep the fields of the style being edited
    set n $::sty(sel)
    if {$n eq "" || ![info exists ::stydata($n)]} return
    set ::stydata($n) [list $::sty(font) $::sty(bold) $::sty(italic) $::sty(height) $::sty(width) $::sty(oblique)]
}
proc styledlg_select {name} {
    if {$name eq ""} return
    styledlg_store
    set ::sty(sel) $name
    lassign $::stydata($name) ::sty(font) ::sty(bold) ::sty(italic) ::sty(height) ::sty(width) ::sty(oblique)
    set i [lsearch -exact $::sty(order) $name]
    .sty.f.list selection clear 0 end
    .sty.f.list selection set $i
    .sty.f.list see $i
    styledlg_preview
}
proc styledlg_preview {} {
    set c .sty.f.pv
    $c delete all
    set f [list [tkfamily $::sty(font)] -30]
    if {$::sty(bold)} { lappend f bold }
    if {$::sty(italic)} { lappend f italic }
    $c create text 12 40 -anchor w -text "AaBbCc 123" -font $f -fill "#ffffff"
}
proc styledlg_setcurrent {} {
    set ::sty(current) $::sty(sel)
    .sty.f.cur configure -text "Current text style: $::sty(current)"
}
proc styledlg_new {} {
    set name [styledlg_askname]
    if {$name eq ""} return
    if {[lsearch -exact -nocase $::sty(order) $name] >= 0} {
        tk_messageBox -parent .sty -title "Text Style" -message "A style called $name already exists."
        return
    }
    styledlg_store
    set ::stydata($name) $::stydata($::sty(sel))
    lappend ::sty(order) $name
    styledlg_fill
    set ::sty(sel) ""
    styledlg_select $name
}
proc styledlg_askname {} {
    set w .sty.ask
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "New Text Style"
    wm transient $w .sty
    set f [ttk::frame $w.f -padding 10]
    pack $f
    set ::sty(newname) "Style[expr {[llength $::sty(order)]}]"
    ttk::label $f.l -text "Style name:"
    ttk::entry $f.e -textvariable ::sty(newname) -width 24
    ttk::button $f.ok -text OK -command {set ::sty(asked) 1; destroy .sty.ask}
    ttk::button $f.no -text Cancel -command {set ::sty(asked) 0; destroy .sty.ask}
    grid $f.l $f.e -sticky w -pady 2
    grid $f.no $f.ok -sticky e -pady {8 0}
    bind $f.e <Return> {set ::sty(asked) 1; destroy .sty.ask}
    $f.e selection range 0 end
    focus $f.e
    set ::sty(asked) 0
    safegrab $w
    tkwait window $w
    safegrab .sty
    return [expr {$::sty(asked) ? [string trim $::sty(newname)] : ""}]
}
proc styledlg_delete {} {
    set n $::sty(sel)
    if {[string equal -nocase $n Standard]} {
        tk_messageBox -parent .sty -title "Text Style" -message "The Standard style can't be deleted."
        return
    }
    if {[string equal -nocase $n $::sty(current)]} {
        tk_messageBox -parent .sty -title "Text Style" -message "$n is the current style. Set another style current first."
        return
    }
    set i [lsearch -exact $::sty(order) $n]
    set ::sty(order) [lreplace $::sty(order) $i $i]
    unset ::stydata($n)
    set ::sty(sel) ""
    styledlg_fill
    styledlg_select [lindex $::sty(order) 0]
}
proc styledlg_done {ok} {
    if {$ok} {
        styledlg_store
        set recs [list $::sty(current)]
        foreach n $::sty(order) { lappend recs [join [concat [list $n] $::stydata($n)] \x1f] }
        set ::sty(result) [join $recs \x1e]
    }
    destroy .sty
}

# ---------------------------------------------------------------- dimension style manager
# records: list of {name {KEY value KEY value ...}}; current: style name;
# arrows: list of {code label} for DIMBLK.
# Returns "CANCEL" or  current \x1e rec \x1e rec ...  with rec = name \x1f KEY \x1f value \x1f ...
set ::dimfields {
    "Lines" {
        DIMCLRD "Dimension line color (0 ByBlock, 256 ByLayer)" entry
        DIMDLI  "Baseline spacing" entry
        DIMCLRE "Extension line color" entry
        DIMEXE  "Extend beyond dimension lines" entry
        DIMEXO  "Offset from origin" entry
    }
    "Symbols and Arrows" {
        DIMBLK  "Arrowheads" arrows
        DIMASZ  "Arrow size" entry
        DIMTSZ  "Tick size (overrides arrowheads when > 0)" entry
        DIMCEN  "Center mark size" entry
    }
    "Text" {
        DIMTXT  "Text height" entry
        DIMCLRT "Text color" entry
        DIMTAD  "Vertical placement" {0 Centered 1 Above}
        DIMGAP  "Offset from dimension line" entry
        DIMTIH  "Text alignment" {0 "Aligned with dimension line" 1 Horizontal}
    }
    "Fit" {
        DIMSCALE "Use overall scale of" entry
    }
    "Primary Units" {
        DIMDEC  "Precision (decimal places)" {0 0 1 0.0 2 0.00 3 0.000 4 0.0000 5 0.00000 6 0.000000}
        DIMLFAC "Measurement scale factor" entry
        DIMPOST "Prefix / suffix (<> is the measurement)" entry
        DIMADEC "Angular precision (decimal places)" {0 0 1 0.0 2 0.00 3 0.000 4 0.0000}
    }
}
proc dimdlg_choices {kind} {
    if {$kind eq "arrows"} {
        set out {}
        foreach a $::dimarrows { lappend out {*}$a }
        return $out
    }
    return $kind
}
proc dimstyledlg {records current arrows} {
    set w .dsm
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Dimension Style Manager"
    wm transient $w .
    set ::dimarrows $arrows
    array unset ::dsdata
    array unset ::dsv
    array unset ::dsl
    set ::ds(order) {}
    foreach r $records {
        lassign $r name vars
        lappend ::ds(order) $name
        set ::dsdata($name) $vars
    }
    set ::ds(current) $current
    set ::ds(sel) ""
    set f [ttk::frame $w.f -padding 10]
    pack $f -fill both -expand 1
    ttk::label $f.cur -text "Current dimension style: $current" -style Dim.TLabel
    grid $f.cur -row 0 -column 0 -columnspan 2 -sticky w -pady {0 6}
    # left: styles
    set left [ttk::frame $f.left]
    ttk::label $left.l -text "Styles:"
    listbox $left.list -height 14 -width 22 -exportselection 0 -background $::panel2 -foreground $::fg \
        -selectbackground $::accent -borderwidth 0 -highlightthickness 0 -activestyle none
    bind $left.list <<ListboxSelect>> {dimdlg_select [.dsm.f.left.list get [.dsm.f.left.list curselection]]}
    ttk::button $left.cur -text "Set Current" -command dimdlg_setcurrent
    ttk::button $left.new -text "New..." -command dimdlg_new
    ttk::button $left.del -text "Delete" -command dimdlg_delete
    pack $left.l -side top -anchor w
    pack $left.list -side top -fill y -expand 1
    pack $left.cur $left.new $left.del -side top -fill x -pady {4 0}
    grid $left -row 1 -column 0 -sticky ns -padx {0 12}
    # right: tabs + preview
    set right [ttk::frame $f.right]
    set nb [ttk::notebook $right.nb]
    set ti 0
    foreach {tab fields} $::dimfields {
        set p [ttk::frame $nb.p$ti -padding 10]
        $nb add $p -text $tab
        set row 0
        foreach {key label kind} $fields {
            ttk::label $p.l$key -text $label
            if {$kind eq "entry"} {
                ttk::entry $p.v$key -textvariable ::dsv($key) -width 14
                bind $p.v$key <KeyRelease> dimdlg_preview
            } else {
                set ch [dimdlg_choices $kind]
                set labels {}
                foreach {code lbl} $ch { lappend labels $lbl }
                ttk::combobox $p.v$key -textvariable ::dsl($key) -values $labels -state readonly -width 26
                bind $p.v$key <<ComboboxSelected>> [list dimdlg_combo $key $kind]
            }
            grid $p.l$key -row $row -column 0 -sticky w -pady 3 -padx {0 10}
            grid $p.v$key -row $row -column 1 -sticky w -pady 3
            incr row
        }
        incr ti
    }
    pack $nb -side top -fill both -expand 1
    canvas $right.pv -width 420 -height 150 -background "#171c23" -highlightthickness 0
    pack $right.pv -side top -fill x -pady {8 0}
    grid $right -row 1 -column 1 -sticky nsew
    set bb [ttk::frame $f.bb]
    ttk::button $bb.ok -text "OK" -command {dimdlg_done 1}
    ttk::button $bb.cancel -text "Cancel" -command {dimdlg_done 0}
    pack $bb.cancel $bb.ok -side right -padx 3
    grid $bb -row 2 -column 0 -columnspan 2 -sticky e -pady {10 0}
    bind $w <Escape> {dimdlg_done 0}
    dimdlg_fill
    dimdlg_select $current
    set ::ds(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 120}]+[expr {[winfo rooty .] + 60}]
    safegrab $w
    tkwait window $w
    return $::ds(result)
}
proc dimdlg_kind {key} {
    foreach {tab fields} $::dimfields { foreach {k label kind} $fields { if {$k eq $key} { return $kind } } }
    return entry
}
proc dimdlg_combo {key kind} {
    foreach {code lbl} [dimdlg_choices $kind] { if {$lbl eq $::dsl($key)} { set ::dsv($key) $code } }
    dimdlg_preview
}
proc dimdlg_fill {} {
    set l .dsm.f.left.list
    $l delete 0 end
    foreach n $::ds(order) { $l insert end $n }
}
proc dimdlg_store {} {
    set n $::ds(sel)
    if {$n eq "" || ![info exists ::dsdata($n)]} return
    set vars {}
    foreach {k v} $::dsdata($n) {
        if {[info exists ::dsv($k)]} { set v $::dsv($k) }
        lappend vars $k $v
    }
    set ::dsdata($n) $vars
}
proc dimdlg_select {name} {
    if {$name eq ""} return
    dimdlg_store
    set ::ds(sel) $name
    foreach {k v} $::dsdata($name) {
        set ::dsv($k) $v
        set kind [dimdlg_kind $k]
        if {$kind ne "entry"} {
            set ::dsl($k) $v
            foreach {code lbl} [dimdlg_choices $kind] { if {[string equal -nocase $code $v]} { set ::dsl($k) $lbl } }
        }
    }
    set i [lsearch -exact $::ds(order) $name]
    .dsm.f.left.list selection clear 0 end
    .dsm.f.left.list selection set $i
    .dsm.f.left.list see $i
    dimdlg_preview
}
proc dimdlg_num {key def} {
    if {[info exists ::dsv($key)] && [string is double -strict $::dsv($key)]} { return $::dsv($key) }
    return $def
}
proc dimdlg_arrow {c x y dir k code tsz} {
    # dir: +1 arrow points right (tip at x), -1 points left; k = pixels per unit
    set asz [expr {[dimdlg_num DIMASZ 0.18] * $k}]
    if {$tsz > 0} {
        set t [expr {$tsz * $k}]
        $c create line [expr {$x - $t}] [expr {$y + $t}] [expr {$x + $t}] [expr {$y - $t}] -fill white
        return
    }
    set b [expr {$x - $dir * $asz}]
    set h [expr {$asz / 6.0}]
    switch -- [string toupper $code] {
        ARCHTICK { set t [expr {$asz / 2.0 / 1.414}]
                   $c create line [expr {$x - $t}] [expr {$y + $t}] [expr {$x + $t}] [expr {$y - $t}] -fill white -width [expr {max(2, $asz * 0.16)}] }
        OBLIQUE  { set t [expr {$asz / 2.0 / 1.414}]
                   $c create line [expr {$x - $t}] [expr {$y + $t}] [expr {$x + $t}] [expr {$y - $t}] -fill white }
        NONE     {}
        DOT      { set r [expr {$asz / 4.0}]; $c create oval [expr {$x-$r}] [expr {$y-$r}] [expr {$x+$r}] [expr {$y+$r}] -fill white -outline white }
        DOTSMALL { set r [expr {$asz / 8.0}]; $c create oval [expr {$x-$r}] [expr {$y-$r}] [expr {$x+$r}] [expr {$y+$r}] -fill white -outline white }
        OPEN     { $c create line $b [expr {$y - $h}] $x $y $b [expr {$y + $h}] -fill white }
        OPEN90   { set b2 [expr {$x - $dir * $asz / 2.0}]; set h2 [expr {$asz / 2.0}]
                   $c create line $b2 [expr {$y - $h2}] $x $y $b2 [expr {$y + $h2}] -fill white }
        CLOSEDBLANK { $c create polygon $x $y $b [expr {$y - $h}] $b [expr {$y + $h}] -fill "" -outline white }
        default  { $c create polygon $x $y $b [expr {$y - $h}] $b [expr {$y + $h}] -fill white -outline white }
    }
}
proc dimdlg_preview {} {
    set c .dsm.f.right.pv
    if {![winfo exists $c]} return
    $c delete all
    set sc [dimdlg_num DIMSCALE 1]
    # the sample: a 1.2-unit wide object, drawn so the arrow size looks as it would on paper
    set k 100.0
    set x1 90; set x2 [expr {$x1 + 2.4 * $k}]; set ybase 138
    $c create line $x1 $ybase $x1 [expr {$ybase - 20}] -fill "#5aa8ff" -width 2
    $c create line $x2 $ybase $x2 [expr {$ybase - 20}] -fill "#5aa8ff" -width 2
    $c create line $x1 $ybase $x2 $ybase -fill "#5aa8ff" -width 2
    set ks [expr {$k * $sc}]
    set exo [expr {[dimdlg_num DIMEXO 0.0625] * $ks}]
    set exe [expr {[dimdlg_num DIMEXE 0.18] * $ks}]
    set dimy [expr {$ybase - 20 - 0.55 * $k}]
    $c create line $x1 [expr {$ybase - 20 - $exo}] $x1 [expr {$dimy - $exe}] -fill white
    $c create line $x2 [expr {$ybase - 20 - $exo}] $x2 [expr {$dimy - $exe}] -fill white
    $c create line $x1 $dimy $x2 $dimy -fill white
    set code [expr {[info exists ::dsv(DIMBLK)] ? $::dsv(DIMBLK) : ""}]
    set tsz [dimdlg_num DIMTSZ 0]
    dimdlg_arrow $c $x1 $dimy -1 $ks $code $tsz
    dimdlg_arrow $c $x2 $dimy 1 $ks $code $tsz
    set txt [expr {max(6, round([dimdlg_num DIMTXT 0.18] * $ks / 0.72))}]
    set dec [expr {int([dimdlg_num DIMDEC 4])}]
    set val [format "%.${dec}f" [expr {2.4 * [dimdlg_num DIMLFAC 1]}]]
    set post [expr {[info exists ::dsv(DIMPOST)] ? $::dsv(DIMPOST) : ""}]
    if {[string first "<>" $post] >= 0} { set val [string map [list "<>" $val] $post] } elseif {$post ne ""} { set val $post }
    set gap [expr {[dimdlg_num DIMGAP 0.09] * $ks}]
    set above [expr {[dimdlg_num DIMTAD 1] != 0}]
    set ty [expr {$above ? $dimy - $gap : $dimy}]
    set anchor [expr {$above ? "s" : "center"}]
    if {!$above} {
        set tw [expr {[font measure [list Helvetica -$txt] $val] / 2 + $gap}]
        $c create rectangle [expr {($x1+$x2)/2 - $tw}] [expr {$dimy - 3}] [expr {($x1+$x2)/2 + $tw}] [expr {$dimy + 3}] -fill "#171c23" -outline ""
    }
    $c create text [expr {($x1 + $x2) / 2}] $ty -text $val -anchor $anchor -font [list Helvetica -$txt] -fill white
}
proc dimdlg_setcurrent {} {
    set ::ds(current) $::ds(sel)
    .dsm.f.cur configure -text "Current dimension style: $::ds(current)"
}
proc dimdlg_new {} {
    set w .dsm.ask
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Create New Dimension Style"
    wm transient $w .dsm
    set f [ttk::frame $w.f -padding 10]
    pack $f
    set ::ds(newname) "Copy of $::ds(sel)"
    set ::ds(startwith) $::ds(sel)
    ttk::label $f.l -text "New style name:"
    ttk::entry $f.e -textvariable ::ds(newname) -width 26
    ttk::label $f.l2 -text "Start with:"
    ttk::combobox $f.c -textvariable ::ds(startwith) -values $::ds(order) -state readonly -width 24
    ttk::button $f.ok -text Continue -command {set ::ds(asked) 1; destroy .dsm.ask}
    ttk::button $f.no -text Cancel -command {set ::ds(asked) 0; destroy .dsm.ask}
    grid $f.l $f.e -sticky w -pady 2
    grid $f.l2 $f.c -sticky w -pady 2
    grid $f.no $f.ok -sticky e -pady {8 0}
    bind $f.e <Return> {set ::ds(asked) 1; destroy .dsm.ask}
    $f.e selection range 0 end
    focus $f.e
    set ::ds(asked) 0
    safegrab $w
    tkwait window $w
    safegrab .dsm
    set name [string trim $::ds(newname)]
    if {!$::ds(asked) || $name eq ""} return
    if {[lsearch -exact -nocase $::ds(order) $name] >= 0} {
        tk_messageBox -parent .dsm -title "Dimension Style" -message "A style called $name already exists."
        return
    }
    dimdlg_store
    set ::dsdata($name) $::dsdata($::ds(startwith))
    lappend ::ds(order) $name
    dimdlg_fill
    set ::ds(sel) ""
    dimdlg_select $name
}
proc dimdlg_delete {} {
    set n $::ds(sel)
    if {[string equal -nocase $n Standard]} {
        tk_messageBox -parent .dsm -title "Dimension Style" -message "The Standard style can't be deleted."
        return
    }
    if {[string equal -nocase $n $::ds(current)]} {
        tk_messageBox -parent .dsm -title "Dimension Style" -message "$n is the current style. Set another style current first."
        return
    }
    set i [lsearch -exact $::ds(order) $n]
    set ::ds(order) [lreplace $::ds(order) $i $i]
    unset ::dsdata($n)
    set ::ds(sel) ""
    dimdlg_fill
    dimdlg_select [lindex $::ds(order) 0]
}
proc dimdlg_done {ok} {
    if {$ok} {
        dimdlg_store
        set recs [list $::ds(current)]
        foreach n $::ds(order) { lappend recs [join [concat [list $n] $::dsdata($n)] \x1f] }
        set ::ds(result) [join $recs \x1e]
    }
    destroy .dsm
}

# ---------------------------------------------------------------- object snap settings (OSNAP / DSETTINGS)
# mode: OSMODE value (bit 16384 = snaps turned off with F3). Returns the new value, or "CANCEL".
set ::osnapmodes {
    1   Endpoint      end
    2   Midpoint      mid
    4   Center        cen
    8   Node          nod
    16  Quadrant      qua
    32  Intersection  int
    64  Insertion     ins
    128 Perpendicular per
    256 Tangent       tan
    512 Nearest       nea
}
proc osnap_glyph {c kind} {
    # the same markers the drawing shows while snapping
    set g "#3fd25f"
    switch $kind {
        end { $c create rectangle 3 3 15 15 -outline $g -width 2 }
        mid { $c create polygon 2 15 16 15 9 3 -outline $g -fill {} -width 2 }
        cen - nod { $c create oval 3 3 15 15 -outline $g -width 2 }
        qua { $c create polygon 9 2 16 9 9 16 2 9 -outline $g -fill {} -width 2 }
        int { $c create line 3 3 15 15 -fill $g -width 2; $c create line 3 15 15 3 -fill $g -width 2 }
        ins { $c create rectangle 3 3 15 15 -outline $g -width 2; $c create line 3 9 15 9 -fill $g }
        per { $c create line 3 3 3 15 15 15 -fill $g -width 2; $c create line 3 9 9 9 9 15 -fill $g }
        tan { $c create oval 3 3 15 15 -outline $g -width 2; $c create line 2 3 16 3 -fill $g -width 2 }
        nea { $c create line 3 3 15 15 3 15 15 3 3 3 -fill $g -width 2 }
    }
}
proc osnapdlg {mode} {
    set w .osd
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Drafting Settings - Object Snap"
    wm transient $w .
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    set ::osd(on) [expr {($mode & 16384) == 0}]
    ttk::checkbutton $f.on -text "Object Snap On (F3)" -variable ::osd(on)
    grid $f.on -row 0 -column 0 -columnspan 4 -sticky w -pady {0 8}
    ttk::label $f.hdr -text "Object snap modes" -style Dim.TLabel
    grid $f.hdr -row 1 -column 0 -columnspan 4 -sticky w
    set i 0
    foreach {bit label kind} $::osnapmodes {
        set ::osd($bit) [expr {($mode & $bit) != 0}]
        set row [expr {2 + $i % 5}]
        set col [expr {2 * ($i / 5)}]
        canvas $f.g$bit -width 18 -height 18 -background $::panel -highlightthickness 0
        osnap_glyph $f.g$bit $kind
        ttk::checkbutton $f.c$bit -text $label -variable ::osd($bit)
        grid $f.g$bit -row $row -column $col -padx {0 4} -pady 3
        grid $f.c$bit -row $row -column [expr {$col + 1}] -sticky w -padx {0 30} -pady 3
        incr i
    }
    set bb [ttk::frame $f.sel]
    ttk::button $bb.all -text "Select All" -command {foreach {b l k} $::osnapmodes { set ::osd($b) 1 }}
    ttk::button $bb.none -text "Clear All" -command {foreach {b l k} $::osnapmodes { set ::osd($b) 0 }}
    pack $bb.all $bb.none -side top -fill x -pady 2
    grid $bb -row 2 -column 4 -rowspan 3 -sticky n -padx {10 0}
    ttk::label $f.tip -style Dim.TLabel -wraplength 420 -justify left \
        -text "Tip: hold Shift and right-click while picking a point for a one-time snap, or type END, MID, CEN, QUA, INT, PER, TAN, NEA at the prompt."
    grid $f.tip -row 7 -column 0 -columnspan 5 -sticky w -pady {10 0}
    set b2 [ttk::frame $f.b2]
    ttk::button $b2.ok -text OK -command {osnapdlg_done 1}
    ttk::button $b2.no -text Cancel -command {osnapdlg_done 0}
    pack $b2.no $b2.ok -side right -padx 3
    grid $b2 -row 8 -column 0 -columnspan 5 -sticky e -pady {10 0}
    bind $w <Escape> {osnapdlg_done 0}
    bind $w <Return> {osnapdlg_done 1}
    set ::osd(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::osd(result)
}
proc osnapdlg_done {ok} {
    if {$ok} {
        set m 0
        foreach {bit label kind} $::osnapmodes { if {$::osd($bit)} { set m [expr {$m | $bit}] } }
        if {!$::osd(on)} { set m [expr {$m | 16384}] }
        set ::osd(result) $m
    }
    destroy .osd
}

# ---------------------------------------------------------------- Match Properties settings
# items: list of {key label on}. Returns "CANCEL" or the keys switched on, space separated.
proc matchpropdlg {items} {
    set w .mpd
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Property Settings"
    wm transient $w .
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    set ::mpd(keys) {}
    ttk::label $f.h1 -text "Basic Properties" -style Dim.TLabel
    ttk::label $f.h2 -text "Special Properties" -style Dim.TLabel
    grid $f.h1 -row 0 -column 0 -sticky w -pady {0 4}
    grid $f.h2 -row 0 -column 1 -sticky w -pady {0 4} -padx {30 0}
    set basic {color layer ltype ltscale lineweight thickness}
    set rb 1; set rs 1
    foreach it $items {
        lassign $it key label on
        lappend ::mpd(keys) $key
        set ::mpd($key) $on
        ttk::checkbutton $f.c$key -text $label -variable ::mpd($key)
        if {$key in $basic} {
            grid $f.c$key -row $rb -column 0 -sticky w -pady 2; incr rb
        } else {
            grid $f.c$key -row $rs -column 1 -sticky w -pady 2 -padx {30 0}; incr rs
        }
    }
    set n [expr {max($rb, $rs)}]
    ttk::label $f.tip -style Dim.TLabel -wraplength 380 -justify left \
        -text "Special properties copy only between objects of the same kind: text style, height, width and oblique angle; dimension style; hatch pattern, scale and angle; polyline width."
    grid $f.tip -row $n -column 0 -columnspan 2 -sticky w -pady {10 0}
    set b [ttk::frame $f.b]
    ttk::button $b.all -text "Select All" -command {foreach k $::mpd(keys) { set ::mpd($k) 1 }}
    ttk::button $b.none -text "Clear All" -command {foreach k $::mpd(keys) { set ::mpd($k) 0 }}
    ttk::button $b.ok -text OK -command {matchpropdlg_done 1}
    ttk::button $b.no -text Cancel -command {matchpropdlg_done 0}
    pack $b.all $b.none -side left -padx 3
    pack $b.no $b.ok -side right -padx 3
    grid $b -row [expr {$n + 1}] -column 0 -columnspan 2 -sticky ew -pady {12 0}
    bind $w <Escape> {matchpropdlg_done 0}
    bind $w <Return> {matchpropdlg_done 1}
    set ::mpd(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::mpd(result)
}
proc matchpropdlg_done {ok} {
    if {$ok} {
        set on {}
        foreach k $::mpd(keys) { if {$::mpd($k)} { lappend on $k } }
        set ::mpd(result) [concat OK $on]
    }
    destroy .mpd
}

# ---------------------------------------------------------------- Drawing Units
# ltypes/atypes/instypes: lists of "code Label"; lprec/aprec: dict code -> 9 precision labels;
# lsamp/asamp: dict "code,prec" -> sample text. Returns "CANCEL" or "OK lu lp au ap ins".
proc unitsdlg {ltypes atypes instypes lprec aprec lsamp asamp lu lp au ap ins} {
    set w .ud
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Drawing Units"
    wm transient $w .
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    array unset ::ud
    foreach {k v} [list ltypes $ltypes atypes $atypes instypes $instypes lprec $lprec aprec $aprec lsamp $lsamp asamp $asamp] { set ::ud($k) $v }
    proc ud_label {list code} { foreach it $list { if {[lindex $it 0] == $code} { return [lrange $it 1 end] } }; return "" }
    proc ud_code {list label} { foreach it $list { if {[lrange $it 1 end] eq $label} { return [lindex $it 0] } }; return 0 }
    set lab {}; foreach it $ltypes { lappend lab [lrange $it 1 end] }
    set aab {}; foreach it $atypes { lappend aab [lrange $it 1 end] }
    set iab {}; foreach it $instypes { lappend iab [lrange $it 1 end] }
    ttk::labelframe $f.len -text " Length " -padding 8
    ttk::label $f.len.tl -text "Type:"
    ttk::combobox $f.len.t -state readonly -width 18 -values $lab
    ttk::label $f.len.pl -text "Precision:"
    ttk::combobox $f.len.p -state readonly -width 18
    grid $f.len.tl -row 0 -column 0 -sticky w; grid $f.len.t -row 1 -column 0 -sticky ew -pady {0 6}
    grid $f.len.pl -row 2 -column 0 -sticky w; grid $f.len.p -row 3 -column 0 -sticky ew
    ttk::labelframe $f.ang -text " Angle " -padding 8
    ttk::label $f.ang.tl -text "Type:"
    ttk::combobox $f.ang.t -state readonly -width 18 -values $aab
    ttk::label $f.ang.pl -text "Precision:"
    ttk::combobox $f.ang.p -state readonly -width 18
    ttk::label $f.ang.cw -style Dim.TLabel -text "Angles measured counterclockwise from East (0)."
    grid $f.ang.tl -row 0 -column 0 -sticky w; grid $f.ang.t -row 1 -column 0 -sticky ew -pady {0 6}
    grid $f.ang.pl -row 2 -column 0 -sticky w; grid $f.ang.p -row 3 -column 0 -sticky ew
    grid $f.ang.cw -row 4 -column 0 -sticky w -pady {6 0}
    grid $f.len -row 0 -column 0 -sticky nsew -padx {0 8}
    grid $f.ang -row 0 -column 1 -sticky nsew
    ttk::labelframe $f.ins -text " Insertion scale " -padding 8
    ttk::label $f.ins.l -text "Units to scale inserted content:"
    ttk::combobox $f.ins.c -state readonly -width 18 -values $iab
    grid $f.ins.l -row 0 -column 0 -sticky w; grid $f.ins.c -row 1 -column 0 -sticky w
    grid $f.ins -row 1 -column 0 -columnspan 2 -sticky ew -pady {10 0}
    ttk::labelframe $f.smp -text " Sample output " -padding 8
    ttk::label $f.smp.l -textvariable ::ud(sample) -font {Courier 11}
    ttk::label $f.smp.a -textvariable ::ud(asample) -font {Courier 11}
    pack $f.smp.l $f.smp.a -anchor w
    grid $f.smp -row 2 -column 0 -columnspan 2 -sticky ew -pady {10 0}
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text OK -command {ud_done 1}
    ttk::button $b.no -text Cancel -command {ud_done 0}
    pack $b.no $b.ok -side right -padx 3
    grid $b -row 3 -column 0 -columnspan 2 -sticky e -pady {12 0}
    $f.len.t set [ud_label $ltypes $lu]
    $f.ang.t set [ud_label $atypes $au]
    $f.ins.c set [ud_label $instypes $ins]
    set ::ud(lp) $lp; set ::ud(ap) $ap
    ud_refresh
    bind $f.len.t <<ComboboxSelected>> ud_refresh
    bind $f.ang.t <<ComboboxSelected>> ud_refresh
    bind $f.len.p <<ComboboxSelected>> {set ::ud(lp) [.ud.f.len.p current]; ud_refresh}
    bind $f.ang.p <<ComboboxSelected>> {set ::ud(ap) [.ud.f.ang.p current]; ud_refresh}
    bind $w <Escape> {ud_done 0}
    bind $w <Return> {ud_done 1}
    set ::ud(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::ud(result)
}
proc ud_refresh {} {
    set f .ud.f
    set lu [ud_code $::ud(ltypes) [$f.len.t get]]
    set au [ud_code $::ud(atypes) [$f.ang.t get]]
    $f.len.p configure -values [dict get $::ud(lprec) $lu]
    $f.ang.p configure -values [dict get $::ud(aprec) $au]
    $f.len.p current $::ud(lp)
    $f.ang.p current $::ud(ap)
    set ::ud(sample) [dict get $::ud(lsamp) "$lu,$::ud(lp)"]
    set ::ud(asample) [dict get $::ud(asamp) "$au,$::ud(ap)"]
}
proc ud_done {ok} {
    if {$ok} {
        set f .ud.f
        set ::ud(result) [list OK [ud_code $::ud(ltypes) [$f.len.t get]] $::ud(lp) \
                              [ud_code $::ud(atypes) [$f.ang.t get]] $::ud(ap) [ud_code $::ud(instypes) [$f.ins.c get]]]
        set ::ud(result) [join $::ud(result) " "]
    }
    destroy .ud
}

# ---------------------------------------------------------------- Block Definition (BLOCK)
# Returns ACTION \x1f name \x1f x \x1f y \x1f mode, ACTION = OK / PICK / SELECT / CANCEL.
proc blockdlg {names name x y count mode} {
    set w .bd
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Block Definition"
    wm transient $w .
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    set ::bd(mode) $mode
    set ::bd(result) CANCEL
    ttk::label $f.nl -text "Name:"
    ttk::combobox $f.n -values $names -width 32
    $f.n set $name
    grid $f.nl -row 0 -column 0 -sticky w
    grid $f.n -row 1 -column 0 -columnspan 2 -sticky ew -pady {0 10}
    ttk::labelframe $f.bp -text " Base point " -padding 8
    ttk::button $f.bp.pick -text "Pick point" -command {bd_done PICK}
    ttk::label $f.bp.xl -text "X:"; ttk::entry $f.bp.x -width 14; $f.bp.x insert 0 $x
    ttk::label $f.bp.yl -text "Y:"; ttk::entry $f.bp.y -width 14; $f.bp.y insert 0 $y
    grid $f.bp.pick -row 0 -column 0 -columnspan 2 -sticky ew -pady {0 8}
    grid $f.bp.xl -row 1 -column 0 -sticky w; grid $f.bp.x -row 1 -column 1 -sticky ew -pady 2
    grid $f.bp.yl -row 2 -column 0 -sticky w; grid $f.bp.y -row 2 -column 1 -sticky ew -pady 2
    ttk::labelframe $f.ob -text " Objects " -padding 8
    ttk::button $f.ob.sel -text "Select objects" -command {bd_done SELECT}
    ttk::radiobutton $f.ob.r -text "Retain" -value Retain -variable ::bd(mode)
    ttk::radiobutton $f.ob.c -text "Convert to block" -value Convert -variable ::bd(mode)
    ttk::radiobutton $f.ob.d -text "Delete" -value Delete -variable ::bd(mode)
    ttk::label $f.ob.n -text [expr {$count ? "$count object[expr {$count == 1 ? {} : {s}}] selected" : "No objects selected"}] \
        -foreground [expr {$count ? "#9fd39f" : "#ffb070"}]
    grid $f.ob.sel -row 0 -column 0 -sticky ew -pady {0 8}
    grid $f.ob.r -row 1 -column 0 -sticky w
    grid $f.ob.c -row 2 -column 0 -sticky w
    grid $f.ob.d -row 3 -column 0 -sticky w
    grid $f.ob.n -row 4 -column 0 -sticky w -pady {8 0}
    grid $f.bp -row 2 -column 0 -sticky nsew -padx {0 8}
    grid $f.ob -row 2 -column 1 -sticky nsew
    ttk::label $f.tip -style Dim.TLabel -wraplength 420 -justify left \
        -text "Convert replaces the objects with one copy of the block at the base point. Insert more copies with INSERT."
    grid $f.tip -row 3 -column 0 -columnspan 2 -sticky w -pady {10 0}
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text OK -command {bd_done OK}
    ttk::button $b.no -text Cancel -command {bd_done CANCEL}
    pack $b.no $b.ok -side right -padx 3
    grid $b -row 4 -column 0 -columnspan 2 -sticky e -pady {12 0}
    bind $w <Escape> {bd_done CANCEL}
    bind $w <Return> {bd_done OK}
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    focus $f.n
    tkwait window $w
    return $::bd(result)
}
proc bd_done {act} {
    set f .bd.f
    set ::bd(result) [join [list $act [$f.n get] [$f.bp.x get] [$f.bp.y get] $::bd(mode)] "\x1f"]
    destroy .bd
}

# ---------------------------------------------------------------- Cursor & Selection settings (OPTIONS)
# colors: list of {name #rrggbb}. Returns "CANCEL" or "OK size color pickbox aperture snapsize gripsize rollover".
proc cursordlg {size color pickbox aperture snapsize gripsize rollover colors {bg "#212830"} {bgs {}}} {
    set w .cud
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Options - Cursor & Selection"
    wm transient $w .
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    array unset ::cud
    set ::cud(size) $size; set ::cud(pick) $pickbox; set ::cud(ap) $aperture
    set ::cud(snap) $snapsize; set ::cud(grip) $gripsize; set ::cud(roll) $rollover
    set ::cud(color) $color; set ::cud(colors) $colors
    set ::cud(bg) $bg; set ::cud(bgs) $bgs
    ttk::labelframe $f.ch -text " Crosshairs " -padding 8
    ttk::label $f.ch.sl -text "Size (% of the screen):"
    ttk::scale $f.ch.s -from 1 -to 100 -orient horizontal -length 200 -variable ::cud(size) \
        -command {apply {{v} {set ::cud(size) [expr {round($v)}]; cud_preview}}}
    ttk::spinbox $f.ch.e -from 1 -to 100 -width 5 -textvariable ::cud(size) -command cud_preview
    ttk::label $f.ch.cl -text "Color:"
    set names {}; set cur ""
    foreach c $colors { lappend names [lindex $c 0]; if {[string equal -nocase [lindex $c 1] $color]} { set cur [lindex $c 0] } }
    ttk::combobox $f.ch.c -state readonly -width 14 -values $names
    if {$cur ne ""} { $f.ch.c set $cur } else { $f.ch.c set [lindex $names 0] }
    bind $f.ch.c <<ComboboxSelected>> cud_preview
    grid $f.ch.sl -row 0 -column 0 -columnspan 2 -sticky w
    grid $f.ch.s -row 1 -column 0 -sticky ew -padx {0 8}; grid $f.ch.e -row 1 -column 1 -sticky w
    grid $f.ch.cl -row 2 -column 0 -columnspan 2 -sticky w -pady {8 0}
    grid $f.ch.c -row 3 -column 0 -sticky w
    ttk::labelframe $f.sz -text " Box sizes (pixels) " -padding 8
    set r 0
    foreach {key label lo hi} {pick "Pickbox (selecting objects)" 0 50 ap "Snap target (aperture)" 1 50 \
                               snap "Snap marker" 1 20 grip "Grips" 1 20} {
        ttk::label $f.sz.l$key -text $label
        ttk::spinbox $f.sz.e$key -from $lo -to $hi -width 5 -textvariable ::cud($key) -command cud_preview
        grid $f.sz.l$key -row $r -column 0 -sticky w -pady 2 -padx {0 10}
        grid $f.sz.e$key -row $r -column 1 -sticky w -pady 2
        incr r
    }
    canvas $f.pv -width 150 -height 150 -background $bg -highlightthickness 1 -highlightbackground "#3a4049"
    ttk::labelframe $f.bgf -text " Drawing background " -padding 8
    set bnames {}; set bcur ""
    foreach c $bgs { lappend bnames [lindex $c 0]; if {[string equal -nocase [lindex $c 1] $bg]} { set bcur [lindex $c 0] } }
    lappend bnames "Custom..."
    ttk::combobox $f.bgf.c -state readonly -width 20 -values $bnames
    $f.bgf.c set [expr {$bcur ne "" ? $bcur : "Custom..."}]
    canvas $f.bgf.sw -width 40 -height 20 -highlightthickness 1 -highlightbackground "#5a626e" -background $bg
    # black-to-white slide: any shade of gray
    lassign [winfo rgb . $bg] r0 g0 b0
    set ::cud(gray) [expr {round((0.299*$r0 + 0.587*$g0 + 0.114*$b0) / 257.0)}]
    ttk::frame $f.bgf.s
    ttk::label $f.bgf.s.lo -text "Black" -style Dim.TLabel
    ttk::scale $f.bgf.s.sc -from 0 -to 255 -orient horizontal -length 220 -variable ::cud(gray) -command cud_bggray
    ttk::label $f.bgf.s.hi -text "White" -style Dim.TLabel
    pack $f.bgf.s.lo -side left
    pack $f.bgf.s.sc -side left -padx 4 -fill x -expand 1
    pack $f.bgf.s.hi -side left
    grid $f.bgf.c -row 0 -column 0 -sticky w -padx 3
    grid $f.bgf.sw -row 0 -column 1 -sticky w -padx 3
    grid $f.bgf.s -row 1 -column 0 -columnspan 2 -sticky ew -pady {6 0}
    bind $f.bgf.c <<ComboboxSelected>> cud_bgpick
    ttk::checkbutton $f.roll -text "Show object sizes when the cursor rests on an object (rollover tips)" -variable ::cud(roll)
    grid $f.ch -row 0 -column 0 -sticky nsew -padx {0 8}
    grid $f.pv -row 0 -column 1 -rowspan 2 -sticky n
    grid $f.sz -row 1 -column 0 -sticky nsew -pady {8 0} -padx {0 8}
    grid $f.bgf -row 2 -column 0 -columnspan 2 -sticky ew -pady {8 0}
    grid $f.roll -row 3 -column 0 -columnspan 2 -sticky w -pady {10 0}
    set b [ttk::frame $f.b]
    ttk::button $b.def -text "Defaults" -command {
        set ::cud(size) 100; set ::cud(pick) 5; set ::cud(ap) 10; set ::cud(snap) 6; set ::cud(grip) 5; set ::cud(roll) 1
        set ::cud(bg) "#212830"; .cud.f.bgf.c set [lindex [lindex $::cud(bgs) 0] 0]
        .cud.f.bgf.sw configure -background $::cud(bg); .cud.f.pv configure -background $::cud(bg)
        .cud.f.ch.c set [lindex [lindex $::cud(colors) 0] 0]; cud_preview }
    ttk::button $b.ok -text OK -command {cud_done 1}
    ttk::button $b.no -text Cancel -command {cud_done 0}
    pack $b.def -side left
    pack $b.no $b.ok -side right -padx 3
    grid $b -row 4 -column 0 -columnspan 2 -sticky ew -pady {12 0}
    foreach key {size pick ap snap grip} { trace add variable ::cud($key) write {apply {args cud_preview}} }
    cud_preview
    bind $w <Escape> {cud_done 0}
    set ::cud(result) CANCEL
    wm geometry $w +[expr {[winfo rootx .] + 200}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    foreach key {size pick ap snap grip} { catch {trace remove variable ::cud($key) write {apply {args cud_preview}}} }
    return $::cud(result)
}
proc cud_bggray {v} {
    set g [expr {max(0, min(255, round($v)))}]
    set ::cud(bg) [format "#%02x%02x%02x" $g $g $g]
    .cud.f.bgf.c set [expr {$g == 0 ? "Black" : ($g == 255 ? "White" : "Custom...")}]
    .cud.f.bgf.sw configure -background $::cud(bg)
    .cud.f.pv configure -background $::cud(bg)
    cud_preview
}
proc cud_bgpick {} {
    set n [.cud.f.bgf.c get]
    if {$n eq "Custom..."} {
        set c [tk_chooseColor -parent .cud -initialcolor $::cud(bg) -title "Drawing background"]
        if {$c eq ""} return
        set ::cud(bg) $c
    } else {
        foreach c $::cud(bgs) { if {[lindex $c 0] eq $n} { set ::cud(bg) [lindex $c 1] } }
    }
    .cud.f.bgf.sw configure -background $::cud(bg)
    .cud.f.pv configure -background $::cud(bg)
    lassign [winfo rgb . $::cud(bg)] r0 g0 b0
    set ::cud(gray) [expr {round((0.299*$r0 + 0.587*$g0 + 0.114*$b0) / 257.0)}]
    cud_preview
}
proc cud_color {} {
    set n [.cud.f.ch.c get]
    foreach c $::cud(colors) { if {[lindex $c 0] eq $n} { return [lindex $c 1] } }
    return "#c9ced6"
}
proc cud_int {key lo hi} {
    if {![string is integer -strict $::cud($key)]} { return $lo }
    return [expr {max($lo, min($hi, $::cud($key)))}]
}
proc cud_preview {} {
    set c .cud.f.pv
    if {![winfo exists $c]} return
    $c delete all
    set col [cud_color]
    # as on the drawing: a light crosshair on a light background is shown dark
    lassign [winfo rgb . $::cud(bg)] br bgg bb
    lassign [winfo rgb . $col] cr cg cb
    set blum [expr {(0.299*$br + 0.587*$bgg + 0.114*$bb) / 257.0}]
    set clum [expr {(0.299*$cr + 0.587*$cg + 0.114*$cb) / 257.0}]
    if {$blum > 140 && $clum > 170} { set col "#30363d" } elseif {$blum <= 140 && $clum < 60} { set col "#c9ced6" }
    set s [cud_int size 1 100]; set pb [cud_int pick 0 50]; set sn [cud_int snap 1 20]; set gp [cud_int grip 1 20]
    # the preview box stands for the screen: 100% reaches its edges
    set len [expr {$s >= 100 ? 100 : max($pb + 3, round($s / 100.0 * 75))}]
    set x 75; set y 75
    $c create line [expr {$x - $len}] $y [expr {$x - $pb}] $y -fill $col
    $c create line [expr {$x + $pb}] $y [expr {$x + $len}] $y -fill $col
    $c create line $x [expr {$y - $len}] $x [expr {$y - $pb}] -fill $col
    $c create line $x [expr {$y + $pb}] $x [expr {$y + $len}] -fill $col
    if {$pb > 0} { $c create rectangle [expr {$x - $pb}] [expr {$y - $pb}] [expr {$x + $pb}] [expr {$y + $pb}] -outline $col }
    $c create rectangle [expr {22 - $sn}] [expr {128 - $sn}] [expr {22 + $sn}] [expr {128 + $sn}] -outline "#3fd25f" -width 2
    $c create rectangle [expr {128 - $gp}] [expr {128 - $gp}] [expr {128 + $gp}] [expr {128 + $gp}] -fill "#3b7dd8" -outline "#3b7dd8"
    $c create text 22 [expr {110 - $sn}] -text "snap" -fill "#8a929c" -font {Helvetica 8}
    $c create text 128 [expr {110 - $gp}] -text "grip" -fill "#8a929c" -font {Helvetica 8}
}
proc cud_done {ok} {
    if {$ok} {
        set ::cud(result) [join [list OK [cud_int size 1 100] [cud_color] [cud_int pick 0 50] [cud_int ap 1 50] \
                                     [cud_int snap 1 20] [cud_int grip 1 20] [expr {$::cud(roll) ? 1 : 0}] $::cud(bg)] " "]
    }
    destroy .cud
}

# ---------------------------------------------------------------- Select Color (AutoCAD Color Index)
# hex: the 256 colors (index 0..255). Returns "CANCEL" or "OK n" / "OK BYLAYER" / "OK BYBLOCK".
proc colordlg {current by hex} {
    set w .cold
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Select Color"
    wm transient $w .
    wm resizable $w 0 0
    set ::cold(hex) $hex
    set ::cold(result) CANCEL
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    ttk::label $f.h -text "AutoCAD Color Index (ACI)" -style Dim.TLabel
    pack $f.h -anchor w
    set s 15
    set c [canvas $f.c -width [expr {24 * $s + 2}] -height [expr {10 * $s + 2 + 2 * ($s + 8) + 10}] \
               -background $::panel -highlightthickness 0]
    pack $c -pady {4 6}
    # the big grid: 24 hues across; lighter shades (even) on top, darker (odd) below, as in AutoCAD
    set rows {0 2 4 6 8 9 7 5 3 1}
    for {set col 0} {$col < 24} {incr col} {
        for {set r 0} {$r < 10} {incr r} {
            set i [expr {10 + $col * 10 + [lindex $rows $r]}]
            set x [expr {1 + $col * $s}]; set y [expr {1 + $r * $s}]
            $c create rectangle $x $y [expr {$x + $s - 1}] [expr {$y + $s - 1}] \
                -fill [lindex $hex $i] -outline "#1b1f24" -tags [list sw c$i]
            $c bind c$i <Enter> [list cold_hover $i]
            $c bind c$i <Button-1> [list cold_pick $i]
            $c bind c$i <Double-Button-1> [list cold_pick $i 1]
        }
    }
    # the standard colors 1-9 and the grays 250-255
    set y0 [expr {10 * $s + 10}]
    set k 0
    foreach i {1 2 3 4 5 6 7 8 9} {
        set x [expr {1 + $k * ($s + 6)}]
        $c create rectangle $x $y0 [expr {$x + $s + 4}] [expr {$y0 + $s + 4}] -fill [lindex $hex $i] -outline "#1b1f24" -tags [list sw c$i]
        $c bind c$i <Enter> [list cold_hover $i]; $c bind c$i <Button-1> [list cold_pick $i]
        $c bind c$i <Double-Button-1> [list cold_pick $i 1]
        incr k
    }
    set k 0
    foreach i {250 251 252 253 254 255} {
        set x [expr {24 * $s - (6 - $k) * ($s + 2)}]
        $c create rectangle $x $y0 [expr {$x + $s}] [expr {$y0 + $s + 4}] -fill [lindex $hex $i] -outline "#1b1f24" -tags [list sw c$i]
        $c bind c$i <Enter> [list cold_hover $i]; $c bind c$i <Button-1> [list cold_pick $i]
        $c bind c$i <Double-Button-1> [list cold_pick $i 1]
        incr k
    }
    $c create rectangle 0 0 0 0 -outline white -width 2 -tags selbox
    set g [ttk::frame $f.g]
    ttk::label $g.hl -text "Index color:" -style Dim.TLabel
    ttk::label $g.hv -textvariable ::cold(hover) -width 6
    pack $g.hl $g.hv -side left
    if {$by} {
        ttk::button $g.bl -text "ByLayer" -command {cold_pick BYLAYER 1}
        ttk::button $g.bb -text "ByBlock" -command {cold_pick BYBLOCK 1}
        pack $g.bb $g.bl -side right -padx 2
    }
    pack $g -fill x
    set b [ttk::frame $f.b]
    ttk::label $b.cl -text "Color:"
    ttk::entry $b.e -width 10 -textvariable ::cold(value)
    canvas $b.pv -width 44 -height 22 -highlightthickness 1 -highlightbackground "#5a626e" -background $::panel
    ttk::button $b.ok -text OK -command {cold_done 1}
    ttk::button $b.no -text Cancel -command {cold_done 0}
    pack $b.cl $b.e $b.pv -side left -padx 3
    pack $b.no $b.ok -side right -padx 3
    pack $b -fill x -pady {10 0}
    bind $b.e <KeyRelease> cold_typed
    bind $w <Escape> {cold_done 0}
    bind $w <Return> {cold_done 1}
    cold_pick [expr {$current == 256 ? "BYLAYER" : ($current == 0 ? "BYBLOCK" : $current)}]
    wm geometry $w +[expr {[winfo rootx .] + 220}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::cold(result)
}
proc cold_name {i} {
    set names {1 Red 2 Yellow 3 Green 4 Cyan 5 Blue 6 Magenta 7 White}
    if {[dict exists $names $i]} { return [dict get $names $i] }
    return $i
}
proc cold_hover {i} { set ::cold(hover) $i }
proc cold_pick {i {done 0}} {
    set c .cold.f.c
    set pv .cold.f.b.pv
    $pv delete all
    if {$i eq "BYLAYER" || $i eq "BYBLOCK"} {
        set ::cold(value) [expr {$i eq "BYLAYER" ? "ByLayer" : "ByBlock"}]
        $c coords selbox 0 0 0 0
        $pv create text 22 11 -text [string range $::cold(value) 2 end] -fill "#c9ced6" -font {Helvetica 8}
    } else {
        set ::cold(value) [cold_name $i]
        set bb [$c bbox c$i]
        if {$bb ne ""} { $c coords selbox {*}$bb; $c raise selbox }
        $pv create rectangle 0 0 46 24 -fill [lindex $::cold(hex) $i] -outline ""
    }
    set ::cold(pick) $i
    if {$done} { cold_done 1 }
}
proc cold_typed {} {
    set v [string trim $::cold(value)]
    set names {red 1 yellow 2 green 3 cyan 4 blue 5 magenta 6 white 7}
    if {[dict exists $names [string tolower $v]]} { set v [dict get $names [string tolower $v]] }
    if {[string is integer -strict $v] && $v >= 1 && $v <= 255} {
        set ::cold(pick) $v
        set bb [.cold.f.c bbox c$v]
        if {$bb ne ""} { .cold.f.c coords selbox {*}$bb; .cold.f.c raise selbox }
        .cold.f.b.pv delete all
        .cold.f.b.pv create rectangle 0 0 46 24 -fill [lindex $::cold(hex) $v] -outline ""
    } elseif {[string equal -nocase $v bylayer]} { set ::cold(pick) BYLAYER
    } elseif {[string equal -nocase $v byblock]} { set ::cold(pick) BYBLOCK }
}
proc cold_done {ok} {
    if {$ok} { set ::cold(result) "OK $::cold(pick)" }
    destroy .cold
}

# ---------------------------------------------------------------- Point Style (DDPTYPE)
# Returns "CANCEL" or "OK mode size relative(1/0)".
set ::pt_modes {0 1 2 3 4 32 33 34 35 36 64 65 66 67 68 96 97 98 99 100}
proc pt_glyph {c x y mode s col tags} {
    set base [expr {$mode % 32}]
    switch -- $base {
        0 { $c create rectangle [expr {$x-1}] [expr {$y-1}] [expr {$x+1}] [expr {$y+1}] -fill $col -outline $col -tags $tags }
        2 { $c create line [expr {$x-$s}] $y [expr {$x+$s}] $y -fill $col -tags $tags
            $c create line $x [expr {$y-$s}] $x [expr {$y+$s}] -fill $col -tags $tags }
        3 { $c create line [expr {$x-$s}] [expr {$y-$s}] [expr {$x+$s}] [expr {$y+$s}] -fill $col -tags $tags
            $c create line [expr {$x-$s}] [expr {$y+$s}] [expr {$x+$s}] [expr {$y-$s}] -fill $col -tags $tags }
        4 { $c create line $x $y $x [expr {$y-$s}] -fill $col -tags $tags }
    }
    if {$mode & 32} { $c create oval [expr {$x-$s}] [expr {$y-$s}] [expr {$x+$s}] [expr {$y+$s}] -outline $col -tags $tags }
    if {$mode & 64} { $c create rectangle [expr {$x-$s}] [expr {$y-$s}] [expr {$x+$s}] [expr {$y+$s}] -outline $col -tags $tags }
}
proc ptypedlg {mode size relative} {
    set w .ptd
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Point Style"
    wm transient $w .
    wm resizable $w 0 0
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    set ::ptd(mode) $mode
    set ::ptd(size) $size
    set ::ptd(rel) $relative
    set ::ptd(result) CANCEL
    set cs 52
    set c [canvas $f.c -width [expr {5*$cs + 4}] -height [expr {4*$cs + 4}] -background $::panel -highlightthickness 0]
    pack $c
    set i 0
    foreach m $::pt_modes {
        set col [expr {$i % 5}]; set row [expr {$i / 5}]
        set x0 [expr {2 + $col*$cs}]; set y0 [expr {2 + $row*$cs}]
        $c create rectangle [expr {$x0+3}] [expr {$y0+3}] [expr {$x0+$cs-3}] [expr {$y0+$cs-3}] \
            -fill "#1b1f24" -outline "#3a4049" -width 1 -tags [list m$m cell cell$m]
        pt_glyph $c [expr {$x0 + $cs/2}] [expr {$y0 + $cs/2}] $m 13 "#e6e9ee" [list m$m]
        $c bind m$m <Button-1> [list ptd_pick $m]
        $c bind m$m <Double-Button-1> [list ptd_pick $m 1]
        incr i
    }
    ptd_pick $mode
    set g [ttk::frame $f.g -padding {0 10 0 0}]
    ttk::label $g.l -text "Point size:"
    ttk::entry $g.e -width 10 -textvariable ::ptd(size)
    ttk::label $g.u -textvariable ::ptd(unit) -style Dim.TLabel
    grid $g.l -row 0 -column 0 -sticky w
    grid $g.e -row 0 -column 1 -sticky w -padx 6
    grid $g.u -row 0 -column 2 -sticky w
    ttk::radiobutton $g.r1 -text "Set size relative to screen" -value 1 -variable ::ptd(rel) -command ptd_unit
    ttk::radiobutton $g.r2 -text "Set size in absolute units" -value 0 -variable ::ptd(rel) -command ptd_unit
    grid $g.r1 -row 1 -column 0 -columnspan 3 -sticky w -pady {6 0}
    grid $g.r2 -row 2 -column 0 -columnspan 3 -sticky w
    pack $g -fill x
    ptd_unit
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text OK -command {ptd_done 1}
    ttk::button $b.no -text Cancel -command {ptd_done 0}
    pack $b.no $b.ok -side right -padx 3
    pack $b -fill x -pady {12 0}
    bind $w <Escape> {ptd_done 0}
    bind $w <Return> {ptd_done 1}
    wm geometry $w +[expr {[winfo rootx .] + 240}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::ptd(result)
}
proc ptd_unit {} { set ::ptd(unit) [expr {$::ptd(rel) ? "% of the screen height" : "drawing units"}] }
proc ptd_pick {m {done 0}} {
    set c .ptd.f.c
    foreach id [$c find withtag cell] { $c itemconfigure $id -fill "#1b1f24" -outline "#3a4049" -width 1 }
    $c itemconfigure cell$m -fill "#2b4a73" -outline "#7fb2e5" -width 2
    set ::ptd(mode) $m
    if {$done} { ptd_done 1 }
}
proc ptd_done {ok} {
    if {$ok} {
        set s [string trim $::ptd(size)]
        if {$s eq "" || [catch {expr {double($s)}}]} { set s 5 }
        set ::ptd(result) "OK $::ptd(mode) $s $::ptd(rel)"
    }
    destroy .ptd
}

# ---------------------------------------------------------------- Drawing Limits (LIMITS)
# Returns "CANCEL", "OK x0 y0 x1 y1 gridinside check" or "SHEET x0 y0 x1 y1 paper scale landscape gridinside check"
# (fields separated by \x1f); SHEET asks the program for the area a sheet covers at a scale.
proc limitsdlg {x0 y0 x1 y1 papers scales gridin check paper scale land} {
    set w .lim
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Drawing Limits"
    wm transient $w .
    wm resizable $w 0 0
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    array unset ::lim
    set ::lim(result) CANCEL
    set ::lim(gridin) $gridin; set ::lim(check) $check; set ::lim(land) $land
    ttk::label $f.h -style Dim.TLabel -wraplength 380 -justify left \
        -text "The drawing's size: the rectangle ZOOM All shows, Plot > Limits prints, and (optionally) the grid fills."
    pack $f.h -anchor w -pady {0 8}
    ttk::labelframe $f.c -text " Corners " -padding 8
    foreach {k label v} [list x0 "Lower left  X" $x0 y0 "Y" $y0 x1 "Upper right  X" $x1 y1 "Y" $y1] {
        ttk::label $f.c.l$k -text $label
        ttk::entry $f.c.e$k -width 14
        $f.c.e$k insert 0 $v
    }
    grid $f.c.lx0 $f.c.ex0 $f.c.ly0 $f.c.ey0 -padx 3 -pady 2 -sticky w
    grid $f.c.lx1 $f.c.ex1 $f.c.ly1 $f.c.ey1 -padx 3 -pady 2 -sticky w
    pack $f.c -fill x
    ttk::labelframe $f.s -text " From a sheet size and scale " -padding 8
    ttk::label $f.s.pl -text "Sheet:"
    ttk::combobox $f.s.p -state readonly -width 26 -values $papers
    $f.s.p set $paper
    ttk::label $f.s.sl -text "Scale:"
    ttk::combobox $f.s.s -state readonly -width 26 -values $scales
    $f.s.s set $scale
    ttk::checkbutton $f.s.land -text "Landscape" -variable ::lim(land)
    ttk::button $f.s.go -text "Use sheet" -command {lim_done SHEET}
    grid $f.s.pl $f.s.p -sticky w -padx 3 -pady 2
    grid $f.s.sl $f.s.s -sticky w -padx 3 -pady 2
    grid $f.s.land $f.s.go -sticky w -padx 3 -pady {6 0}
    pack $f.s -fill x -pady {10 0}
    ttk::checkbutton $f.g -text "Show the grid only inside the limits" -variable ::lim(gridin)
    ttk::checkbutton $f.k -text "Don't accept points outside the limits (LIMCHECK)" -variable ::lim(check)
    pack $f.g $f.k -anchor w -pady {8 0}
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text OK -command {lim_done OK}
    ttk::button $b.no -text Cancel -command {lim_done CANCEL}
    pack $b.no $b.ok -side right -padx 3
    pack $b -fill x -pady {12 0}
    bind $w <Escape> {lim_done CANCEL}
    bind $w <Return> {lim_done OK}
    wm geometry $w +[expr {[winfo rootx .] + 220}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    tkwait window $w
    return $::lim(result)
}
proc lim_done {act} {
    set f .lim.f
    set c [list [$f.c.ex0 get] [$f.c.ey0 get] [$f.c.ex1 get] [$f.c.ey1 get]]
    switch -- $act {
        OK { set ::lim(result) "OK [join [concat $c [list $::lim(gridin) $::lim(check)]] \x1f]" }
        SHEET { set ::lim(result) "SHEET [join [concat $c [list [$f.s.p get] [$f.s.s get] $::lim(land) $::lim(gridin) $::lim(check)]] \x1f]" }
        default { set ::lim(result) CANCEL }
    }
    destroy .lim
}

# ---------------------------------------------------------------- Change Layer: pick a layer from a list
# layers: list of {name #color frozen locked}. Returns "CANCEL" or "OK name".
proc layerpickdlg {title layers current} {
    set w .lpk
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w $title
    wm transient $w .
    set f [ttk::frame $w.f -padding 10]
    pack $f -fill both -expand 1
    set ::lpk(result) CANCEL
    ttk::treeview $f.t -columns {state} -height [expr {min(16, max(4, [llength $layers]))}] -selectmode browse
    $f.t heading #0 -text "Layer"
    $f.t heading state -text ""
    $f.t column #0 -width 220
    $f.t column state -width 90 -anchor w
    set i 0
    foreach l $layers {
        lassign $l name col frz lck
        set img [image create photo -width 14 -height 12]
        $img put "#1b1f24" -to 0 0 14 12
        $img put $col -to 1 1 13 11
        set st [string trim "[expr {$frz ? {frozen} : {}}] [expr {$lck ? {locked} : {}}]"]
        set id [$f.t insert {} end -text "  $name" -image $img -values [list $st]]
        set ::lpk(name,$id) $name
        if {$name eq $current} { $f.t selection set $id; $f.t see $id }
        incr i
    }
    pack $f.t -fill both -expand 1
    bind $f.t <Double-Button-1> {lpk_done 1}
    set b [ttk::frame $f.b]
    ttk::button $b.ok -text OK -command {lpk_done 1}
    ttk::button $b.no -text Cancel -command {lpk_done 0}
    pack $b.no $b.ok -side right -padx 3
    pack $b -fill x -pady {10 0}
    bind $w <Escape> {lpk_done 0}
    bind $w <Return> {lpk_done 1}
    wm geometry $w +[expr {[winfo rootx .] + 300}]+[expr {[winfo rooty .] + 120}]
    safegrab $w
    focus $f.t
    tkwait window $w
    return $::lpk(result)
}
proc lpk_done {ok} {
    set s [.lpk.f.t selection]
    if {$ok && $s ne ""} { set ::lpk(result) "OK $::lpk(name,$s)" }
    destroy .lpk
}

# ---------------------------------------------------------------- right-click menus on the drawing
# items: list of  {label command}  |  -  |  {> label {items}}
proc ctxmenu_build {m items} {
    catch {destroy $m}
    menu $m -tearoff 0
    set i 0
    foreach it $items {
        if {$it eq "-"} { $m add separator; continue }
        if {[lindex $it 0] eq ">"} {
            ctxmenu_build $m.s$i [lindex $it 2]
            $m add cascade -label [lindex $it 1] -menu $m.s$i
        } else {
            lassign $it label cmd
            $m add command -label $label -command [list emit MENU $cmd]
        }
        incr i
    }
}
proc ctxmenu {X Y items} {
    ctxmenu_build .ctx $items
    tk_popup .ctx $X $Y
}
# Shift + right-click while picking a point: one-time object snap
proc osnapmenu {X Y} {
    set m .osm
    catch {destroy $m}
    menu $m -tearoff 0
    foreach {lbl word} {Endpoint END Midpoint MID Intersection INT Center CEN Quadrant QUA Tangent TAN \
                        Perpendicular PER Insertion INS Node NOD Nearest NEA None NON} {
        $m add command -label $lbl -command [list emit L $word]
        if {$word in {INT QUA PER NEA}} { $m add separator }
    }
    $m add command -label "Osnap Settings..." -command {emit MENU 'OSNAP}
    tk_popup $m $X $Y
}

# ---------------------------------------------------------------- plot dialog
# Returns "CANCEL" or  action \x1f device \x1f paper \x1f landscape \x1f area \x1f scale \x1f units \x1f
#                      center \x1f mono \x1f lineweights \x1f copies      (action: OK, PREVIEW or PICK)
proc plotdlg {devices device papers paper landscape area scales scale units center mono lw copies wintext} {
    set w .plot
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Plot"
    wm transient $w .
    array unset ::pl
    array set ::pl [list device $device paper $paper landscape $landscape area $area scale $scale units $units \
                        center $center mono $mono lw $lw copies $copies result CANCEL]
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    set r 0
    foreach {lbl var vals} [list "Printer / plotter" device $devices "Paper size" paper $papers] {
        ttk::label $f.l$var -text $lbl
        ttk::combobox $f.$var -textvariable ::pl($var) -values $vals -state readonly -width 34
        grid $f.l$var -row $r -column 0 -sticky w -pady 3 -padx {0 10}
        grid $f.$var -row $r -column 1 -columnspan 2 -sticky w -pady 3
        bind $f.$var <<ComboboxSelected>> plotdlg_preview
        incr r
    }
    ttk::label $f.lcopies -text "Copies (printer)"
    ttk::spinbox $f.copies -from 1 -to 99 -textvariable ::pl(copies) -width 5
    grid $f.lcopies -row $r -column 0 -sticky w -pady 3; grid $f.copies -row $r -column 1 -sticky w -pady 3; incr r
    ttk::label $f.lori -text "Orientation"
    set of [ttk::frame $f.ori]
    ttk::radiobutton $of.l -text Landscape -variable ::pl(landscape) -value 1 -command plotdlg_preview
    ttk::radiobutton $of.p -text Portrait -variable ::pl(landscape) -value 0 -command plotdlg_preview
    pack $of.l $of.p -side left -padx {0 10}
    grid $f.lori -row $r -column 0 -sticky w -pady 3; grid $of -row $r -column 1 -columnspan 2 -sticky w; incr r
    ttk::label $f.larea -text "What to plot"
    ttk::combobox $f.area -textvariable ::pl(area) -values {Extents Display Limits Window} -state readonly -width 12
    ttk::button $f.pick -text "Window <" -command {set ::pl(result) PICK; destroy .plot}
    grid $f.larea -row $r -column 0 -sticky w -pady 3; grid $f.area -row $r -column 1 -sticky w; grid $f.pick -row $r -column 2 -sticky w
    incr r
    if {$wintext ne ""} {
        ttk::label $f.win -text "Window: $wintext" -style Dim.TLabel
        grid $f.win -row $r -column 1 -columnspan 2 -sticky w; incr r
    }
    ttk::label $f.lscale -text "Plot scale"
    ttk::combobox $f.scale -textvariable ::pl(scale) -values $scales -width 20
    grid $f.lscale -row $r -column 0 -sticky w -pady 3; grid $f.scale -row $r -column 1 -columnspan 2 -sticky w; incr r
    ttk::label $f.lunits -text "Drawing units are"
    ttk::combobox $f.units -textvariable ::pl(units) -values {Inches Feet Millimeters Meters} -state readonly -width 12
    grid $f.lunits -row $r -column 0 -sticky w -pady 3; grid $f.units -row $r -column 1 -sticky w; incr r
    set opts [ttk::frame $f.opts]
    ttk::checkbutton $opts.c -text "Center the plot" -variable ::pl(center)
    ttk::checkbutton $opts.m -text "Black ink only (monochrome)" -variable ::pl(mono)
    ttk::checkbutton $opts.w -text "Plot lineweights" -variable ::pl(lw)
    pack $opts.c $opts.m $opts.w -side top -anchor w
    grid $opts -row $r -column 0 -columnspan 3 -sticky w -pady {6 0}; incr r
    canvas $f.pv -width 180 -height 140 -background $::panel -highlightthickness 0
    grid $f.pv -row 0 -column 3 -rowspan 8 -padx {16 0} -sticky n
    set bb [ttk::frame $f.bb]
    ttk::button $bb.pre -text "Preview" -command {set ::pl(result) PREVIEW; destroy .plot}
    ttk::button $bb.ok -text "Plot" -command {set ::pl(result) OK; destroy .plot}
    ttk::button $bb.no -text "Cancel" -command {set ::pl(result) CANCEL; destroy .plot}
    pack $bb.pre -side left
    pack $bb.no $bb.ok -side right -padx 3
    grid $bb -row $r -column 0 -columnspan 4 -sticky ew -pady {12 0}
    bind $w <Escape> {set ::pl(result) CANCEL; destroy .plot}
    plotdlg_preview
    wm geometry $w +[expr {[winfo rootx .] + 160}]+[expr {[winfo rooty .] + 90}]
    safegrab $w
    tkwait window $w
    if {$::pl(result) eq "CANCEL"} { return CANCEL }
    return [join [list $::pl(result) $::pl(device) $::pl(paper) $::pl(landscape) $::pl(area) $::pl(scale) \
                      $::pl(units) $::pl(center) $::pl(mono) $::pl(lw) $::pl(copies)] \x1f]
}
proc plotdlg_preview {} {
    set c .plot.f.pv
    if {![winfo exists $c]} return
    $c delete all
    # paper outline in the right proportions
    if {[regexp {\(([0-9.]+) x ([0-9.]+) (in|mm)\)} $::pl(paper) -> a b u]} {} else { set a 11; set b 17 }
    set long [expr {max($a,$b)}]; set short [expr {min($a,$b)}]
    if {$::pl(landscape)} { set pw $long; set ph $short } else { set pw $short; set ph $long }
    set k [expr {min(160.0 / $pw, 120.0 / $ph)}]
    set x0 [expr {90 - $pw * $k / 2}]; set y0 [expr {70 - $ph * $k / 2}]
    $c create rectangle [expr {$x0+3}] [expr {$y0+3}] [expr {$x0 + $pw*$k + 3}] [expr {$y0 + $ph*$k + 3}] -fill "#1b2027" -outline ""
    $c create rectangle $x0 $y0 [expr {$x0 + $pw*$k}] [expr {$y0 + $ph*$k}] -fill white -outline "#8a94a3"
    set m 6
    $c create rectangle [expr {$x0+$m}] [expr {$y0+$m}] [expr {$x0 + $pw*$k - $m}] [expr {$y0 + $ph*$k - $m}] -outline $::accent -dash {3 2}
}

# ---------------------------------------------------------------- spelling
# word list written by the Lisp side (one lower-case word per line); custom words are appended
set ::spell_loaded 0
proc spell_load {} {
    if {$::spell_loaded} { return 1 }
    if {![info exists ::spell_file] || ![file readable $::spell_file]} { return 0 }
    set ch [open $::spell_file r]
    fconfigure $ch -encoding utf-8
    foreach w [split [read $ch] "\n"] { if {$w ne ""} { set ::spell_words($w) 1 } }
    close $ch
    set ::spell_loaded 1
    return 1
}
proc spell_ok {w} {
    set lw [string tolower [string trimright $w ']]
    if {[string length $lw] < 2 || [regexp {[0-9]} $lw]} { return 1 }
    if {[info exists ::spell_words($lw)] || [info exists ::spell_ignore($lw)]} { return 1 }
    if {[string match *'s $lw] && [info exists ::spell_words([string range $lw 0 end-2])]} { return 1 }
    return 0
}
proc spell_suggest {w} {
    set lw [string tolower $w]
    set letters "abcdefghijklmnopqrstuvwxyz'"
    set out {}
    set n [string length $lw]
    for {set i 0} {$i <= $n} {incr i} {
        set a [string range $lw 0 [expr {$i-1}]]; set b [string range $lw $i end]
        set cands {}
        if {$b ne ""} { lappend cands $a[string range $b 1 end] }
        if {[string length $b] > 1} { lappend cands $a[string index $b 1][string index $b 0][string range $b 2 end] }
        foreach c [split $letters ""] {
            if {$b ne ""} { lappend cands $a$c[string range $b 1 end] }
            lappend cands $a$c$b
        }
        foreach x $cands { if {[info exists ::spell_words($x)] && $x ni $out} { lappend out $x } }
    }
    # likeliest first: swapped letters (same letters as typed), then same length, then the rest
    set key [join [lsort [split $lw ""]] ""]
    set ranked {}
    foreach x $out {
        set r [expr {[join [lsort [split $x ""]] ""] eq $key ? 0 : ([string length $x] == $n ? 1 : 2)}]
        lappend ranked [list $r [llength $ranked] $x]
    }
    set out {}
    foreach e [lsort -command {apply {{p q} {expr {[lindex $p 0] != [lindex $q 0] ? [lindex $p 0] - [lindex $q 0] : [lindex $p 1] - [lindex $q 1]}}}} $ranked] { lappend out [lindex $e 2] }
    # same capitalisation as the word typed
    set res {}
    foreach x [lrange $out 0 7] {
        if {[string toupper $w] eq $w} { lappend res [string toupper $x] } \
        elseif {[string toupper [string index $w 0]] eq [string index $w 0]} { lappend res [string totitle $x] } \
        else { lappend res $x }
    }
    return $res
}
proc spell_add {w} {
    set lw [string tolower $w]
    set ::spell_words($lw) 1
    catch {
        file mkdir [file dirname $::spell_custom]
        set ch [open $::spell_custom a]; fconfigure $ch -encoding utf-8; puts $ch $w; close $ch
    }
}
# underline unknown words in the in-place editor (after a short pause in typing)
proc mted_spellcheck {} {
    set t $::mted
    if {$t eq "" || ![winfo exists $t] || ![spell_load]} return
    $t tag remove misspelled 1.0 end
    set text [$t get 1.0 end-1c]
    foreach r [regexp -all -inline -indices {[[:alpha:]][[:alpha:]']*} $text] {
        lassign $r a b
        set w [string range $text $a $b]
        if {![spell_ok $w]} { $t tag add misspelled "1.0 + $a chars" "1.0 + [expr {$b + 1}] chars" }
    }
}
proc mted_spell_later {} {
    catch {after cancel $::mted_spell_id}
    set ::mted_spell_id [after 400 mted_spellcheck]
}

# the Check Spelling dialog (SPELL). Returns "CANCEL" or action \x1f replacement
proc spelldlg {word context suggestions dictname} {
    set w .spl
    catch {destroy $w}
    toplevel $w -background $::panel
    wm title $w "Check Spelling"
    wm transient $w .
    set ::spl(result) CANCEL
    set ::spl(new) [expr {[llength $suggestions] ? [lindex $suggestions 0] : $word}]
    set f [ttk::frame $w.f -padding 12]
    pack $f -fill both -expand 1
    ttk::label $f.l1 -text "Not in dictionary:"
    ttk::label $f.word -text $word -font {Helvetica 11 bold} -foreground "#ff8080"
    ttk::label $f.ctx -text $context -style Dim.TLabel -wraplength 380
    ttk::label $f.l2 -text "Change to:"
    ttk::entry $f.new -textvariable ::spl(new) -width 30
    ttk::label $f.l3 -text "Suggestions:"
    listbox $f.sug -height 7 -width 30 -background $::panel2 -foreground $::fg -selectbackground $::accent \
        -borderwidth 0 -highlightthickness 0 -exportselection 0
    foreach s $suggestions { $f.sug insert end $s }
    if {[llength $suggestions]} { $f.sug selection set 0 } else { $f.sug insert end "(no suggestions)" }
    bind $f.sug <<ListboxSelect>> {set i [.spl.f.sug curselection]; if {$i ne ""} { set v [.spl.f.sug get $i]; if {![string match (* $v]} { set ::spl(new) $v } }}
    bind $f.sug <Double-Button-1> {spl_done CHANGE}
    grid $f.l1 -row 0 -column 0 -sticky w
    grid $f.word -row 0 -column 1 -sticky w
    grid $f.ctx -row 1 -column 0 -columnspan 2 -sticky w -pady {2 8}
    grid $f.l2 -row 2 -column 0 -sticky w
    grid $f.new -row 2 -column 1 -sticky w -pady 2
    grid $f.l3 -row 3 -column 0 -sticky nw
    grid $f.sug -row 3 -column 1 -sticky w -pady 2
    set bb [ttk::frame $f.bb]
    foreach {b lbl act} {ch Change CHANGE cha "Change All" CHANGEALL ig Ignore IGNORE iga "Ignore All" IGNOREALL add "Add to Dictionary" ADD} {
        ttk::button $bb.$b -text $lbl -command [list spl_done $act]
        pack $bb.$b -side top -fill x -pady 2
    }
    ttk::button $bb.no -text "Close" -command {spl_done CANCEL}
    pack $bb.no -side top -fill x -pady {12 0}
    grid $bb -row 0 -column 2 -rowspan 4 -sticky n -padx {14 0}
    ttk::label $f.dict -text "Dictionary: $dictname" -style Dim.TLabel
    grid $f.dict -row 4 -column 0 -columnspan 3 -sticky w -pady {8 0}
    bind $w <Escape> {spl_done CANCEL}
    bind $w <Return> {spl_done CHANGE}
    wm geometry $w +[expr {[winfo rootx .] + 240}]+[expr {[winfo rooty .] + 140}]
    safegrab $w
    focus $f.new
    tkwait window $w
    if {$::spl(result) eq "CANCEL"} { return CANCEL }
    return "$::spl(result)\x1f$::spl(new)"
}
proc spl_done {act} { set ::spl(result) $act; destroy .spl }

# ---------------------------------------------------------------- file dialog
# Uses the desktop's own file chooser when one is available (zenity = GNOME/GTK, kdialog = KDE),
# so the sidebar shows Home, Recent, bookmarks and every mounted drive. Falls back to Tk's dialog.
set ::lastdir [pwd]
set ::filedlg_tool ""
if {[auto_execok zenity] ne ""} {
    set ::filedlg_tool zenity
    set ::zenity_major 4
    catch { regexp {^(\d+)} [exec zenity --version 2>/dev/null] -> ::zenity_major }
} elseif {[auto_execok kdialog] ne ""} {
    set ::filedlg_tool kdialog
}
if {[info exists env(FANCYPENCIL_FILEDIALOG)]} { set ::filedlg_tool $env(FANCYPENCIL_FILEDIALOG) }

proc run_external_dialog {cmd} {
    # run the chooser without blocking Tk, so the drawing keeps repainting behind it
    set ::extdlg_out ""
    set ::extdlg_done 0
    if {[catch {open "|$cmd 2>/dev/null" r} ch]} { return -code error $ch }
    fconfigure $ch -blocking 0 -encoding utf-8
    fileevent $ch readable [list apply {{ch} {
        append ::extdlg_out [read $ch]
        if {[eof $ch]} { fconfigure $ch -blocking 1; catch {close $ch}; set ::extdlg_done 1 }
    }} $ch]
    catch {tk busy hold .}
    update idletasks
    vwait ::extdlg_done
    catch {tk busy forget .}
    return [string trimright $::extdlg_out "\n"]
}

proc filedlg {save title initial exts} {
    # EXTS: one extension ("dxf") or several ("dwg dxf"); the first is added when a saved name has none
    set dir $::lastdir
    if {$initial ne "" && [file isdirectory [file dirname $initial]] && [file dirname $initial] ne "."} {
        set dir [file dirname $initial]
    }
    set name [file tail $initial]
    set ext [lindex $exts 0]
    # filters: all drawing types together first (when several), then each, then everything
    set zf {}; set kf {}; set types {}
    if {[llength $exts] > 1} {
        set globs {}; set tglobs {}
        foreach e $exts { lappend globs *.$e *.[string toupper $e]; lappend tglobs .$e .[string toupper $e] }
        lappend zf --file-filter "Drawings ([join [lmap e $exts {string toupper $e}] ", "]) | [join $globs " "]"
        lappend kf "[join $globs " "]|Drawings"
        lappend types [list "Drawings" $tglobs]
    }
    foreach e $exts {
        set E [string toupper $e]
        lappend zf --file-filter "$E files | *.$e *.$E"
        lappend kf "*.$e *.$E|$E files"
        lappend types [list "$E files" [list .$e .$E]]
    }
    lappend zf --file-filter "All files | *"
    lappend kf "*|All files"
    lappend types {{All files} *}
    set r ""
    set tool $::filedlg_tool
    if {$tool eq "zenity"} {
        set cmd [concat [list zenity --file-selection --title $title --modal] $zf]
        if {$save} {
            lappend cmd --save
            if {$::zenity_major < 4} { lappend cmd --confirm-overwrite }
        }
        lappend cmd --filename [expr {$name eq "" ? "[string trimright $dir /]/" : [file join $dir $name]}]
        if {[catch {set r [run_external_dialog $cmd]}]} { set tool tk }
    } elseif {$tool eq "kdialog"} {
        set cmd [list kdialog --title $title [expr {$save ? "--getsavefilename" : "--getopenfilename"}] \
                     [file join $dir $name] [join $kf "\n"]]
        if {[catch {set r [run_external_dialog $cmd]}]} { set tool tk }
    }
    if {$tool ne "zenity" && $tool ne "kdialog"} {
        if {$save} {
            set r [tk_getSaveFile -parent . -title $title -initialdir $dir -initialfile $name \
                       -filetypes $types -defaultextension .$ext]
        } else {
            set r [tk_getOpenFile -parent . -title $title -initialdir $dir -filetypes $types]
        }
    }
    if {$r ne ""} {
        if {$save && [file extension $r] eq ""} { append r .$ext }
        set ::lastdir [file dirname $r]
    }
    focus .cmd.e
    return $r
}

# ---------------------------------------------------------------- layer dialog
proc layerdlg {rows {ltypes {}} {lweights {}}} {
    set w .ld
    if {![winfo exists $w]} {
        toplevel $w -background $::panel
        wm title $w "Layer Properties Manager"
        wm geometry $w 790x420+[expr {[winfo rootx .] + 120}]+[expr {[winfo rooty .] + 140}]
        wm transient $w .
        ttk::frame $w.tb -padding 6
        foreach {lbl act} {"New Layer" new "Rename" rename "Delete" delete "Set Current" current} {
            ttk::button $w.tb.[string map {" " ""} [string tolower $lbl]] -text $lbl -command [list layeract $act]
            pack $w.tb.[string map {" " ""} [string tolower $lbl]] -side left -padx 2
        }
        ttk::button $w.tb.cs -text "Color..." -command {layeract pickcolor}
        pack $w.tb.cs -side left -padx 2
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
        ttk::treeview $w.t -columns {status name on freeze lock plot color ltype lweight} -show headings -selectmode browse
        foreach {c t wd} {status "" 26 name Name 170 on On 50 freeze Freeze 60 lock Lock 60 plot Plot 64 color Color 50 ltype Linetype 100 lweight Lineweight 80} {
            $w.t heading $c -text $t
            $w.t column $c -width $wd -anchor [expr {$c eq "name" ? "w" : "center"}]
        }
        pack $w.t -side top -fill both -expand 1 -padx 6
        ttk::label $w.hint -style Dim.TLabel -padding 6 -wraplength 760 \
            -text "Click On / Freeze / Lock / Plot cells to toggle, the Color cell to choose a color (a No Plot layer shows on screen but is left out of plots). Double-click a name (or press F2, or Rename) to rename it; double-click elsewhere in the row to make the layer current."
        pack $w.hint -side top -fill x
        ttk::button $w.close -text Close -command {destroy .ld; focus .cmd.e}
        pack $w.close -side bottom -anchor e -padx 6 -pady 6
        bind $w.t <ButtonRelease-1> {layerclick %x %y}
        bind $w.t <Double-Button-1> {layerdbl %x %y}
        bind $w.t <F2> {ld_edit_name [.ld.t selection]}
        bind $w <Escape> {destroy .ld; focus .cmd.e; break}
    }
    $w.tb2.lt configure -values $ltypes
    $w.tb2.lw configure -values $lweights
    set sel [$w.t selection]
    set selname ""
    if {$sel ne ""} { set selname [lindex [$w.t item $sel -values] 1] }
    $w.t delete [$w.t children {}]
    foreach r $rows {
        lassign $r name on frz lck color cur ltype lweight plt
        if {$plt eq ""} { set plt 1 }
        if {$cur} { set mark [format %c 0x2713] } else { set mark "" }
        set id [$w.t insert {} end -values [list $mark $name \
                    [expr {$on ? "On" : "off"}] [expr {$frz ? "Frozen" : "-"}] [expr {$lck ? "Locked" : "-"}] \
                    [expr {$plt ? "Plot" : "No Plot"}] $color $ltype $lweight]]
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
        "#6" { emit LAYERACT plot $name }
        "#7" { emit LAYERACT pickcolor $name }
    }
}
proc layerdbl {x y} {
    set row [.ld.t identify row $x $y]
    if {$row eq ""} return
    if {[.ld.t identify column $x $y] eq "#2"} { ld_edit_name $row } else { layeract current }
}
# rename in place: an entry over the Name cell; Enter keeps the new name, Esc leaves it
proc ld_edit_name {item} {
    if {$item eq "" || ![winfo exists .ld.t]} return
    catch {destroy .ld.t.ed}
    .ld.t see $item
    update idletasks
    set bb [.ld.t bbox $item name]
    if {$bb eq ""} return
    lassign $bb x y wd ht
    set old [lindex [.ld.t item $item -values] 1]
    set ::ld_old $old
    set ::ld_new $old
    entry .ld.t.ed -textvariable ::ld_new -relief solid -borderwidth 1 \
        -background $::panel2 -foreground $::fg -insertbackground $::fg
    place .ld.t.ed -x $x -y $y -width [expr {max($wd, 120)}] -height $ht
    .ld.t.ed selection range 0 end
    .ld.t.ed icursor end
    focus .ld.t.ed
    bind .ld.t.ed <Return> {ld_edit_done 1; break}
    bind .ld.t.ed <KP_Enter> {ld_edit_done 1; break}
    bind .ld.t.ed <Escape> {ld_edit_done 0; break}
    bind .ld.t.ed <FocusOut> {ld_edit_done 1}
}
proc ld_edit_done {keep} {
    if {![winfo exists .ld.t.ed]} return
    set new [string trim $::ld_new]
    destroy .ld.t.ed
    focus .ld.t
    if {$keep && $new ne "" && $new ne $::ld_old} {
        emit LAYERACT rename "$::ld_old\x1f$new"
    }
}
proc ld_edit_named {name} {
    foreach id [.ld.t children {}] {
        if {[lindex [.ld.t item $id -values] 1] eq $name} { .ld.t selection set $id; ld_edit_name $id; return }
    }
}
proc layeract {act {arg ""}} {
    if {$act eq "rename"} { ld_edit_name [.ld.t selection]; return }
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
    safegrab $w
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
    safegrab $w
    tkwait window $w
    return $::aliasresult
}
emit READY
# test hook: FANCYPENCIL_TEST_TCL=script.tcl is sourced after startup (used by the automated GUI tests)
if {[info exists env(FANCYPENCIL_TEST_TCL)]} { after 1500 [list source $env(FANCYPENCIL_TEST_TCL)] }
