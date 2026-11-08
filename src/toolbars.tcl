# toolbars.tcl - toolbars of command icons, one per menu (and per sub-menu), that can be docked
# on any edge of the window (top, bottom, left, right, in as many rows or columns as you like)
# or floated as small windows. Drag a toolbar by its grip (the dotted end) to move it; right-click
# a toolbar, or use View > Toolbars, to show or hide toolbars. The layout is remembered in
# ~/.config/fancypencil/toolbars.tcl.
#
# Each command gets an icon: a drawing for the common ones, a lettered tile for the rest. Resting
# the mouse on an icon shows the command's name.

set ::tbx_menus {file File edit Edit view View layout Layout draw Draw solid Solids dim Dimension
                 modify Modify format Format tools Tools express Express window Window help Help}
set ::tbx_names {}
set ::tbx_open ""
set ::tbx_file [file join $env(HOME) .config fancypencil toolbars.tcl]

# ---------------------------------------------------------------- look (Toolbar Options)
# size: button size in pixels; thick: line weight of the drawings; pad: space between buttons;
# bg / fg / ac / fill: toolbar background, icon lines, icon highlights, filled shapes
set ::tbo_defaults [list size 26 thick 1.0 pad 1 bg $::panel fg $::fg ac $::accent fill $::panel2]
array set ::tbo $::tbo_defaults
set ::tbo_themes {
    "Dark (standard)"  {bg #2b313b fg #d8dde4 ac #3b8ee8 fill #353c47}
    "Light"            {bg #e9ecf0 fg #2a3038 ac #1f6fd1 fill #ffffff}
    "High contrast"    {bg #000000 fg #ffffff ac #ffd400 fill #303030}
    "Blueprint"        {bg #123a6b fg #e8f0ff ac #7fd4ff fill #1d4f8c}
    "Warm"             {bg #3a302a fg #f2e6d8 ac #f08a3c fill #4a3d35}
    "Green screen"     {bg #0e1a12 fg #b8f5c8 ac #3ddc6e fill #1a2e20}
}
proc tbx_hex {c} {
    lassign [winfo rgb . $c] r g b
    format "#%02x%02x%02x" [expr {$r / 256}] [expr {$g / 256}] [expr {$b / 256}]
}
proc tbx_mix {a b t} {
    lassign [winfo rgb . $a] r1 g1 b1
    lassign [winfo rgb . $b] r2 g2 b2
    format "#%02x%02x%02x" [expr {int(($r1 + ($r2 - $r1) * $t) / 256)}] [expr {int(($g1 + ($g2 - $g1) * $t) / 256)}] [expr {int(($b1 + ($b2 - $b1) * $t) / 256)}]
}
proc tbx_colors {} {
    foreach k {bg fg ac fill} { if {[catch {winfo rgb . $::tbo($k)}]} { set ::tbo($k) [dict get $::tbo_defaults $k] } }
    set ::tbx_bg $::tbo(bg); set ::tbx_fg $::tbo(fg); set ::tbx_ac $::tbo(ac); set ::tbx_fill $::tbo(fill)
    set ::tbx_hover [tbx_mix $::tbo(bg) $::tbo(fg) 0.18]
    set ::tbx_edge [tbx_mix $::tbo(bg) $::tbo(fg) 0.12]
    set ::tbx_grip [tbx_mix $::tbo(bg) $::tbo(fg) 0.45]
    set ::tbx_k [expr {($::tbo(size) - 6) / 20.0}]
    foreach d {.dock_t .dock_b .dock_l .dock_r} { catch {$d configure -background $::tbx_bg} }
}
tbx_colors

# ---------------------------------------------------------------- the toolbars, from the menus
proc tbx_add {name title items {kind cmd}} {
    if {![llength $items]} return
    lappend ::tbx_names $name
    set ::tbx(t,$name) $title
    set ::tbx(i,$name) $items
    set ::tbx(k,$name) $kind
    set ::tbx(c,$name) [lindex [split $name _] 0]
}
proc tbx_from_menu {name title m} {
    set items {}
    set subs {}
    set last [$m index end]
    if {$last eq "none"} return
    for {set i 0} {$i <= $last} {incr i} {
        set t [$m type $i]
        if {$t eq "cascade"} {
            set sub [$m entrycget $i -menu]
            if {[string match *recent* $sub]} continue
            lappend subs [list ${name}_[lindex [split $sub .] end] [$m entrycget $i -label] $sub]
            continue
        }
        if {$t ni {command radiobutton}} continue
        set cmd [$m entrycget $i -command]
        if {[string match "*DOCSWITCH*" $cmd] || [string match "*OPENRECENT*" $cmd]} continue
        set key [string toupper [lindex $cmd end]]
        if {[lindex $cmd 0] eq "menucmd"} { set key [string toupper [lindex $cmd 1]] }
        set lbl [regsub {\s+\(Ctrl\+.*\)$|\s+\(F\d+\)$} [$m entrycget $i -label] ""]
        lappend items [list $lbl $cmd $key]
    }
    tbx_add $name $title $items
    foreach sb $subs { tbx_from_menu {*}$sb }
}
proc tbx_collect {} {
    set ::tbx_names {}
    set items {}
    foreach {n l} $::tbx_menus { if {[winfo exists .mb.$n]} { lappend items [list $l $n $n] } }
    tbx_add menus "Menus" $items menus
    foreach {n l} $::tbx_menus { if {[winfo exists .mb.$n]} { tbx_from_menu $n $l .mb.$n } }
}

# ---------------------------------------------------------------- icons
# glyphs are drawn on a 20 x 20 grid. Ops: L line, l thin line, A accent line, D dashed line,
# > accent arrow, <> accent arrow both ways, S smooth line, C smooth accent arrow, P polygon,
# F color polygon (a = accent), O oval, o accent dot, E accent oval outline, R rect, B filled rect,
# r accent rect, X dashed rect, a arc (x0 y0 x1 y1 start extent), aa accent arc, G big text,
# t text, T accent text, W wide line
proc tbx_draw {c ops} {
    set f $::tbx_fg; set a $::tbx_ac; set k $::tbx_k
    set w1 [expr {max(1, 1.5 * $k * $::tbo(thick))}]
    set w0 [expr {max(1, 1.0 * $k * $::tbo(thick))}]
    set w3 [expr {max(2, 3.5 * $k * $::tbo(thick))}]
    set ar [list [expr {5 * $k}] [expr {6 * $k}] [expr {2.5 * $k}]]
    set fb [list Helvetica [expr {-round(15 * $k)}] bold]
    set fs [list Helvetica [expr {-max(7, round(9 * $k))}] bold]
    foreach op $ops {
        set kk [lindex $op 0]; set v [lrange $op 1 end]
        switch -- $kk {
            L  { $c create line {*}$v -fill $f -width $w1 -capstyle round -joinstyle round }
            l  { $c create line {*}$v -fill $f -width $w0 }
            W  { $c create line {*}$v -fill $f -width $w3 -capstyle round }
            A  { $c create line {*}$v -fill $a -width $w1 -capstyle round -joinstyle round }
            D  { $c create line {*}$v -fill $f -width $w0 -dash {2 2} }
            >  { $c create line {*}$v -fill $a -width $w1 -arrow last -arrowshape $ar }
            <> { $c create line {*}$v -fill $a -width $w1 -arrow both -arrowshape $ar }
            S  { $c create line {*}$v -fill $f -width $w1 -smooth 1 }
            s  { $c create line {*}$v -fill $a -width $w1 -smooth 1 }
            C  { $c create line {*}$v -fill $a -width $w1 -smooth 1 -arrow last -arrowshape $ar }
            P  { $c create polygon {*}$v -fill "" -outline $f -width $w1 -joinstyle round }
            F  { set col [lindex $v 0]; if {$col eq "a"} { set col $a } elseif {$col eq "p"} { set col $::tbx_fill }
                 $c create polygon {*}[lrange $v 1 end] -fill $col -outline $f -width $w0 }
            O  { $c create oval {*}$v -outline $f -width $w1 }
            E  { $c create oval {*}$v -outline $a -width $w1 }
            o  { $c create oval {*}$v -fill $a -outline $a }
            R  { $c create rectangle {*}$v -outline $f -width $w1 }
            B  { $c create rectangle {*}$v -outline $f -width $w0 -fill $::tbx_fill }
            r  { $c create rectangle {*}$v -outline $a -fill $a }
            X  { $c create rectangle {*}$v -outline $a -width $w0 -dash {2 2} }
            a  { $c create arc {*}[lrange $v 0 3] -start [lindex $v 4] -extent [lindex $v 5] -style arc -outline $f -width $w1 }
            aa { $c create arc {*}[lrange $v 0 3] -start [lindex $v 4] -extent [lindex $v 5] -style arc -outline $a -width $w1 }
            G  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $f -font $fb }
            t  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $f -font $fs }
            T  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $a -font $fs }
        }
    }
}
# the glyph was drawn on the 20 x 20 grid: bring it to the button's size, centred
proc tbx_fit {c} {
    $c scale all 0 0 $::tbx_k $::tbx_k
    set off [expr {($::tbo(size) - 20 * $::tbx_k) / 2.0}]
    $c move all $off $off
}
proc tbx_dots {pts} { set out {}; foreach {x y} $pts { lappend out [list o [expr {$x-1.6}] [expr {$y-1.6}] [expr {$x+1.6}] [expr {$y+1.6}]] }; return $out }
proc tbx_cube {{face ""}} {
    set ops {}
    foreach {nm pts} {top {10 1 18 5.5 10 10 2 5.5} left {2 5.5 10 10 10 19 2 14.5} right {10 10 18 5.5 18 14.5 10 19}} {
        lappend ops [list F [expr {$nm eq $face ? "a" : "p"}] {*}$pts]
    }
    return $ops
}
set ::tbx_page {P 4 1 12 1 16 5 16 19 4 19}
array set ::gl {}
proc gl {keys ops} { foreach k $keys { set ::gl($k) $ops } }
# File
gl NEW          [list $::tbx_page {l 12 1 12 5 16 5} {A 10 9 10 16} {A 6.5 12.5 13.5 12.5}]
gl OPEN         {{P 1 4 7 4 9 6 17 6 17 16 1 16} {F a 1 16 4 9 20 9 17 16}}
gl QSAVE        {{P 2 2 16 2 18 4 18 18 2 18} {r 5 2 14 7} {R 5 11 15 18}}
gl SAVEAS       {{P 2 2 16 2 18 4 18 18 2 18} {r 5 2 14 7} {R 5 11 11 18} {W 13 18 19 11}}
gl {CLOSE CLOSEALL} [list $::tbx_page {A 7 8 13 14} {A 13 8 7 14}]
gl {PLOT}       {{R 2 8 18 15} {P 6 8 6 2 14 2 14 8} {F a 6 13 14 13 14 19 6 19} {o 14.5 9.5 16.5 11.5}}
gl DXFIN        {{P 8 1 18 1 18 19 8 19} {> 1 10 13 10}}
gl {DXFOUT}     {{P 2 1 12 1 12 19 2 19} {> 7 10 19 10}}
gl {IMAGEATTACH IMAGE} {{R 1 3 19 17} {F a 2 16 8 8 12 13 14 11 18 16} {o 13 5 16 8}}
gl PDFATTACH    [list $::tbx_page {T 10 12 PDF}]
gl DWFATTACH    [list $::tbx_page {T 10 12 DWF}]
gl DGNATTACH    [list $::tbx_page {T 10 12 DGN}]
gl DRAWINGRECOVERY [list $::tbx_page {C 13 15 7 15 6 9 11 7}]
gl {3DPRINT}    [concat [tbx_cube top] {{L 0 19.5 20 19.5}}]
gl STLIN        [concat [tbx_cube left] {{> 20 2 13 7}}]
gl QUIT         {{R 3 1 12 19} {> 8 10 19 10}}
# Edit
gl U            {{C 16 16 17 8 10 4 3 9}}
gl REDO         {{C 4 16 3 8 10 4 17 9}}
gl CUTCLIP      {{O 2 13 7 18} {O 13 13 18 18} {L 6 14 15 2} {L 14 14 5 2}}
gl {COPYCLIP}   {{B 2 2 12 14} {B 7 6 17 18}}
gl COPYBASE     {{B 2 2 12 14} {B 7 6 17 18} {o 5.5 15.5 8.5 18.5}}
gl {PASTECLIP PASTEORIG} {{R 3 3 15 19} {r 6 1 12 5} {B 9 8 18 18}}
gl PASTEBLOCK   {{R 3 3 15 19} {r 6 1 12 5} {B 9 8 18 18} {l 9 13 18 13} {l 13.5 8 13.5 18}}
gl ERASE        {{F a 3 13 11 5 17 11 9 19} {l 7 9 13 15} {L 1 19.5 19 19.5}}
gl SELECTALL    {{X 2 2 18 18} {F p 9 7 9 18 12 15 14 19 16 18 14 14 18 14}}
# View
gl {ZOOM}       {{O 2 2 13 13} {W 12 12 18 18} {R 5 5 10 10}}
gl {PAN}        {{<> 10 1 10 19} {<> 1 10 19 10}}
gl REGEN        {{C 16 7 10 3 4 7 3 12} {C 4 13 10 17 16 13 17 8}}
gl PLAN         {{R 3 3 17 17} {l 3 10 17 10} {l 10 3 10 17} {T 10 10 T}}
gl 3DORBIT      {{O 5 5 15 15} {E 1 7 19 13} {> 16 8 18 10}}
gl 3DFORBIT     {{O 5 5 15 15} {E 1 7 19 13} {E 7 1 13 19}}
gl 3DCORBIT     {{O 5 5 15 15} {C 2 13 1 8 5 3 11 1} {C 18 7 19 12 15 17 9 19}}
gl 3DDISTANCE   {{O 2 9 8 15} {R 12 6 19 13} {<> 9 4 9 18}}
gl 3DSWIVEL     {{R 2 7 11 14} {P 11 9 15 6 15 15 11 12} {C 17 3 19 10 17 17}}
gl 3DZOOM       {{O 2 2 13 13} {W 12 12 18 18} {A 5 7.5 10 7.5} {A 7.5 5 7.5 10}}
gl 3DPAN        {{<> 10 1 10 19} {<> 1 10 19 10} {O 7 7 13 13}}
gl HISTORY      [list $::tbx_page {l 7 7 13 7} {l 7 10 13 10} {l 7 13 13 13} {l 7 16 11 16}]
# Draw
gl LINE         [concat {{L 3 17 17 3}} [tbx_dots {3 17 17 3}]]
gl PLINE        [concat {{L 2 17 6 5 11 13 18 3}} [tbx_dots {2 17 6 5 11 13 18 3}]]
gl RECTANG      [concat {{R 2 5 18 15}} [tbx_dots {2 15 18 5}]]
gl POLYGON      {{P 10 2 17 6 17 14 10 18 3 14 3 6} {o 8.5 8.5 11.5 11.5}}
gl CIRCLE       [concat {{O 2 2 18 18} {A 10 10 17 6.5}} [tbx_dots {10 10}]]
gl ARC          [concat {{a 2 4 18 20 0 180}} [tbx_dots {2 12 10 4 18 12}]]
gl ELLIPSE      {{O 1 5 19 15} {l 1 10 19 10} {l 10 5 10 15}}
gl SPLINE       [concat {{S 2 16 6 4 12 16 18 4}} [tbx_dots {2 16 18 4}]]
gl HELIX        {{S 10 10 12 9 12 12 9 13 7 10 9 6 14 7 16 11 14 16 9 17 4 14 3 8 6 3 12 2}}
gl HATCH        {{R 2 2 18 18} {A 2 8 8 2} {A 2 14 14 2} {A 6 18 18 6} {A 12 18 18 12}}
gl REVCLOUD     {{S 3 12 3 8 6 6 8 4 11 4 13 5 15 4 17 6 17 9 18 12 16 15 13 16 10 17 7 16 4 15 3 12}}
gl POINT        {{l 10 3 10 17} {l 3 10 17 10} {o 7.5 7.5 12.5 12.5}}
gl {TEXT MTEXT} {{G 10 11 A} {A 3 19 17 19}}
gl BLOCK        {{B 2 9 10 17} {B 8 3 16 11} {B 10 11 18 18}}
gl BEDIT        {{B 2 9 10 17} {B 8 3 16 11} {W 11 18 18 11}}
gl RENAME       {{B 2 9 10 17} {B 8 3 16 11} {T 15 16 ab}}
gl INSERT       {{B 9 9 18 18} {> 2 2 11 11}}
# Solids
gl BOX          [tbx_cube top]
gl WEDGE        {{F p 2 17 14 17 14 7} {F a 14 17 19 13 19 3 14 7} {F p 2 17 14 7 19 3 7 13}}
gl CYLINDER     {{L 4 5 4 15} {L 16 5 16 15} {a 4 12 16 18 180 180} {F a 4 5 4.5 3.5 6 2.5 10 2 14 2.5 15.5 3.5 16 5 15.5 6.5 14 7.5 10 8 6 7.5 4.5 6.5}}
gl CONE         {{L 10 2 3 15} {L 10 2 17 15} {a 3 12 17 18 180 180} {D 3 15 6 13.5 10 13 14 13.5 17 15}}
gl SPHERE       {{O 2 2 18 18} {E 2 8 18 12} {l 10 2 10 18}}
gl TORUS        {{O 1 5 19 15} {E 6 8 14 12}}
gl EXTRUDE      {{F p 2 16 12 16 17 12 7 12} {> 9.5 14 9.5 2}}
gl REVOLVE      {{L 4 2 4 18} {S 4 3 9 6 7 11 10 17} {C 13 4 18 10 13 16}}
gl SWEEP        {{S 2 17 6 6 13 13 18 3} {E 0 13 5 20} {E 15 0 20 6}}
gl LOFT         {{E 2 14 12 18} {E 8 2 18 6} {l 2 16 8 4} {l 12 16 18 4}}
gl PRESSPULL    {{F a 3 13 13 13 17 9 7 9} {<> 10 1 10 19}}
gl UNION        {{o 2 5 13 16} {o 7 5 18 16}}
gl SUBTRACT     {{o 2 5 13 16} {O 8 5 19 16} {F p 13 6 15 8 16 10.5 15 13 13 15 11 13 10.5 10.5 11 8}}
gl INTERSECT    {{O 2 5 13 16} {O 7 5 18 16} {F a 10 6.5 12 9 12 12 10 14.5 8 12 8 9}}
gl SLICE        [concat [tbx_cube] {{A 0 14 20 4}}]
gl MASSPROP     [concat [tbx_cube] {{T 15 4 kg}}]
# Dimension
gl DIMLINEAR    {{l 3 19 3 4} {l 17 19 17 4} {<> 3 8 17 8} {L 3 17 17 17}}
gl DIMALIGNED   {{L 4 18 16 6} {l 4 18 1 15} {l 16 6 13 3} {<> 1.5 14.5 12.5 3.5}}
gl DIMRADIUS    {{O 2 2 18 18} {> 10 10 16 4} {T 7 14 R}}
gl DIMDIAMETER  {{O 2 2 18 18} {<> 4.5 15.5 15.5 4.5}}
gl DIMANGULAR   {{L 2 18 19 18} {L 2 18 14 3} {aa -9 7 13 29 0 52}}
gl DIMCONTINUE  {{l 2 19 2 6} {l 10 19 10 6} {l 18 19 18 6} {<> 2 10 10 10} {<> 10 10 18 10}}
gl DIMBASELINE  {{l 2 19 2 3} {l 10 19 10 9} {l 18 19 18 3} {<> 2 12 10 12} {<> 2 6 18 6}}
gl LEADER       {{L 12 5 19 5} {> 12 5 3 15} {l 13 9 19 9}}
gl DIMSTYLE     {{l 3 19 3 8} {l 17 19 17 8} {<> 3 11 17 11} {T 10 4 Aa}}
# Modify
gl MOVE         {{<> 10 1 10 19} {<> 1 10 19 10} {o 8 8 12 12}}
gl COPY         {{B 2 8 10 16} {B 9 3 17 11} {> 6 6 12 1}}
gl ROTATE       {{P 10 6 15 11 10 16 5 11} {C 3 8 7 2 13 2 17 6}}
gl SCALE        {{X 2 2 18 18} {B 2 10 9 17} {> 9 10 16 3}}
gl MIRROR       {{F a 2 4 8 16 2 16} {P 18 4 12 16 18 16} {D 10 1 10 19}}
gl OFFSET       {{S 2 18 7 9 18 7} {s 2 12 6 4 18 2}}
gl STRETCH      {{R 2 6 12 14} {X 8 3 16 17} {> 12 10 19 10}}
gl {PEDIT MPEDIT} {{L 2 17 6 6 11 12} {W 12 18 18 12}}
gl HATCHEDIT    {{R 2 2 14 14} {A 2 8 8 2} {A 2 14 14 2} {W 12 18 18 12}}
gl {TRIM EXTRIM} {{L 10 1 10 19} {L 2 10 10 10} {D 10 10 18 10} {A 12 7 17 13} {A 17 7 12 13}}
gl EXTEND       {{L 17 1 17 19} {L 2 10 9 10} {> 9 10 16 10}}
gl FILLET       {{L 3 2 3 10} {aa 3 2 19 18 180 90} {L 11 18 18 18}}
gl CHAMFER      {{L 3 2 3 10} {A 3 10 11 18} {L 11 18 18 18}}
gl BREAK        [concat {{L 2 10 8 10} {L 12 10 18 10}} [tbx_dots {8 10 12 10}]]
gl {BREAKLINE}  {{L 1 10 7 10 9 5 11 15 13 10 19 10}}
gl {EXPLODE BURST} {{A 10 1 10 6} {A 10 14 10 19} {A 1 10 6 10} {A 14 10 19 10} {A 4 4 7 7} {A 13 13 16 16} {A 16 4 13 7} {A 4 16 7 13} {o 8.5 8.5 11.5 11.5}}
gl JOIN         [concat {{L 2 10 8 10} {L 12 10 18 10} {A 8 10 12 10}} [tbx_dots {8 10 12 10}]]
gl ARRAYRECT    {{B 2 2 6 6} {B 8 2 12 6} {B 14 2 18 6} {B 2 8 6 12} {B 8 8 12 12} {B 14 8 18 12} {B 2 14 6 18} {B 8 14 12 18} {B 14 14 18 18}}
gl ARRAYPOLAR   [concat {{l 10 10 10 10}} [tbx_dots {10 2 17 6 17 14 10 18 3 14 3 6}] {{l 9 10 11 10}}]
gl ALIGN        {{B 2 12 9 18} {P 11 2 18 5 16 11 9 8} {> 6 11 11 5}}
gl MATCHPROP    {{F a 12 2 18 8 14 12 8 6} {W 9 10 3 17}}
gl {PROPERTIES QUICKPROPERTIES} {{R 3 2 17 18} {l 8 6 14 6} {l 8 10 14 10} {l 8 14 12 14} {o 5 5 7 7} {o 5 9 7 11} {o 5 13 7 15}}
gl {DRAWORDER TEXTTOFRONT HATCHTOBACK} {{B 2 2 12 12} {F a 8 8 18 8 18 18 8 18} {> 15 16 15 2}}
gl {TCASE}      {{G 10 11 Aa}}
gl {TXT2MTXT}   {{t 6 7 abc} {t 14 14 abc} {> 4 10 9 15}}
gl {TCOUNT}     {{t 6 6 1.} {t 6 11 2.} {t 6 16 3.} {l 10 6 18 6} {l 10 11 18 11} {l 10 16 18 16}}
# Format, Tools, Express
gl {LAYER}      {{L 3 17 12 22 21 17} {L 3 13 12 18 21 13} {F a 12 4 21 9 12 14 3 9}}
gl {LAYMCUR LAYCUR LAYMCH CHANGELAYER LAYMRG LAYDEL LAYISO LAYUNISO LAYOFF LAYFRZ LAYON LAYTHW LAYLCK LAYULK} \
                {{L 1 15 9 19 17 15} {F a 9 6 17 10 9 14 1 10}}
gl LINETYPE     {{D 2 5 18 5} {L 2 10 7 10} {L 9 10 11 10} {L 13 10 18 10} {L 2 15 18 15}}
gl LWEIGHT      {{l 2 4 18 4} {L 2 9 18 9} {W 2 15 18 15}}
gl {STYLE}      {{G 8 11 A} {T 16 15 a}}
gl TEXTSIZE     {{G 7 11 A} {t 16 14 A}}
gl UNITS        {{R 1 7 19 13} {l 4 7 4 10} {l 7 7 7 9} {l 10 7 10 11} {l 13 7 13 9} {l 16 7 16 10}}
gl LIMITS       {{X 2 3 18 17} {o 0.5 15.5 3.5 18.5} {o 16.5 1.5 19.5 4.5}}
gl DDPTYPE      {{A 2 2 8 8} {A 8 2 2 8} {O 12 2 18 8} {l 2 15 8 15} {l 5 12 5 18} {o 13.5 13.5 16.5 16.5}}
gl HPSET        {{R 2 2 18 18} {A 2 8 8 2} {A 2 14 14 2} {A 6 18 18 6} {A 12 18 18 12}}
gl QUICKCALC    {{R 4 1 16 19} {r 6 3 14 7} {o 6 9.5 8 11.5} {o 9 9.5 11 11.5} {o 12 9.5 14 11.5} {o 6 13 8 15} {o 9 13 11 15} {o 12 13 14 15}}
gl DIST         [concat {{<> 3 15 17 5}} [tbx_dots {3 15 17 5}]]
gl ID           {{l 10 3 10 17} {l 3 10 17 10} {o 8 8 12 12} {T 15 16 xy}}
gl {LIST XLIST} [list $::tbx_page {l 7 7 13 7} {l 7 10 13 10} {l 7 13 13 13} {l 7 16 11 16}]
gl AREA         {{F p 2 16 6 3 17 6 15 17} {T 10 11 A}}
gl OSNAP        {{l 10 1 10 19} {l 1 10 19 10} {X 6 6 14 14}}
gl SPELL        {{t 9 7 abc} {A 5 13 8 17 16 9}}
gl OVERKILL     {{L 2 6 18 6} {D 2 9 18 9} {A 13 12 18 18} {A 18 12 13 18}}
gl COPYM        {{B 2 2 8 8} {B 7 7 13 13} {B 12 12 18 18}}
gl MOCORO       {{<> 10 1 10 12} {<> 4 6 16 6} {C 4 15 10 19 16 15}}
gl FLATTEN      [concat [tbx_cube] {{> 10 14 10 19}}]
gl SELECTSIMILAR {{O 2 2 9 9} {O 11 2 18 9} {E 2 11 9 18} {E 11 11 18 18}}
gl APPLOAD      [list $::tbx_page {T 10 12 LSP}]
gl {ALIASEDIT ALIASFILE ALIASFOLDER REINIT} {{T 10 7 L} {> 4 10 16 10} {T 10 15 LINE}}
gl OPTIONS      {{l 10 1 10 19} {l 1 10 19 10} {O 6 6 14 14}}
gl {DOCNEXT}    [list $::tbx_page {> 6 12 14 12}]
gl {DOCPREV}    [list $::tbx_page {> 14 12 6 12}]
gl HELP         {{O 2 2 18 18} {T 10 10.5 ?}}
gl ABOUT        {{O 2 2 18 18} {T 10 10.5 i}}
# Layout
gl {MODEL MSPACE} [concat [tbx_cube top]]
gl {LAYOUTNEW PAGESETUP LAYOUT PSPACE} {{R 2 3 18 17} {l 2 14 18 14} {l 12 14 12 17} {X 5 6 15 11}}
gl {MVIEW VPSCALE} {{R 2 3 18 17} {R 4 5 16 15}}
gl KEEPPROFILES [tbx_cube]

proc tbx_variant {key} {
    # icons that depend on the command's option: views, styles, UCS, viewports, iso planes
    switch -glob -- $key {
        "-VIEW TOP" { return [tbx_cube top] }
        "-VIEW BOTTOM" { return [concat [tbx_cube] {{A 2 15 10 19 18 15}}] }
        "-VIEW LEFT" { return [tbx_cube left] }
        "-VIEW RIGHT" { return [tbx_cube right] }
        "-VIEW FRONT" { return [tbx_cube left] }
        "-VIEW BACK" { return [concat [tbx_cube] {{A 10 2 18 6}}] }
        "-VIEW *ISO" { return [concat [tbx_cube top] [list [list T 3 2 [string range [lindex $key 1] 0 1]]]] }
        "VSCURRENT WIREFRAME" { return {{O 2 2 18 18} {l 2 10 18 10} {l 10 2 10 18} {E 2 7 18 13}} }
        "VSCURRENT HIDDEN" { return {{O 2 2 18 18} {l 2 10 18 10} {D 10 2 10 18}} }
        "VSCURRENT SHADED" { return {{o 2 2 18 18}} }
        "VSCURRENT EDGES" { return {{o 2 2 18 18} {O 2 2 18 18} {L 2 10 18 10}} }
        "UCS FACE" { return [concat [tbx_cube right] {{T 4 3 UCS}}] }
        "UCS*" {
            set ops {{> 4 16 4 3} {> 4 16 17 16}}
            set w [string toupper [lindex $key 1]]
            if {$w eq ""} { set w O }
            lappend ops [list T 13 7 [string range $w 0 1]]
            return $ops
        }
        "ISODRAFT*" - ISOPLANE { return {{l 2 6 10 2 18 6} {l 2 6 2 14 10 18 18 14 18 6} {l 10 10 10 18} {l 2 6 10 10 18 6}} }
        "VPORTS*" {
            set ops {{R 1 3 19 17}}
            switch -glob -- $key {
                "*2 VERTICAL" { lappend ops {l 10 3 10 17} }
                "*2 HORIZONTAL" { lappend ops {l 1 10 19 10} }
                "*3 RIGHT" { lappend ops {l 10 3 10 17} {l 1 10 10 10} }
                "*3 LEFT" { lappend ops {l 10 3 10 17} {l 10 10 19 10} }
                "*4*" { lappend ops {l 10 3 10 17} {l 1 10 19 10} }
                "*JOIN" { lappend ops {D 10 3 10 17} {<> 5 10 15 10} }
            }
            return $ops
        }
        "MVIEW 2" { return {{R 2 3 18 17} {l 2 14 18 14} {R 4 5 9 12} {R 11 5 16 12}} }
        "MVIEW 3" { return {{R 2 3 18 17} {l 2 14 18 14} {R 4 5 9 12} {R 11 5 16 8} {R 11 9 16 12}} }
        "MVIEW 4*" { return {{R 2 3 18 17} {l 2 14 18 14} {R 4 5 9 8} {R 11 5 16 8} {R 4 9 9 12} {R 11 9 16 12}} }
        "MVIEW LOCK ON" { return {{R 3 9 17 18} {a 6 2 14 14 0 180}} }
        "MVIEW LOCK OFF" { return {{R 3 9 17 18} {a 8 2 16 14 0 180}} }
        "CIRCLE 2P" { return [concat {{O 2 2 18 18} {A 2 10 18 10}} [tbx_dots {2 10 18 10}]] }
        "CIRCLE 3P" { return [concat {{O 2 2 18 18}} [tbx_dots {10 2 3 14 17 14}]] }
        "ARC C" { return [concat {{a 2 4 18 20 0 180} {D 10 12 18 12}} [tbx_dots {10 12}]] }
        "SPLINE M CV" { return [concat {{D 2 16 6 4 12 16 18 4} {S 2 16 6 4 12 16 18 4}} [tbx_dots {2 16 6 4 12 16 18 4}]] }
        "ZOOM E" { return {{O 2 2 13 13} {W 12 12 18 18} {R 5 5 10 10}} }
        "ZOOM W" { return {{O 2 2 13 13} {W 12 12 18 18} {X 4 5 11 9}} }
        "ZOOM P" { return {{O 2 2 13 13} {W 12 12 18 18} {> 10 7.5 4 7.5}} }
        "DRAWORDER BACK" - "HATCHTOBACK" { return {{F a 2 2 12 2 12 12 2 12} {B 8 8 18 18} {> 15 2 15 16}} }
        "DRAWORDER UNDER" { return {{F a 2 2 12 2 12 12 2 12} {B 8 8 18 18} {> 5 14 5 19}} }
        "TCASE UPPER" { return {{G 10 11 AB}} }
        "TCASE LOWER" { return {{G 10 10 ab}} }
        "TCASE TOGGLE" { return {{G 10 11 aB}} }
        "REVOLVE MO SU" { return {{L 4 2 4 18} {S 4 3 9 6 7 11 10 17} {C 13 4 18 10 13 16} {T 15 18 S}} }
    }
    return ""
}
proc tbx_abbrev {label} {
    set l [regsub -all {\(.*?\)|\.\.\.|[:/,]} $label " "]
    set words {}
    foreach w $l { if {[string tolower $w] ni {a an the to of on and with from for in by}} { lappend words $w } }
    if {[llength $words] >= 2} {
        set s ""
        foreach w [lrange $words 0 2] { append s [string toupper [string index $w 0]] }
        return $s
    }
    set w [lindex $words 0]
    return "[string toupper [string index $w 0]][string tolower [string range $w 1 1]]"
}
set ::tbx_catcolors {file #4a6a8c edit #6a5a8c view #3d7a6a layout #5a7a3d draw #2f6fb8 solid #7a4a8c dim #8c6a2f
                     modify #b8562f format #3d6a7a tools #6a6a6a express #8c7a2f window #4a5a6a help #4a5a6a}
proc tbx_icon {c key label cat} {
    if {[set ops [tbx_variant $key]] ne ""} { tbx_draw $c $ops } \
    elseif {[info exists ::gl($key)]} { tbx_draw $c $::gl($key) } \
    elseif {[info exists ::gl([lindex $key 0])]} { tbx_draw $c $::gl([lindex $key 0]) } \
    else {
        set col #4a5a6a
        if {[dict exists $::tbx_catcolors $cat]} { set col [dict get $::tbx_catcolors $cat] }
        $c create rectangle 1 3 19 17 -fill $col -outline ""
        $c create text 10 10 -text [tbx_abbrev $label] -fill white -font [list Helvetica [expr {-max(7, round(9 * $::tbx_k))}] bold]
    }
    tbx_fit $c
}
proc tbx_menuglyph {c name} {
    switch -- $name {
        file { tbx_draw $c [list $::tbx_page {l 12 1 12 5 16 5} {A 7 10 13 10} {A 7 13 13 13} {A 7 16 11 16}] }
        edit { tbx_draw $c {{R 4 3 16 19} {r 7 1 13 5} {l 7 9 13 9} {l 7 12 13 12} {l 7 15 11 15}} }
        view { tbx_draw $c {{S 1 10 10 4 19 10 10 16 1 10} {o 7 7 13 13}} }
        layout { tbx_draw $c {{R 2 2 18 18} {l 2 15 18 15} {l 12 15 12 18} {E 5 5 15 12}} }
        draw { tbx_draw $c {{P 14 1 19 6 7 18 2 13} {F a 2 13 7 18 1 19}} }
        solid { tbx_draw $c [tbx_cube top] }
        dim { tbx_draw $c {{l 3 3 3 18} {l 17 3 17 18} {<> 3 12 17 12} {L 8 7 12 7}} }
        modify { tbx_draw $c {{<> 10 1 10 19} {<> 1 10 19 10} {o 8 8 12 12}} }
        format { tbx_draw $c {{L 1 15 10 20 19 15} {L 1 11 10 16 19 11} {F a 10 2 19 7 10 12 1 7}} }
        tools { tbx_draw $c {{W 3 17 12 8} {O 10 1 19 10} {o 2 16 4 18}} }
        express { $c create polygon 12 0 3 11 9 11 7 20 17 8 11 8 14 0 -fill "#f0c040" -outline "#c89a20" }
        window { tbx_draw $c {{B 1 3 13 13} {r 1 3 13 5.5} {B 7 7 19 17} {r 7 7 19 9.5}} }
        help { tbx_draw $c {{O 1 1 19 19} {G 10 10.5 ?}} }
    }
    tbx_fit $c
}

# ---------------------------------------------------------------- docks
frame .dock_t -background $panel -height 0
frame .dock_b -background $panel -height 0
frame .dock_l -background $panel -width 0
frame .dock_r -background $panel -width 0
pack .dock_t -side top -fill x -after .tb
pack .dock_b -side bottom -fill x -after .bottom
pack .dock_l -in .main -side left -fill y -before .c
pack .dock_r -in .main -side right -fill y -before .c
# a dock's size is set from its toolbars, so a long column never stretches the window
foreach w {.dock_t .dock_b .dock_l .dock_r} { pack propagate $w 0 }
proc tbx_dockw {d} { return [dict get {top .dock_t bottom .dock_b left .dock_l right .dock_r} $d] }
foreach d {top bottom left right} { bind [tbx_dockw $d] <Button-3> {tbx_rmenu %X %Y} }

proc tbx_defaults {} {
    foreach n $::tbx_names { set ::tbx(d,$n) hidden; set ::tbx(last,$n) top; set ::tbx(line,$n) 0; set ::tbx(pos,$n) 0 }
    set ::tbx(d,menus) left; set ::tbx(line,menus) 0
    set ::tbx(d,draw) left; set ::tbx(line,draw) 1
    set ::tbx(d,modify) right; set ::tbx(line,modify) 0
}
proc tbx_save {} {
    set out {}
    foreach n $::tbx_names {
        lappend out [list $n $::tbx(d,$n) $::tbx(line,$n) $::tbx(pos,$n) [expr {[info exists ::tbx(g,$n)] ? $::tbx(g,$n) : ""}] $::tbx(last,$n)]
    }
    catch {
        file mkdir [file dirname $::tbx_file]
        set fd [open $::tbx_file w]
        puts $fd "# Fancy Pencil toolbar layout (written by the program; delete it to get the standard layout)"
        foreach r $out { puts $fd [list $r] }
        puts $fd [list [concat :opts [array get ::tbo]]]
        close $fd
    }
}
proc tbx_load {} {
    if {[catch {set fd [open $::tbx_file]; set data [read $fd]; close $fd}]} return
    foreach line [split $data \n] {
        if {[string match "#*" $line] || [string trim $line] eq ""} continue
        if {[lindex [lindex $line 0] 0] eq ":opts"} {
            foreach {k v} [lrange [lindex $line 0] 1 end] {
                switch -- $k {
                    size { if {[string is integer -strict $v] && $v >= 16 && $v <= 72} { set ::tbo(size) $v } }
                    thick { if {[string is double -strict $v] && $v >= 0.4 && $v <= 4} { set ::tbo(thick) $v } }
                    pad { if {[string is integer -strict $v] && $v >= 0 && $v <= 10} { set ::tbo(pad) $v } }
                    bg - fg - ac - fill { if {![catch {winfo rgb . $v}]} { set ::tbo($k) $v } }
                }
            }
            continue
        }
        if {[catch {lassign [lindex $line 0] n d ln pos g last}]} continue
        if {$n ni $::tbx_names || $d ni {top bottom left right float hidden}} continue
        if {![string is double -strict $ln] || ![string is double -strict $pos]} continue
        set ::tbx(d,$n) $d; set ::tbx(line,$n) $ln; set ::tbx(pos,$n) $pos
        if {[regexp {^\+-?\d+\+-?\d+$} $g]} { set ::tbx(g,$n) $g }
        if {$last in {top bottom left right float}} { set ::tbx(last,$n) $last }
    }
}

# ---------------------------------------------------------------- laying out
proc tbx_layout {} {
    foreach d {top bottom left right} { tbx_layout_dock $d }
    foreach n $::tbx_names {
        set ::tbx_vis($n) [expr {$::tbx(d,$n) ne "hidden"}]
        if {$::tbx(d,$n) eq "float"} { tbx_float $n } else { catch {destroy .tbf_$n} }
    }
    tbx_save
}
proc tbx_layout_dock {d} {
    set D [tbx_dockw $d]
    foreach w [winfo children $D] { destroy $w }
    set here {}
    foreach n $::tbx_names { if {$::tbx(d,$n) eq $d} { lappend here [list $::tbx(line,$n) $::tbx(pos,$n) $n] } }
    set here [lsort -real -index 1 $here]
    set here [lsort -real -index 0 $here]
    set vert [expr {$d in {left right}}]
    update idletasks
    set avail [expr {$vert ? [winfo height .c] : [winfo width .]}]
    if {$avail < 200} { set avail [expr {$vert ? 560 : 1200}] }
    set ::tbx_avail($d) $avail
    set maxrun [expr {max(1, ($avail - 14) / ($::tbo(size) + 2 * $::tbo(pad) + 2))}]
    set side [dict get {left left right right top top bottom bottom} $d]
    set li -1; set prev ""; set k 0
    foreach rec $here {
        lassign $rec ln pos n
        if {$ln ne $prev} {
            incr li; set prev $ln; set k 0
            frame $D.l$li -background $::tbx_bg
            pack $D.l$li -side $side -fill [expr {$vert ? "y" : "x"}] -padx [expr {$vert ? 1 : 0}] -pady [expr {$vert ? 0 : 1}]
            bind $D.l$li <Button-3> {tbx_rmenu %X %Y}
        }
        set ::tbx(line,$n) $li; set ::tbx(pos,$n) $k
        incr k
        tbx_make $D.l$li.$n $n $vert $maxrun
        pack $D.l$li.$n -side [expr {$vert ? "top" : "left"}] -anchor nw -padx 1 -pady 1
    }
    # the dock as thick as its rows (columns) need
    update idletasks
    set size 0
    foreach w [winfo children $D] { incr size [expr {$vert ? [winfo reqwidth $w] + 2 : [winfo reqheight $w] + 2}] }
    if {$vert} { $D configure -width $size } else { $D configure -height $size }
}
proc tbx_make {w n vert maxrun} {
    frame $w -background $::tbx_bg -highlightthickness 1 -highlightbackground $::tbx_edge -highlightcolor $::tbx_edge
    set g [canvas $w.grip -background $::tbx_bg -highlightthickness 0 -cursor fleur \
               -width [expr {$vert ? $::tbo(size) : 8}] -height [expr {$vert ? 8 : $::tbo(size)}]]
    if {$vert} {
        for {set x 6} {$x < $::tbo(size) - 4} {incr x 4} { $g create rectangle $x 2 [expr {$x+1}] 3 -fill $::tbx_grip -outline ""; $g create rectangle [expr {$x+2}] 5 [expr {$x+3}] 6 -fill $::tbx_grip -outline "" }
    } else {
        for {set y 6} {$y < $::tbo(size) - 4} {incr y 4} { $g create rectangle 2 $y 3 [expr {$y+1}] -fill $::tbx_grip -outline ""; $g create rectangle 5 [expr {$y+2}] 6 [expr {$y+3}] -fill $::tbx_grip -outline "" }
    }
    bind $g <ButtonPress-1> [list tbx_drag_start $n %X %Y]
    bind $g <B1-Motion> [list tbx_drag_move $n %X %Y]
    bind $g <ButtonRelease-1> [list tbx_drag_end $n %X %Y]
    bind $g <Enter> [list tbx_tip $g $::tbx(t,$n) 1]
    bind $g <Leave> tbx_tip_hide
    bind $g <Button-3> {tbx_rmenu %X %Y}
    bind $w <Button-3> {tbx_rmenu %X %Y}
    set b [frame $w.b -background $::tbx_bg]
    if {$vert} { pack $g -side top -fill x; pack $b -side top } else { pack $g -side left -fill y; pack $b -side left }
    set i 0
    foreach it $::tbx(i,$n) {
        lassign $it label cmd key
        set c [canvas $b.c$i -width $::tbo(size) -height $::tbo(size) -background $::tbx_bg -highlightthickness 0 -borderwidth 0]
        if {$::tbx(k,$n) eq "menus"} { tbx_menuglyph $c $cmd } else { tbx_icon $c $key $label $::tbx(c,$n) }
        set run [expr {$i % $maxrun}]; set other [expr {$i / $maxrun}]
        if {$vert} { grid $c -row $run -column $other -padx $::tbo(pad) -pady $::tbo(pad) } else { grid $c -row $other -column $run -padx $::tbo(pad) -pady $::tbo(pad) }
        bind $c <Enter> [list tbx_benter $c $n $label]
        bind $c <Leave> [list tbx_bleave $c]
        bind $c <ButtonPress-1> [list $c configure -background $::tbx_ac]
        bind $c <ButtonRelease-1> [list tbx_bclick $c $n $i %X %Y]
        bind $c <Button-3> {tbx_rmenu %X %Y}
        incr i
    }
}
proc tbx_benter {c n label} {
    if {$::tbx_open ne $c} { $c configure -background $::tbx_hover }
    if {$::tbx(k,$n) eq "menus" && $::tbx_open ne "" && $::tbx_open ne $c && $::fp_popup_menu ne ""} {
        set i [string range [lindex [split $c .] end] 1 end]
        tbx_openmenu $c $n $i; return
    }
    tbx_tip $c [expr {$::tbx(k,$n) eq "menus" ? "$label commands" : $label}]
}
proc tbx_bleave {c} {
    if {$::tbx_open ne $c && [winfo exists $c]} { $c configure -background $::tbx_bg }
    tbx_tip_hide
}
proc tbx_bclick {c n i X Y} {
    tbx_tip_hide
    if {![winfo exists $c]} return
    if {[winfo containing $X $Y] ne $c} { $c configure -background $::tbx_bg; return }
    $c configure -background $::tbx_hover
    set it [lindex $::tbx(i,$n) $i]
    if {$::tbx(k,$n) eq "menus"} {
        if {$::tbx_open eq $c && $::fp_popup_menu ne ""} { popup_close; return }
        tbx_openmenu $c $n $i
        return
    }
    uplevel #0 [lindex $it 1]
}
# a menu opened from the Menus toolbar opens away from the edge it sits on
proc tbx_openmenu {c n i} {
    set m .mb.[lindex [lindex $::tbx(i,$n) $i] 1]
    set d $::tbx(d,$n)
    set X [winfo rootx $c]; set Y [winfo rooty $c]
    update idletasks
    switch -- $d {
        left - float { set X [expr {$X + [winfo width $c] + 4}] }
        right { set X [expr {$X - [winfo reqwidth $m] - 4}] }
        top { set Y [expr {$Y + [winfo height $c] + 2}] }
        bottom { set Y [expr {$Y - [winfo reqheight $m] - 2}] }
    }
    fp_popup $m $X $Y
    set ::tbx_open $c
    $c configure -background $::tbx_ac
}
proc tbx_menu_reset {} {
    if {$::tbx_open ne "" && [winfo exists $::tbx_open]} { $::tbx_open configure -background $::tbx_bg }
    set ::tbx_open ""
}

# ---------------------------------------------------------------- tooltips
proc tbx_tip {w text {now 0}} {
    catch {after cancel $::tbx_tipid}
    set ::tbx_tipid [after [expr {$now ? 250 : 450}] [list tbx_tip_show $w $text]]
}
proc tbx_tip_show {w text} {
    if {![winfo exists $w] || $::tbx_open ne "" || [info exists ::tbx_dragging]} return
    if {[winfo containing {*}[winfo pointerxy .]] ne $w} return
    catch {destroy .tbxtip}
    toplevel .tbxtip -background "#11151a"
    wm overrideredirect .tbxtip 1
    label .tbxtip.l -text $text -background "#11151a" -foreground $::fg -padx 6 -pady 2
    pack .tbxtip.l -padx 1 -pady 1
    update idletasks
    set X [expr {[winfo rootx $w] + [winfo width $w] + 6}]
    set Y [expr {[winfo rooty $w] + 4}]
    if {$X + [winfo reqwidth .tbxtip] > [winfo screenwidth .]} {
        set X [expr {[winfo rootx $w] - [winfo reqwidth .tbxtip] - 6}]
    }
    if {[winfo rooty $w] + [winfo height $w] + 30 < [winfo screenheight .] && [winfo height $w] < 20} {
        set Y [expr {[winfo rooty $w] + [winfo height $w] + 2}]
    }
    wm geometry .tbxtip +$X+$Y
}
proc tbx_tip_hide {} { catch {after cancel $::tbx_tipid}; catch {destroy .tbxtip} }

# ---------------------------------------------------------------- dragging a toolbar
proc tbx_drag_start {n X Y} {
    tbx_tip_hide
    set ::tbx_dragging $n
    catch {destroy .tbxghost}
    toplevel .tbxghost -background $::accent
    wm overrideredirect .tbxghost 1
    label .tbxghost.l -text $::tbx(t,$n) -background "#11151a" -foreground $::fg -padx 8 -pady 3
    pack .tbxghost.l -padx 2 -pady 2
    wm geometry .tbxghost +[expr {$X + 14}]+[expr {$Y + 14}]
}
proc tbx_drag_move {n X Y} {
    if {![winfo exists .tbxghost]} return
    set tg [tbx_target $n $X $Y]
    set where [dict get {top "Dock at the top" bottom "Dock at the bottom" left "Dock on the left" right "Dock on the right" float "Float here"} [lindex $tg 0]]
    .tbxghost.l configure -text "$::tbx(t,$n): $where"
    wm geometry .tbxghost +[expr {$X + 14}]+[expr {$Y + 14}]
}
proc tbx_drag_end {n X Y} {
    catch {destroy .tbxghost}
    unset -nocomplain ::tbx_dragging
    set tg [tbx_target $n $X $Y]
    lassign $tg d ln pos
    if {$d eq "float"} {
        set ::tbx(g,$n) +[expr {$X - 10}]+[expr {$Y - 10}]
    } else {
        set ::tbx(line,$n) $ln; set ::tbx(pos,$n) $pos
    }
    set ::tbx(d,$n) $d
    set ::tbx(last,$n) $d
    tbx_layout
}
proc tbx_inside {w X Y} {
    if {![winfo exists $w] || ![winfo ismapped $w]} { return 0 }
    set x0 [winfo rootx $w]; set y0 [winfo rooty $w]
    expr {$X >= $x0 && $X < $x0 + [winfo width $w] && $Y >= $y0 && $Y < $y0 + [winfo height $w]}
}
# where a toolbar dropped at X,Y goes: {dock line position} or {float}
proc tbx_target {n X Y} {
    set d ""
    foreach dd {left right top bottom} {
        set D [tbx_dockw $dd]
        if {[winfo width $D] > 2 && [winfo height $D] > 2 && [tbx_inside $D $X $Y]} { set d $dd; break }
    }
    if {$d eq "" && [tbx_inside .c $X $Y]} {
        set cx0 [winfo rootx .c]; set cy0 [winfo rooty .c]
        set dl [expr {$X - $cx0}]; set dr [expr {$cx0 + [winfo width .c] - $X}]
        set dt [expr {$Y - $cy0}]; set db [expr {$cy0 + [winfo height .c] - $Y}]
        set m [expr {min($dl, $dr, $dt, $db)}]
        if {$m < 70} {
            if {$m == $dl} { set d left } elseif {$m == $dr} { set d right } elseif {$m == $dt} { set d top } else { set d bottom }
        } else { return float }
    }
    if {$d eq "" && [tbx_inside . $X $Y]} {
        if {$Y < [winfo rooty .main]} { set d top } elseif {$Y >= [winfo rooty .main] + [winfo height .main]} { set d bottom } else { return float }
    }
    if {$d eq ""} { return float }
    # which row / column of that dock, and where along it
    set D [tbx_dockw $d]
    set vert [expr {$d in {left right}}]
    set lines {}
    foreach w [winfo children $D] { if {[string match $D.l* $w]} { lappend lines $w } }
    set p [expr {$vert ? $X : $Y}]
    set found -1
    set i 0
    foreach L $lines {
        set a [expr {$vert ? [winfo rootx $L] : [winfo rooty $L]}]
        set b [expr {$a + ($vert ? [winfo width $L] : [winfo height $L])}]
        if {$p >= $a && $p < $b} { set found $i; break }
        incr i
    }
    if {$found < 0} {
        set nl [llength $lines]
        if {!$nl} { return [list $d 0 0] }
        # past the outside edge: a new first line; toward the drawing: a new last line
        set L [lindex $lines 0]
        set out [switch -- $d {
            left { expr {$X < [winfo rootx $L]} }
            right { expr {$X >= [winfo rootx $L] + [winfo width $L]} }
            top { expr {$Y < [winfo rooty $L]} }
            bottom { expr {$Y >= [winfo rooty $L] + [winfo height $L]} }
        }]
        return [list $d [expr {$out ? -1 : $nl}] 0]
    }
    set q [expr {$vert ? $Y : $X}]
    set k 0
    foreach w [winfo children [lindex $lines $found]] {
        set wn [lindex [split $w .] end]
        if {$wn eq $n} continue
        set c [expr {$vert ? [winfo rooty $w] + [winfo height $w] / 2 : [winfo rootx $w] + [winfo width $w] / 2}]
        if {$c < $q} { incr k }
    }
    return [list $d $found [expr {$k - 0.5}]]
}

# ---------------------------------------------------------------- floating toolbars
proc tbx_float {n} {
    set t .tbf_$n
    if {[winfo exists $t]} { destroy $t }
    toplevel $t -background $::tbx_bg
    wm title $t $::tbx(t,$n)
    catch {wm attributes $t -type utility}
    wm transient $t .
    wm resizable $t 0 0
    wm protocol $t WM_DELETE_WINDOW [list tbx_setvis $n 0]
    set cnt [llength $::tbx(i,$n)]
    set per [expr {$cnt <= 12 ? $cnt : int(ceil(sqrt($cnt * 2.0)))}]
    tbx_make $t.tb $n 0 [expr {max(1, $per)}]
    pack $t.tb
    if {[info exists ::tbx(g,$n)]} { wm geometry $t $::tbx(g,$n) } else {
        wm geometry $t +[expr {[winfo rootx .c] + 80}]+[expr {[winfo rooty .c] + 60}]
    }
    bind $t <Configure> [list tbx_float_moved $n %W]
}
proc tbx_float_moved {n w} {
    if {$w ne ".tbf_$n"} return
    set ::tbx(g,$n) +[winfo rootx $w]+[winfo rooty $w]
    catch {after cancel $::tbx_saveid}
    set ::tbx_saveid [after 600 tbx_save]
}

# ---------------------------------------------------------------- showing / hiding
proc tbx_setvis {n on} {
    if {$on} {
        if {$::tbx(d,$n) eq "hidden"} {
            set d $::tbx(last,$n)
            if {$d eq "" || $d eq "hidden"} { set d top }
            set ::tbx(d,$n) $d
            if {$d ne "float"} { tbx_place_end $n $d }
        }
    } else {
        if {$::tbx(d,$n) ne "hidden"} { set ::tbx(last,$n) $::tbx(d,$n) }
        set ::tbx(d,$n) hidden
    }
    tbx_layout
}
# a toolbar newly put on dock D goes at the end of its last row (column), or starts a new one
# when it would not fit there
proc tbx_place_end {n d} {
    set D [tbx_dockw $d]
    set lines {}
    foreach w [winfo children $D] { if {[string match $D.l* $w]} { lappend lines $w } }
    set ::tbx(pos,$n) 999
    if {![llength $lines]} { set ::tbx(line,$n) 0; return }
    set L [lindex $lines end]
    set vert [expr {$d in {left right}}]
    set used [expr {$vert ? [winfo reqheight $L] : [winfo reqwidth $L]}]
    set avail [expr {$vert ? [winfo height .c] : [winfo width .]}]
    set need [expr {([llength $::tbx(i,$n)] + 1) * ($::tbo(size) + 2 * $::tbo(pad) + 2) + 12}]
    set ::tbx(line,$n) [expr {$used + $need <= $avail ? [llength $lines] - 1 : [llength $lines]}]
}
# ---------------------------------------------------------------- the Toolbar Options window
# a small floating window: drag the sliders or pick colors and the toolbars change as you go
proc tbx_options {} {
    set t .tbopt
    if {[winfo exists $t]} { wm deiconify $t; raise $t; return }
    toplevel $t -background $::panel
    wm title $t "Toolbar Options"
    catch {wm attributes $t -type utility}
    wm transient $t .
    wm resizable $t 1 1
    wm minsize $t 360 300
    set f [ttk::frame $t.f -padding 10]
    pack $f -fill both -expand 1
    set r 0
    foreach {key lbl from to res} {size "Icon size" 16 64 1 thick "Line weight" 0.5 3.0 0.1 pad "Spacing" 0 8 1} {
        ttk::label $f.l$key -text $lbl
        scale $f.s$key -from $from -to $to -resolution $res -orient horizontal -showvalue 1 -variable ::tbo($key) \
            -command [list tbx_opt_changed] -background $::panel -foreground $::fg -troughcolor $::panel2 \
            -highlightthickness 0 -activebackground $::accent -length 200 -borderwidth 0
        grid $f.l$key -row $r -column 0 -sticky w -pady 2
        grid $f.s$key -row $r -column 1 -columnspan 2 -sticky ew -pady 2
        incr r
    }
    ttk::label $f.lres -text "Icons are drawn as lines, so they stay sharp at every size." -style Dim.TLabel
    grid $f.lres -row $r -column 0 -columnspan 3 -sticky w -pady {0 8}
    incr r
    foreach {key lbl} {bg "Background" fg "Icon lines" ac "Highlights" fill "Filled shapes"} {
        ttk::label $f.c$key -text $lbl
        set sw [frame $f.w$key -width 34 -height 18 -background $::tbo($key) -relief solid -borderwidth 1 -cursor hand2]
        bind $sw <Button-1> [list tbx_opt_pick $key]
        set p [frame $f.p$key -background $::panel]
        set i 0
        foreach col {#000000 #1b1f24 #2b313b #46505e #808890 #c9ced6 #ffffff #123a6b #3b8ee8 #7fd4ff #3ddc6e #ffd400 #f08a3c #e04848} {
            set b [frame $p.b$i -width 14 -height 14 -background $col -relief solid -borderwidth 1 -cursor hand2]
            bind $b <Button-1> [list tbx_opt_set $key $col]
            pack $b -side left -padx 1
            incr i
        }
        ttk::button $f.m$key -text "More..." -width 0 -command [list tbx_opt_pick $key]
        grid $f.c$key -row $r -column 0 -sticky w -pady 3
        grid $sw -row $r -column 1 -sticky w -pady 3 -padx {0 6}
        grid $p -row $r -column 2 -sticky w -pady 3
        grid $f.m$key -row $r -column 3 -sticky w -padx 4
        incr r
    }
    ttk::label $f.lt -text "Color scheme"
    ttk::combobox $f.theme -state readonly -width 18 -values [dict keys $::tbo_themes]
    bind $f.theme <<ComboboxSelected>> {tbx_opt_theme [.tbopt.f.theme get]}
    grid $f.lt -row $r -column 0 -sticky w -pady {8 3}
    grid $f.theme -row $r -column 1 -columnspan 2 -sticky w -pady {8 3}
    incr r
    ttk::label $f.lp -text "Preview"
    set pv [frame $f.pv -background $::tbx_bg -relief solid -borderwidth 1]
    grid $f.lp -row $r -column 0 -sticky nw -pady 6
    grid $pv -row $r -column 1 -columnspan 3 -sticky w -pady 6
    incr r
    set bb [ttk::frame $f.bb]
    ttk::button $bb.def -text "Standard Look" -command tbx_opt_defaults
    ttk::button $bb.close -text "Close" -command {destroy .tbopt}
    pack $bb.def -side left
    pack $bb.close -side right
    grid $bb -row $r -column 0 -columnspan 4 -sticky ew -pady {10 0}
    grid columnconfigure $f 2 -weight 1
    tbx_opt_preview
    wm geometry $t +[expr {[winfo rootx .c] + 120}]+[expr {[winfo rooty .c] + 40}]
}
proc tbx_opt_preview {} {
    set pv .tbopt.f.pv
    if {![winfo exists $pv]} return
    foreach w [winfo children $pv] { destroy $w }
    $pv configure -background $::tbx_bg
    set i 0
    foreach {key lbl} {LINE Line CIRCLE Circle HATCH Hatch MOVE Move DIMLINEAR Linear BOX Box} {
        set c [canvas $pv.c$i -width $::tbo(size) -height $::tbo(size) -background $::tbx_bg -highlightthickness 0]
        tbx_icon $c $key $lbl draw
        grid $c -row 0 -column $i -padx $::tbo(pad) -pady $::tbo(pad)
        incr i
    }
    foreach key {bg fg ac fill} { catch {.tbopt.f.w$key configure -background $::tbo($key)} }
}
proc tbx_opt_set {key col} {
    set ::tbo($key) $col
    tbx_opt_changed
}
proc tbx_opt_pick {key} {
    set c [fpcolor_choose $::tbo($key) "Toolbar color" .tbopt [list tbx_opt_set $key]]
    if {$c ne ""} { tbx_opt_set $key $c }
}

# ---------------------------------------------------------------- color chooser
# Red, Green and Blue sliders plus a Light / Dark slider (the lightness of the same color: drag
# left for darker shades, right for lighter tints), a hex box and a preview. Returns "#rrggbb",
# or "" when cancelled. LIVE, if given, is called with each color as it changes.
proc fpcolor_choose {initial title parent {live ""}} {
    set t .fpcolor
    catch {destroy $t}
    toplevel $t -background $::panel
    wm title $t $title
    catch {wm attributes $t -type dialog}
    if {[winfo exists $parent]} { wm transient $t $parent }
    wm resizable $t 0 0
    if {[catch {lassign [winfo rgb . $initial] r g b}]} { lassign {0 0 0} r g b }
    set ::fpc(r) [expr {$r / 256}]; set ::fpc(g) [expr {$g / 256}]; set ::fpc(b) [expr {$b / 256}]
    set ::fpc(live) $live
    set ::fpc(orig) [format "#%02x%02x%02x" $::fpc(r) $::fpc(g) $::fpc(b)]
    fpcolor_from_rgb
    set f [ttk::frame $t.f -padding 10]
    pack $f
    set row 0
    foreach {k lbl} {r Red g Green b Blue l "Light / Dark"} {
        ttk::label $f.l$k -text $lbl
        ttk::spinbox $f.e$k -from 0 -to [expr {$k eq "l" ? 100 : 255}] -width 5 -textvariable ::fpc(e$k) \
            -command [list fpcolor_typed $k]
        bind $f.e$k <Return> [list fpcolor_typed $k]
        bind $f.e$k <FocusOut> [list fpcolor_typed $k]
        set c [canvas $f.c$k -width 258 -height 24 -background $::panel -highlightthickness 0 -cursor hand2]
        bind $c <Button-1> [list fpcolor_drag $k %x]
        bind $c <B1-Motion> [list fpcolor_drag $k %x]
        grid $f.l$k -row $row -column 0 -sticky w -padx {0 6} -pady 3
        grid $f.e$k -row $row -column 1 -sticky w -padx {0 6} -pady 3
        grid $c -row $row -column 2 -sticky w -pady 3
        incr row
    }
    ttk::label $f.lh -text "Hex"
    ttk::entry $f.hex -width 9 -textvariable ::fpc(hex)
    bind $f.hex <Return> fpcolor_hex
    bind $f.hex <FocusOut> fpcolor_hex
    grid $f.lh -row 0 -column 3 -sticky w -padx {14 4}
    grid $f.hex -row 1 -column 3 -sticky w -padx {14 4}
    frame $f.pv -width 90 -height 52 -relief solid -borderwidth 1
    grid $f.pv -row 2 -column 3 -rowspan 2 -sticky nw -padx {14 4}
    set bb [ttk::frame $f.bb]
    ttk::button $bb.ok -text "OK" -command {set ::fpc(done) ok}
    ttk::button $bb.cancel -text "Cancel" -command {set ::fpc(done) cancel}
    pack $bb.ok -side left -padx 4
    pack $bb.cancel -side left -padx 4
    grid $bb -row $row -column 0 -columnspan 4 -pady {10 0}
    bind $t <Return> {set ::fpc(done) ok}
    bind $t <Escape> {set ::fpc(done) cancel}
    wm protocol $t WM_DELETE_WINDOW {set ::fpc(done) cancel}
    fpcolor_show
    if {[winfo exists $parent]} {
        wm geometry $t +[expr {[winfo rootx $parent] + 30}]+[expr {[winfo rooty $parent] + 60}]
    }
    catch {grab $t}
    focus $t
    set ::fpc(done) ""
    vwait ::fpc(done)
    catch {grab release $t}
    set res [expr {$::fpc(done) eq "ok" ? [fpcolor_hexnow] : ""}]
    if {$res eq "" && $live ne ""} { catch {uplevel #0 [concat $live [list $::fpc(orig)]]} }
    destroy $t
    return $res
}
proc fpcolor_hexnow {} { format "#%02x%02x%02x" $::fpc(r) $::fpc(g) $::fpc(b) }
# RGB (0-255) <-> hue, saturation, lightness (0-1)
proc fpcolor_from_rgb {} {
    set r [expr {$::fpc(r) / 255.0}]; set g [expr {$::fpc(g) / 255.0}]; set b [expr {$::fpc(b) / 255.0}]
    set mx [expr {max($r, $g, $b)}]; set mn [expr {min($r, $g, $b)}]
    set l [expr {($mx + $mn) / 2.0}]
    if {$mx - $mn > 1e-6} {
        set d [expr {$mx - $mn}]
        set s [expr {$l > 0.5 ? $d / (2.0 - $mx - $mn) : $d / ($mx + $mn)}]
        if {$mx == $r} { set h [expr {($g - $b) / $d + ($g < $b ? 6 : 0)}] } \
        elseif {$mx == $g} { set h [expr {($b - $r) / $d + 2}] } \
        else { set h [expr {($r - $g) / $d + 4}] }
        set ::fpc(h) [expr {$h / 6.0}]; set ::fpc(s) $s
    } elseif {![info exists ::fpc(h)]} { set ::fpc(h) 0.0; set ::fpc(s) 0.0 } \
    elseif {$l > 0.0 && $l < 1.0} { set ::fpc(s) 0.0 }
    set ::fpc(l) $l
}
proc fpcolor_hsl {h s l} {
    if {$s < 1e-6} { set v [expr {round($l * 255)}]; return [list $v $v $v] }
    set q [expr {$l < 0.5 ? $l * (1 + $s) : $l + $s - $l * $s}]
    set p [expr {2 * $l - $q}]
    set out {}
    foreach tt [list [expr {$h + 1/3.0}] $h [expr {$h - 1/3.0}]] {
        if {$tt < 0} { set tt [expr {$tt + 1}] }
        if {$tt > 1} { set tt [expr {$tt - 1}] }
        if {$tt < 1/6.0} { set v [expr {$p + ($q - $p) * 6 * $tt}] } \
        elseif {$tt < 0.5} { set v $q } \
        elseif {$tt < 2/3.0} { set v [expr {$p + ($q - $p) * (2/3.0 - $tt) * 6}] } \
        else { set v $p }
        lappend out [expr {max(0, min(255, round($v * 255)))}]
    }
    return $out
}
proc fpcolor_set {k v} {
    if {$k eq "l"} {
        set ::fpc(l) [expr {max(0.0, min(1.0, $v / 100.0))}]
        lassign [fpcolor_hsl $::fpc(h) $::fpc(s) $::fpc(l)] ::fpc(r) ::fpc(g) ::fpc(b)
    } else {
        set ::fpc($k) [expr {max(0, min(255, round($v)))}]
        fpcolor_from_rgb
    }
    fpcolor_show
}
proc fpcolor_drag {k x} {
    set v [expr {max(0.0, min(1.0, ($x - 1) / 256.0))}]
    fpcolor_set $k [expr {$k eq "l" ? $v * 100 : $v * 255}]
}
proc fpcolor_typed {k} {
    set v $::fpc(e$k)
    if {[string is double -strict $v]} { fpcolor_set $k $v } else { fpcolor_show }
}
proc fpcolor_hex {} {
    set v [string trim $::fpc(hex)]
    if {![string match "#*" $v]} { set v "#$v" }
    if {[regexp {^#[0-9a-fA-F]{6}$} $v] && ![catch {winfo rgb . $v} rgb]} {
        lassign $rgb r g b
        set ::fpc(r) [expr {$r / 256}]; set ::fpc(g) [expr {$g / 256}]; set ::fpc(b) [expr {$b / 256}]
        fpcolor_from_rgb
    }
    fpcolor_show
}
proc fpcolor_show {} {
    set f .fpcolor.f
    if {![winfo exists $f]} return
    foreach k {r g b} { set ::fpc(e$k) $::fpc($k) }
    set ::fpc(el) [expr {round($::fpc(l) * 100)}]
    set ::fpc(hex) [fpcolor_hexnow]
    $f.pv configure -background $::fpc(hex)
    foreach k {r g b l} {
        set c $f.c$k
        $c delete all
        for {set i 0} {$i < 64} {incr i} {
            set t [expr {($i + 0.5) / 64.0}]
            if {$k eq "l"} {
                lassign [fpcolor_hsl $::fpc(h) $::fpc(s) $t] rr gg bb
            } else {
                set rr $::fpc(r); set gg $::fpc(g); set bb $::fpc(b)
                set [dict get {r rr g gg b bb} $k] [expr {round($t * 255)}]
            }
            set x0 [expr {1 + $i * 4}]
            $c create rectangle $x0 1 [expr {$x0 + 4}] 15 -fill [format "#%02x%02x%02x" $rr $gg $bb] -outline ""
        }
        $c create rectangle 1 1 257 15 -outline $::dim
        set pos [expr {$k eq "l" ? $::fpc(l) : $::fpc($k) / 255.0}]
        set x [expr {1 + $pos * 256}]
        $c create polygon $x 16 [expr {$x - 5}] 23 [expr {$x + 5}] 23 -fill $::fg -outline $::panel
    }
    if {$::fpc(live) ne ""} {
        catch {after cancel $::fpc(liveid)}
        set ::fpc(liveid) [after 120 [list catch [concat $::fpc(live) [list [fpcolor_hexnow]]]]]
    }
}
proc tbx_opt_theme {name} {
    if {![dict exists $::tbo_themes $name]} return
    foreach {k v} [dict get $::tbo_themes $name] { set ::tbo($k) $v }
    catch {.tbopt.f.theme set $name}
    tbx_opt_changed
}
proc tbx_opt_defaults {} {
    array set ::tbo $::tbo_defaults
    tbx_opt_changed
}
# redraw the toolbars a moment after the last change (sliders send many changes)
proc tbx_opt_changed {args} {
    tbx_colors
    tbx_opt_preview
    catch {after cancel $::tbx_optid}
    set ::tbx_optid [after 150 { tbx_colors; tbx_layout }]
}

proc tbx_reset {} {
    foreach n $::tbx_names { catch {destroy .tbf_$n}; unset -nocomplain ::tbx(g,$n) }
    tbx_defaults
    tbx_layout
}
proc tbx_menus_fill {m} {
    $m delete 0 end
    set sub 0
    foreach n $::tbx_names {
        set title $::tbx(t,$n)
        if {[string match *_* $n]} { set title "    $title" }
        $m add checkbutton -label $title -variable ::tbx_vis($n) -command "tbx_setvis $n \$::tbx_vis($n)"
    }
    $m add separator
    $m add command -label "Show All Main Toolbars" -command tbx_showmain
    $m add command -label "Hide All" -command tbx_hideall
    $m add command -label "Reset Toolbar Layout" -command tbx_reset
    $m add separator
    $m add command -label "Toolbar Options (size, colors)..." -command tbx_options
}
proc tbx_showmain {} {
    foreach n $::tbx_names { if {![string match *_* $n] && $::tbx(d,$n) eq "hidden"} { tbx_setvis $n 1 } }
}
proc tbx_hideall {} {
    foreach n $::tbx_names { if {$::tbx(d,$n) ne "hidden"} { set ::tbx(last,$n) $::tbx(d,$n); set ::tbx(d,$n) hidden } }
    tbx_layout
}
proc tbx_rmenu {X Y} {
    tbx_tip_hide
    if {![winfo exists .tbxmenu]} { menu .tbxmenu -tearoff 0 }
    tbx_menus_fill .tbxmenu
    fp_popup .tbxmenu $X $Y
}

# the window changed size: re-wrap the docked toolbars
proc tbx_resized {} {
    catch {after cancel $::tbx_rsid}
    set ::tbx_rsid [after 150 {
        foreach d {top bottom left right} {
            set vert [expr {$d in {left right}}]
            set avail [expr {$vert ? [winfo height .c] : [winfo width .]}]
            if {![info exists ::tbx_avail($d)] || abs($avail - $::tbx_avail($d)) > 20} { tbx_layout_dock $d }
        }
    }]
}
bind .main <Configure> {+tbx_resized}

tbx_collect
tbx_defaults
tbx_load
tbx_colors
tbx_menus_fill .mb.view.tbars
# first layout once the main window is on screen (floating toolbars need it there)
proc tbx_first_layout {} {
    if {![winfo ismapped .]} { after 100 tbx_first_layout; return }
    tbx_layout
}
after idle tbx_first_layout
