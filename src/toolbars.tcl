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
set ::tbx_bsize 26

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
    set f $::fg; set a $::accent
    foreach op $ops {
        set k [lindex $op 0]; set v [lrange $op 1 end]
        switch -- $k {
            L  { $c create line {*}$v -fill $f -width 1.5 -capstyle round -joinstyle round }
            l  { $c create line {*}$v -fill $f -width 1 }
            W  { $c create line {*}$v -fill $f -width 3.5 -capstyle round }
            A  { $c create line {*}$v -fill $a -width 1.5 -capstyle round -joinstyle round }
            D  { $c create line {*}$v -fill $f -width 1 -dash {2 2} }
            >  { $c create line {*}$v -fill $a -width 1.5 -arrow last -arrowshape {5 6 2.5} }
            <> { $c create line {*}$v -fill $a -width 1.5 -arrow both -arrowshape {5 6 2.5} }
            S  { $c create line {*}$v -fill $f -width 1.5 -smooth 1 }
            s  { $c create line {*}$v -fill $a -width 1.5 -smooth 1 }
            C  { $c create line {*}$v -fill $a -width 1.5 -smooth 1 -arrow last -arrowshape {5 6 2.5} }
            P  { $c create polygon {*}$v -fill "" -outline $f -width 1.5 -joinstyle round }
            F  { set col [lindex $v 0]; if {$col eq "a"} { set col $a } elseif {$col eq "p"} { set col $::panel2 }
                 $c create polygon {*}[lrange $v 1 end] -fill $col -outline $f -width 1 }
            O  { $c create oval {*}$v -outline $f -width 1.5 }
            E  { $c create oval {*}$v -outline $a -width 1.5 }
            o  { $c create oval {*}$v -fill $a -outline $a }
            R  { $c create rectangle {*}$v -outline $f -width 1.5 }
            B  { $c create rectangle {*}$v -outline $f -width 1.2 -fill $::panel2 }
            r  { $c create rectangle {*}$v -outline $a -fill $a }
            X  { $c create rectangle {*}$v -outline $a -width 1 -dash {2 2} }
            a  { $c create arc {*}[lrange $v 0 3] -start [lindex $v 4] -extent [lindex $v 5] -style arc -outline $f -width 1.5 }
            aa { $c create arc {*}[lrange $v 0 3] -start [lindex $v 4] -extent [lindex $v 5] -style arc -outline $a -width 1.5 }
            G  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $f -font {Helvetica -15 bold} }
            t  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $f -font {Helvetica -9 bold} }
            T  { $c create text [lindex $v 0] [lindex $v 1] -text [lindex $v 2] -fill $a -font {Helvetica -9 bold} }
        }
    }
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
        $c create text 10 10 -text [tbx_abbrev $label] -fill white -font {Helvetica -9 bold}
    }
    $c move all 3 3
}
proc tbx_menuglyph {c name} {
    set f $::fg; set a $::accent; set p $::panel2
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
    $c move all 3 3
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
        close $fd
    }
}
proc tbx_load {} {
    if {[catch {set fd [open $::tbx_file]; set data [read $fd]; close $fd}]} return
    foreach line [split $data \n] {
        if {[string match "#*" $line] || [string trim $line] eq ""} continue
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
    set maxrun [expr {max(1, ($avail - 14) / ($::tbx_bsize + 2))}]
    set side [dict get {left left right right top top bottom bottom} $d]
    set li -1; set prev ""; set k 0
    foreach rec $here {
        lassign $rec ln pos n
        if {$ln ne $prev} {
            incr li; set prev $ln; set k 0
            frame $D.l$li -background $::panel
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
    frame $w -background $::panel -highlightthickness 1 -highlightbackground "#3a424e" -highlightcolor "#3a424e"
    set g [canvas $w.grip -background $::panel -highlightthickness 0 -cursor fleur \
               -width [expr {$vert ? $::tbx_bsize : 8}] -height [expr {$vert ? 8 : $::tbx_bsize}]]
    if {$vert} {
        for {set x 6} {$x < $::tbx_bsize - 4} {incr x 4} { $g create rectangle $x 2 [expr {$x+1}] 3 -fill $::dim -outline ""; $g create rectangle [expr {$x+2}] 5 [expr {$x+3}] 6 -fill $::dim -outline "" }
    } else {
        for {set y 6} {$y < $::tbx_bsize - 4} {incr y 4} { $g create rectangle 2 $y 3 [expr {$y+1}] -fill $::dim -outline ""; $g create rectangle 5 [expr {$y+2}] 6 [expr {$y+3}] -fill $::dim -outline "" }
    }
    bind $g <ButtonPress-1> [list tbx_drag_start $n %X %Y]
    bind $g <B1-Motion> [list tbx_drag_move $n %X %Y]
    bind $g <ButtonRelease-1> [list tbx_drag_end $n %X %Y]
    bind $g <Enter> [list tbx_tip $g $::tbx(t,$n) 1]
    bind $g <Leave> tbx_tip_hide
    bind $g <Button-3> {tbx_rmenu %X %Y}
    bind $w <Button-3> {tbx_rmenu %X %Y}
    set b [frame $w.b -background $::panel]
    if {$vert} { pack $g -side top -fill x; pack $b -side top } else { pack $g -side left -fill y; pack $b -side left }
    set i 0
    foreach it $::tbx(i,$n) {
        lassign $it label cmd key
        set c [canvas $b.c$i -width $::tbx_bsize -height $::tbx_bsize -background $::panel -highlightthickness 0 -borderwidth 0]
        if {$::tbx(k,$n) eq "menus"} { tbx_menuglyph $c $cmd } else { tbx_icon $c $key $label $::tbx(c,$n) }
        set run [expr {$i % $maxrun}]; set other [expr {$i / $maxrun}]
        if {$vert} { grid $c -row $run -column $other -padx 1 -pady 1 } else { grid $c -row $other -column $run -padx 1 -pady 1 }
        bind $c <Enter> [list tbx_benter $c $n $label]
        bind $c <Leave> [list tbx_bleave $c]
        bind $c <ButtonPress-1> [list $c configure -background $::accent]
        bind $c <ButtonRelease-1> [list tbx_bclick $c $n $i %X %Y]
        bind $c <Button-3> {tbx_rmenu %X %Y}
        incr i
    }
}
proc tbx_benter {c n label} {
    if {$::tbx_open ne $c} { $c configure -background "#46505e" }
    if {$::tbx(k,$n) eq "menus" && $::tbx_open ne "" && $::tbx_open ne $c && $::fp_popup_menu ne ""} {
        set i [string range [lindex [split $c .] end] 1 end]
        tbx_openmenu $c $n $i; return
    }
    tbx_tip $c [expr {$::tbx(k,$n) eq "menus" ? "$label commands" : $label}]
}
proc tbx_bleave {c} {
    if {$::tbx_open ne $c && [winfo exists $c]} { $c configure -background $::panel }
    tbx_tip_hide
}
proc tbx_bclick {c n i X Y} {
    tbx_tip_hide
    if {![winfo exists $c]} return
    if {[winfo containing $X $Y] ne $c} { $c configure -background $::panel; return }
    $c configure -background "#46505e"
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
    $c configure -background $::accent
}
proc tbx_menu_reset {} {
    if {$::tbx_open ne "" && [winfo exists $::tbx_open]} { $::tbx_open configure -background $::panel }
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
    toplevel $t -background $::panel
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
    set need [expr {([llength $::tbx(i,$n)] + 1) * ($::tbx_bsize + 2) + 12}]
    set ::tbx(line,$n) [expr {$used + $need <= $avail ? [llength $lines] - 1 : [llength $lines]}]
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
tbx_menus_fill .mb.view.tbars
# first layout once the main window is on screen (floating toolbars need it there)
proc tbx_first_layout {} {
    if {![winfo ismapped .]} { after 100 tbx_first_layout; return }
    tbx_layout
}
after idle tbx_first_layout
