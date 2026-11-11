proc bgerror {m} { set f [open /tmp/shoterr.log a]; puts $f "$m\n$::errorInfo"; close $f }
proc sub {c} { .cmd.e delete 0 end; .cmd.e insert 0 $c; cmdsubmit }
proc shot {n} { update; exec import -window root /home/claude/manual/shots/$n.png
  set f [open /home/claude/manual/shots/$n.geo w]
  foreach w [wm stackorder .] { if {$w ne "." && [winfo ismapped $w]} { puts $f "$w [winfo class $w] [winfo width $w] [winfo height $w] [winfo rootx $w] [winfo rooty $w]" } }
  foreach w [winfo children .] { if {[winfo class $w] eq "Menu" && [winfo ismapped $w]} { puts $f "$w Menu [winfo width $w] [winfo height $w] [winfo rootx $w] [winfo rooty $w]" } }
  foreach w {.mb.draw} { if {[winfo exists $w] && [winfo ismapped $w]} { puts $f "$w Menu [winfo width $w] [winfo height $w] [winfo rootx $w] [winfo rooty $w]" } }
  close $f }
proc plate {} { sub {(progn (load "/home/claude/lispdraft/examples/demo-plate.lsp") (princ))}; after 1500 {sub PLATE}; after 4000 {sub {ZOOM E}} }
wm geometry . 1600x1000+0+0
after 1000 {sub {(progn (command "box" (quote (0 0 0)) (quote (10 6 0)) 2) (command "cylinder" (quote (5 3 2)) 1.5 4) (command "sphere" (quote (14 3 2)) 2) (command "cone" (quote (20 3 0)) 2 5) (command "torus" (quote (5 12 1)) 3 0.8) (command "-view" "SEiso") (command "zoom" "e") (princ))}}
after 6000 {sub {VSCURRENT E}}
after 7500 {shot solids}
after 8000 {sub {VSCURRENT W}}
after 9000 {shot solids-wire}
