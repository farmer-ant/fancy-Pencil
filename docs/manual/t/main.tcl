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
after 500 plate
after 6000 {shot main}
after 6500 {.mb.draw post 175 32}
after 7500 {shot menu-draw; .mb.draw unpost}
after 8000 {sub {(progn (sssetfirst nil (ssget "X" (quote ((0 . "CIRCLE") (8 . "HOLES"))))) (princ))}}
after 9500 {shot grips}
after 10000 {sub PROPERTIES}
after 12000 {shot properties}
after 12500 {emit R 700 450 760 520 0}
after 14000 {shot rightclick}
