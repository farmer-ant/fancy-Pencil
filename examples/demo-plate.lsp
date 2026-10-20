;;; demo-plate.lsp — draws a dimensioned mounting plate using only AutoLISP + COMMAND.
;;; Load it with APPLOAD, then type PLATE.
(defun c:plate (/ os w h r ctr i a p)
  (setq os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (command "-linetype" "load" "CENTER,HIDDEN" "" "")
  (command "-layer" "make" "OUTLINE" "color" "7" "OUTLINE" "lweight" "0.50" "OUTLINE"
           "make" "HOLES" "color" "4" "HOLES"
           "make" "CENTER" "color" "1" "CENTER" "ltype" "CENTER" "CENTER"
           "make" "DIMS" "color" "2" "DIMS"
           "make" "HATCH" "color" "8" "HATCH"
           "make" "TEXT" "color" "3" "TEXT" "")
  (setvar "LTSCALE" 0.5)
  (setvar "LWDISPLAY" 1)
  (setq w 8.0 h 5.0 r 0.5)
  ;; outline: rectangle with filleted corners drawn as a polyline
  (setvar "CLAYER" "OUTLINE")
  (command "pline" (list r 0) (list (- w r) 0) "A" (list w r) "L" (list w (- h r))
           "A" (list (- w r) h) "L" (list r h) "A" (list 0 (- h r)) "L" (list 0 r) "A" "C")
  ;; bolt circle
  (setq ctr (list (/ w 2) (/ h 2)))
  (setvar "CLAYER" "HOLES")
  (command "circle" ctr 1.25)
  (setq i 0)
  (repeat 6
    (setq a (* i (/ pi 3)) p (polar ctr a 1.9))
    (command "circle" p 0.2)
    (setq i (1+ i)))
  (foreach p (list '(0.6 0.6) (list (- w 0.6) 0.6) (list (- w 0.6) (- h 0.6)) (list 0.6 (- h 0.6)))
    (command "circle" p 0.25))
  ;; hatched ring between the bore and the bolt circle (the bolt holes become islands)
  (setvar "CLAYER" "CENTER")
  (command "circle" ctr 1.9)
  (setvar "CLAYER" "HATCH")
  (command "-hatch" "P" "ANSI31" 0.5 0 (polar ctr (/ pi 6) 1.55) "")
  ;; center lines
  (setvar "CLAYER" "CENTER")
  (command "line" (polar ctr pi 2.6) (polar ctr 0 2.6) "")
  (command "line" (polar ctr (* 1.5 pi) 2.4) (polar ctr (/ pi 2) 2.4) "")
  ;; dimensions and notes
  (setvar "CLAYER" "DIMS")
  (command "dimlinear" '(0 0) (list w 0) (list (/ w 2) -0.8))
  (command "dimlinear" (list w 0) (list w h) (list (+ w 0.8) (/ h 2)))
  (command "dimradius" (list (ssname (ssget "X" '((0 . "CIRCLE") (40 . 1.25))) 0) (polar ctr 0 1.25)) (polar ctr (* 0.3 pi) 2.9))
  (setvar "CLAYER" "TEXT")
  (command "text" (list 0 (+ h 0.5)) 0.25 0 "MOUNTING PLATE  6061-T6  t=0.25" "")
  (command "text" '(0 -1.6) 0.16 0 "6X %%c0.40 ON %%c3.80 B.C.   4X %%c0.50 THRU" "")
  (setvar "CLAYER" "0")
  (setvar "OSMODE" os)
  (command "zoom" "e")
  (princ "\nPlate drawn.")
  (princ))
(princ "\nType PLATE to draw the demo part.")
(princ)
