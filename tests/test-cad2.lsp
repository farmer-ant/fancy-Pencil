;;; More command tests: trim/extend/break/join/array/polygon/ellipse/stretch/dims/script/offset/chamfer
(setq *pass* 0 *fail* 0)
(defun check (name got want)
  (if (equal got want 1e-6) (setq *pass* (1+ *pass*))
    (progn (setq *fail* (1+ *fail*))
           (princ (strcat "\nFAIL " name ": got " (vl-prin1-to-string got) " want " (vl-prin1-to-string want))))))
(defun pt= (a b) (< (distance a b) 1e-6))
(defun g (e c) (cdr (assoc c (entget e))))
(defun count-type (ty / s) (setq s (ssget "X" (list (cons 0 ty)))) (if s (sslength s) 0))
(setvar "OSMODE" 0) (setvar "CMDECHO" 0)

;; TRIM a circle between two lines -> arc
(command "circle" '(0 0) 5) (setq c (entlast))
(command "line" '(-10 0) '(10 0) "") (setq l1 (entlast))
(command "trim" l1 "" (list c '(0 5)) "")
(check "trim circle->arc" (g (entlast) 0) "ARC")
(check "trim circle keeps lower" (pt= (polar (g (entlast) 10) (* 1.5 pi) 5) '(0 -5 0)) T)
;; EXTEND a line to a boundary
(command "line" '(20 -10) '(20 10) "") (setq b (entlast))
(command "line" '(12 3) '(15 3) "") (setq ex (entlast))
(command "extend" b "" (list ex '(14.9 3)) "")
(check "extend line" (pt= (g ex 11) '(20 3 0)) T)
;; BREAK a line
(command "line" '(0 20) '(10 20) "") (setq bl (entlast))
(command "break" (list bl '(3 20)) '(6 20))
(check "break left" (pt= (g bl 11) '(3 20 0)) T)
(check "break right" (pt= (g (entlast) 10) '(6 20 0)) T)
;; JOIN lines + arc into a polyline
(command "line" '(0 30) '(10 30) "") (setq j1 (entlast))
(command "line" '(10 30) '(10 35) "") (setq j2 (entlast))
(command "line" '(10 35) '(0 35) "") (setq j3 (entlast))
(command "join" j1 j2 j3 "")
(check "join" (g (entlast) 0) "LWPOLYLINE")
(check "join verts" (g (entlast) 90) 4)
;; ARRAY rectangular 2x3
(setq n0 (count-type "CIRCLE"))
(command "circle" '(50 0) 1) (setq ac (entlast))
(command "array" ac "" "R" 2 3 5 4)
(check "array rect" (- (count-type "CIRCLE") n0) 6)
;; polar array of 4
(command "circle" '(80 10) 1) (setq pc (entlast))
(command "array" pc "" "P" '(80 0) 4 360 "Y")
(check "array polar" (pt= (g (entlast) 10) '(90 0 0)) T)
;; POLYGON
(command "polygon" 6 '(0 60) "I" 5)
(check "polygon sides" (g (entlast) 90) 6)
;; ELLIPSE
(command "ellipse" '(0 80) '(10 80) 2)
(check "ellipse ratio" (g (entlast) 40) 0.4)
;; STRETCH: move right end of a line
(command "line" '(0 100) '(10 100) "") (setq sl (entlast))
(command "stretch" "C" '(8 98) '(12 102) "" '(0 0) '(5 0))
(check "stretch C" (pt= (g sl 11) '(15 100 0)) T)
(command "line" '(0 110) '(10 110) "") (setq sl2 (entlast))
(command "stretch" '(12 112) '(8 108) "" '(0 0) '(5 0))
(check "stretch" (pt= (g sl2 11) '(15 110 0)) T)
(check "stretch keeps start" (pt= (g sl2 10) '(0 110 0)) T)
;; DIMLINEAR
(command "dimlinear" '(0 120) '(10 120) '(5 118))
(check "dim measurement" (g (entlast) 42) 10.0)
(check "dim block" (wcmatch (g (entlast) 2) "`*D*") T)
;; moving a dimension regenerates its block
(setq dm (entlast) old (g dm 2))
(command "move" dm "" '(0 0) '(0 5))
(check "dim moved" (pt= (g dm 13) '(0 125 0)) T)
;; OFFSET a polyline
(command "rectang" '(0 140) '(10 150)) (setq rp (entlast))
(command "offset" 1 (list rp '(10 145)) '(20 145) "")
(check "offset pline" (pt= (cdr (assoc 10 (entget (entlast)))) '(-1.0 139.0)) T)
;; CHAMFER
(command "line" '(30 140) '(40 140) "") (setq c1 (entlast))
(command "line" '(30 140) '(30 150) "") (setq c2 (entlast))
(command "chamfer" "D" 2 2 (list c1 '(35 140)) (list c2 '(30 145)))
(check "chamfer line" (pt= (g (entlast) 10) '(32 140 0)) T)
;; SCRIPT
(setq f (open "/tmp/ld-test.scr" "w"))
(write-line "circle 200,200 3" f)
(write-line "line 200,200 210,210 " f)
(close f)
(setq nl (count-type "LINE"))
(command "script" "/tmp/ld-test.scr")
(princ (strcat "\nCommand tests 2: " (itoa *pass*) " passed, " (itoa *fail*) " failed.\n"))
(princ)
