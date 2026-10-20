;;; tools.lsp — everyday AutoLISP utilities, written exactly as they would be for AutoCAD.
;;; Load with APPLOAD (or put (load "tools") in acaddoc.lsp), then run TLEN, BCIRC, NUM, LAYOFF, CL.

;; TLEN — total length of selected lines, arcs, circles and polylines
(defun c:tlen (/ ss i e d typ total)
  (setq total 0.0)
  (if (setq ss (ssget '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (entget (ssname ss i)) typ (cdr (assoc 0 e)))
        (setq total
          (+ total
             (cond ((= typ "LINE") (distance (cdr (assoc 10 e)) (cdr (assoc 11 e))))
                   ((= typ "CIRCLE") (* 2 pi (cdr (assoc 40 e))))
                   ((= typ "ARC")
                    (setq d (- (cdr (assoc 51 e)) (cdr (assoc 50 e))))
                    (if (minusp d) (setq d (+ d (* 2 pi))))
                    (* d (cdr (assoc 40 e))))
                   ((= typ "LWPOLYLINE") (pl-length e))
                   (t 0.0))))
        (setq i (1+ i)))
      (princ (strcat "\nTotal length of " (itoa (sslength ss)) " object(s): " (rtos total 2 4))))
    (princ "\nNothing selected."))
  (princ))

(defun pl-length (e / pts len)
  (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) e)) len 0.0)
  (if (= 1 (logand 1 (cdr (assoc 70 e)))) (setq pts (append pts (list (car pts)))))
  (while (cadr pts) (setq len (+ len (distance (car pts) (cadr pts))) pts (cdr pts)))
  len)

;; BCIRC — bolt circle: holes evenly spaced on a pitch circle
(defun c:bcirc (/ *error* os ctr pcd n hole a i)
  (defun *error* (msg)
    (if os (setvar "OSMODE" os))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ))
  (setq os (getvar "OSMODE"))
  (if (setq ctr (getpoint "\nCenter of bolt circle: "))
    (progn
      (initget 7)
      (setq pcd (getdist ctr "\nPitch circle diameter: "))
      (initget 7)
      (setq n (getint "\nNumber of holes: "))
      (initget 6)
      (setq hole (cond ((getdist "\nHole diameter <0.5>: ")) (0.5)))
      (initget "Yes No")
      (setq a (if (= (getkword "\nDraw pitch circle? [Yes/No] <Yes>: ") "No") nil T))
      (setvar "OSMODE" 0)
      (if a (command "_.circle" ctr (/ pcd 2.0)))
      (setq i 0)
      (repeat n
        (command "_.circle" (polar ctr (* i (/ (* 2 pi) n)) (/ pcd 2.0)) (/ hole 2.0))
        (setq i (1+ i)))
      (setvar "OSMODE" os)
      (princ (strcat "\n" (itoa n) " holes placed."))))
  (princ))

;; NUM — pick points to place incrementing numbers
(defun c:num (/ n p h)
  (setq n (cond ((getint "\nStarting number <1>: ")) (1))
        h (getvar "TEXTSIZE"))
  (while (setq p (getpoint (strcat "\nPlace number " (itoa n) " (Enter to finish): ")))
    (entmake (list '(0 . "TEXT") (cons 10 p) (cons 40 h) (cons 1 (itoa n))))
    (setq n (1+ n)))
  (princ))

;; LAYOFF — turn off the layer of a picked object
(defun c:layoff (/ sel lay)
  (if (setq sel (entsel "\nSelect object on layer to turn off: "))
    (progn
      (setq lay (cdr (assoc 8 (entget (car sel)))))
      (if (= (strcase lay) (strcase (getvar "CLAYER")))
        (princ "\nCannot turn off the current layer.")
        (progn (command "_.-layer" "_off" lay "") (princ (strcat "\nLayer " lay " is off.")))))
    (princ "\nNothing selected."))
  (princ))

;; CL — draw center marks on a selected circle or arc
(defun c:cl (/ sel e c r ext)
  (if (and (setq sel (entsel "\nSelect circle or arc: "))
           (member (cdr (assoc 0 (setq e (entget (car sel))))) '("CIRCLE" "ARC")))
    (progn
      (setq c (cdr (assoc 10 e)) r (cdr (assoc 40 e)) ext (* r 1.2))
      (entmake (list '(0 . "LINE") (cons 10 (polar c pi ext)) (cons 11 (polar c 0 ext))))
      (entmake (list '(0 . "LINE") (cons 10 (polar c (* 1.5 pi) ext)) (cons 11 (polar c (/ pi 2) ext)))))
    (princ "\nPlease select a circle or arc."))
  (princ))

(princ "\nLoaded: TLEN  BCIRC  NUM  LAYOFF  CL")
(princ)
