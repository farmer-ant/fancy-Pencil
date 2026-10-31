;;;; Fancy Pencil — a 2D drafting program with an AutoLISP-compatible interpreter.
;;;; package.lisp

(defpackage :alisp-user
  (:use)
  (:import-from :cl #:nil #:t)
  (:documentation "Home package of every AutoLISP symbol. NIL and T are shared with CL."))

(defpackage :fancy-pencil
  (:use :cl)
  (:export #:start #:main #:run-tty #:run-lisp-file #:alisp-eval-string))

(in-package :fancy-pencil)

(defparameter *version* "0.3.74")
(defparameter *program-name* "Fancy Pencil")

;; view and UI state shared by every module (defined early so all files see them as special)
(defvar *ui* :tty "Either :TTY or :GUI.")
(defvar *vx* 6d0) (defvar *vy* 4.5d0) (defvar *vscale* 60d0)
(defvar *cw* 1000) (defvar *ch* 600)
;; tiled model viewports (VPORTS): *cw* x *ch* is the active viewport, at *tx*,*ty* on the canvas,
;; which is *full-cw* x *full-ch*
(defvar *tx* 0) (defvar *ty* 0) (defvar *full-cw* 1000) (defvar *full-ch* 600)
(defvar *last-command* nil "Command repeated by Enter at an empty prompt.")
(defvar *lisp-buffer* "" "Unfinished AutoLISP typed at the command line.")
(defvar *plotting* nil "True while generating a plot (turns off screen-only simplifications).")
(defvar *dimline-snap* nil
  "While placing or dragging a dimension line: a function (cursor) -> the line's angle. The cursor
then snaps onto the dimension line of an existing parallel dimension, so they line up.")
(defvar *dimline-exclude* nil "The dimension being edited (not a snap target).")
(defvar *dimline-match* nil "(from to) of the dimension line last snapped to, for drawing the guide.")
(defvar *local-cache* (make-hash-table :test #'eq)
  "ename -> (frame world-data local-data): each object as the current working plane sees it.")
(defvar *planar-only* nil "Bound true in commands that can only work on objects in the working plane.")
(defparameter *planar-commands*
  '("TRIM" "EXTEND" "OFFSET" "FILLET" "CHAMFER" "BREAK" "BREAKATPOINT" "LENGTHEN" "JOIN" "PEDIT" "STRETCH"
    "EXPLODE" "DIVIDE" "MEASURE" "SPLINEDIT" "BOUNDARY" "HATCH" "BHATCH")
  "Commands that work on an object's shape in its own plane: they skip objects lying in other planes.")
(defvar *view* nil
  "NIL for the plan (Top) view, else (name right up dir): the 3D view direction, with RIGHT and
UP the world directions that point right and up on the screen and DIR pointing at the viewer.")
(defvar *vstyle-override* nil "Bound to a visual style name to draw with it instead of VSCURRENT (viewports).")
(defvar *prims-for-display* nil "True while plot primitives are made for the screen (layers that don't plot still show).")
(defvar *write-paper* nil "True while paper space objects are written to a DXF file (group 67 = 1).")
(defvar *ring-centers* nil "Centre snaps found from the round edges of solids (count as on their edge).")
(defvar *ring-circles* nil "Round edges of solids near the cursor: (centre radius normal).")
(defvar *command-word* "" "The command as typed (\"-HATCH\" for the command-line version of HATCH).")
(defvar *quick-shown* nil "True while the Quick Properties panel is open.")
(defvar *undo-ran* 0 "How many times U/UNDO/REDO have run (never rebound, so every thread sees it).")
