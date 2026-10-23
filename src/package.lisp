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

(defparameter *version* "0.3.23")
(defparameter *program-name* "Fancy Pencil")

;; view and UI state shared by every module (defined early so all files see them as special)
(defvar *ui* :tty "Either :TTY or :GUI.")
(defvar *vx* 6d0) (defvar *vy* 4.5d0) (defvar *vscale* 60d0)
(defvar *cw* 1000) (defvar *ch* 600)
(defvar *last-command* nil "Command repeated by Enter at an empty prompt.")
(defvar *lisp-buffer* "" "Unfinished AutoLISP typed at the command line.")
(defvar *plotting* nil "True while generating a plot (turns off screen-only simplifications).")
(defvar *dimline-snap* nil
  "While placing or dragging a dimension line: a function (cursor) -> the line's angle. The cursor
then snaps onto the dimension line of an existing parallel dimension, so they line up.")
(defvar *dimline-exclude* nil "The dimension being edited (not a snap target).")
(defvar *dimline-match* nil "(from to) of the dimension line last snapped to, for drawing the guide.")
(defvar *view* nil
  "NIL for the plan (Top) view, else (name right up dir): the 3D view direction, with RIGHT and
UP the world directions that point right and up on the screen and DIR pointing at the viewer.")
(defvar *undo-ran* 0 "How many times U/UNDO/REDO have run (never rebound, so every thread sees it).")
