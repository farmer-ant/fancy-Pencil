;;;; LispDraft — a 2D drafting program with an AutoLISP-compatible interpreter.
;;;; package.lisp

(defpackage :alisp-user
  (:use)
  (:import-from :cl #:nil #:t)
  (:documentation "Home package of every AutoLISP symbol. NIL and T are shared with CL."))

(defpackage :lispdraft
  (:use :cl)
  (:export #:start #:main #:run-tty #:run-lisp-file #:alisp-eval-string))

(in-package :lispdraft)

(defparameter *version* "0.1")
(defparameter *program-name* "LispDraft")
