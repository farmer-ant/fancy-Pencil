;;; Tests for Fancy Pencil 0.3 features: MTEXT box/width/justification, TEXT, TEXTEDIT.
;;; (The in-place editor itself is GUI-only; from AutoLISP these commands take text on the command line.)
(setq *pass* 0 *fail* 0)
(defun check (name got want)
  (if (equal got want 1e-6) (setq *pass* (1+ *pass*))
    (progn (setq *fail* (1+ *fail*))
           (princ (strcat "\nFAIL " name ": got " (vl-prin1-to-string got) " want " (vl-prin1-to-string want))))))
(defun g (e c) (cdr (assoc c (entget e))))
(setvar "OSMODE" 0) (setvar "CMDECHO" 0) (setvar "TEXTSIZE" 0.25)

;; MTEXT from two corners: insertion at top-left, width from the box
(command "mtext" '(1 8) '(7 5) "Hello" "World" "")
(setq m (entlast))
(check "mtext type" (g m 0) "MTEXT")
(check "mtext content" (g m 1) "Hello\\PWorld")
(check "mtext width" (g m 41) 6.0)
(check "mtext insertion" (g m 10) '(1.0 8.0 0.0))
(check "mtext attach" (g m 71) 1)
(check "mtext height" (g m 40) 0.25)

;; corners given bottom-right first still give a top-left box
(command "mtext" '(7 0) '(1 3) "x" "")
(check "mtext box normalised" (g (entlast) 10) '(1.0 3.0 0.0))

;; Height and Justify options
(command "mtext" '(0 0) "h" 0.5 "j" "MC" '(4 2) "Centered" "")
(setq m (entlast))
(check "mtext height option" (g m 40) 0.5)
(check "mtext MC attach" (g m 71) 5)
(check "mtext MC insertion" (g m 10) '(2.0 1.0 0.0))

;; Width option (no box)
(command "mtext" '(10 10) "w" 3 "Wide" "")
(check "mtext width option" (g (entlast) 41) 3.0)

;; empty text makes nothing
(setq before (entlast))
(command "mtext" '(0 0) '(2 2) "")
(check "empty mtext not created" (eq (entlast) before) T)

;; TEXT and TEXTEDIT
(command "text" '(0 -2) 0.3 0 "Single" "")
(setq t1 (entlast))
(check "dtext" (g t1 1) "Single")
(command "textedit" '(0.2 -1.9) "Changed" "")
(check "textedit" (g t1 1) "Changed")

(princ (strcat "\nv0.3 tests: " (itoa *pass*) " passed, " (itoa *fail*) " failed.\n"))
(princ)
