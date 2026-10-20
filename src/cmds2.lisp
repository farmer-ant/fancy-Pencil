;;;; cmds2.lisp — LINETYPE, LWEIGHT, HATCH, HATCHEDIT, HPSET, REVCLOUD.

(in-package :lispdraft)

;;; ---------- linetypes ----------

(defun load-linetypes (pattern file)
  "Load linetypes whose names match PATTERN from FILE (or the built-in acad.lin set). Returns names loaded."
  (let* ((path (and file (not (string-equal (file-namestring file) "acad.lin")) (find-file-in-support file)))
         (lib (if path
                  (parse-lin-file path)
                  (mapcar (lambda (e) (make-ltype :name (first e) :desc (second e) :pattern (mapcar #'d (cddr e))))
                          *standard-linetypes*)))
         (loaded nil))
    (dolist (lt lib (nreverse loaded))
      (when (and (ltype-pattern lt) (wcmatch (string-upcase (ltype-name lt)) (string-upcase pattern)))
        (add-ltype lt)
        (push (ltype-name lt) loaded)))))

(defcmd "LINETYPE" ("LT" "LTYPE" "-LINETYPE" "DDLTYPE") "Loads, sets and lists linetypes."
  (if (and (gui-p) (not (from-lisp-p)))
      (let* ((loaded (mapcar #'ltype-name (drawing-ltypes *dwg*)))
             (r (gui-form "Linetype Manager"
                          (list (list "LOAD" "Load linetype (or * for all)" "" :combo (cons "*" (mapcar #'first *standard-linetypes*)))
                                (list "CURRENT" "Current linetype" (ltype-label (sysvar "CELTYPE")) :combo (append '("ByLayer" "ByBlock" "Continuous") loaded))
                                (list "LTSCALE" "Global scale factor (LTSCALE)" (fmt-len (sysvar "LTSCALE")) :entry)
                                (list "CELTSCALE" "Current object scale (CELTSCALE)" (fmt-len (sysvar "CELTSCALE")) :entry))
                          "OK")))
        (when r
          (let ((ld (cdr (assoc "LOAD" r :test #'string=))) (cur (cdr (assoc "CURRENT" r :test #'string=)))
                (gs (parse-number-text (or (cdr (assoc "LTSCALE" r :test #'string=)) "")))
                (cs (parse-number-text (or (cdr (assoc "CELTSCALE" r :test #'string=)) ""))))
            (when (plusp (length ld))
              (out (format nil "~D linetype~:P loaded.~%" (length (load-linetypes ld nil)))))
            (when (and cur (plusp (length cur)))
              (ensure-ltype cur)
              (setf (sysvar "CELTYPE") (cond ((string-equal cur "ByLayer") "BYLAYER") ((string-equal cur "ByBlock") "BYBLOCK") (t cur))))
            (when (and gs (> gs 0)) (setf (sysvar "LTSCALE") (d gs)))
            (when (and cs (> cs 0)) (setf (sysvar "CELTSCALE") (d cs)))
            (touch))))
      (loop
        (let ((k (gkw "Enter an option [?/Create/Load/Set]: " '("?" "Create" "Load" "Set"))))
          (cond
            ((null k) (return))
            ((equal k "?")
             (out (format nil "~%Linetypes defined in the drawing:~%  CONTINUOUS        Solid line~%"))
             (dolist (lt (drawing-ltypes *dwg*)) (out (format nil "  ~16A  ~A~%" (ltype-name lt) (ltype-desc lt)))))
            ((equal k "Load")
             (let* ((pat (gstring "Enter linetype(s) to load: " t))
                    (file (gstring "Enter name of linetype file <acad.lin>: " t))
                    (names (load-linetypes (if (plusp (length pat)) pat "*") (if (plusp (length file)) file nil))))
               (if names (out (format nil "Linetype~P ~{~A~^, ~} loaded.~%" (length names) names))
                   (out (format nil "No matching linetypes found.~%")))))
            ((equal k "Set")
             (let ((n (gstring (format nil "Specify linetype name or [?] <~A>: " (sysvar "CELTYPE")) t)))
               (when (plusp (length n))
                 (if (ensure-ltype n)
                     (setf (sysvar "CELTYPE") (if (member n '("BYLAYER" "BYBLOCK") :test #'string-equal) (string-upcase n) n))
                     (out (format nil "Linetype ~A not found. Use the Load option to load it.~%" n))))))
            ((equal k "Create")
             (let* ((n (gstring "Enter name of linetype to create: " t))
                    (desc (gstring "Descriptive text: " t))
                    (pat (gstring "Enter linetype pattern (on next line): A," t))
                    (vals (remove nil (mapcar #'parse-number-text (split-on pat #\,)))))
               (when (and (plusp (length n)) vals)
                 (add-ltype (make-ltype :name (string-upcase n) :desc desc :pattern (mapcar #'d vals)))
                 (out (format nil "Linetype ~A created.~%" (string-upcase n)))))))))))

(defcmd "LTSCALE" ("LTS") "Sets the global linetype scale factor."
  (let ((v (greal (format nil "Enter new linetype scale factor <~A>: " (fmt-len (sysvar "LTSCALE"))) :flags 6)))
    (when v (setf (sysvar "LTSCALE") (d v)) (out (format nil "Regenerating model.~%")) (when (gui-p) (gui-regen)))))

(defcmd "LWEIGHT" ("LW" "LINEWEIGHT" "-LWEIGHT") "Sets the current lineweight and lineweight display."
  (if (and (gui-p) (not (from-lisp-p)))
      (let ((r (gui-form "Lineweight Settings"
                         (list (list "LW" "Current lineweight" (lineweight-label (sysvar "CELWEIGHT")) :combo (lweight-choices))
                               (list "DISP" "Display lineweight" (if (eql (sysvar "LWDISPLAY") 1) "Yes" "No") :combo '("Yes" "No")))
                         "OK")))
        (when r
          (let ((lw (parse-lweight-label (cdr (assoc "LW" r :test #'string=)))))
            (when lw (setf (sysvar "CELWEIGHT") lw)))
          (setf (sysvar "LWDISPLAY") (if (string-equal (cdr (assoc "DISP" r :test #'string=)) "Yes") 1 0))
          (gui-regen) (gui-sync)))
      (progn
        (out (format nil "Current lineweight: ~A~%" (lineweight-label (sysvar "CELWEIGHT"))))
        (let ((s (gstring "Enter default lineweight for new objects or [?]: ")))
          (cond ((string= s "?") (out (format nil "~{~A~^, ~}~%" (lweight-choices))))
                ((plusp (length s))
                 (let ((lw (or (parse-lweight-label s)
                               (let ((n (parse-number-text s))) (and n (round (* 100 n)))))))
                   (if lw (setf (sysvar "CELWEIGHT") lw) (out (format nil "Invalid lineweight.~%"))))))))))

;;; ---------- hatch ----------

(defun hatch-settings-prompt ()
  (if (and (gui-p) (not (from-lisp-p)))
      (let ((r (gui-form "Hatch Pattern"
                         (list (list "P" "Pattern" (sysvar "HPNAME") :combo (mapcar #'first *hatch-patterns*))
                               (list "S" "Scale" (fmt-len (sysvar "HPSCALE")) :entry)
                               (list "A" "Angle" (fmt-ang (sysvar "HPANG")) :entry))
                         "OK")))
        (when r
          (let ((p (cdr (assoc "P" r :test #'string=)))
                (s (parse-number-text (cdr (assoc "S" r :test #'string=))))
                (a (angtof (cdr (assoc "A" r :test #'string=)))))
            (when (hatch-pattern-def p) (setf (sysvar "HPNAME") (string-upcase p)))
            (when (and s (> s 0)) (setf (sysvar "HPSCALE") (d s)))
            (when a (setf (sysvar "HPANG") a)))))
      (let ((p (gstring (format nil "Enter a pattern name or [?/Solid] <~A>: " (sysvar "HPNAME")))))
        (cond ((string= p "?") (dolist (h *hatch-patterns*) (out (format nil "  ~10A ~A~%" (first h) (second h)))))
              ((string-equal p "S") (setf (sysvar "HPNAME") "SOLID"))
              ((plusp (length p))
               (if (hatch-pattern-def p) (setf (sysvar "HPNAME") (string-upcase p)) (out (format nil "Unknown pattern ~A.~%" p)))))
        (unless (solid-pattern-p (sysvar "HPNAME"))
          (let ((s (greal (format nil "Specify a scale for the pattern <~A>: " (fmt-len (sysvar "HPSCALE"))) :flags 6))
                (a (gangle (format nil "Specify an angle for the pattern <~A>: " (fmt-ang (sysvar "HPANG"))))))
            (when s (setf (sysvar "HPSCALE") (d s)))
            (when a (setf (sysvar "HPANG") a)))))))

(defcmd "HPSET" () "Sets the hatch pattern, scale and angle used by HATCH." (hatch-settings-prompt))

(defcmd "HATCH" ("H" "BH" "BHATCH" "-HATCH" "GRADIENT") "Fills an enclosed area with a hatch pattern or solid fill."
  (let ((e nil) (loops nil) (history nil))
    (labels ((update ()
               (cond ((and e (null loops)) (delete-entity e) (setf e nil))
                     (e (set-entity-data e (append (list (cons 0 "HATCH") (cons 5 (ename-handle e)))
                                                   (rest (make-hatch-data loops)) (common-props (ent-data e)))))
                     (loops (setf e (add (make-hatch-data loops)))))))
      (loop
        (out (format nil "Pattern: ~A   Scale: ~A   Angle: ~A~%" (sysvar "HPNAME") (fmt-len (sysvar "HPSCALE")) (fmt-ang (sysvar "HPANG"))))
        (let ((r (gpoint "Pick internal point or [Select objects/Properties/Undo]: "
                         :keywords '("Select" "Properties" "Undo"))))
          (cond
            ((null r) (return))
            ((equal r "Properties") (hatch-settings-prompt) (when e (update)))
            ((equal r "Undo") (when history (setf loops (pop history)) (update)))
            ((equal r "Select")
             (let* ((ents (select-objects :editable nil))
                    (new (remove nil (mapcar #'closed-entity-loop ents))))
               (if new (progn (push loops history) (setf loops (append loops new)) (update))
                   (out (format nil "No closed boundaries were selected.~%")))))
            ((pointp r)
             (out (format nil "Analyzing the selected data...~%"))
             (let ((found (detect-boundary r)))
               (if found
                   (progn (push loops history) (setf loops (append loops found)) (update)
                          (when (gui-p) (gui-regen)))
                   (out (format nil "Valid hatch boundary not found.~%")))))))))))

(defcmd "HATCHEDIT" ("HE" "-HATCHEDIT") "Changes the pattern, scale or angle of existing hatches."
  (let ((e (sel-entity (get-user-input :entity :prompt "Select hatch object: "))))
    (if (not (and e (equal (ent-type e) "HATCH")))
        (out (format nil "Object selected is not a hatch.~%"))
        (let* ((data (ent-data e))
               (saved (list (sysvar "HPNAME") (sysvar "HPSCALE") (sysvar "HPANG"))))
          (setf (sysvar "HPNAME") (or (dxf 2 data) "ANSI31") (sysvar "HPSCALE") (d (or (dxf 41 data) 1)) (sysvar "HPANG") (d (or (dxf 52 data) 0)))
          (hatch-settings-prompt)
          (set-entity-data e (append (rem-dxf :pattern (rem-dxf 2 (rem-dxf 70 (rem-dxf 41 (rem-dxf 52 data)))))
                                     (list (cons 2 (sysvar "HPNAME")) (cons 70 (if (solid-pattern-p (sysvar "HPNAME")) 1 0))
                                           (cons 41 (d (sysvar "HPSCALE"))) (cons 52 (d (sysvar "HPANG"))))))
          (destructuring-bind (n s a) saved (setf (sysvar "HPNAME") n (sysvar "HPSCALE") s (sysvar "HPANG") a))))))

;;; ---------- REVCLOUD ----------

(defun cloud-verts (pts arc-len closed)
  "Polyline vertices with outward arc bulges every ARC-LEN along PTS."
  (let ((verts nil))
    (loop for (a b) on (if closed (append pts (list (first pts))) pts) while b
          do (let* ((len (dist2d a b)) (n (max 1 (round len arc-len))))
               (loop for i from 0 below n do (push (list (at-param a b (/ i n)) -0.5d0) verts))))
    (unless closed (push (list (car (last pts)) 0d0) verts))
    (let ((v (nreverse verts)))
      ;; arcs bulge outward: flip the sign for counter-clockwise outlines
      (if (and closed (> (loop-signed-area (mapcar #'first v)) 0))
          v
          (mapcar (lambda (x) (list (first x) (- (second x)))) v)))))

(defcmd "REVCLOUD" ("RC") "Creates a revision cloud."
  (let ((arc (or (sysvar "REVCLOUDARC") 0.5d0)))
    (out (format nil "Minimum arc length: ~A   Style: Normal~%" (fmt-len arc)))
    (let ((r (gpoint "Specify first point or [Arc length/Object/Rectangular] <Object>: " :keywords '("Arc" "Object" "Rectangular"))))
      (when (equal r "Arc")
        (let ((a (gdist (format nil "Specify minimum length of arc <~A>: " (fmt-len arc)) :flags 6)))
          (when a (setf arc a (sysvar "REVCLOUDARC") (d a))))
        (setf r (gpoint "Specify first point or [Object/Rectangular] <Object>: " :keywords '("Object" "Rectangular"))))
      (cond
        ((or (null r) (equal r "Object"))
         (let ((e (sel-entity (get-user-input :entity :prompt "Select object: "))))
           (when e
             (let* ((lp (closed-entity-loop e))
                    (pts (if lp (loop-polygon-directed lp 8)
                             (and (equal (ent-type e) "LWPOLYLINE") (mapcar #'first (lwpoly-vertices (ent-data e)))))))
               (when pts
                 (add (append (make-lwpoly-data (cloud-verts pts arc (and lp t)) (and lp t)) (common-props (ent-data e))))
                 (delete-entity e))))))
        ((equal r "Rectangular")
         (let* ((a (gpoint "Specify first corner point: "))
                (b (get-user-input :corner :base a :prompt "Specify opposite corner: ")))
           (when (pointp b)
             (add (make-lwpoly-data (cloud-verts (list a (pt (px b) (py a)) b (pt (px a) (py b))) arc t) t)))))
        ((pointp r)
         (let ((pts (list r)))
           (loop for p = (gpoint "Guide crosshairs along cloud path, or pick points (Enter to close): " :base (first pts)
                                 :drag (lambda (cur) (lwpoly-pieces (make-lwpoly-data (cloud-verts (reverse (cons cur pts)) arc nil) nil))))
                 while (pointp p) do (push p pts))
           (when (> (length pts) 2)
             (add (make-lwpoly-data (cloud-verts (nreverse pts) arc t) t)))))))))
