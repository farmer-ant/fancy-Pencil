;;;; visual.lisp - 3D orbit and visual styles.
;;;;
;;;; 3DORBIT (ORBIT, 3DO): drag with the mouse to turn the view around the drawing (left-right
;;;; turns it about the vertical, up-down tilts it); click or release to stop, drag again, Enter
;;;; to finish. The view stays as "Custom" until you pick a standard one.
;;;;
;;;; VSCURRENT (VS, SHADEMODE): Wireframe (every edge), Hidden (solids hide what is behind them),
;;;; Shaded, and Shaded with Edges. Faces are painted far to near, so nearer ones cover farther
;;;; ones; other objects are sorted in among them by their distance. Plots follow the style.

(in-package :fancy-pencil)

(defun visual-style ()
  (let ((v (sysvar "VSCURRENT")))
    (if (member v '("Hidden" "Shaded" "ShadedEdges") :test #'equal) v "Wireframe")))

(defun shading-light ()
  "Direction of the light: from over the viewer's shoulder."
  (if (projected-view-p)
      (unit3 (v+ (fourth *view*) (v+ (v* (third *view*) 0.45d0) (v* (second *view*) 0.25d0))))
      (unit3 (pt 0.25 0.45 1))))

(defun shade-hex (hex normal)
  (let* ((i (+ 0.28d0 (* 0.72d0 (abs (vdot3 (unit3 normal) (shading-light)))))))
    (destructuring-bind (r g b) (hex-rgb hex) (rgb-hex (* r i) (* g i) (* b i)))))

(defun entity-depth (e)
  "How near entity E is to the viewer (bigger = nearer), from its points."
  (let ((ds nil) (k 0))
    (dolist (pc (entity-pieces e))
      (when (> (incf k) 60) (return))
      (case (first pc)
        ((:seg :gseg) (push (view-depth (second pc)) ds) (push (view-depth (third pc)) ds))
        ((:pt :circle :arc) (push (view-depth (second pc)) ds))))
    (if ds (/ (reduce #'+ ds) (length ds)) 0d0)))

(defun screen-coords (pts) (loop for p in pts append (multiple-value-bind (x y) (w->s p) (list (fmtn x) (fmtn y)))))

(defun view-toward-viewer ()
  (if (projected-view-p) (fourth *view*) (pt 0 0 1)))

(defun mesh-face-items (e style)
  "(depth . canvas-commands) for each face of solid E in visual STYLE. Faces of a closed solid
that point away from the viewer are left out (they are hidden anyway)."
  (let* ((data (ent-data e))
         (tags (format nil "ent h~A" (ename-handle e)))
         (hl (gethash e *hl*))
         (color (if hl "#8fc4ff" (aci->hex (entity-aci data))))
         (edges (not (equal style "Shaded")))
         (toward (view-toward-viewer)))
    (multiple-value-bind (faces closed) (mesh-display-faces data)
      (loop for (ps n vis) in faces
            when (and n (or (not closed) (> (vdot3 n toward) -1d-9)))
              collect (let* ((fill (if (equal style "Hidden") (background-color) (shade-hex color n)))
                             (cmds (list (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline ~A -tags {~A}"
                                                 (screen-coords ps) fill (if (equal style "Hidden") "{}" fill) tags))))
                        (when edges
                          (loop for i from 0 below (length ps)
                                for a = (nth i ps) for b = (nth (mod (1+ i) (length ps)) ps)
                                do (when (nth i vis)
                                     (push (format nil ".c create line ~{~A~^ ~} -fill ~A~A -tags {~A}"
                                                   (screen-coords (list a b)) color (if hl " -dash {6 3}" "") tags)
                                           cmds))))
                        (cons (/ (reduce #'+ (mapcar #'view-depth ps)) (length ps)) (nreverse cmds)))))))

(defun mesh-silhouette-items (e style)
  "The outline of a curved solid as seen now: edges between a face turned toward the viewer and
one turned away (drawn in Hidden and Shaded with Edges), each just after its front face."
  (unless (equal style "Shaded")
    (let* ((data (ent-data e)) (toward (view-toward-viewer))
           (tags (format nil "ent h~A" (ename-handle e)))
           (color (if (gethash e *hl*) "#8fc4ff" (aci->hex (entity-aci data))))
           (edge-faces (make-hash-table :test #'equal)) (ends (make-hash-table :test #'equal)) (out nil))
      (multiple-value-bind (faces closed) (mesh-display-faces data)
        (when closed
          (dolist (f faces)
            (destructuring-bind (ps n vis) f
              (declare (ignore vis))
              (when n
                (let ((depth (/ (reduce #'+ (mapcar #'view-depth ps)) (length ps))))
                  (loop for (a b) on (append ps (list (first ps))) while b
                        do (let* ((ka (weld-key a)) (kb (weld-key b))
                                  (k (if (string< (prin1-to-string ka) (prin1-to-string kb)) (list ka kb) (list kb ka))))
                             (push (cons n depth) (gethash k edge-faces))
                             (setf (gethash k ends) (cons a b))))))))
          (maphash (lambda (k fs)
                     (when (= (length fs) 2)
                       (let ((f1 (plusp (vdot3 (car (first fs)) toward))) (f2 (plusp (vdot3 (car (second fs)) toward))))
                         (unless (eq f1 f2)
                           (let ((ab (gethash k ends)) (front (if f1 (first fs) (second fs))))
                             (push (cons (+ (cdr front) 1d-9)
                                         (list (format nil ".c create line ~{~A~^ ~} -fill ~A -tags {~A}"
                                                       (screen-coords (list (car ab) (cdr ab))) color tags)))
                                   out))))))
                   edge-faces)))
      out)))

(defun visual-style-cmds ()
  "Canvas commands for the whole drawing in the Hidden / Shaded styles, far to near."
  (let ((style (visual-style)) (items nil))
    (dolist (e (live-entities))
      (when (and (entity-visible-p e) (entity-in-view-p e))
        (if (equal (ent-type e) "POLYFACE")
            (setf items (nconc (mesh-face-items e style) (mesh-silhouette-items e style) items))
            (let ((cmds (entity-render-cmds e)))
              (when cmds (push (cons (entity-depth e) cmds) items))))))
    (loop for it in (stable-sort items #'< :key #'car) append (cdr it))))

(defun set-visual-style (style)
  (setf (sysvar "VSCURRENT") style)
  (out (format nil "Visual style: ~A.~%" (if (equal style "ShadedEdges") "Shaded with Edges" style)))
  (when (gui-p) (gui-regen) (tk "set ::vstyle ~A" style)))

(defcmd "VSCURRENT" ("VS" "SHADEMODE" "VISUALSTYLES") "Sets the visual style: Wireframe, Hidden, Shaded or Shaded with Edges."
  (let ((r (gkw (format nil "Enter an option [Wireframe/Hidden/Shaded/shaded with Edges] <~A>: " (visual-style))
                '("Wireframe" "Hidden" "Shaded" "Edges" "2dwireframe") nil)))
    (when r
      (set-visual-style (cond ((equal r "Edges") "ShadedEdges") ((equal r "2dwireframe") "Wireframe") (t r))))))

(defcmd "HIDE" ("HI") "Shows the drawing with hidden lines removed (visual style Hidden)." (set-visual-style "Hidden"))

;;; ---------- orbit ----------

(defun view-angles (v)
  "(values azimuth elevation) of view V (NIL = Top)."
  (let ((d (if v (fourth v) (pt 0 0 1))))
    (values (if (and (< (abs (px d)) 1d-9) (< (abs (py d)) 1d-9))
                (if v (atan (- (px (second v))) (py (second v))) (- (/ pi 2)))
                (atan (py d) (px d)))
            (asin (max -1d0 (min 1d0 (pz d)))))))

(defun orbit-view (az el)
  (let* ((lim (- (/ pi 2) 1d-4))
         (el (max (- lim) (min lim el)))
         (d (pt (* (cos el) (cos az)) (* (cos el) (sin az)) (sin el)))
         (r (unit3 (cross3 (pt 0 0 1) d)))
         (u (unit3 (cross3 d r))))
    (list "Custom" r u d :orbit)))

(defvar *orbit-redrawing* nil)

(defcmd "3DORBIT" ("3DO" "ORBIT") "Turns the view around the drawing with the mouse."
  (out (format nil "Drag to orbit (left-right turns, up-down tilts). Enter to finish.~%"))
  (loop
    (let ((p (get-user-input :point :prompt "Press and drag to orbit, or Enter to finish: " :nosnap t)))
      (unless (pointp p) (return))
      (let ((sx0 *mouse-sx*) (sy0 *mouse-sy*))
        (multiple-value-bind (az0 el0) (view-angles *view*)
          ;; keep the middle of the screen in the middle while turning
          (let* ((centre (s->w (/ *cw* 2d0) (/ *ch* 2d0)))
                 (turn (lambda (cur)
                         (declare (ignore cur))
                         (let ((dx (- *mouse-sx* sx0)) (dy (- *mouse-sy* sy0)))
                           (setf *view* (orbit-view (- az0 (* dx 0.01d0)) (+ el0 (* dy 0.01d0))))
                           (multiple-value-bind (u v) (view-project centre) (setf *vx* u *vy* v))
                           (clear-geom-cache)
                           ;; redraw (the redraw itself shows the drag again: do not recurse)
                           (unless *orbit-redrawing*
                             (let ((*orbit-redrawing* t)) (gui-regen))))
                         nil)))
            (get-user-input :corner :base p :prompt "Release (or click) to stop: " :drag turn :nosnap t)
            (clear-geom-cache)
            (when (gui-p) (gui-regen) (gui-sync)))))))
  (out (format nil "View: ~A.~%" (if *view* (first *view*) "Top"))))

;;; ---------- plots in the Hidden and Shaded styles ----------

(defun plot-shade (rgb normal)
  (let ((i (+ 0.35d0 (* 0.65d0 (abs (vdot3 (unit3 normal) (shading-light)))))))
    (mapcar (lambda (c) (* c i)) rgb)))

(defun mesh-plot-items (data aci style)
  "(depth . prims) for each front face (and outline edge) of solid DATA, for a plot."
  (let* ((color (plot-color aci))
         (base (if (every #'zerop color) '(0.85d0 0.85d0 0.85d0) color))
         (lw (plot-lw-mm (entity-lineweight data nil)))
         (toward (view-toward-viewer))
         (items nil))
    (multiple-value-bind (faces closed) (mesh-display-faces data)
      (dolist (f faces)
        (destructuring-bind (ps n vis) f
          (when (and n (or (not closed) (> (vdot3 n toward) -1d-9)))
            (let* ((uv (mapcar #'plot-uv ps))
                   (fill (if (equal style "Hidden") '(1d0 1d0 1d0) (plot-shade base n)))
                   (prims (list (list :fill (list uv) fill))))
              ;; shaded faces: a hairline in their own colour closes the seams between them
              (unless (equal style "Hidden")
                (push (list :poly (append uv (list (first uv))) fill 0.08d0) prims))
              (unless (equal style "Shaded")
                (loop for i below (length ps)
                      do (when (nth i vis)
                           (push (list :poly (list (nth i uv) (nth (mod (1+ i) (length uv)) uv)) color lw) prims))))
              (push (cons (/ (reduce #'+ (mapcar #'view-depth ps)) (length ps)) (nreverse prims)) items)))))
      ;; outlines of curved solids
      (when (and closed (not (equal style "Shaded")))
        (let ((edge-faces (make-hash-table :test #'equal)) (ends (make-hash-table :test #'equal)))
          (dolist (f faces)
            (destructuring-bind (ps n vis) f
              (declare (ignore vis))
              (when n
                (let ((depth (/ (reduce #'+ (mapcar #'view-depth ps)) (length ps))))
                  (loop for (a b) on (append ps (list (first ps))) while b
                        do (let* ((ka (weld-key a)) (kb (weld-key b))
                                  (k (if (string< (prin1-to-string ka) (prin1-to-string kb)) (list ka kb) (list kb ka))))
                             (push (cons n depth) (gethash k edge-faces))
                             (setf (gethash k ends) (cons a b))))))))
          (maphash (lambda (k fs)
                     (when (= (length fs) 2)
                       (let ((f1 (plusp (vdot3 (car (first fs)) toward))) (f2 (plusp (vdot3 (car (second fs)) toward))))
                         (unless (eq f1 f2)
                           (let ((ab (gethash k ends)) (front (if f1 (first fs) (second fs))))
                             (push (cons (+ (cdr front) 1d-9)
                                         (list (list :poly (list (plot-uv (car ab)) (plot-uv (cdr ab))) color lw)))
                                   items))))))
                   edge-faces))))
    items))

(defun styled-plot-prims (x0 y0 x1 y1)
  "Plot primitives for the Hidden / Shaded styles: everything sorted far to near."
  (let ((style (visual-style)) (items nil))
    (dolist (e (live-entities))
      (let ((b (ignore-errors (if (projected-view-p) (entity-view-bbox e) (entity-bbox e)))))
        (when (and (entity-visible-p e)
                   (or (null b) (not (or (> (first b) x1) (< (third b) x0) (> (second b) y1) (< (fourth b) y0)))))
          (let ((data (ent-data e)))
            (if (equal (dxf 0 data) "POLYFACE")
                (setf items (nconc (mesh-plot-items data (entity-aci data) style) items))
                (let ((*plot-prims* nil))
                  (ignore-errors (plot-data data (entity-aci data)))
                  (when *plot-prims* (push (cons (entity-depth e) (reverse *plot-prims*)) items))))))))
    (loop for it in (stable-sort items #'< :key #'car) append (cdr it))))
