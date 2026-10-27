;;;; tiles.lisp — VPORTS: the Model tab split into several viewports, as in AutoCAD.
;;;;
;;;; Each viewport shows the model with its own view (Top, Front, an isometric, an orbit ...),
;;;; zoom and visual style. One is active (blue border): you draw, pick and zoom in it as usual, and
;;;; anything drawn shows in all of them. Click in another viewport (or turn the wheel there) to make
;;;; it the active one. VPORTS 2/3/4 splits the active viewport; Single goes back to one.
;;;;
;;;; The active viewport lives in the usual view variables; *tx*,*ty* place it on the canvas and
;;;; *cw* x *ch* is its size. The others are drawn from plot primitives, clipped to their rectangles.

(in-package :fancy-pencil)

(defun tiled-p () (and (null (drawing-ctab *dwg*)) (drawing-tiles *dwg*) t))
(defun active-tile () (nth (drawing-active-tile *dwg*) (drawing-tiles *dwg*)))

(defun tile-screen-rect (tl)
  "TL's rectangle on the canvas in pixels: (x0 y0 x1 y1), y0 at the top."
  (destructuring-bind (x0 y0 x1 y1) (tile-rect tl)
    (list (round (* x0 *full-cw*)) (round (* y0 *full-ch*)) (round (* x1 *full-cw*)) (round (* y1 *full-ch*)))))

(defun apply-tile-geometry ()
  "Point the screen variables at the active viewport (or the whole canvas)."
  (if (tiled-p)
      (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect (active-tile))
        (setf *tx* x0 *ty* y0 *cw* (max 20 (- x1 x0)) *ch* (max 20 (- y1 y0))))
      (setf *tx* 0 *ty* 0 *cw* *full-cw* *ch* *full-ch*)))

(defun save-active-tile ()
  (let ((tl (active-tile)))
    (when (and tl (tiled-p))
      (setf (tile-view tl) *view* (tile-vx tl) *vx* (tile-vy tl) *vy* (tile-vscale tl) *vscale*
            (tile-vstyle tl) (visual-style)))))

(defun load-tile (tl)
  (setf *view* (tile-view tl) *vx* (tile-vx tl) *vy* (tile-vy tl) *vscale* (tile-vscale tl)
        (sysvar "VSCURRENT") (or (tile-vstyle tl) "Wireframe")))

(defun tile-index-at (sx sy)
  (position-if (lambda (tl) (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect tl) (and (<= x0 sx x1) (<= y0 sy y1))))
               (drawing-tiles *dwg*)))

(defun activate-tile (i)
  (save-active-tile)
  (setf (drawing-active-tile *dwg*) i)
  (load-tile (active-tile))
  (setf *view-history* nil)
  (clear-geom-cache)
  (apply-tile-geometry)
  (when (gui-p) (gui-regen) (gui-sync) (refresh-grips)))

(defun tile-switch-at (sx sy)
  "A click (wheel, pan) at SX,SY in another viewport makes it the active one. True if it did."
  (when (tiled-p)
    (let ((i (tile-index-at sx sy)))
      (when (and i (/= i (drawing-active-tile *dwg*)))
        (activate-tile i)
        t))))

;;; ---------- drawing the other viewports ----------

(defvar *tile-prims-cache* (make-hash-table :test #'eq))

(defun tile-window (tl)
  "The model area TL shows, in its view's coordinates: (u0 v0 u1 v1)."
  (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect tl)
    (let ((hw (/ (- x1 x0) 2d0 (tile-vscale tl))) (hh (/ (- y1 y0) 2d0 (tile-vscale tl))))
      (list (- (tile-vx tl) hw) (- (tile-vy tl) hh) (+ (tile-vx tl) hw) (+ (tile-vy tl) hh)))))

(defun tile-prims (tl)
  (let* ((key (list (display-signature) (tile-view tl) (tile-vstyle tl) (tile-window tl) (background-color)))
         (c (gethash tl *tile-prims-cache*)))
    (if (and c (equal (car c) key))
        (cdr c)
        (let ((prims (with-model-view ((tile-view tl) (or (tile-vstyle tl) "Wireframe"))
                       (let ((*prims-for-display* t))
                         (ignore-errors (apply #'collect-plot-prims (tile-window tl)))))))
          (setf (gethash tl *tile-prims-cache*) (cons key prims))
          prims))))

(defun tile-label (tl)
  (let ((v (tile-view tl))) (string-upcase (if v (first v) "Top"))))

(defun tiles-overlay-cmds ()
  "Over the active viewport's drawing: the other viewports (each on its own background), then
every viewport's border, the active one in blue."
  (let ((out nil) (bg (canvas-bg)) (active (active-tile)))
    (dolist (tl (drawing-tiles *dwg*))
      (unless (eq tl active)
        (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect tl)
          (push (format nil ".c create rectangle ~D ~D ~D ~D -fill ~A -outline {} -tags pover" x0 y0 x1 y1 bg) out)
          (let* ((vs (tile-vscale tl)) (cx (/ (+ x0 x1) 2d0)) (cy (/ (+ y0 y1) 2d0)) (vx (tile-vx tl)) (vy (tile-vy tl)))
            (setf out (revappend (prims-canvas-cmds (tile-prims tl) "pover" (tile-window tl)
                                                    :to-screen (lambda (p) (values (+ cx (* (- (px p) vx) vs)) (- cy (* (- (py p) vy) vs))))
                                                    :ppx vs :mm-px 3.78d0)
                                 out)))
          (push (format nil ".c create text ~D ~D -anchor sw -text ~A -fill ~A -font {Helvetica 9 bold} -tags pover"
                        (+ x0 8) (- y1 6) (tq (tile-label tl)) (bg-mix 0.45))
                out))))
    (dolist (tl (drawing-tiles *dwg*))
      (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect tl)
        (if (eq tl active)
            (push (format nil ".c create rectangle ~D ~D ~D ~D -outline #1e6fff -width 3 -tags pover" (1+ x0) (1+ y0) (1- x1) (1- y1)) out)
            (push (format nil ".c create rectangle ~D ~D ~D ~D -outline ~A -width 1 -tags pover" x0 y0 x1 y1 (bg-mix 0.3)) out))))
    (nreverse out)))

;;; ---------- VPORTS ----------

(defun split-tile-rect (rect n arrangement)
  "RECT (fractions, y down) split into N rectangles, in reading order."
  (destructuring-bind (x0 y0 x1 y1) rect
    (let ((xm (/ (+ x0 x1) 2)) (ym (/ (+ y0 y1) 2)))
      (case n
        (2 (if (equal arrangement "Horizontal")
               (list (list x0 y0 x1 ym) (list x0 ym x1 y1))
               (list (list x0 y0 xm y1) (list xm y0 x1 y1))))
        (3 (cond ((equal arrangement "Horizontal")
                  (let ((h (/ (- y1 y0) 3))) (loop for i below 3 collect (list x0 (+ y0 (* i h)) x1 (+ y0 (* (1+ i) h))))))
                 ((equal arrangement "Vertical")
                  (let ((w (/ (- x1 x0) 3))) (loop for i below 3 collect (list (+ x0 (* i w)) y0 (+ x0 (* (1+ i) w)) y1))))
                 ((equal arrangement "Left")
                  (list (list x0 y0 xm y1) (list xm y0 x1 ym) (list xm ym x1 y1)))
                 (t (list (list x0 y0 xm ym) (list x0 ym xm y1) (list xm y0 x1 y1)))))
        (t (list (list x0 y0 xm ym) (list xm y0 x1 ym) (list x0 ym xm y1) (list xm ym x1 y1)))))))

(defun tile-fit (tl &optional scale)
  "Zoom TL to the whole model (at SCALE pixels per unit if given)."
  (destructuring-bind (x0 y0 x1 y1) (tile-screen-rect tl)
    (let ((b (model-extents-in-view (tile-view tl))) (w (max 20 (- x1 x0))) (h (max 20 (- y1 y0))))
      (if (null b)
          (setf (tile-vx tl) 6d0 (tile-vy tl) 4.5d0 (tile-vscale tl) (or scale (* 0.9d0 (min (/ w 12) (/ h 9)))))
          (destructuring-bind (u0 v0 u1 v1) b
            (setf (tile-vx tl) (/ (+ u0 u1) 2) (tile-vy tl) (/ (+ v0 v1) 2)
                  (tile-vscale tl) (or scale (* 0.88d0 (min (/ w (max 1d-9 (- u1 u0))) (/ h (max 1d-9 (- v1 v0))))))))))
    tl))

(defun split-active-tile (n arrangement &optional standard)
  "Split the active viewport (or the whole Model tab) into N."
  (save-active-tile)
  (let* ((tiles (or (drawing-tiles *dwg*)
                    (list (make-tile :rect (list 0d0 0d0 1d0 1d0) :view *view* :vx *vx* :vy *vy* :vscale *vscale* :vstyle (visual-style)))))
         (k (if (drawing-tiles *dwg*) (drawing-active-tile *dwg*) 0))
         (old (nth k tiles))
         (new (mapcar (lambda (r) (make-tile :rect r :view (tile-view old) :vx (tile-vx old) :vy (tile-vy old)
                                             :vscale (tile-vscale old) :vstyle (tile-vstyle old)))
                      (split-tile-rect (tile-rect old) n arrangement))))
    (setf (drawing-tiles *dwg*) (append (subseq tiles 0 k) new (nthcdr (1+ k) tiles))
          (drawing-active-tile *dwg*) k)
    (if standard
        ;; Top, SE Isometric / Front, Right: the flat views at one scale, lined up
        (let ((views (list (make-view "Top") (make-view "SE Isometric") (make-view "Front") (make-view "Right"))))
          (loop for tl in new for v in views do (setf (tile-view tl) v (tile-vstyle tl) "Wireframe") (tile-fit tl))
          (let ((s (reduce #'min (mapcar #'tile-vscale (list (first new) (third new) (fourth new))))))
            (dolist (tl (list (first new) (third new) (fourth new))) (setf (tile-vscale tl) s))))
        (dolist (tl new) (tile-fit tl (tile-vscale old))))
    (load-tile (active-tile))
    (apply-tile-geometry)
    (clear-geom-cache)
    (touch)))

(defun single-tile ()
  (save-active-tile)
  (when (drawing-tiles *dwg*)
    (load-tile (active-tile))
    (setf (drawing-tiles *dwg*) nil (drawing-active-tile *dwg*) 0)
    (apply-tile-geometry)
    (touch)))

(defun join-tiles ()
  "Join the active viewport with a neighbour that makes a rectangle with it."
  (save-active-tile)
  (let* ((tiles (drawing-tiles *dwg*)) (a (active-tile)))
    (destructuring-bind (ax0 ay0 ax1 ay1) (tile-rect a)
      (flet ((close-to (p q) (< (abs (- p q)) 1d-6)))
        (let ((b (find-if (lambda (tl) (and (not (eq tl a))
                                            (destructuring-bind (bx0 by0 bx1 by1) (tile-rect tl)
                                              (or (and (close-to ay0 by0) (close-to ay1 by1) (or (close-to ax1 bx0) (close-to bx1 ax0)))
                                                  (and (close-to ax0 bx0) (close-to ax1 bx1) (or (close-to ay1 by0) (close-to by1 ay0)))))))
                          tiles)))
          (if (null b)
              (progn (out (format nil "No viewport beside this one makes a rectangle with it.~%")) nil)
              (destructuring-bind (bx0 by0 bx1 by1) (tile-rect b)
                (setf (tile-rect a) (list (min ax0 bx0) (min ay0 by0) (max ax1 bx1) (max ay1 by1)))
                (setf (drawing-tiles *dwg*) (remove b tiles))
                (setf (drawing-active-tile *dwg*) (position a (drawing-tiles *dwg*)))
                (when (= (length (drawing-tiles *dwg*)) 1) (setf (drawing-tiles *dwg*) nil (drawing-active-tile *dwg*) 0))
                (apply-tile-geometry)
                (touch)
                t)))))))

(defcmd "VPORTS" ("-VPORTS" "VIEWPORTS" "VPORT") "Splits the Model tab into several viewports, each with its own view: Single, Join, 2, 3, 4."
  (when (drawing-ctab *dwg*)
    ;; on a layout, viewports are MVIEW's
    (funcall (gethash "MVIEW" *commands*))
    (return-from-cmd))
  (let ((r (gkw "Enter an option [Single/Join/2/3/4] <3>: " '("Single" "Join" "2" "3" "4") "3")))
    (cond
      ((equal r "Single") (single-tile) (out (format nil "One viewport.~%")))
      ((equal r "Join")
       (if (drawing-tiles *dwg*)
           (when (join-tiles) (out (format nil "Viewports joined.~%")))
           (out (format nil "There is only one viewport.~%"))))
      (t
       (let* ((n (parse-integer r))
              (arr (case n
                     (2 (gkw "Enter a configuration option [Horizontal/Vertical] <Vertical>: " '("Horizontal" "Vertical") "Vertical"))
                     (3 (gkw "Enter a configuration option [Horizontal/Vertical/Left/Right] <Right>: " '("Horizontal" "Vertical" "Left" "Right") "Right"))
                     (t (gkw "Views [Standard (Top, Front, Right, SE Iso)/Same] <Standard>: " '("Standard" "Same") "Standard")))))
         (split-active-tile n arr (and (= n 4) (equal arr "Standard")))
         (out (format nil "~D viewports~:[~; (Top, SE Isometric, Front and Right)~]. Click in a viewport to work in it; VPORTS Single goes back to one.~%"
                      (length (drawing-tiles *dwg*)) (and (= n 4) (equal arr "Standard")))))))
    (when (gui-p) (gui-regen) (gui-sync) (refresh-grips))))

;;; ---------- files: the VPORT table's *ACTIVE records ----------

(defun tile-dxf-records ()
  "For writing: one (lower-left upper-right center-dcs direction height aspect twist style) per
viewport, the active one first (one record for the whole area when not split)."
  (let* ((tiles (if (tiled-p)
                    (progn (save-active-tile)
                           (let ((a (active-tile))) (cons a (remove a (drawing-tiles *dwg*)))))
                    (list (make-tile :rect (list 0d0 0d0 1d0 1d0)
                                     :view (if (drawing-ctab *dwg*) (first (drawing-model-view *dwg*)) *view*)
                                     :vx (if (drawing-ctab *dwg*) (or (second (drawing-model-view *dwg*)) *vx*) *vx*)
                                     :vy (if (drawing-ctab *dwg*) (or (third (drawing-model-view *dwg*)) *vy*) *vy*)
                                     :vscale (if (drawing-ctab *dwg*) (or (fourth (drawing-model-view *dwg*)) *vscale*) *vscale*))))))
    (mapcar (lambda (tl)
              (destructuring-bind (x0 y0 x1 y1) (tile-rect tl)
                (let* ((view (tile-view tl)) (dir (if view (pt3 (fourth view)) (pt 0 0 1)))
                       (w (view-uv->world view (pt (tile-vx tl) (tile-vy tl))))
                       (hpx (max 1 (* (- y1 y0) *full-ch*))) (wpx (max 1 (* (- x1 x0) *full-cw*))))
                  (destructuring-bind (r u) (dcs-axes dir)
                    (let* ((vr (if view (pt3 (second view)) (pt 1 0 0)))
                           (twist (atan (- (vdot3 vr u)) (vdot3 vr r))))
                      (list (pt x0 (- 1 y1)) (pt x1 (- 1 y0)) (pt (vdot3 w r) (vdot3 w u)) dir
                            (/ hpx (tile-vscale tl)) (/ wpx hpx) (if (< (abs twist) 1d-9) 0d0 twist)
                            (tile-vstyle tl)))))))
            tiles)))

(defun import-tiles (tables)
  "Viewports of the Model tab from a file's VPORT table (several *ACTIVE records)."
  (let ((recs (remove-if-not (lambda (g) (and (equal (cdr (first g)) "VPORT") (equalp (dxf 2 g) "*ACTIVE"))) tables)))
    (if (< (length recs) 2)
        (setf (drawing-tiles *dwg*) nil (drawing-active-tile *dwg*) 0)
        (setf (drawing-tiles *dwg*)
              (mapcar (lambda (g)
                        (let* ((g (group-coords g '(10 11 12 13 14 15)))
                               (ll (or (dxf 10 g) (pt 0 0))) (ur (or (dxf 11 g) (pt 1 1)))
                               (dir (or (dxf 16 g) (pt 0 0 1))) (twist (rad (d (or (dxf 51 g) 0))))
                               (view (view-from-direction dir twist))
                               (dc (or (dxf 12 g) (pt 0 0)))
                               (w (destructuring-bind (r u) (dcs-axes dir) (v+ (v* r (px dc)) (v* u (py dc)))))
                               (uv (world->view-uv view w))
                               (h (d (or (dxf 40 g) 9)))
                               (rect (list (d (px ll)) (- 1 (d (py ur))) (d (px ur)) (- 1 (d (py ll))))))
                          (make-tile :rect rect :view view :vx (px uv) :vy (py uv)
                                     :vscale (/ (max 1 (* (- (fourth rect) (second rect)) *full-ch*)) (max 1d-9 h))
                                     :vstyle (case (dxf 281 g) (2 "Hidden") (3 "Shaded") ((4 5 6) "ShadedEdges") (t "Wireframe")))))
                      recs)
              (drawing-active-tile *dwg*) 0))))

(defun tiles-after-open ()
  "After opening: the first viewport is the active one."
  (when (tiled-p)
    (load-tile (active-tile))
    (apply-tile-geometry)))
