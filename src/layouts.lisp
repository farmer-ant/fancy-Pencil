;;;; layouts.lisp — paper space: layout tabs, sheets of paper and viewports (MVIEW), as in AutoCAD.
;;;;
;;;; Every drawing has the Model tab and one or more layouts (Layout1 ...). A layout is a sheet of
;;;; paper drawn at its real size (inches, or millimeters in metric drawings). On it you put a title
;;;; block, notes and VIEWPORTS: windows that show model space, each with its own view (Top, Front,
;;;; an isometric, an orbit ...), visual style and scale. A layout plots at 1:1.
;;;;
;;;;   MVIEW (MV)       make viewports: two corners, Fit, or 2/3/4 at once (4 = Top, Front, Right, SE Iso)
;;;;   MSPACE (MS)      work in model space through a viewport (or double-click inside it)
;;;;   PSPACE (PS)      back to the sheet (or double-click outside the viewports)
;;;;   VPSCALE          set a viewport's scale (1:2, 1/4"=1'-0", Fit ...); inside a viewport ZOOM nXP
;;;;   LAYOUT (LO)      New / Copy / Delete / Rename / Set / ? ; PAGESETUP: paper size and orientation
;;;;
;;;; Objects keep their space in their data: paper space objects carry (:layout . name). Viewports
;;;; are VIEWPORT objects: center (10), width (40) and height (41) on the sheet, the model point at
;;;; their middle (12, in the view's own coordinates) and the model height they show (45), so the
;;;; scale is 41/45. Their view is (:view . *view*-style list), their visual style (:vstyle . name).

(in-package :fancy-pencil)

;;; ---------- layouts ----------

(defun find-layout (name)
  (find name (drawing-layouts *dwg*) :key #'layout-name :test #'string-equal))
(defun current-layout () (and (drawing-ctab *dwg*) (find-layout (drawing-ctab *dwg*))))
(defun in-layout-p () (and (drawing-ctab *dwg*) t))

(defun layout-units* (l)
  "The layout's paper units, :in or :mm (metric drawings get millimeter sheets)."
  (or (layout-units l)
      (setf (layout-units l)
            (if (or (member (sysvar "INSUNITS") '(4 5 6)) (eql (sysvar "MEASUREMENT") 1)) :mm :in))))

(defun paper-unit-name (l) (if (eq (layout-units* l) :mm) "mm" "in"))

(defun round2 (x) (/ (fround (* (d x) 100)) 100))

(defun layout-sheet-size (l)
  "(values width height) of the layout's sheet in its paper units."
  (let* ((p (or (assoc (layout-paper l) *paper-sizes* :test #'string-equal) (third *paper-sizes*)))
         (k (if (eq (layout-units* l) :mm) 25.4d0 1d0))
         (big (round2 (* k (max (second p) (third p))))) (small (round2 (* k (min (second p) (third p))))))
    (if (layout-landscape l) (values big small) (values small big))))

(defun layout-margin (l) (if (eq (layout-units* l) :mm) 6.35d0 0.25d0))

(defun printable-rect (l)
  (multiple-value-bind (w h) (layout-sheet-size l)
    (let ((m (layout-margin l))) (list m m (- w m) (- h m)))))

(defun ensure-layout (name)
  (or (find-layout name)
      (let ((l (make-layout :name name)))
        (setf (drawing-layouts *dwg*) (append (drawing-layouts *dwg*) (list l)))
        l)))

(defun layout-entities (name)
  (let ((*space-override* name)) (live-entities)))

(defun layout-viewports (&optional (name (drawing-ctab *dwg*)))
  (remove-if-not (lambda (e) (equal (ent-type e) "VIEWPORT")) (layout-entities name)))

;;; ---------- viewport data ----------

(defun vp-center (d) (pt3 (dxf 10 d)))
(defun vp-width (d) (d (or (dxf 40 d) 1)))
(defun vp-height (d) (d (or (dxf 41 d) 1)))
(defun vp-rect (d)
  (let ((c (vp-center d)) (hw (/ (vp-width d) 2)) (hh (/ (vp-height d) 2)))
    (list (- (px c) hw) (- (py c) hh) (+ (px c) hw) (+ (py c) hh))))
(defun vp-view-height (d) (d (or (dxf 45 d) (vp-height d))))
(defun vp-scale (d) "Paper units per model unit." (/ (vp-height d) (max 1d-12 (vp-view-height d))))
(defun vp-view (d) (dxf :view d))
(defun vp-view-center (d) (let ((c (dxf 12 d))) (if c (pt3 c) (pt 0 0 0))))
(defun vp-style (d) (or (dxf :vstyle d) "Wireframe"))
(defun vp-locked-p (d) (and (dxf :locked d) t))
(defun vp-on-p (d) (not (dxf :off d)))
(defun vp-view-name (d) (let ((v (vp-view d))) (if v (first v) "Top")))

(defun set-vp-key (key val d)
  "Set or (VAL NIL) remove a keyword entry of viewport data D."
  (let ((d (rem-dxf key d))) (if val (append d (list (cons key val))) d)))

(defun make-vp-data (rect view vcenter vheight &optional style)
  (destructuring-bind (x0 y0 x1 y1) rect
    (let ((d (list (cons 0 "VIEWPORT") (cons 10 (pt (/ (+ x0 x1) 2) (/ (+ y0 y1) 2) 0))
                   (cons 40 (d (abs (- x1 x0)))) (cons 41 (d (abs (- y1 y0))))
                   (cons 12 (pt3 vcenter)) (cons 45 (d vheight)))))
      (setf d (set-vp-key :view view d))
      (set-vp-key :vstyle (and style (not (equal style "Wireframe")) style) d))))

(defmacro with-model-view ((view &optional (style "Wireframe")) &body body)
  "Run BODY looking at model space in VIEW (NIL = Top) with visual STYLE."
  `(let ((*view* ,view) (*space-override* nil) (*vstyle-override* ,style))
     ,@body))

(defun model-extents-in-view (view)
  (with-model-view (view) (ignore-errors (view-extents))))

(defun vp-fit (d &optional view)
  "Viewport data D (with VIEW, default its own) zoomed to show all of model space."
  (let* ((view (if view view (vp-view d)))
         (b (model-extents-in-view view))
         (w (vp-width d)) (h (vp-height d)))
    (if (null b)
        (set-dxf 45 (* h 1d0) (set-dxf 12 (pt (/ w 2) (/ h 2) 0) d))
        (destructuring-bind (x0 y0 x1 y1) b
          (let* ((bw (max 1d-9 (- x1 x0))) (bh (max 1d-9 (- y1 y0)))
                 (vh (* 1.08d0 (max bh (* bw (/ h w))))))
            (set-dxf 45 vh (set-dxf 12 (pt (/ (+ x0 x1) 2) (/ (+ y0 y1) 2) 0) d)))))))

;;; ---------- scales ----------

(defun vp-scale-choices () (remove "Fit to paper" *plot-scales* :test #'string=))

(defun scale-text-value (text &optional (l (current-layout)))
  "Paper units per model unit for a scale such as 1:2 or 1/4\"=1'-0\" (NIL if not understood)."
  (let ((ipu (ignore-errors (scale-paper-inches-per-unit text (default-plot-units)))))
    (when ipu (* ipu (if (and l (eq (layout-units* l) :mm)) 25.4d0 1d0)))))

(defun parse-vp-scale (text)
  "A typed scale: a standard one (1:2, 1/4\"=1'-0\"), n:m, nXP or a plain factor."
  (let* ((s (string-trim " " text)) (u (string-upcase s)))
    (cond ((zerop (length s)) nil)
          ((and (> (length u) 2) (string= (subseq u (- (length u) 2)) "XP"))
           (let ((n (parse-number-text (subseq s 0 (- (length s) 2))))) (and n (plusp n) (d n))))
          ((or (position #\: s) (search "=" s))
           (let ((s2 (if (and (search "=" s) (not (search " = " s)))
                         (let ((k (position #\= s))) (format nil "~A = ~A" (string-trim " " (subseq s 0 k)) (string-trim " " (subseq s (1+ k)))))
                         s)))
             (scale-text-value s2)))
          (t (let ((n (parse-number-text s))) (and n (plusp n) (d n)))))))

(defun vp-scale-label (d)
  (let ((s (vp-scale d)))
    (or (find-if (lambda (c) (let ((v (scale-text-value c))) (and v (< (abs (- v s)) (* 1d-6 s))))) (vp-scale-choices))
        "Custom")))

(defun vp-with-scale (d s)
  "D at scale S (paper units per model unit), looking at the same model point."
  (set-dxf 45 (/ (vp-height d) s) d))

;;; ---------- the paper view ----------

(defun paper-view ()
  "(vx vy vscale): how the sheet is shown on the screen."
  (if (drawing-active-vp *dwg*)
      (or (and (current-layout) (layout-pview (current-layout))) (list 5d0 4d0 50d0))
      (list *vx* *vy* *vscale*)))

(defun paper->screen (p)
  (destructuring-bind (vx vy s) (paper-view)
    (values (+ (/ *cw* 2d0) (* (- (px p) vx) s)) (- (/ *ch* 2d0) (* (- (py p) vy) s)))))
(defun screen->paper (sx sy)
  (destructuring-bind (vx vy s) (paper-view)
    (pt (+ vx (/ (- sx (/ *cw* 2d0)) s)) (- vy (/ (- sy (/ *ch* 2d0)) s)))))

(defun sheet-view-values (l)
  "(vx vy vscale) showing the whole sheet."
  (multiple-value-bind (w h) (layout-sheet-size l)
    (list (/ w 2) (/ h 2) (max 1d-6 (* 0.9d0 (min (/ *cw* (max 1d-9 w)) (/ *ch* (max 1d-9 h))))))))

;;; ---------- switching tabs ----------

(defun model-state () (list *view* *vx* *vy* *vscale* (sysvar "VSCURRENT")))

(defun leave-space ()
  "Remember the view of the tab being left."
  (let ((dw *dwg*))
    (cond ((null (drawing-ctab dw))
           (when (fboundp 'save-active-tile) (funcall 'save-active-tile))
           (setf (drawing-model-view dw) (model-state)))
          (t (when (drawing-active-vp dw) (leave-viewport))
             (let ((l (current-layout))) (when l (setf (layout-pview l) (list *vx* *vy* *vscale*))))))))

(defun clear-selection-state ()
  (setf *pickfirst* nil *view-history* nil *drawn-modcount* -1)
  (clrhash *hl*)
  (clear-geom-cache)
  (clrhash *local-cache*))

(defun set-ctab (name)
  "Show the Model tab (NAME NIL or \"Model\") or the layout NAME."
  (let* ((name (if (or (null name) (string-equal name "Model")) nil name))
         (l (and name (find-layout name))))
    (when (and name (null l)) (return-from set-ctab nil))
    (unless (equal (and name (layout-name l)) (drawing-ctab *dwg*))
      (leave-space)
      (setf (drawing-ctab *dwg*) (and l (layout-name l)) (drawing-active-vp *dwg*) nil)
      (when (fboundp 'apply-tile-geometry) (funcall 'apply-tile-geometry))
      (setf (sysvar "CTAB") (if l (layout-name l) "Model"))
      (if (null l)
          (let ((mv (drawing-model-view *dwg*)))
            (if mv
                (destructuring-bind (view vx vy vs style) mv
                  (setf *view* view *vx* vx *vy* vy *vscale* vs (sysvar "VSCURRENT") (or style "Wireframe")))
                (setf *view* nil (sysvar "VSCURRENT") "Wireframe")))
          (progn
            (setf *view* nil (sysvar "VSCURRENT") "Wireframe")
            (destructuring-bind (vx vy vs) (or (layout-pview l) (sheet-view-values l))
              (setf *vx* vx *vy* vy *vscale* vs))))
      (clear-selection-state)
      (when (and (null l) (null (drawing-model-view *dwg*)) (gui-p)) (zoom-extents))
      (after-space-change))
    t))

(defun after-space-change ()
  (when (gui-p)
    (gui-regen) (refresh-grips) (refresh-palette) (gui-sync) (refresh-layout-tabs t)))

;;; ---------- working inside a viewport (MSPACE) ----------

(defun load-vp-globals (d)
  "Point the view at viewport D's model view, placed on the screen where the viewport is."
  (destructuring-bind (pvx pvy ps) (paper-view)
    (let* ((s (vp-scale d)) (vs (* ps s)) (c (vp-center d)) (vc (vp-view-center d)))
      (setf *view* (vp-view d) (sysvar "VSCURRENT") (vp-style d) *vscale* vs
            *vx* (- (px vc) (/ (* (- (px c) pvx) ps) vs))
            *vy* (- (py vc) (/ (* (- (py c) pvy) ps) vs))))))

(defun enter-viewport (vp)
  (let ((l (current-layout)))
    (when (drawing-active-vp *dwg*) (vp-capture))
    (unless (drawing-active-vp *dwg*) (setf (layout-pview l) (list *vx* *vy* *vscale*)))
    (setf (drawing-active-vp *dwg*) vp)
    (clear-selection-state)
    (load-vp-globals (ename-data vp))
    (after-space-change)))

(defun mark-modified ()
  "The drawing has unsaved changes (without a step in its edit count)."
  (setf (drawing-saved-modcount *dwg*) -1))

(defun vp-capture ()
  "Store the view being worked in back into the active viewport (zooms, pans, view, style)."
  (let ((vp (drawing-active-vp *dwg*)) (l (current-layout)))
    (when (and vp l (ent-live-p vp) (layout-pview l))
      (let ((d (ename-data vp)))
        (unless (vp-locked-p d)
          (destructuring-bind (pvx pvy ps) (layout-pview l)
            (let* ((s (/ *vscale* ps)) (c (vp-center d))
                   (vc (pt (+ *vx* (/ (* (- (px c) pvx) ps) *vscale*)) (+ *vy* (/ (* (- (py c) pvy) ps) *vscale*)) 0))
                   (nd (set-vp-key :vstyle (let ((st (visual-style))) (and (not (equal st "Wireframe")) st))
                                   (set-vp-key :view *view* (set-dxf 45 (/ (vp-height d) s) (set-dxf 12 vc d))))))
              (unless (and (equal (vp-view d) (vp-view nd)) (equal (vp-style d) (vp-style nd))
                           (< (dist (vp-view-center d) vc) (* 1d-9 (max 1d0 (vp-view-height d))))
                           (< (abs (- (vp-view-height d) (vp-view-height nd))) (* 1d-9 (vp-view-height d))))
                (setf (ename-data vp) nd)
                (mark-modified)))))))))

(defun leave-viewport ()
  (vp-capture)
  (setf (drawing-active-vp *dwg*) nil)
  (let ((l (current-layout)))
    (setf *view* nil (sysvar "VSCURRENT") "Wireframe")
    (destructuring-bind (vx vy vs) (or (layout-pview l) (sheet-view-values l))
      (setf *vx* vx *vy* vy *vscale* vs))))

(defun layout-prepare ()
  "Before a redraw on a layout: keep the active viewport and the view in step."
  (let ((vp (drawing-active-vp *dwg*)))
    (when vp
      (cond ((not (ent-live-p vp)) (leave-viewport))
            ((vp-locked-p (ename-data vp)) (load-vp-globals (ename-data vp)))
            (t (vp-capture))))))

(defun set-active-vp-scale (s)
  (let ((vp (drawing-active-vp *dwg*)))
    (when vp
      (vp-capture)
      (setf (ename-data vp) (vp-with-scale (ename-data vp) s))
      (touch)
      (load-vp-globals (ename-data vp))
      (out (format nil "Viewport scale ~A.~%" (vp-scale-label (ename-data vp))))
      (gui-regen))))

(defun viewport-at (p &optional exclude)
  "The topmost viewport (on and visible) whose rectangle holds paper point P."
  (find-if (lambda (e) (and (not (eq e exclude)) (entity-visible-p e)
                            (destructuring-bind (x0 y0 x1 y1) (vp-rect (ename-data e))
                              (and (<= x0 (px p) x1) (<= y0 (py p) y1)))))
           (reverse (layout-viewports))))

(defvar *vp-target* nil "The viewport a double-click asked MSPACE to enter.")

(defun viewport-dblclick (sx sy picked)
  "Command for a double-click at screen SX,SY on a layout: MSPACE, PSPACE or NIL."
  (let* ((p (screen->paper sx sy)) (active (drawing-active-vp *dwg*)))
    (cond ((and (null active) picked (member (ent-type picked) '("TEXT" "MTEXT" "ATTDEF") :test #'equal)) nil)
          ((null active) (let ((vp (viewport-at p))) (when vp (setf *vp-target* vp) "MSPACE")))
          ((destructuring-bind (x0 y0 x1 y1) (vp-rect (ename-data active)) (and (<= x0 (px p) x1) (<= y0 (py p) y1))) nil)
          (t (let ((vp (viewport-at p active))) (if vp (progn (setf *vp-target* vp) "MSPACE") "PSPACE"))))))

;;; ---------- drawing a viewport: model space through it, in paper coordinates ----------

(defun xform-prim (p f s)
  "Plot primitive P with points mapped by F and sizes scaled by S."
  (flet ((lw (w) (if (and (realp w) (minusp w)) (* w s) w)))
    (case (first p)
      (:poly (destructuring-bind (pts color w) (rest p) (list :poly (mapcar f pts) color (lw w))))
      (:arc (destructuring-bind (c r a1 a2 color w) (rest p) (list :arc (funcall f c) (* r s) a1 a2 color (lw w))))
      (:fill (list :fill (mapcar (lambda (poly) (mapcar f poly)) (second p)) (third p)))
      (:text (destructuring-bind (str pos size rot ha va color font) (rest p)
               (list :text str (funcall f pos) (* size s) rot ha va color font)))
      (:image (destructuring-bind (ppm w h ll ur &rest more) (rest p)
                (list* :image ppm w h (funcall f ll) (funcall f ur) more)))
      (t p))))

(defun vp-paper-prims (d &optional ppu)
  "Plot primitives for model space seen through viewport data D, in paper coordinates (unclipped).
PPU: PDF points per paper unit when plotting (NIL on the screen)."
  (when (vp-on-p d)
    (let* ((s (vp-scale d)) (c (vp-center d)) (vc (vp-view-center d))
           (hw (/ (vp-width d) 2 s)) (hh (/ (vp-height d) 2 s))
           (prims (with-model-view ((vp-view d) (vp-style d))
                    (let ((*plot-ppu* (and ppu (* ppu s))))
                      (collect-plot-prims (- (px vc) hw) (- (py vc) hh) (+ (px vc) hw) (+ (py vc) hh))))))
      (mapcar (lambda (p) (xform-prim p (lambda (q) (pt (+ (px c) (* s (- (px q) (px vc)))) (+ (py c) (* s (- (py q) (py vc)))))) s))
              prims))))

(defvar *vp-prims-cache* (make-hash-table :test #'eq))

(defun display-signature ()
  (list (drawing-modcount *dwg*) (sysvar "LTSCALE") (sysvar "PDMODE") (sysvar "PDSIZE")
        (mapcar (lambda (l) (list (layer-name l) (layer-on l) (layer-frozen l) (layer-color l) (layer-ltype l)))
                (drawing-layers *dwg*))))

(defun vp-display-prims (e)
  (let* ((d (ename-data e)) (key (cons d (display-signature))) (c (gethash e *vp-prims-cache*)))
    (if (and c (eq (car (car c)) d) (equal (cdr (car c)) (cdr key)))
        (cdr c)
        (let ((prims (let ((*prims-for-display* t)) (ignore-errors (vp-paper-prims d)))))
          (setf (gethash e *vp-prims-cache*) (cons key prims))
          prims))))

;;; ---------- prims on the canvas, clipped to a rectangle ----------

(defun clip-run (pts rect)
  "Pieces of polyline PTS inside RECT (x0 y0 x1 y1), as lists of points."
  (if (null rect)
      (list pts)
      (destructuring-bind (x0 y0 x1 y1) rect
        (let ((runs nil) (cur nil))
          (loop for (a b) on pts while b
                do (let ((c (clip-seg (px a) (py a) (px b) (py b) x0 y0 x1 y1)))
                     (if (null c)
                         (when cur (push (nreverse cur) runs) (setf cur nil))
                         (destructuring-bind (ax ay bx by) c
                           (let ((pa (pt ax ay)) (pb (pt bx by)))
                             (if (and cur (< (dist2d (first cur) pa) 1d-12))
                                 (push pb cur)
                                 (progn (when cur (push (nreverse cur) runs)) (setf cur (list pb pa)))))
                           ;; left the rectangle: end this run
                           (when (> (dist2d (pt bx by) b) 1d-12) (push (nreverse cur) runs) (setf cur nil))))))
          (when cur (push (nreverse cur) runs))
          (nreverse runs)))))

(defun clip-polygon (poly rect)
  "Sutherland-Hodgman: polygon POLY clipped to RECT."
  (if (null rect)
      poly
      (destructuring-bind (x0 y0 x1 y1) rect
        (flet ((clip (pts inside cut)
                 (let ((out nil))
                   (loop for i from 0 below (length pts)
                         for a = (nth i pts) for b = (nth (mod (1+ i) (length pts)) pts)
                         do (let ((ia (funcall inside a)) (ib (funcall inside b)))
                              (cond ((and ia ib) (push b out))
                                    (ia (push (funcall cut a b) out))
                                    (ib (push (funcall cut a b) out) (push b out)))))
                   (nreverse out))))
          (let ((p poly))
            (flet ((xcut (x) (lambda (a b) (let ((tt (/ (- x (px a)) (- (px b) (px a))))) (pt x (+ (py a) (* tt (- (py b) (py a))))))))
                   (ycut (y) (lambda (a b) (let ((tt (/ (- y (py a)) (- (py b) (py a))))) (pt (+ (px a) (* tt (- (px b) (px a)))) y)))))
              (setf p (clip p (lambda (q) (>= (px q) x0)) (xcut x0)))
              (when p (setf p (clip p (lambda (q) (<= (px q) x1)) (xcut x1))))
              (when p (setf p (clip p (lambda (q) (>= (py q) y0)) (ycut y0))))
              (when p (setf p (clip p (lambda (q) (<= (py q) y1)) (ycut y1)))))
            p)))))

(defun rgb-hex01 (c) (rgb-hex (* 255 (first c)) (* 255 (second c)) (* 255 (third c))))

(defun prims-canvas-cmds (prims tags &optional rect &key (to-screen #'paper->screen) ppx (mm-px nil))
  "Canvas commands drawing plot primitives PRIMS, clipped to RECT (both in the primitives'
coordinates: paper, unless TO-SCREEN maps others; PPX is pixels per unit)."
  (let* ((out nil) (ppx (or ppx (third (paper-view))))
         (lwd (eql (sysvar "LWDISPLAY") 1))
         (mm-px (or mm-px (* ppx (if (and (current-layout) (eq (layout-units* (current-layout)) :mm)) 1d0 (/ 25.4d0))))))
    (labels ((scr (p) (multiple-value-bind (x y) (funcall to-screen p) (format nil "~A ~A" (fmtn x) (fmtn y))))
             (width (lw) (cond ((and (realp lw) (minusp lw)) (max 1 (round (* (- lw) ppx))))
                               ((and lwd (realp lw)) (max 1 (round (* lw mm-px))))
                               (t 1)))
             (line (pts color lw)
               (dolist (run (clip-run pts rect))
                 (when (>= (length run) 2)
                   (push (format nil ".c create line ~{~A~^ ~} -fill ~A -width ~D -tags {~A}"
                                 (mapcar #'scr run) (rgb-hex01 color) (width lw) tags)
                         out)))))
      (dolist (p prims)
        (case (first p)
          (:poly (destructuring-bind (pts color lw) (rest p) (line pts color lw)))
          ;; a picture seen through a sheet viewport: its frame (the picture shows when plotted)
          (:image (destructuring-bind (ll ur) (list (fifth p) (sixth p))
                    (line (list ll (pt (px ur) (py ll)) ur (pt (px ll) (py ur)) ll) '(0.55d0 0.6d0 0.65d0) 0)))
          (:arc (destructuring-bind (c r a1 a2 color lw) (rest p)
                  (let* ((full (>= (abs (- a2 a1)) (- +2pi+ 1d-9)))
                         (sw (if full +2pi+ (arc-sweep a1 a2)))
                         (n (max 8 (min 128 (ceiling (* sw r ppx) 4)))))
                    (line (loop for i from 0 to n collect (polar c (+ a1 (* sw (/ i n))) r)) color lw))))
          (:fill (destructuring-bind (polys color) (rest p)
                   (dolist (poly polys)
                     (let ((q (clip-polygon poly rect)))
                       (when (>= (length q) 3)
                         (push (format nil ".c create polygon ~{~A~^ ~} -fill ~A -outline {} -tags {~A}"
                                       (mapcar #'scr q) (rgb-hex01 color) tags)
                               out))))))
          (:text (destructuring-bind (s pos size rot ha va color font) (rest p)
                   (declare (ignore font))
                   (let ((fs (* size ppx)))
                     (when (and (>= fs 3) (plusp (length (string-trim " " s)))
                                (or (null rect) (destructuring-bind (x0 y0 x1 y1) rect (and (<= x0 (px pos) x1) (<= y0 (py pos) y1)))))
                       (multiple-value-bind (sx sy) (funcall to-screen pos)
                         (let* ((w (* fs (text-em-width s)))
                                (dx (case ha (1 (- (/ w 2))) (2 (- w)) (t 0)))
                                (dy (case va (1 (* 0.207 fs)) (2 (- (* 0.359 fs))) (3 (- (* +cap+ fs))) (t 0)))
                                (c (cos rot)) (sn (sin rot)) (desc (* 0.22 fs))
                                (x (+ sx (* c dx) (- (* sn dy)) (* sn desc)))
                                (y (- sy (+ (* sn dx) (* c dy)) (- (* c desc)))))
                           (push (format nil ".c create text ~A ~A -text ~A -anchor sw -angle ~,2F -fill ~A -font {Helvetica -~D} -tags {~A}"
                                         (fmtn x) (fmtn y) (tq s) (deg rot) (rgb-hex01 color) (max 1 (round fs)) tags)
                                 out)))))))))
      (nreverse out))))

;;; ---------- the layout on the screen ----------

(defmacro push-end (x place) `(setf ,place (nconc ,place (list ,x))))

(defun screen-rect (rect)
  (destructuring-bind (x0 y0 x1 y1) rect
    (multiple-value-bind (ax ay) (paper->screen (pt x0 y1))
      (multiple-value-bind (bx by) (paper->screen (pt x1 y0))
        (list ax ay bx by)))))

(defun sheet-cmds (l &optional tags)
  (multiple-value-bind (w h) (layout-sheet-size l)
    (destructuring-bind (ax ay bx by) (screen-rect (list 0 0 w h))
      (let ((tg (or tags "sheet")))
        (list (format nil ".c create rectangle ~A ~A ~A ~A -fill #2a2e35 -outline {} -tags {~A}" (fmtn (+ ax 5)) (fmtn (+ ay 5)) (fmtn (+ bx 5)) (fmtn (+ by 5)) tg)
              (format nil ".c create rectangle ~A ~A ~A ~A -fill #ffffff -outline #9aa1ab -tags {~A}" (fmtn ax) (fmtn ay) (fmtn bx) (fmtn by) tg)
              (destructuring-bind (px0 py0 px1 py1) (screen-rect (printable-rect l))
                (format nil ".c create rectangle ~A ~A ~A ~A -outline #b8bec7 -dash {4 4} -tags {~A}"
                        (fmtn px0) (fmtn py0) (fmtn px1) (fmtn py1) tg)))))))

(defun vp-content-cmds (e tags)
  (prims-canvas-cmds (vp-display-prims e) tags (vp-rect (ename-data e))))

(defun pspace-body-cmds ()
  (let ((l (current-layout)) (cmds nil))
    (setf cmds (sheet-cmds l))
    (setf cmds (nconc cmds (grid-cmds)))
    (dolist (vp (layout-viewports))
      (when (entity-visible-p vp) (setf cmds (nconc cmds (vp-content-cmds vp "vpc")))))
    (dolist (e (live-entities))
      (when (and (entity-visible-p e) (entity-in-view-p e))
        (setf cmds (nconc cmds (entity-render-cmds e)))))
    cmds))

(defun paper-entity-prims (name)
  "Plot primitives for the paper space objects of layout NAME (as on the screen)."
  (let ((*plot-prims* nil) (*plotting* t) (*prims-for-display* t) (*view* nil) (*space-override* name))
    (dolist (e (live-entities))
      (when (entity-visible-p e)
        (ignore-errors (plot-data (ename-data e) (entity-aci (ename-data e))))))
    (reverse *plot-prims*)))

(defun mspace-body-cmds ()
  "Working in a viewport: the model as usual inside it; the sheet, the other viewports and the
paper space objects drawn over everything outside it."
  (let* ((l (current-layout)) (vp (drawing-active-vp *dwg*)) (d (ename-data vp))
         (r (screen-rect (vp-rect d))) (cmds nil))
    (destructuring-bind (ax ay bx by) r
      (push (format nil ".c create rectangle ~A ~A ~A ~A -fill #ffffff -outline {} -tags sheet" (fmtn ax) (fmtn ay) (fmtn bx) (fmtn by)) cmds)
      (setf cmds (nconc cmds (model-body-cmds)))
      ;; outside the viewport: the gray around the sheet, then the sheet itself
      (let* ((big 100000) (bands (list (list (- big) (- big) big ay) (list (- big) by big big)
                                       (list (- big) ay ax by) (list bx ay big by))))
        (dolist (b bands)
          (push-end (format nil ".c create rectangle ~{~A~^ ~} -fill ~A -outline {} -tags pover" (mapcar #'fmtn b) (canvas-bg)) cmds))
        (multiple-value-bind (w h) (layout-sheet-size l)
          (destructuring-bind (sx0 sy0 sx1 sy1) (screen-rect (list 0 0 w h))
            (dolist (b bands)
              (destructuring-bind (bx0 by0 bx1 by1) b
                (let ((x0 (max bx0 sx0)) (y0 (max by0 sy0)) (x1 (min bx1 sx1)) (y1 (min by1 sy1)))
                  (when (and (< x0 x1) (< y0 y1))
                    (push-end (format nil ".c create rectangle ~A ~A ~A ~A -fill #ffffff -outline {} -tags pover"
                                      (fmtn x0) (fmtn y0) (fmtn x1) (fmtn y1))
                              cmds))))))))
      (dolist (other (layout-viewports))
        (unless (or (eq other vp) (not (entity-visible-p other)))
          (setf cmds (nconc cmds (vp-content-cmds other "pover")))))
      (setf cmds (nconc cmds (prims-canvas-cmds (paper-entity-prims (layout-name l)) "pover")))
      ;; the viewport being worked in: a thick border
      (push-end (format nil ".c create rectangle ~A ~A ~A ~A -outline #1e6fff -width 3 -tags pover" (fmtn ax) (fmtn ay) (fmtn bx) (fmtn by)) cmds))
    cmds))

(defun layout-body-cmds ()
  (if (drawing-active-vp *dwg*) (mspace-body-cmds) (pspace-body-cmds)))

(defun layout-extents ()
  "The sheet together with everything on it (for ZOOM Extents on a layout)."
  (let ((l (current-layout)) (b (drawing-extents)))
    (multiple-value-bind (w h) (layout-sheet-size l)
      (if b
          (list (min 0 (first b)) (min 0 (second b)) (max w (third b)) (max h (fourth b)))
          (list 0d0 0d0 w h)))))

;;; ---------- layout tabs ----------

(defvar *ltabs-shown* nil)

(defun refresh-layout-tabs (&optional force)
  (when (gui-p)
    (let* ((names (cons "Model" (mapcar #'layout-name (drawing-layouts *dwg*))))
           (cur (if (drawing-ctab *dwg*) (1+ (or (position (drawing-ctab *dwg*) (drawing-layouts *dwg*) :key #'layout-name :test #'string-equal) 0)) 0))
           (status (cond ((null (drawing-ctab *dwg*)) "MODEL") ((drawing-active-vp *dwg*) "MODEL (viewport)") (t "PAPER")))
           (key (list names cur status)))
      (when (or force (not (equal key *ltabs-shown*)))
        (setf *ltabs-shown* key)
        (tk "setlayouttabs ~A ~D ~A" (tcl-list names) cur (tq status))))))

(defun layout-index-arg ()
  "The tab number after LAYOUTTAB and friends (0 = Model)."
  (let ((s (or (pop *script-queue*) (gstring "Tab number: "))))
    (ignore-errors (parse-integer (string-trim " " s)))))

(defun layout-by-index (i) (and i (plusp i) (nth (1- i) (drawing-layouts *dwg*))))

(defcmd "LAYOUTTAB" () "Shows a layout tab by its number (0 = Model); used by the tabs under the drawing."
  (let ((i (layout-index-arg)))
    (cond ((eql i 0) (set-ctab nil))
          ((layout-by-index i) (set-ctab (layout-name (layout-by-index i)))))))

(defcmd "MODEL" ("TILEMODE1") "Shows the Model tab." (set-ctab nil))

;;; ---------- LAYOUT ----------

(defun unique-layout-name (&optional (base "Layout"))
  (loop for i from 1 for n = (format nil "~A~D" base i) unless (find-layout n) return n))

(defun valid-layout-name-p (n)
  (and (plusp (length n)) (<= (length n) 255) (not (string-equal n "Model"))
       (not (find-if (lambda (c) (find c "<>/\\\":;?*|,=`")) n))))

(defun new-layout (name &optional template)
  (let ((l (make-layout :name name)))
    (when template
      (setf (layout-paper l) (layout-paper template) (layout-landscape l) (layout-landscape template)
            (layout-units l) (layout-units* template)))
    (setf (drawing-layouts *dwg*) (append (drawing-layouts *dwg*) (list l)))
    (touch)
    l))

(defun duplicate-layout (from name)
  (let ((l (new-layout name from)))
    (let ((*space-override* name))
      (dolist (e (layout-entities (layout-name from)))
        (add-entity (copy-tree (rem-dxf 5 (ename-data e))))))
    l))

(defun delete-layout (l)
  (cond ((<= (length (drawing-layouts *dwg*)) 1)
         (out (format nil "A drawing needs at least one layout: ~A was not deleted.~%" (layout-name l))) nil)
        (t
         (when (equal (drawing-ctab *dwg*) (layout-name l))
           (let ((others (remove l (drawing-layouts *dwg*))))
             (set-ctab (layout-name (first others)))))
         (with-undo-group (lambda () (dolist (e (layout-entities (layout-name l))) (delete-entity e))))
         (setf (drawing-layouts *dwg*) (remove l (drawing-layouts *dwg*)))
         (touch) (refresh-layout-tabs t)
         t)))

(defun rename-layout (l new)
  (let ((old (layout-name l)))
    (cond ((not (valid-layout-name-p new)) (out (format nil "~A can't be used as a layout name.~%" new)) nil)
          ((and (find-layout new) (not (eq (find-layout new) l))) (out (format nil "A layout named ~A already exists.~%" new)) nil)
          (t (dolist (e (layout-entities old)) (setf (ename-data e) (set-vp-key :layout new (ename-data e))))
             (setf (layout-name l) new)
             (when (equal (drawing-ctab *dwg*) old) (setf (drawing-ctab *dwg*) new (sysvar "CTAB") new))
             (touch) (refresh-layout-tabs t)
             t))))

(defun ask-layout (prompt &optional (default (drawing-ctab *dwg*)))
  (let* ((s (gstring (format nil "~A~@[ <~A>~]: " prompt default) t))
         (s (if (string= s "") default s)))
    (and s (or (find-layout s) (progn (out (format nil "Layout ~A not found.~%" s)) nil)))))

(defcmd "LAYOUT" ("-LAYOUT" "LO") "Makes, copies, deletes, renames and shows layouts (paper space sheets)."
  (let ((r (gkw "Enter layout option [Copy/Delete/New/Rename/Set/?] <set>: "
                '("Copy" "Delete" "New" "Rename" "Set" "?") "Set")))
    (cond
      ((equal r "New")
       (let* ((def (unique-layout-name)) (n (gstring (format nil "Enter name of new layout <~A>: " def) t))
              (n (if (string= n "") def n)))
         (cond ((find-layout n) (out (format nil "A layout named ~A already exists.~%" n)))
               ((not (valid-layout-name-p n)) (out (format nil "~A can't be used as a layout name.~%" n)))
               (t (new-layout n (current-layout)) (set-ctab n) (out (format nil "Layout ~A made.~%" n))))))
      ((equal r "Copy")
       (let ((from (ask-layout "Enter name of layout to copy" (or (drawing-ctab *dwg*) (layout-name (first (drawing-layouts *dwg*)))))))
         (when from
           (let* ((def (format nil "~A (2)" (layout-name from))) (n (gstring (format nil "Enter layout name for copy <~A>: " def) t))
                  (n (if (string= n "") def n)))
             (if (or (find-layout n) (not (valid-layout-name-p n)))
                 (out (format nil "~A can't be used: pick another name.~%" n))
                 (progn (duplicate-layout from n) (set-ctab n) (out (format nil "Layout ~A copied to ~A.~%" (layout-name from) n))))))))
      ((equal r "Delete")
       (let ((l (ask-layout "Enter name of layout to delete")))
         (when (and l (delete-layout l)) (out (format nil "Layout ~A deleted.~%" (layout-name l))))))
      ((equal r "Rename")
       (let ((l (ask-layout "Enter layout to rename")))
         (when l
           (let ((n (gstring (format nil "Enter new layout name <~A>: " (layout-name l)) t)))
             (unless (string= n "")
               (let ((old (layout-name l))) (when (rename-layout l n) (out (format nil "Layout ~A renamed to ~A.~%" old n)))))))))
      ((equal r "?")
       (out (format nil "Layouts:~%~{  ~A~%~}" (mapcar (lambda (l) (multiple-value-bind (w h) (layout-sheet-size l)
                                                                      (format nil "~A  (~A x ~A ~A, ~D viewport~:P)~:[~; - current~]"
                                                                              (layout-name l) (fmt-num w) (fmt-num h) (paper-unit-name l)
                                                                              (length (layout-viewports (layout-name l)))
                                                                              (equal (layout-name l) (drawing-ctab *dwg*)))))
                                                        (drawing-layouts *dwg*)))))
      (t
       (let* ((s (gstring (format nil "Enter layout to make current <~A>: " (or (drawing-ctab *dwg*) "Model")) t)))
         (cond ((string= s "") nil)
               ((string-equal s "Model") (set-ctab nil))
               ((find-layout s) (set-ctab s))
               (t (out (format nil "Layout ~A not found.~%" s)))))))))

(defcmd "LAYOUTNEW" () "Adds a layout (the + after the layout tabs)."
  (let ((n (unique-layout-name)))
    (new-layout n (or (current-layout) (first (drawing-layouts *dwg*))))
    (set-ctab n)
    (out (format nil "Layout ~A made. MVIEW puts viewports on it.~%" n))))

(defcmd "LAYOUTRENAME" () "Renames a layout from its tab."
  (let ((l (layout-by-index (layout-index-arg))))
    (when l
      (let ((n (if (use-inplace-editor-p)
                   (cdr (assoc "NAME" (gui-form "Rename Layout" (list (list "NAME" "Layout name" (layout-name l) :entry)) "Rename") :test #'string=))
                   (gstring (format nil "Enter new layout name <~A>: " (layout-name l)) t))))
        (when (and n (string/= n "") (string/= n (layout-name l)))
          (rename-layout l (string-trim " " n)))))))

(defcmd "LAYOUTDELETE" () "Deletes a layout from its tab."
  (let ((l (layout-by-index (layout-index-arg))))
    (when (and l (or (not (use-inplace-editor-p))
                     (equal "yes" (gui-confirm (format nil "Delete layout ~A and everything on it?" (layout-name l))))))
      (when (delete-layout l) (out (format nil "Layout ~A deleted.~%" (layout-name l)))))))

(defcmd "LAYOUTCOPY" () "Copies a layout from its tab."
  (let ((l (layout-by-index (layout-index-arg))))
    (when l
      (let ((n (loop for i from 2 for n = (format nil "~A (~D)" (layout-name l) i) unless (find-layout n) return n)))
        (duplicate-layout l n) (set-ctab n)))))

(defcmd "LAYOUTMOVE" () "Moves a layout tab one place left or right."
  (let* ((i (layout-index-arg)) (dir (layout-index-arg)) (l (layout-by-index i)) (ls (drawing-layouts *dwg*)))
    (when (and l dir)
      (let* ((k (position l ls)) (j (+ k (if (minusp dir) -1 1))))
        (when (< -1 j (length ls))
          (rotatef (nth k ls) (nth j ls))
          (setf (drawing-layouts *dwg*) ls)
          (touch) (refresh-layout-tabs t))))))

;;; ---------- PAGESETUP ----------

(defun paper-name-for-dims (a b &optional (units :in))
  "The standard paper whose size matches A x B (either way round), in UNITS."
  (let ((k (if (eq units :mm) 25.4d0 1d0)) (lo (min a b)) (hi (max a b)) (best nil) (bd nil))
    (dolist (p *paper-sizes* (first best))
      (let ((dd (+ (abs (- lo (* k (min (second p) (third p))))) (abs (- hi (* k (max (second p) (third p))))))))
        (when (or (null bd) (< dd bd)) (setf best p bd dd))))))

(defun page-setup (l)
  (if (use-inplace-editor-p)
      (let ((r (gui-form (format nil "Page Setup - ~A" (layout-name l))
                         (list (list "PAPER" "Paper size" (layout-paper l) :combo (mapcar #'first *paper-sizes*))
                               (list "ORIENT" "Orientation" (if (layout-landscape l) "Landscape" "Portrait") :combo '("Landscape" "Portrait"))
                               (list "UNITS" "Paper units" (if (eq (layout-units* l) :mm) "Millimeters" "Inches") :combo '("Inches" "Millimeters")))
                         "OK")))
        (when r
          (let ((p (cdr (assoc "PAPER" r :test #'string=))) (o (cdr (assoc "ORIENT" r :test #'string=)))
                (u (cdr (assoc "UNITS" r :test #'string=))))
            (when (assoc p *paper-sizes* :test #'string-equal) (setf (layout-paper l) (first (assoc p *paper-sizes* :test #'string-equal))))
            (when o (setf (layout-landscape l) (string-equal o "Landscape")))
            (when u (set-layout-units l (if (string-equal u "Millimeters") :mm :in))))))
      (let* ((p (gstring (format nil "Enter paper size <~A>: " (layout-paper l)) t))
             (o (gkw (format nil "Enter orientation [Portrait/Landscape] <~:[Portrait~;Landscape~]>: " (layout-landscape l))
                     '("Portrait" "Landscape") (if (layout-landscape l) "Landscape" "Portrait"))))
        (let ((m (find-if (lambda (x) (search (string-upcase (string-trim " " p)) (string-upcase (first x)))) *paper-sizes*)))
          (when (and m (string/= p "")) (setf (layout-paper l) (first m))))
        (setf (layout-landscape l) (equal o "Landscape"))))
  (touch)
  (multiple-value-bind (w h) (layout-sheet-size l)
    (out (format nil "Layout ~A: ~A, ~A x ~A ~A.~%" (layout-name l) (layout-paper l) (fmt-num w) (fmt-num h) (paper-unit-name l))))
  (when (and (gui-p) (equal (drawing-ctab *dwg*) (layout-name l)) (not (drawing-active-vp *dwg*)))
    (destructuring-bind (vx vy vs) (sheet-view-values l) (setf *vx* vx *vy* vy *vscale* vs)))
  (when (gui-p) (gui-regen)))

(defun set-layout-units (l units)
  "Change a layout between inches and millimeters: everything on it is rescaled to stay the same size."
  (let ((old (layout-units* l)))
    (unless (eq old units)
      (let ((k (if (eq units :mm) 25.4d0 (/ 25.4d0))) (*space-override* (layout-name l)))
        (dolist (e (live-entities))
          (setf (ename-data e) (xform-data (ename-data e) (xf-scaler (pt 0 0 0) k))))
        (setf (layout-units l) units (layout-pview l) nil)
        (clear-geom-cache)))))

(defcmd "PAGESETUP" ("PAGESETUPS") "Sets the current layout's paper size, orientation and units."
  (let ((l (or (current-layout) (ask-layout "Enter layout" (layout-name (first (drawing-layouts *dwg*)))))))
    (when l (page-setup l))))

(defcmd "LAYOUTPAGESETUP" () "Page setup for a layout from its tab."
  (let ((l (layout-by-index (layout-index-arg))))
    (when l (set-ctab (layout-name l)) (page-setup l))))

;;; ---------- MSPACE / PSPACE ----------

(defun require-layout (what)
  "Layout commands work on a layout tab: from the Model tab they go to the first layout."
  (declare (ignore what))
  (or (in-layout-p)
      (progn (set-ctab (layout-name (first (drawing-layouts *dwg*))))
             (out (format nil "Switched to the ~A tab (paper space).~%" (drawing-ctab *dwg*)))
             (in-layout-p))))

(defcmd "MSPACE" ("MS") "Works in model space through a layout viewport."
  (when (require-layout "MSPACE")
    (let* ((vps (remove-if-not (lambda (e) (and (entity-visible-p e) (vp-on-p (ename-data e)))) (layout-viewports)))
           (target (or (and *vp-target* (member *vp-target* vps) *vp-target*)
                       (and (drawing-active-vp *dwg*) nil)
                       (first vps))))
      (setf *vp-target* nil)
      (cond ((null vps) (out (format nil "There are no viewports on this layout: make one with MVIEW.~%")))
            ((null target) nil)
            (t (enter-viewport target)
               (out (format nil "Working in a viewport (~A, ~A). PSPACE or a double-click outside it goes back to the sheet.~%"
                            (vp-view-name (ename-data target)) (vp-scale-label (ename-data target)))))))))

(defcmd "PSPACE" ("PS") "Goes back to the layout's sheet (paper space) from a viewport."
  (when (require-layout "PSPACE")
    (when (drawing-active-vp *dwg*)
      (leave-viewport)
      (clear-selection-state)
      (after-space-change))))

(defcmd "MSPACETOGGLE" () "Switches between paper space and the last viewport (the MODEL/PAPER button)."
  (cond ((not (in-layout-p)) (set-ctab (layout-name (first (drawing-layouts *dwg*)))))
        ((drawing-active-vp *dwg*) (leave-viewport) (clear-selection-state) (after-space-change))
        (t (let ((vp (first (layout-viewports)))) (if vp (enter-viewport vp) (out (format nil "No viewports here: make one with MVIEW.~%")))))))

;;; ---------- MVIEW ----------

(defun model-view-for-new-viewports ()
  "The view new viewports start with: the Model tab's."
  (first (drawing-model-view *dwg*)))

(defun split-rect (rect n arrangement)
  "RECT split into N viewport rectangles."
  (destructuring-bind (x0 y0 x1 y1) rect
    (let ((xm (/ (+ x0 x1) 2)) (ym (/ (+ y0 y1) 2)))
      (case n
        (2 (if (equal arrangement "Horizontal")
               (list (list x0 ym x1 y1) (list x0 y0 x1 ym))
               (list (list x0 y0 xm y1) (list xm y0 x1 y1))))
        (3 (cond ((equal arrangement "Horizontal")
                  (let ((h (/ (- y1 y0) 3))) (loop for i below 3 collect (list x0 (- y1 (* (1+ i) h)) x1 (- y1 (* i h))))))
                 ((equal arrangement "Vertical")
                  (let ((w (/ (- x1 x0) 3))) (loop for i below 3 collect (list (+ x0 (* i w)) y0 (+ x0 (* (1+ i) w)) y1))))
                 ((equal arrangement "Left")
                  (list (list x0 y0 xm y1) (list xm ym x1 y1) (list xm y0 x1 ym)))
                 (t (list (list x0 ym xm y1) (list x0 y0 xm ym) (list xm y0 x1 y1)))))
        ;; 4: top left, top right, bottom left, bottom right
        (t (list (list x0 ym xm y1) (list xm ym x1 y1) (list x0 y0 xm ym) (list xm y0 x1 ym)))))))

(defun add-viewport (rect view &optional style)
  (let ((d (vp-fit (make-vp-data rect view (pt 0 0 0) 1d0 style))))
    (add-entity d)))

(defun standard-four-viewports (rects)
  "Top (top left), SE Isometric (top right), Front (bottom left) and Right (bottom right): the
three flat views at one scale and lined up, as a drawing's views are."
  (let* ((views (list (make-view "Top") (make-view "SE Isometric") (make-view "Front") (make-view "Right")))
         (datas (loop for r in rects for v in views collect (vp-fit (make-vp-data r v (pt 0 0 0) 1d0) v)))
         (flat (list (first datas) (third datas) (fourth datas)))
         (s (reduce #'min (mapcar #'vp-scale flat))))
    (loop for d in datas for i from 0
          do (add-entity (if (= i 1) d (vp-with-scale d s))))))

(defun ask-area (prompt)
  "Two corners on the sheet, or Fit (the printable area). Returns (x0 y0 x1 y1) or NIL."
  (let ((r (gpoint prompt :keywords '("Fit"))))
    (cond ((or (null r) (equal r "Fit")) (printable-rect (current-layout)))
          ((pointp r)
           (let ((b (get-user-input :corner :base r :prompt "Specify opposite corner: ")))
             (when (pointp b)
               (list (min (px r) (px b)) (min (py r) (py b)) (max (px r) (px b)) (max (py r) (py b))))))
          (t nil))))

(defun pick-viewports (prompt)
  (remove-if-not (lambda (e) (equal (ent-type e) "VIEWPORT")) (get-selection prompt)))

(defcmd "MVIEW" ("MV") "Makes layout viewports: windows on the sheet that show model space."
  (unless (require-layout "MVIEW") (return-from-cmd))
  (when (drawing-active-vp *dwg*) (leave-viewport) (clear-selection-state) (after-space-change))
  (let ((r (gpoint "Specify corner of viewport or [ON/OFF/Fit/Lock/2/3/4] <Fit>: "
                   :keywords '("ON" "OFF" "Fit" "Lock" "2" "3" "4"))))
    (cond
      ((or (null r) (equal r "Fit"))
       (add-viewport (printable-rect (current-layout)) (model-view-for-new-viewports))
       (out (format nil "Viewport made (~A). Double-click inside it to work in it; VPSCALE sets its scale.~%"
                    (vp-scale-label (ename-data (last-entity))))))
      ((pointp r)
       (let ((b (get-user-input :corner :base r :prompt "Specify opposite corner: ")))
         (when (and (pointp b) (> (abs (- (px b) (px r))) 1d-6) (> (abs (- (py b) (py r))) 1d-6))
           (add-viewport (list (min (px r) (px b)) (min (py r) (py b)) (max (px r) (px b)) (max (py r) (py b)))
                         (model-view-for-new-viewports))
           (out (format nil "Viewport made (~A).~%" (vp-scale-label (ename-data (last-entity))))))))
      ((member r '("ON" "OFF") :test #'equal)
       (let ((vps (pick-viewports "Select viewports: ")))
         (with-undo-group (lambda () (dolist (e vps) (set-entity-data e (set-vp-key :off (equal r "OFF") (ename-data e))))))
         (out (format nil "~D viewport~:P turned ~A.~%" (length vps) (if (equal r "OFF") "off" "on")))))
      ((equal r "Lock")
       (let* ((on (equal "ON" (gkw "Viewport View Locking [ON/OFF] <ON>: " '("ON" "OFF") "ON")))
              (vps (pick-viewports "Select viewports: ")))
         (with-undo-group (lambda () (dolist (e vps) (set-entity-data e (set-vp-key :locked on (ename-data e))))))
         (out (format nil "~D viewport~:P ~:[unlocked~;locked: their scale and view stay put~].~%" (length vps) on))))
      ((member r '("2" "3" "4") :test #'equal)
       (let* ((n (parse-integer r))
              (arr (case n
                     (2 (gkw "Enter viewport arrangement [Horizontal/Vertical] <Vertical>: " '("Horizontal" "Vertical") "Vertical"))
                     (3 (gkw "Enter viewport arrangement [Horizontal/Vertical/Left/Right] <Right>: " '("Horizontal" "Vertical" "Left" "Right") "Right"))
                     (t (gkw "Views [Standard (Top, Front, Right, SE Iso)/Same] <Standard>: " '("Standard" "Same") "Standard"))))
              (area (ask-area "Specify first corner or [Fit] <Fit>: ")))
         (when area
           (let ((rects (split-rect area n arr)))
             (with-undo-group
                 (lambda ()
                   (if (and (= n 4) (equal arr "Standard"))
                       (standard-four-viewports rects)
                       (dolist (rc rects) (add-viewport rc (model-view-for-new-viewports))))))
             (out (format nil "~D viewports made~:[~; (Top, SE Isometric, Front and Right, the flat views at one scale)~].~%"
                          n (and (= n 4) (equal arr "Standard")))))))))
    (when (gui-p) (gui-regen))))

;;; ---------- VPSCALE ----------

(defcmd "VPSCALE" ("VPS") "Sets the scale of layout viewports (1:2, 1/4\"=1'-0\", 2:1, Fit ...)."
  (unless (require-layout "VPSCALE") (return-from-cmd))
  (let* ((active (drawing-active-vp *dwg*))
         (vps (if active (list active) (pick-viewports "Select viewports: "))))
    (when vps
      (let* ((cur (vp-scale-label (ename-data (first vps))))
             (s (gstring (format nil "Enter scale (1:2, 1/4\"=1'-0\", 2:1, nXP ...) or Fit <~A>: " cur) t)))
        (cond ((string= s "") nil)
              ((string-equal s "Fit")
               (with-undo-group (lambda () (dolist (e vps) (setf (ename-data e) (vp-fit (ename-data e))) (touch)))))
              (t (let ((v (parse-vp-scale s)))
                   (if (null v)
                       (out (format nil "Scale not understood: ~A~%" s))
                       (dolist (e vps) (setf (ename-data e) (vp-with-scale (ename-data e) v)) (touch))))))
        (clear-geom-cache)
        (when active (load-vp-globals (ename-data active)))
        (out (format nil "Viewport scale: ~A.~%" (vp-scale-label (ename-data (first vps)))))
        (when (gui-p) (gui-regen))))))

;;; ---------- Properties palette rows ----------

(defun vs-label (st) (if (equal st "ShadedEdges") "Shaded with Edges" st))
(defun vs-from-label (s)
  (cond ((string-equal s "Shaded with Edges") "ShadedEdges")
        ((member s '("Wireframe" "Hidden" "Shaded") :test #'string-equal) (string-capitalize s))
        (t nil)))

(defun viewport-palette-rows (sel)
  (flet ((v (fn) (common-value sel fn)))
    (list '("" "Viewport" "" :head)
          (list "VPSCALE" "Standard scale" (v #'vp-scale-label) :combo (cons "Fit" (vp-scale-choices)))
          (list "VPCUSTOM" "Custom scale" (v (lambda (d) (fmt-factor (vp-scale d)))) :entry)
          (list "VPVIEW" "View" (v #'vp-view-name) :combo (mapcar #'first *standard-views*))
          (list "VPSTYLE" "Visual style" (v (lambda (d) (vs-label (vp-style d)))) :combo '("Wireframe" "Hidden" "Shaded" "Shaded with Edges"))
          (list "VPLOCK" "Display locked" (v (lambda (d) (if (vp-locked-p d) "Yes" "No"))) :combo '("No" "Yes"))
          (list "VPON" "On" (v (lambda (d) (if (vp-on-p d) "Yes" "No"))) :combo '("Yes" "No"))
          (list "VPW" "Width" (v (lambda (d) (fmt-num (vp-width d)))) :entry)
          (list "VPH" "Height" (v (lambda (d) (fmt-num (vp-height d)))) :entry)
          (list "CX" "Center X" (v (lambda (d) (fmt-num (px (vp-center d))))) :entry)
          (list "CY" "Center Y" (v (lambda (d) (fmt-num (py (vp-center d))))) :entry))))

(defun viewport-palette-edit (d key val num)
  (cond
    ((string= key "VPSCALE")
     (if (string-equal val "Fit") (vp-fit d) (let ((s (scale-text-value val))) (if s (vp-with-scale d s) d))))
    ((string= key "VPCUSTOM") (let ((s (parse-vp-scale val))) (if s (vp-with-scale d s) d)))
    ((string= key "VPVIEW")
     (if (assoc val *standard-views* :test #'string-equal)
         ;; a new direction: look at the model's middle from it, at the same scale
         (let* ((view (make-view val)) (s (vp-scale d)) (fitted (vp-fit (set-vp-key :view view d) view)))
           (vp-with-scale fitted s))
         d))
    ((string= key "VPSTYLE") (let ((st (vs-from-label val))) (if st (set-vp-key :vstyle (and (not (equal st "Wireframe")) st) d) d)))
    ((string= key "VPLOCK") (set-vp-key :locked (string-equal val "Yes") d))
    ((string= key "VPON") (set-vp-key :off (string-equal val "No") d))
    ((and (string= key "VPW") num (plusp num)) (set-dxf 40 (d num) d))
    ((and (string= key "VPH") num (plusp num)) (set-dxf 45 (* (vp-view-height d) (/ num (vp-height d))) (set-dxf 41 (d num) d)))
    (t d)))

;;; ---------- plotting a layout ----------

(defun layout-plot-prims (l ppu)
  "Everything on layout L as plot primitives in paper coordinates: each viewport's view (clipped
to it), then the paper space objects on top."
  (let ((out nil))
    (dolist (vp (layout-viewports (layout-name l)))
      (when (entity-visible-p vp)
        (let ((d (ename-data vp)))
          (when (vp-on-p d)
            (setf out (append out (list (cons :clip (vp-rect d))) (vp-paper-prims d ppu) (list (list :unclip))))))))
    (multiple-value-bind (w h) (layout-sheet-size l)
      (let ((paper (let ((*view* nil) (*space-override* (layout-name l)) (*vstyle-override* "Wireframe") (*plot-ppu* ppu))
                     (collect-plot-prims (- w) (- h) (* 2 w) (* 2 h)))))
        (append out paper)))))

(defun make-layout-plot (s path)
  "Plot the current layout at 1:1 (or shrunk to fit a smaller paper) to PDF PATH."
  (let* ((l (current-layout))
         (paper (or (assoc (plot-settings-paper s) *paper-sizes* :test #'string-equal) (third *paper-sizes*)))
         (pw (* 72 (if (plot-settings-landscape s) (max (second paper) (third paper)) (min (second paper) (third paper)))))
         (ph (* 72 (if (plot-settings-landscape s) (min (second paper) (third paper)) (max (second paper) (third paper)))))
         (unit-pt (if (eq (layout-units* l) :mm) (/ 72 25.4d0) 72d0)))
    (multiple-value-bind (w h) (layout-sheet-size l)
      (let* ((fits (and (<= (* w unit-pt) (+ pw 0.5)) (<= (* h unit-pt) (+ ph 0.5))))
             (ppu (if fits unit-pt (min (/ pw w) (/ ph h))))
             (offset (list (/ (- pw (* w ppu)) 2) (/ (- ph (* h ppu)) 2)))
             (prims (let ((*plot-mono* (plot-settings-mono s)) (*plot-lw* (plot-settings-lineweights s)) (*plot-ppu* ppu))
                      (layout-plot-prims l ppu))))
        (write-plot-pdf path prims :paper-w pw :paper-h ph :window (list 0 0 w h) :scale-ppu ppu :offset offset
                                   :title (format nil "~A - ~A" (drawing-display-name) (layout-name l)))
        (format nil "layout ~A, ~A, ~A" (layout-name l) (string-trim " " (first (split-string (first paper) #\()))
                (if fits "1:1" "shrunk to fit the paper"))))))

(defun layout-plot-settings (s)
  "Plot settings for the current layout: its paper and orientation, the whole sheet at 1:1."
  (let ((l (current-layout)) (s (copy-plot-settings s)))
    (setf (plot-settings-paper s) (layout-paper l) (plot-settings-landscape s) (layout-landscape l)
          (plot-settings-area s) "Layout" (plot-settings-scale s) "1:1")
    s))

;;; ---------- DXF: viewports in and out ----------

(defun view-from-direction (dir twist)
  "Our view list for a DXF view direction DIR (toward the viewer) and twist angle."
  (let* ((dir (unit3 (pt3 dir))))
    (if (and (< (abs (px dir)) 1d-9) (< (abs (py dir)) 1d-9) (plusp (pz dir)) (< (abs twist) 1d-9))
        nil
        (let ((named (find-if (lambda (spec) (< (dist (unit3 (pt3 (second spec))) dir) 1d-6)) *standard-views*)))
          (if (and named (< (abs twist) 1d-9) (not (string-equal (first named) "Top")))
              (make-view (first named))
              (destructuring-bind (r u) (dcs-axes dir)
                (let ((c (cos twist)) (sn (sin twist)))
                  ;; twist turns the picture clockwise on the screen
                  (list "Custom" (v+ (v* r c) (v* u (- sn))) (v+ (v* u c) (v* r sn)) dir :orbit))))))))

(defun dcs-axes (dir)
  "AutoCAD's display axes (right up) for view direction DIR: right = Z x DIR (X when looking along Z)."
  (let ((dir (unit3 (pt3 dir))))
    (if (and (< (abs (px dir)) 1d-9) (< (abs (py dir)) 1d-9))
        (let ((r (pt 1 0 0))) (list r (unit3 (cross3 dir r))))
        (let ((r (unit3 (cross3 (pt 0 0 1) dir)))) (list r (unit3 (cross3 dir r)))))))

(defun view-uv->world (view uv)
  "The world point at view coordinates UV of VIEW (on the working plane for the flat views)."
  (let ((*view* view) (*ucs-off* nil))
    (cond ((null view) (pt (px uv) (py uv) 0))
          ((ucs) (ucs-point-to-world (pt (px uv) (py uv) 0)))
          (t (v+ (v* (second view) (px uv)) (v* (third view) (py uv)))))))

(defun world->view-uv (view p)
  (let ((*view* view) (*ucs-off* nil))
    (cond ((null view) (pt (px p) (py p)))
          ((ucs) (let ((q (funcall (xf-fn (ucs-xf (ucs))) p))) (pt (px q) (py q))))
          (t (pt (vdot3 p (second view)) (vdot3 p (third view)))))))

(defun vp-dxf-view (d)
  "(values direction dcs-center twist) for writing viewport D."
  (let* ((view (vp-view d)) (dir (if view (pt3 (fourth view)) (pt 0 0 1)))
         (w (view-uv->world view (vp-view-center d))))
    (destructuring-bind (r u) (dcs-axes dir)
      (let* ((vr (if view (pt3 (second view)) (pt 1 0 0)))
             (twist (atan (- (vdot3 vr u)) (vdot3 vr r))))
        (values dir (pt (vdot3 w r) (vdot3 w u)) (if (< (abs twist) 1d-9) 0d0 twist))))))

(defun vp-from-dxf (data)
  "Internal viewport data from a DXF VIEWPORT, or NIL for a layout's own (paper) viewport."
  (unless (eql (dxf 69 data) 1)
    (let* ((dir (or (dxf 16 data) (pt 0 0 1)))
           (twist (rad (d (or (dxf 51 data) 0))))
           (view (view-from-direction dir twist))
           (dc (or (dxf 12 data) (pt 0 0)))
           (w (destructuring-bind (r u) (dcs-axes dir) (v+ (v* r (px dc)) (v* u (py dc)))))
           (style (case (dxf 281 data) (2 "Hidden") (3 "Shaded") ((4 5 6) "ShadedEdges") (t nil)))
           (flags (or (dxf 90 data) 0))
           (d (make-vp-data (let ((c (pt3 (or (dxf 10 data) (pt 0 0)))) (hw (/ (d (or (dxf 40 data) 1)) 2)) (hh (/ (d (or (dxf 41 data) 1)) 2)))
                              (list (- (px c) hw) (- (py c) hh) (+ (px c) hw) (+ (py c) hh)))
                            view (world->view-uv view w) (d (or (dxf 45 data) (dxf 41 data) 1)) style)))
      (when (dxf 8 data) (setf d (append d (list (cons 8 (dxf 8 data))))))
      (when (logtest flags 16384) (setf d (set-vp-key :locked t d)))
      (when (or (eql (dxf 68 data) 0) (logtest flags 131072)) (setf d (set-vp-key :off t d)))
      d)))

(defun write-viewport-2000 (out d handle owner id &key overall)
  "A VIEWPORT entity. OVERALL: the layout's own viewport (id 1), the whole sheet."
  (multiple-value-bind (dir dc twist) (if overall (values (pt 0 0 1) (vp-center d) 0d0) (vp-dxf-view d))
    (w out 0 "VIEWPORT") (w out 5 handle) (w out 330 owner) (w out 100 "AcDbEntity") (w out 67 1)
    (w out 8 (or (dxf 8 d) "0"))
    (let ((c (dxf 62 d))) (when c (w out 62 c)))
    (w out 100 "AcDbViewport")
    (wp out 10 (vp-center d)) (w out 40 (vp-width d)) (w out 41 (vp-height d))
    (w out 68 (cond (overall 1) ((vp-on-p d) (1+ id)) (t 0))) (w out 69 id)
    (wp out 12 dc t) (wp out 13 (pt 0 0) t) (wp out 14 (pt 0.5 0.5) t) (wp out 15 (pt 0.5 0.5) t)
    (wp out 16 dir) (wp out 17 (pt 0 0 0))
    (w out 42 50d0) (w out 43 0d0) (w out 44 0d0) (w out 45 (vp-view-height d))
    (w out 50 0d0) (w out 51 (deg twist)) (w out 72 1000)
    (w out 90 (logior 32864 (if (vp-locked-p d) 16384 0) (if (vp-on-p d) 0 131072)))
    (w out 1 "")
    (w out 281 (let ((st (vp-style d))) (cond ((equal st "Hidden") 2) ((equal st "Shaded") 3) ((equal st "ShadedEdges") 5) (t 0))))
    (w out 71 1) (w out 74 0)
    (wp out 110 (pt 0 0 0)) (wp out 111 (pt 1 0 0)) (wp out 112 (pt 0 1 0)) (w out 79 0) (w out 146 0d0)))

(defun overall-vp-data (l)
  (multiple-value-bind (w h) (layout-sheet-size l)
    (make-vp-data (list (* -0.05 w) (* -0.05 h) (* 1.05 w) (* 1.05 h)) nil (pt (/ w 2) (/ h 2)) (* 1.1d0 h))))

(defun dxf-paper-mm (l)
  "(values short long rotation units) for the LAYOUT object's plot settings."
  (let* ((p (or (assoc (layout-paper l) *paper-sizes* :test #'string-equal) (third *paper-sizes*)))
         (a (* 25.4d0 (min (second p) (third p)))) (b (* 25.4d0 (max (second p) (third p)))))
    (values (round2 a) (round2 b) (if (layout-landscape l) 1 0) (if (eq (layout-units* l) :mm) 1 0))))

(defun write-layout-object (out handle owner-dict name tab br-handle last-vp &optional l)
  (w out 0 "LAYOUT") (w out 5 handle)
  (w out 102 "{ACAD_REACTORS") (w out 330 owner-dict) (w out 102 "}")
  (w out 330 owner-dict)
  (w out 100 "AcDbPlotSettings") (w out 1 "") (w out 2 "none_device")
  (multiple-value-bind (a b rot units) (if l (dxf-paper-mm l) (values 279.4d0 431.8d0 1 0))
    (w out 4 (if l (substitute #\_ #\Space (string-trim " " (first (split-string (layout-paper l) #\()))) ""))
    (w out 6 "") (w out 40 6.35d0) (w out 41 6.35d0) (w out 42 6.35d0) (w out 43 6.35d0)
    (w out 44 a) (w out 45 b) (w out 46 0d0) (w out 47 0d0) (w out 48 0d0) (w out 49 0d0)
    (w out 140 0d0) (w out 141 0d0) (w out 142 1d0) (w out 143 1d0)
    (w out 70 (if l 688 1712)) (w out 72 units) (w out 73 rot) (w out 74 (if l 5 0)) (w out 7 "")
    (w out 75 (if l 16 0)) (w out 147 1d0) (w out 148 0d0) (w out 149 0d0))
  (w out 100 "AcDbLayout") (w out 1 name) (w out 70 1) (w out 71 tab)
  (multiple-value-bind (sw sh) (if l (layout-sheet-size l) (values 12d0 9d0))
    (wp out 10 (pt 0 0) t) (wp out 11 (pt sw sh) t) (wp out 12 (pt 0 0 0))
    (wp out 14 (pt 0 0 0)) (wp out 15 (pt sw sh 0)))
  (w out 146 0d0) (wp out 13 (pt 0 0 0)) (wp out 16 (pt 1 0 0)) (wp out 17 (pt 0 1 0)) (w out 76 1)
  (w out 330 br-handle)
  (when last-vp (w out 331 last-vp)))

(defun layout-from-dxf (g)
  "(name tab block-record paper landscape units) from a LAYOUT object group."
  (let* ((pos (position-if (lambda (p) (and (consp p) (eql (car p) 100) (equal (cdr p) "AcDbLayout"))) g))
         (head (subseq g 0 (or pos (length g)))) (tail (if pos (nthcdr pos g) nil)))
    (flet ((hv (code) (cdr (assoc code head))) (tv (code) (cdr (assoc code tail))))
      (let* ((a (d (or (hv 44) 279.4))) (b (d (or (hv 45) 431.8))) (rot (or (hv 73) 0))
             (units (if (eql (hv 72) 1) :mm :in))
             (w (if (oddp rot) b a)) (h (if (oddp rot) a b)))
        (list (tv 1) (or (tv 71) 0) (and (tv 330) (princ-to-string (tv 330)))
              (paper-name-for-dims (/ a 25.4d0) (/ b 25.4d0)) (> w h) units)))))

(defun import-layouts (objects tables replace)
  "Read a file's LAYOUT objects into the drawing's layouts. Returns a function from a paper space
object's owner (block record handle) to its layout's name."
  (declare (ignore tables))
  (let ((found (loop for g in objects when (equal (cdr (first g)) "LAYOUT") collect (layout-from-dxf g)))
        (map (make-hash-table :test #'equalp)))
    (setf found (sort (remove-if (lambda (f) (or (null (first f)) (string-equal (first f) "Model"))) found)
                      #'< :key (lambda (f) (or (second f) 0))))
    (when (and found replace) (setf (drawing-layouts *dwg*) nil))
    (dolist (f found)
      (destructuring-bind (name tab br paper land units) f
        (declare (ignore tab))
        (let ((l (ensure-layout name)))
          (setf (layout-paper l) paper (layout-landscape l) land (layout-units l) units)
          (when br (setf (gethash br map) (layout-name l))))))
    (unless (drawing-layouts *dwg*) (setf (drawing-layouts *dwg*) (list (make-layout :name "Layout1"))))
    (let ((default (layout-name (first (drawing-layouts *dwg*)))))
      (lambda (owner) (or (gethash owner map) default)))))
