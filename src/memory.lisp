;;;; memory.lisp — how much memory Fancy Pencil may use, and how fast it runs (Options > Memory and speed).
;;;;
;;;; Two settings, in ~/.config/fancypencil/memory.conf (heap=..., speed=...):
;;;;   heap   the most memory Fancy Pencil may use: "auto" (half the computer's memory) or megabytes.
;;;;          It is fixed when Fancy Pencil starts (SBCL's --dynamic-space-size, set by the
;;;;          fancypencil launcher), so a change takes effect at the next start. It is only a
;;;;          ceiling: memory is taken from the computer when a big model needs it.
;;;;   speed  0 automatic, 1 Light ... 4 Fastest: how much work Fancy Pencil does between clean-ups
;;;;          (garbage collections; SBCL's bytes-consed-between-gcs). Fewer clean-ups make big
;;;;          models (solids, terrain, large drawings) faster and use more memory. Takes effect at once.

(in-package :fancy-pencil)

(defparameter *speed-levels*
  ;; level  name       megabytes between clean-ups
  '((1 "Light"   50)
    (2 "Normal"  200)
    (3 "Fast"    800)
    (4 "Fastest" 2048)))

(defun memory-conf-path ()
  (merge-pathnames ".config/fancypencil/memory.conf"
                   (concatenate 'string (string-right-trim "/" (or (getenv "HOME") "/tmp")) "/")))

(defun read-memory-conf ()
  "(values HEAP SPEED): HEAP :auto or megabytes, SPEED 0 (automatic) to 4."
  (let ((heap :auto) (speed 0))
    (ignore-errors
     (with-open-file (i (memory-conf-path))
       (loop for line = (read-line i nil) while line
             do (let* ((p (position #\= line))
                       (k (and p (string-trim " " (subseq line 0 p))))
                       (v (and p (string-trim " " (subseq line (1+ p))))))
                  (cond ((null k))
                        ((string-equal k "heap")
                         (let ((n (ignore-errors (parse-integer v))))
                           (setf heap (if (and n (>= n 512)) n :auto))))
                        ((string-equal k "speed")
                         (let ((n (ignore-errors (parse-integer v))))
                           (when (and n (<= 0 n 4)) (setf speed n)))))))))
    (values heap speed)))

(defun write-memory-conf (heap speed)
  (let ((p (memory-conf-path)))
    (ensure-directories-exist p)
    (with-open-file (o p :direction :output :if-exists :supersede)
      (format o "# Fancy Pencil memory settings (Options > Memory and speed; safe to delete)~%~
# heap: the most memory Fancy Pencil may use, in MB, or auto (half the computer's memory); used at start-up~%~
# speed: 0 automatic, 1 Light, 2 Normal, 3 Fast, 4 Fastest~%~
heap=~A~%speed=~D~%" (if (eq heap :auto) "auto" heap) speed))))

(defun ram-mb ()
  "The computer's memory in MB (from /proc/meminfo), or NIL."
  (ignore-errors
   (with-open-file (i "/proc/meminfo")
     (loop for line = (read-line i nil) while line
           when (and (> (length line) 9) (string= "MemTotal:" line :end2 9))
             return (floor (parse-integer line :start 9 :junk-allowed t) 1024)))))

(defun auto-heap-mb (ram)
  "What Automatic gives (the same rule as the fancypencil launcher): half the memory, at least 2 GB."
  (max 2048 (floor (or ram 8192) 2)))

(defun heap-limit-mb (ram)
  "The most the heap may be set to: four fifths of the memory (as the launcher allows)."
  (max 2048 (floor (* 4 (or ram 8192)) 5)))

(defun current-heap-mb ()
  #+sbcl (floor (sb-ext:dynamic-space-size) (* 1024 1024)) #-sbcl nil)

(defun used-mb ()
  #+sbcl (floor (sb-kernel:dynamic-usage) (* 1024 1024)) #-sbcl nil)

(defun auto-speed (ram)
  "The speed Automatic picks for a computer with RAM megabytes."
  (cond ((null ram) 2) ((>= ram 32000) 4) ((>= ram 15000) 3) ((>= ram 7500) 2) (t 1)))

(defun speed-bytes (level)
  "Bytes between clean-ups for LEVEL, never more than a tenth of the memory Fancy Pencil has."
  (let ((mb (third (assoc level *speed-levels*))))
    (min (* (or mb 200) 1024 1024)
         (max (* 50 1024 1024) (floor (* (or (current-heap-mb) 4096) 1024 1024) 10)))))

(defun apply-speed (&optional speed)
  "Sets how much work Fancy Pencil does between clean-ups. Returns the level used."
  (let* ((s (or speed (nth-value 1 (read-memory-conf))))
         (level (if (and (integerp s) (<= 1 s 4)) s (auto-speed (ram-mb)))))
    #+sbcl
    (let ((new (speed-bytes level)))
      ;; SBCL won't lower it while more than the new amount is waiting to be cleaned up: clean up first
      (when (< new (sb-ext:bytes-consed-between-gcs)) (sb-ext:gc))
      (handler-case (setf (sb-ext:bytes-consed-between-gcs) new)
        (error () (ignore-errors (sb-ext:gc :full t) (setf (sb-ext:bytes-consed-between-gcs) new)))))
    level))

(defun gb (mb) (if (< mb 1024) (format nil "~D MB" mb)
                   (let ((g (/ mb 1024.0))) (if (< (abs (- g (round g))) 0.05) (format nil "~D GB" (round g)) (format nil "~,1F GB" g)))))

(defun heap-choices (ram)
  "The sizes offered: 2, 4, 8, 16, 24, 32, 48, 64 ... GB up to four fifths of the memory."
  (remove-if (lambda (mb) (> mb (heap-limit-mb ram)))
             (mapcar (lambda (g) (* g 1024)) '(2 4 8 16 24 32 48 64 96 128 192 256))))

(defun memory-dialog-arg ()
  "The Options window's Memory and speed box: {ramMB heapMB usedMB setting speed autoHeapMB autoSpeed {choices}}."
  (let ((ram (or (ram-mb) 0)))
    (multiple-value-bind (heap speed) (read-memory-conf)
      (format nil "{~D ~D ~D ~A ~D ~D ~D {~{~D~^ ~}}}" ram (or (current-heap-mb) 0) (or (used-mb) 0)
              (if (eq heap :auto) "auto" heap) speed (auto-heap-mb ram) (auto-speed ram) (heap-choices ram)))))

(defun memory-report ()
  (let ((ram (ram-mb)))
    (multiple-value-bind (heap speed) (read-memory-conf)
      (format nil "This computer: ~A. Fancy Pencil may use up to ~A (~A), using ~A now.~%~
Speed: ~A~A.~%"
              (if ram (gb ram) "unknown") (gb (current-heap-mb))
              (if (eq heap :auto) "Automatic" (format nil "set to ~A" (gb heap)))
              (gb (used-mb))
              (second (assoc (apply-speed speed) *speed-levels*))
              (if (zerop speed) " (Automatic)" "")))))

(defun set-memory (heap speed)
  "Saves the settings and applies the speed now. True when the memory limit changes at the next start."
  (multiple-value-bind (old-heap) (read-memory-conf)
    (let ((ram (ram-mb)))
      (when (integerp heap) (setf heap (max 1024 (min heap (heap-limit-mb ram))))))
    (write-memory-conf heap speed)
    (apply-speed speed)
    (not (equal old-heap heap))))

(defcmd "MEMORY" ("SPEED" "MEMORYSPEED")
    "How much memory Fancy Pencil may use and how fast it runs (also in Options > Memory and speed)."
  (out (memory-report))
  (if (and (gui-p) (not (from-lisp-p)))
      (cursor-dialog)                     ; the Options window, which has Memory and speed
    (let* ((ram (ram-mb)) (choices (heap-choices ram)))
      (multiple-value-bind (heap speed) (read-memory-conf)
        (let* ((h (gkw (format nil "Most memory Fancy Pencil may use [Auto/~{~D~^/~}] (GB) <~A>: "
                               (mapcar (lambda (mb) (floor mb 1024)) choices)
                               (if (eq heap :auto) "Auto" (floor heap 1024)))
                       (cons "Auto" (mapcar (lambda (mb) (princ-to-string (floor mb 1024))) choices))
                       (if (eq heap :auto) "Auto" (princ-to-string (floor heap 1024)))))
               (s (gkw (format nil "Speed [Auto/Light/Normal/Fast/Fastest] <~A>: "
                               (if (zerop speed) "Auto" (second (assoc speed *speed-levels*))))
                       '("Auto" "Light" "Normal" "Fast" "Fastest")
                       (if (zerop speed) "Auto" (second (assoc speed *speed-levels*)))))
               (new-heap (if (or (null h) (string-equal h "Auto")) :auto
                             (* 1024 (or (ignore-errors (parse-integer h)) 4))))
               (new-speed (if (or (null s) (string-equal s "Auto")) 0
                              (or (first (find s *speed-levels* :key #'second :test #'string-equal)) 0))))
          (if (set-memory new-heap new-speed)
              (out (format nil "Saved. The memory limit changes the next time Fancy Pencil starts; the speed has changed now.~%"))
              (out (format nil "Saved. The speed has changed now.~%"))))))))
