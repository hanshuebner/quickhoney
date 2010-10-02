(in-package :pixel-pdf)

(defvar *colors* nil)
(defconstant +paper-width+ 600)
(defvar *converter*)

(defclass converter ()
  ((x :initform 0
      :accessor x)
   (y :initform 0
      :accessor y)
   (pixels :reader pixels)
   (seen :reader %seen)
   (color :accessor color)
   (color-map :reader color-map
              :initform (make-hash-table :test #'eql))
   (looking-in-direction :initform :east
                         :accessor looking-in-direction)))

(defmacro with-converter ((&rest args) &body body)
  `(let ((*converter* (apply #'make-instance 'converter ,args)))
     ,@body))

(defun width (converter)
  (array-dimension (pixels converter) 0))

(defun height (converter)
  (array-dimension (pixels converter) 1))

(defun seen (x y)
  (aref (%seen *converter*) x y))

(defun (setf seen) (new-value x y)
  (setf (aref (%seen *converter*) x y) new-value))

(defun convert-color (converter raw-pixel)
  (or (gethash raw-pixel (color-map converter))
      (setf (gethash raw-pixel (color-map converter))
            (cond
              ((cl-gd:true-color-p)
               (ldb (byte 24 0) raw-pixel))
              (t
               (let ((retval 0)
                     (img (cl-gd::img cl-gd:*default-image*)))
                 (setf (ldb (byte 8 16) retval) (cl-gd::gd-image-get-red img raw-pixel)
                       (ldb (byte 8 8) retval) (cl-gd::gd-image-get-green img raw-pixel)
                       (ldb (byte 8 0) retval) (cl-gd::gd-image-get-blue img raw-pixel))
                 retval))))))

(defmethod initialize-instance :after ((converter converter) &key)
  (let ((width (cl-gd:image-width))
        (height (cl-gd:image-height)))
    (with-slots (seen pixels) converter
      (setf seen (make-array (list width height)
                             :element-type 'boolean :initial-element nil)
            pixels (make-array (list width height)))
      (cl-gd:do-rows (y)
        (cl-gd:do-pixels-in-row (x)
          (setf (aref pixels x y) (convert-color converter (cl-gd:raw-pixel)))))))
  (turtle:reset))

(defun in-range (x y)
  (and (< -1 x (width *converter*))
       (< -1 y (height *converter*))))

(defun same-color (x y)
  (when (in-range x y)
    (eql (color *converter*) (aref (pixels *converter*) x y))))

(defun look (direction fn)
  (let ((x (x *converter*))
        (y (y *converter*)))
    (ecase (looking-in-direction *converter*)
      (:east
       (ecase direction
         (:left
          (funcall fn (1+ x) (1+ y)))
         (:forward
          (funcall fn (1+ x) y))
         (:right
          (funcall fn (1+ x) (1- y)))))
      (:south
       (ecase direction
         (:left
          (funcall fn (1+ x) (1- y)))
         (:forward
          (funcall fn x (1- y)))
         (:right
          (funcall fn (1- x) (1- y)))))
      (:west
       (ecase direction
         (:left
          (funcall fn (1- x) (1- y)))
         (:forward
          (funcall fn (1- x) y))
         (:right
          (funcall fn (1- x) (1+ y)))))
      (:north
       (ecase direction
         (:left
          (funcall fn (1- x) (1+ y)))
         (:forward
          (funcall fn x (1+ y)))
         (:right
          (funcall fn (1+ x) (1+ y))))))))

(defun turn (direction)
  (turtle:turn direction)
  (setf (looking-in-direction *converter*)
        (ecase (looking-in-direction *converter*)
          (:east
           (ecase direction
             (:left :north)
             (:right :south)))
          (:south
           (ecase direction
             (:left :east)
             (:right :west)))
          (:west
           (ecase direction
             (:left :south)
             (:right :north)))
          (:north
           (ecase direction
             (:left :west)
             (:right :east))))))

(defun forward ()
  (turtle:forward))

(defun move-to-pixel (x y)
  (setf (x *converter*) x
        (y *converter*) y))

(defun flood-fill ()
  ;; This function certainly is stack hungry.  If needed, increase the
  ;; stack size of the Lisp runtime (SBCL: --control-stack-size 64)
  (labels
      ((maybe-descend (x y)
         (when (and (same-color x y)
                    (not (seen x y)))
           (recurse x y)))
       (recurse (x y)
         (setf (seen x y) t)
         (maybe-descend (1- x) y)
         (maybe-descend (1+ x) y)
         (maybe-descend x (1- y))
         (maybe-descend x (1+ y))))
    (recurse (x *converter*) (y *converter*))))

(defun fill-from (from-x from-y)
  (setf (looking-in-direction *converter*) :east
        (x *converter*) from-x
        (y *converter*) from-y
        (color *converter*) (aref (pixels *converter*) from-x from-y))
  (flood-fill)
  (turtle:reset)
  (turtle:move-to from-x from-y)
  (turtle:set-rgb-fill (ldb (byte 8 16) (color *converter*))
                       (ldb (byte 8 8) (color *converter*))
                       (ldb (byte 8 0) (color *converter*)))
  (turtle:pen-down)
  (turtle:forward)
  (do ()
      ((and (eql from-x (turtle:x))
            (eql from-y (turtle:y))))
    (cond
      ((look :forward #'same-color)
       (cond
         ((look :right #'same-color)
          (look :right #'move-to-pixel)
          (turn :right)
          (turtle:forward))
         (t
          (look :forward #'move-to-pixel)
          (turtle:forward))))
      (t
       (turn :left)
       (turtle:forward))))
  (turtle:pen-up))

(defun convert-pixels-to-pdf (pdf-pathname)
  (with-converter ()
    (pdf:with-document ()
      (let ((bounds (if (> (width *converter*)
                           (height *converter*))
                        pdf:*letter-landscape-page-bounds*
                        pdf:*letter-portrait-page-bounds*)))
        (pdf:with-page (:bounds bounds)
          (let* ((*print-pretty* nil)
                 (border 36)
                 (page-width (- (aref bounds 2) (* border 2)))
                 (page-height (- (aref bounds 3) (* border 2)))
                 (scale (/ 1 (max (/ (width *converter*) page-width)
                                  (/ (height *converter*) page-height))))
                 (x-offset (/ (- page-width (* (width *converter*) scale)) 2))
                 (y-offset (/ (- page-height (* (height *converter*) scale)) 2)))
            (pdf:with-saved-state 
              (pdf:set-transform-matrix scale 0.0 0.0 (- scale)
                                        (+ border x-offset)
                                        (+ border y-offset (* scale (height *converter*))))
              (dotimes (y (height *converter*))
                (dotimes (x (width *converter*))
                  (unless (seen x y)
                    (fill-from x y)))))
            (pdf:with-saved-state
              (pdf:in-text-mode
                (pdf:set-font (pdf:get-font "Helvetica") 7.0)
                (pdf:set-rgb-fill 0.5 0.5 0.5)
		#|
                (pdf:translate (+ border x-offset 3 (* scale (width *converter*)))
                               (+ y-offset 125.5))
                (pdf:rotate -90.0)
		|#
                (pdf:translate (+ border x-offset 7 (* scale (width *converter*)))
                               (+ y-offset 30.5))
                (pdf:rotate 90.0)
                (pdf:show-text (format nil "~C Nana Rausch QuickHoney" #\Copyright_Sign))))))
        (pdf:write-document pdf-pathname)))))

(defun convert-image-file-to-pdf (image-pathname
                                  &optional (pdf-pathname (make-pathname :type "pdf" :defaults image-pathname)))
  (cl-gd:with-image-from-file* (image-pathname)
    (convert-pixels-to-pdf pdf-pathname)))

(defun convert-store-image-to-pdf (store-image pdf-pathname)
  (bknr.images:with-store-image* (store-image)
    (convert-pixels-to-pdf pdf-pathname)))

(defun print-seen ()
  (dotimes (y (height *converter*))
    (dotimes (x (width *converter*))
      (write-char (if (seen x (- (height *converter*) y 1))
                      #\* #\.)
                  *error-output*))
    (terpri *error-output*)))

