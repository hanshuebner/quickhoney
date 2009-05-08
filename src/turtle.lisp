(in-package :turtle)

(defclass turtle ()
  ((x :initform 0
      :accessor turtle-x)
   (y :initform 0
      :accessor turtle-y)
   (directions :initform '#1=(:east :south :west :north . #1#)
               :accessor turtle-directions)
   (drawing :accessor turtle-drawing
            :initform nil)
   (turned :accessor turtle-turned
           :initform nil)))

(defmethod print-object ((turtle turtle) stream)
  (print-unreadable-object (turtle stream :type t)
    (format stream "at ~A/~A looking ~A pen ~:[UP~;DOWN~]~:[~; TURNED~]"
            (turtle-x turtle)
            (turtle-y turtle)
            (turtle-direction turtle)
            (turtle-drawing turtle)
            (turtle-turned turtle))))

(defun line-to (x y)
  ;; optimized pdf:line-to
  (let ((*standard-output* pdf::*page-stream*))
      (princ (float x))
      (princ #\space)
      (princ (float y))
      (princ #\space)
      (princ #\l)
      (terpri)))

(defun set-rgb-fill (r g b)
  ;; optimized pdf:set-rgb-fill
  (let ((*standard-output* pdf::*page-stream*))
    (labels
        ((print-color-float (component)
           (princ (/ (floor (* 1000.0 (/ (float component) 256.0))) 1000.0))))
      (print-color-float r)
      (princ #\Space)
      (print-color-float g)
      (princ #\Space)
      (print-color-float b)
      (princ " rg")
      (terpri))))

(defvar *turtle* (make-instance 'turtle))

(defun turtle-direction (turtle)
  (car (turtle-directions turtle)))

(defun x ()
  (turtle-x *turtle*))

(defun y ()
  (turtle-y *turtle*))

(defun pen-up ()
  (line-to (turtle-x *turtle*) (turtle-y *turtle*))
  (pdf:close-and-fill)
  (setf (turtle-drawing *turtle*) nil
        (turtle-turned *turtle*) nil)
  *turtle*)

(defun pen-down ()
  (setf (turtle-drawing *turtle*) t)
  (pdf:move-to (turtle-x *turtle*) (turtle-y *turtle*))
  (pdf:set-line-width 0.0)
  *turtle*)

(defun move-to (x y)
  (when (turtle-drawing *turtle*)
    (error "turtle can't move while drawing"))
  (setf (turtle-x *turtle*) x
        (turtle-y *turtle*) y)
  *turtle*)

(defun forward ()
  (when (turtle-turned *turtle*)
    (line-to (turtle-x *turtle*) (turtle-y *turtle*))
    (setf (turtle-turned *turtle*) nil))
  (ecase (turtle-direction *turtle*)
    (:east
     (incf (turtle-x *turtle*)))
    (:south
     (decf (turtle-y *turtle*)))
    (:west
     (decf (turtle-x *turtle*)))
    (:north
     (incf (turtle-y *turtle*))))
  *turtle*)

(defun turn (direction)
  (ecase direction
    (:left
     (setf (turtle-directions *turtle*) (cdddr (turtle-directions *turtle*))))
    (:right
     (setf (turtle-directions *turtle*) (cdr (turtle-directions *turtle*)))))
  (setf (turtle-turned *turtle*) t)
  *turtle*)

(defun reset ()
  (setf *turtle* (make-instance 'turtle))
  *turtle*)
