(in-package :quickhoney.imageproc)

(defparameter *button-size* 208)
(defparameter *big-button-size* 318)

(cl-interpol:enable-interpol-syntax)

(defun corner-cutout-coords (image-width image-height radius)
  "Return a list of coordinates that need to be made transparent or
  colored in background color to get a rounded corner effect.
  IMAGE-WIDTH and IMAGE-HEIGHT are the dimensions of the image, RADIUS
  is the desired corner rounding radius.  The list of coordinates that
  is returned is ordered by row and column so that DO-ROWS and
  DO-PIXELS-IN-ROW can be used to iterate over the image and pop
  coordinate pairs off the front of the list at the same time."
  (let ((radius (floor radius))
        (diameter (+ 1 radius radius))
        coords)
    (assert (and (>= image-width diameter)
                 (>= image-height diameter)))
    (with-image (circle diameter diameter)
      (let ((white (allocate-color 255 255 255 :image circle))
            (black (allocate-color 0 0 0 :image circle)))
        (fill-image 0 0 :color white :image circle)
        (draw-filled-circle radius radius radius :color black :image circle)
        (do-rows (y circle)
          (do-pixels-in-row (x)
            (when (eql (raw-pixel) white)
              (push (list (if (< x radius) x (+ (- image-width diameter) x))
                          (if (< y radius) y (+ (- image-height diameter) y)))
                    coords))))))
    (nreverse coords)))

(defun corner-image (&key (image *default-image*)
                          (radius (/ (max (image-width image) (image-height image)) 40))
                          corner-color)
  (with-default-image (image)
    (unless corner-color
      (setf (save-alpha-p) t
            corner-color (if (true-color-p) #x7f000000
                             (or (transparent-color)
                                 (allocate-color 255 255 255 :alpha 127)
                                 (error "can't allocate transparent color for button")))
            (transparent-color) corner-color))
    (let ((coords (corner-cutout-coords (image-width) (image-height) radius)))
      (destructuring-bind (x-tx y-tx) (car coords)
        (do-rows (y)
          (do-pixels-in-row (x)
            (when (and (eql x x-tx)
                       (eql y y-tx))
              (setf (raw-pixel) corner-color)
              (when (cdr coords)
                (setf coords (cdr coords)
                      x-tx (caar coords)
                      y-tx (cadar coords))))))))))

(define-imageproc-handler cutout-button (input-image
                                         &optional keyword
                                                   (background-color "ffffff")
                                                   (button-width "208")
                                                   (button-height "208")
                                                   (radius "8")
                                                   category)
  (let* ((button-width (parse-integer button-width))
         (button-height (parse-integer button-height))
         (button-image (create-image button-width button-height t))
         (category (bknr.utils:make-keyword-from-string category))
         (scale-factor (funcall (if (eq :pixel category)
                                    #'ceiling #'identity)
                                (if (> (/ (image-width input-image) (image-height input-image))
                                       (/ button-width button-height))
                                    (/ button-height (image-height input-image))
                                    (/ button-width (image-width input-image)))))
         (cutout-width (floor (/ button-width scale-factor)))
         (cutout-height (floor (/ button-height scale-factor)))
         (radius (parse-integer radius)))
    (copy-image input-image button-image
                (floor (- (image-width input-image) cutout-width) 2)
                (if (eq :pixel category)
                    (floor (- (image-height input-image) cutout-height) 2)
                    0)
                0 0
                cutout-width cutout-height
                :resize t :resample t
                :dest-width button-width :dest-height button-height)
    (when keyword
      (let ((type-store-image (store-image-with-name (format nil "type-~(~A~)" keyword))))
        (unless type-store-image
          (error "can't find type image for keyword ~A" keyword))
        (with-store-image (type-image type-store-image)
          (copy-image type-image button-image
                      0 0
                      0 0
                      (image-width type-image) (image-height type-image)))))
    (unless (zerop radius)
      (corner-image :image button-image
                    :radius radius
                    :corner-color (parse-color background-color :image button-image)))
    button-image))

(define-imageproc-handler center-thumbnail (input-image width height)
  (setq width (parse-integer width)
	height (parse-integer height))
  (unless (or (> width (image-width input-image))
	      (> height (image-width input-image)))
    (let ((thumbnail-image (create-image width height t)))
      (copy-image input-image thumbnail-image
		  (round (/ (- (image-width input-image) width) 2))
		  (round (/ (- (image-height input-image) height) 2))
		  0 0
		  width height)
      thumbnail-image)))

(defparameter +news-image-width+ 486
  "Width of news images")
(defparameter +news-image-corner-radius+ 8
  "Corner radius for news images")

(define-imageproc-handler news-article-cutout (input-image)
  (let* ((image-height (floor (* +news-image-width+
                                 (/ (image-height input-image) (image-width input-image)))))
         (output-image (create-image +news-image-width+ image-height t)))
    (copy-image input-image output-image
                0 0
                0 0
                +news-image-width+ image-height
                :resize t :resample t
                :dest-width +news-image-width+ :dest-height image-height)
    (corner-image :image output-image
                  :radius +news-image-corner-radius+
                  :corner-color (allocate-color 255 255 255 :image output-image))
    output-image))

(define-imageproc-handler download (input-image filename)
  (setf (tbnl:header-out :content-disposition) #?"attachment; filename=$(filename)")
  input-image)