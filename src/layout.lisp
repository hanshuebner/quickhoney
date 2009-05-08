(in-package :quickhoney)

(enable-interpol-syntax)

;;; layouter parameters

(defvar *layout-max-width* 648)
(defvar *layout-max-height* 648)

(defvar *layout-cell-widths* '(54 72 81 108 162 216 324 648))
(defvar *layout-cell-heights* '(54 108 162 216 270 324 378 432 486 540 594 648))
(defvar *thumbnails-per-page* 24)

(defun object-name (object)
  (slot-value object 'name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; layouter classes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cell ()
  ((layout :initarg :layout :accessor cell-layout :documentation "up pointer")))

(defgeneric cell-width (cell))
(defgeneric cell-height (cell) (:method-combination +))
(defgeneric cell-image-height (cell))

(defmethod cell-object ((cell cell)) nil)
(defmethod cell-caption ((cell cell)) nil)
(defmethod cell-keywords ((cell cell)) nil)
(defmethod cell-name ((cell cell)) nil)

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t)
    (format stream "NAME: ~A WIDTH: ~A HEIGHT: ~A"
	    (cell-name cell)
	    (cell-width cell)
	    (cell-height cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass image-cell (cell)
  ((image :initarg :image :reader cell-image)
   (width :initform nil :accessor cell-width)
   (height :initform nil)))

(defmethod cell-height + ((cell image-cell))
	   (cell-image-height cell))

(defmethod cell-image-height ((cell image-cell))
  (slot-value cell 'height))

(defmethod initialize-instance :after ((cell image-cell) &rest args)
  (declare (ignore args))
  (let* ((width (store-image-width (cell-image cell)))
	 (height (store-image-height (cell-image cell)))
	 (num-pixels (* width height))
	 (layout (cell-layout cell)))
    (when (> num-pixels (image-layout-thumbnail-pixel-limit layout))
      (let ((factor (/ (sqrt num-pixels) (sqrt (image-layout-thumbnail-pixel-limit layout)))))
 	(when (> 1.0 factor)
 	  (setq factor (min 1.1 factor))) ; ensure minimum reduction - slightly reduced images don't look good
	(setq width (round (/ width factor))
	      height (round (/ height factor)))))
    (setf (cell-width cell)
	  (or (find (+ 10 width) (image-layout-cell-widths layout) :test #'<)  (image-layout-cell-widths layout)))
    (setf (slot-value cell 'height)
	  (or (find (+ 10 height) (image-layout-cell-heights layout) :test #'<) (image-layout-cell-heights layout)))))

(defmethod cell-caption ((cell image-cell))
  (store-image-name (cell-image cell)))

(defmethod cell-object ((cell image-cell))
  (cell-image cell))

(defmethod cell-keywords ((cell image-cell))
  (store-image-keywords (cell-image cell)))

(defmethod cell-name ((cell image-cell))
  (store-image-name (cell-image cell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item-cell (image-cell)
  ((item :initarg :item :reader cell-item)))

#+(or)
(defmethod cell-image ((cell item-cell))
  (item-poster-image (cell-item cell)))

#+(or)
(defmethod cell-keywords ((cell item-cell))
  (item-image-keywords (cell-item cell)))

(defmethod cell-name ((cell item-cell))
  (string-downcase (symbol-name (object-name (cell-item cell)))))

#+(or)
(defmethod cell-caption ((cell item-cell))
  (article-subject (cell-item cell)))

(defmethod cell-object ((cell item-cell))
  (cell-item cell))

(defmethod cell-height + ((cell item-cell))
	   25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass row ()
  ((cells :initarg :cells :initform nil :accessor row-cells)
   (layout :initarg :layout :reader row-layout)))

(defmethod print-object ((row row) stream)
  (print-unreadable-object (row stream :type t)
    (format stream "WIDTH: ~A FILL-RATE: ~A (~A%) CELLS:~( ~A~)>"
	    (row-width row)
	    (row-fill-rate row)
	    (row-filled-percent row)
	    (row-cells row))))

(defmethod row-width ((row row))
  (reduce #'+ (row-cells row) :key #'cell-width))

(defmethod row-fill-rate ((row row))
  (round (/ (row-width row) *layout-max-width*)))

(defmethod row-filled-percent ((row row))
  (round (/ (row-width row)
	    (/ *layout-max-width* 100))))

(defmethod row-full-p ((row row))
  (eql (row-width row) *layout-max-width*))

(defmethod row-cell-width ((row row))
  (cell-width (first (row-cells row))))

(defmethod row-cell-height ((row row))
  (cell-height (first (row-cells row))))

(defmethod row-height ((row row))
  (apply #'max (mapcar #'cell-height (row-cells row))))

(defmethod row-images ((row row))
  (map 'list #'cell-image (row-cells row)))

(defmethod row-objects ((row row))
  (map 'list #'cell-object (row-cells row)))

(defmethod object-in-row-p (object (row row))
  (find object (row-cells row) :key #'cell-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass page ()
  ((rows :initarg :rows :initform nil :accessor page-rows)
   (layout :initarg :layout :reader page-layout)))

(defmethod page-height ((page page))
  (loop for row in (page-rows page)
	sum (apply #'max (mapcar #'cell-height (row-cells row)))))

(defmethod object-in-page-p (object (page page))
  (find object (page-rows page) :test #'object-in-row-p))

(defmethod page-add-row ((page page) row)
  (with-slots (rows) page
    ;; some day i'll find out how to properly append an atom to a list
    (setf rows (append rows (cons row nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass layout ()
  ((pages :reader layout-pages)
   (page-width :initarg :width :initform *layout-max-width* :reader layout-page-width)
   (page-height :initarg :height :initform *layout-max-height* :reader layout-page-height))
  (:documentation "A layout is a mapping of a list of objects to a number of pages"))

(defgeneric make-cell (layout object))
(defgeneric compute-pages (layout objects))
(defgeneric make-pages (layout))

(defmethod initialize-instance :after ((layout layout) &key objects)
  (compute-pages layout objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass row-layout (layout)
  ((rows :initform nil :accessor row-layout-rows)))

(defgeneric make-cells (layout objects))
(defgeneric sorted-cells (layout cells))
(defgeneric make-rows (row-layout cells))
(defgeneric optimize-layout (row-layout))

(defmethod optimize-layout ((layout row-layout))
  "Default is no optimization")

(defmethod compute-pages ((layout row-layout) objects)
  (with-slots (rows pages) layout
    (setf rows (make-rows layout
			  (sorted-cells layout
					(make-cells layout objects))))
    (optimize-layout layout)
    (setf pages (make-pages layout))))

(defmethod make-pages ((layout row-layout))
  "Creates pages from rows after optimization"
  (let (pages)
    (loop with page = (make-instance 'page :layout layout)
	  for row in (row-layout-rows layout)
	  do (if (or (not (page-rows page))
		     (<= (+ (page-height page)
			    (row-height row)) ; assume that the row is as high as the first cell
			 (layout-page-height layout)))
		 (page-add-row page row)
		 (progn
		   (push page pages)
		   (setf page (make-instance 'page :layout layout :rows (list row)))))
	  finally (push page pages))
    (reverse pages)))

(defun group-in-rows (cells)
  "given a list of cells, return list of lists containing cells with equal dimensions"
  (let ((results-hash (make-hash-table :test #'equal))
	cell-sizes)
    (loop for cell in cells
	  for cell-dims = (list (cell-width cell) (cell-height cell))
	  do (setf (gethash cell-dims results-hash) (cons cell (gethash cell-dims results-hash)))
	  do (unless (equal cell-dims (car cell-sizes))
	       (pushnew cell-dims cell-sizes :test #'equal)))
    (reverse (loop for cell-dims in cell-sizes
		   collect (gethash cell-dims results-hash)))))

(defmethod make-rows ((layout row-layout) cells)
  "given a list of cells, set the rows slot of the layout to a list of row objects"
  (loop for cells in (group-in-rows cells)
	append (loop for group in (group-by cells (if (< (layout-page-width layout) (cell-width (first cells)))
						      1
						      (round (/ (layout-page-height layout) (cell-width (first cells))))))
		     collect (make-instance 'row :layout layout :cells group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass image-layout (layout)
  ((min-thumbnails-per-page :initarg :min-thumbnails-per-page :initform *thumbnails-per-page*
			    :reader image-layout-min-thumbnails-per-page)
   (cell-widths :initarg cell-widths :initform *layout-cell-widths*
		:reader image-layout-cell-widths)
   (cell-heights :initarg cell-heights :initform *layout-cell-heights*
		 :reader image-layout-cell-heights)
   (captions :initarg :captions :initform nil :reader image-layout-captions)))

(defmethod image-layout-thumbnail-pixel-limit ((layout image-layout))
  (/ (* (layout-page-width layout) (layout-page-height layout)) (image-layout-min-thumbnails-per-page layout)))

(defmethod make-cell ((layout image-layout) image)
  (make-instance 'image-cell :layout layout :image image))

(defmethod find-image-page-index ((layout image-layout) image)
  "Return the number of the current page (i.e. the page in which the
 current image is shown) or 0 if the image could not be found"
  (loop for i from 0 by 1
	for page in (layout-pages layout)
	when (object-in-page-p image page)
	do (return-from find-image-page-index i))
  0)

(defmethod make-cells ((layout image-layout) objects)
  (loop for image in objects
	collect (make-cell layout image)))

(defmethod sorted-cells ((layout image-layout) cells)
  (sort (copy-list cells)
	(lambda (a b) (if (= (cell-width a) (cell-width b))
                          (< (cell-height a) (cell-height b))
                          (< (cell-width a) (cell-width b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ecity-layout (image-layout row-layout)
  ())

(defmethod row-image-newest-timestamp ((row row))
  (apply #'max (mapcar #'blob-timestamp (row-images row))))

(defmethod ecity-optimize ((layout ecity-layout) aspect-threshold)
  (let ((rows (row-layout-rows layout))
	unfilled-rows filled-rows)
    (dolist (row rows)
      (if (= 1 (row-fill-rate row))
	  (push row filled-rows)
	  (push row unfilled-rows)))
    (setf unfilled-rows (sort unfilled-rows #'< :key #'row-fill-rate))
    #+layout-debug
    (format t "~a unfilled rows before optimization: ~a~%" (length unfilled-rows) unfilled-rows)
    (let ((target-rows (reverse unfilled-rows)))
      (dolist (row unfilled-rows)
	(unless (= 1 (row-fill-rate row))
	  (let ((cells (row-cells row)))
	    (tagbody retry-this-row
	      (dolist (cell cells)
		(dolist (to-row target-rows)
		  (unless (or (eql to-row row)
			      (= 1 (row-fill-rate to-row))
			      (not (row-images to-row)))
		    (when (< (abs (- (store-image-aspect-ratio (cell-image cell))
				     (/ (row-cell-width to-row) (row-cell-height to-row))))
			     aspect-threshold)
		      ;; note:  we really want to the best, not the first match.  sometime.
		      #+layout-debug
		      (format t "moving cell ~a to row ~a~%" cell to-row)
		      (push cell (row-cells to-row))
		      (setq cells (delete cell cells))
		      (setf (slot-value cell 'width) (row-cell-width to-row))
		      (setf (slot-value cell 'height) (row-cell-height to-row))
		      #+layout-debug
		      (format t "row ~a is now ~a pixels wide (fill-rate is ~a)~%" to-row (row-width to-row) (row-fill-rate to-row))
		      (go retry-this-row))))))
	    (setf (row-cells row) cells)))))
    #+layout-debug
    (format t "after optimization: ~a~%" unfilled-rows)
    (setf rows (append filled-rows (reverse (delete-if-not #'row-images unfilled-rows))))
    (setf rows (sort rows #'> :key #'row-image-newest-timestamp))
    (dolist (row rows)
      (setf (row-cells row) (sort (row-cells row) #'>
				  :key (lambda (cell) (blob-timestamp (cell-image cell))))))
    (setf (row-layout-rows layout) (reverse rows))))

(defmethod unfilled-percentage ((layout ecity-layout))
  (let ((percentage 0))
    (mapc (lambda (row) (incf percentage (- 100 (row-filled-percent row)))) (row-layout-rows layout))
    percentage))

(defmethod optimize-layout ((layout ecity-layout))
  (ecity-optimize layout 0.3)
  (when (> (unfilled-percentage layout) 100)
    (ecity-optimize layout 0.6))
  (when (> (unfilled-percentage layout) 100)
    (ecity-optimize layout 0.9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass keyword-affinity-layout (image-layout row-layout)
  ())

(defmethod sorted-cells ((layout keyword-affinity-layout) cells)
  #+layout-debug
  (format t "; sorting ~a cells~%" (length cells))
  (let ((keywords-hash (make-hash-table))
	keywords-count
	keywords-list)
    (dolist (cell cells)
      (dolist (keyword (cell-keywords cell))
	(if (gethash keyword keywords-hash)
	    (incf (gethash keyword keywords-hash))
	    (setf (gethash keyword keywords-hash) 1))))
    (setq keywords-count (loop for keyword being the hash-keys of keywords-hash
			       for count = (gethash keyword keywords-hash)
			       collect (list keyword count)))
    (setf keywords-count (sort keywords-count (lambda (a b)
                                                (if (= (second a) (second b))
                                                    (string-lessp (symbol-name (first a))
                                                                  (symbol-name (first b)))
                                                    (< (second a) (second b))))))
    (setf keywords-list (loop for (keyword count) in keywords-count collect keyword))
    (setf cells (reverse (sort cells #'string-lessp :key #'cell-name)))
    (dolist (keyword (reverse keywords-list))
      (setf cells (stable-sort cells (lambda (cell1 cell2)
                                       (and (not (find keyword (cell-keywords cell1)))
                                            (find keyword (cell-keywords cell2))))))))
  #+layout-debug
  (format t "; returning ~a cells~%" (length cells))
  cells)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass no-layout (image-layout row-layout)
  ())

(defun index-from-cell-name (cell)
  (do-register-groups (index-string) (#?r"(\d+)$" (cell-name cell))
		      (when index-string
			(return-from index-from-cell-name (parse-integer index-string)))))

(defmethod sorted-cells ((layout no-layout) cells)
  (reverse (sort (copy-list cells) (lambda (cell1 cell2)
                                     (let ((cell1-index (index-from-cell-name cell1))
                                           (cell2-index (index-from-cell-name cell2)))
                                       (if (and cell1-index cell2-index)
                                           (< cell1-index cell2-index)
                                           (string-lessp (cell-name cell1) (cell-name cell2))))))))
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass time-layout (image-layout)
  ())

(defmethod cell-timestamp ((cell cell))
  (blob-timestamp (cell-image cell)))

(defmethod sorted-cells  ((layout time-layout) cells)
  (sort (copy-list cells)
	#'>
	:key #'cell-timestamp))

(defclass name-layout (image-layout)
  ())

(defmethod cell-name ((cell cell))
  (store-image-name (cell-image cell)))

(defmethod sorted-cells ((layout name-layout) cells)
  (sort (copy-list cells)
	#'string-lessp
	:key #'cell-name))

(defclass cell-size-layout (row-layout)
  ())

(defun cell-equal (cell1 cell2)
  (and (eql (cell-width cell1) (cell-width cell2))
       (eql (cell-height cell1) (cell-height cell2))))

(defmethod make-rows ((layout cell-size-layout) cells)
  (let ((cells-used (make-hash-table)))
    (labels ((make-row-with-equal-cells-up-to-row-width (cells)
	       (make-instance 'row :cells (reverse (loop with first-cell = (first cells)
							 with row-cells = (list first-cell)
							 with width = (cell-width first-cell)
							 for next-cell in (cdr cells)
							 while (< width *layout-max-width*)
							 when (and (cell-equal first-cell next-cell)
								   (not (gethash next-cell cells-used)))
							 do (push next-cell row-cells)
							 and do (incf width (cell-width first-cell))
							 and do (setf (gethash next-cell cells-used) t)
							 finally (return row-cells))))))
      (loop for remaining-cells on cells
	    unless (gethash (car remaining-cells) cells-used)
	    collect (make-row-with-equal-cells-up-to-row-width remaining-cells)))))

(defclass quickhoney-standard-layout (time-layout cell-size-layout)
  ())

(defclass quickhoney-name-layout (name-layout cell-size-layout)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass shop-layout (keyword-affinity-layout)
  ())

(defmethod initialize-instance :after ((layout shop-layout) &key &allow-other-keys)
  (setf (slot-value layout 'captions) t))

(defmethod make-cell ((layout shop-layout) item)
  (make-instance 'item-cell :layout layout :item item))

(defun make-item-url (index)
  (format nil "~a/~(~a~)" (request-variable :template-path) (object-name (nth index (session-value :current-query-result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general layout helpers

(defun reorder-query-result ()
  "reorder the query result in the session so that it reflects the order in the layout."
  (setf (session-value :current-query-result)
	(loop for page in (layout-pages (session-value :current-thumbnail-layout))
	      append (loop for row in (page-rows page)
			   append (row-objects row)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; web stuff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *layouters* (list (list :ecity 'ecity-layout)
			  (list :affinity 'keyword-affinity-layout)
			  (list :shop 'shop-layout)
			  (list :no 'no-layout)))
