(in-package :quickhoney)

(define-persistent-class quickhoney-image (store-image rss-item)
  ((client :update
           :initform nil
           :index-type hash-index :index-initargs (:test #'equal)
           :index-reader images-for-client
           :index-keys all-clients)
   (cat-sub :update
            :initform nil
            :index-type hash-index :index-initargs (:test #'equal)
            :index-reader images-in-category
            :index-keys all-categories
            :documentation
            "Category this image belongs to, as a list of one or two keywords")
   (spider-keywords :update
                    :initform nil)
   (description :update
                :initform nil)
   (center-x :update :initform 0)
   (center-y :update :initform 0)))

(defmethod quickhoney-image-init-center ((image quickhoney-image))
  (with-transaction (:init-center)
    (setf (quickhoney-image-center-x image) (floor (store-image-width image) 2)
          (quickhoney-image-center-y image) (floor (store-image-height image) 2))))

#+(or)
(with-transaction (:fix-center)
  (dolist (image (class-instances 'quickhoney-image))
    (with-slots (center-x center-y) image
      (when (and (slot-boundp image 'center-x)
                 (stringp center-x))
        (setf center-x (parse-integer center-x)))
      (when (and (slot-boundp image 'center-y)
                 (stringp center-y))
        (setf center-y (parse-integer center-y)))
      (when (and (slot-boundp image 'center-x)
                 (not center-x))
        (setf center-x (floor (store-image-width image) 2)))
      (when (and (slot-boundp image 'center-y)
                 (not center-y))
        (setf center-y (floor (store-image-height image) 2))))))

(defmethod quickhoney-image-center-x :before ((image quickhoney-image))
  (unless (and (slot-boundp image 'center-x)
               (slot-value image 'center-x))
    (quickhoney-image-init-center image)))

(defmethod quickhoney-image-center-y :before ((image quickhoney-image))
  (unless (and (slot-boundp image 'center-y)
               (slot-value image 'center-y))
    (quickhoney-image-init-center image)))

(defvar *last-image-upload-timestamp* 0)

(defmethod initialize-transient-instance :after ((image quickhoney-image))
  (setf *last-image-upload-timestamp* (max *last-image-upload-timestamp* (blob-timestamp image))))

(defun last-image-upload-timestamp ()
  *last-image-upload-timestamp*)

(defun change-class-of-imported-images ()
  (mapc (lambda (image)
          (persistent-change-class image 'quickhoney-image)
          (store-object-remove-keywords image 'bknr.web::keywords '(:import)))
        (get-keywords-intersection-store-images '(:import))))

(defmethod quickhoney-image-category ((image quickhoney-image))
  (car (quickhoney-image-cat-sub image)))

(defmethod quickhoney-image-subcategory ((image quickhoney-image))
  (cadr (quickhoney-image-cat-sub image)))

(defun subcategories-of (category)
  (loop
     for cat-sub in (all-categories)
     when (and (eq category (car cat-sub))
               (cadr cat-sub))
     collect (cadr cat-sub)))

(defmethod make-image-link ((image quickhoney-image) &key internal)
  (format nil "~@[~A~]/#~(~A~@[/~A~]~)/~A"
          (unless internal
            (format nil "http://~A" (website-host)))
          (quickhoney-image-category image) (quickhoney-image-subcategory image) (store-image-name image)))

(defun convert-all-pixel-images (directory)
  (dolist (category (remove :pixel (quickhoney::all-categories) :test-not #'eql :key #'car))
    (dolist (image (quickhoney:images-in-category category))
      (format t "; image ~A~%" image)
      (handler-case
          (pixel-pdf:convert-store-image-to-pdf image
                                                (make-pathname :name (store-image-name image)
                                                               :type "pdf"
                                                               :defaults directory))
        (error (e)
          (format t "; error ~A~%" e))))))

(defun export-image (img file)
  (copy-file (blob-pathname img) file)
  (format t "(import-image ~S :keywords-from-dir nil :keywords '~S)~%" file (store-image-keywords img)))

(defun export-images (imgs dir)
  (dolist (img imgs)
    (export-image img (merge-pathnames (make-pathname :name (store-image-name img)
						      :type  (string-downcase (symbol-name (blob-type img))))
				       dir))))
