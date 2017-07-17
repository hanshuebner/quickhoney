(in-package :quickhoney.tags)

(cl-interpol:enable-interpol-syntax)

(define-bknr-tag version-and-last-change ()
  (html "v4.0 | updated "
        (:princ-safe (string-downcase
                      (substitute #\Space #\-
                                  (format-date-time (last-image-upload-timestamp)
                                                    :vms-style t :show-time nil))))))

(defun image-url (image)
  (format nil "/~(~A/~A~)/~A"
          (quickhoney-image-category image)
          (quickhoney-image-subcategory image)
          (bknr.images:store-image-name image)))

(defun navigation (&key previous up next)
  (html ((:div :class "nav")
         (when previous
           (html ((:div :class "previous")
                  ((:a :href previous) "<<"))))
         ((:div :class "up")
          ((:a :href up)
           "up"))
         (when next
           (html ((:div :class "next")
                  ((:a :href next) ">>")))))))

(defmacro with-navigation ((&key previous up next) &body body)
  `(progn
     (navigation :previous ,previous :up ,up :next ,next)
     ,@body))

(defun all-images-sorted-by-date (category &optional subcategory)
  (setf category (make-keyword-from-string category)
        subcategory (when subcategory (make-keyword-from-string subcategory)))
  (sort (remove-if-not (lambda (image)
                         (and (eq category (quickhoney-image-category image))
                              (or (null subcategory)
                                  (eq subcategory (quickhoney-image-subcategory image)))))
                       (bknr.datastore:class-instances 'quickhoney-image))
        #'>
        :key #'bknr.datastore:blob-timestamp))

(defun show-image (image)
  (let ((category (quickhoney-image-category image))
        (subcategory (quickhoney-image-subcategory image))
        (image-name (bknr.images:store-image-name image)))
    (html
     "category" (:br)
     (:princ #?"$(category) / $(subcategory)") (:br)
     ((:img :src #?"/image/$(image-name)"
            :alt ""
            :width (bknr.images:store-image-width image)
            :height (bknr.images:store-image-height image)))
     ((:div :class "metadata")
      (:table
       (:tbody
        (:tr (:th "name")
             (:td (:princ image-name)))
        (:tr (:th "artist")
             (:td (:princ (bknr.user:user-full-name (or (bknr.user:owned-object-owner image)
                                                        (bknr.user:find-user "hans"))))
                  ", QuickHoney"))
        (:tr (:th "date")
             (:td (:princ (format-date-time (bknr.datastore:blob-timestamp image) :vms-style t :show-time nil))))
        (when (and (quickhoney-image-client image)
                   (not (equal "" (quickhoney-image-client image))))
          (html (:tr (:th "client")
                     (:td (:princ (quickhoney-image-client image))))))
        (when (quickhoney-image-description image)
          (html (:tr (:th "description")
                     (:td (:princ (quickhoney-image-description image))))))
        (when (quickhoney-image-spider-keywords image)
          (html (:tr (:th "keywords")
                     (:td (:princ (quickhoney-image-spider-keywords image))))))))))))

(define-bknr-tag simple-image-browser ()
  (tbnl:handle-if-modified-since (last-image-upload-timestamp))
  (destructuring-bind (&optional category subcategory image-name)
      (cdr (mapcar #'hunchentoot:url-decode (cl-ppcre:split "/" (hunchentoot:script-name*))))
    (cond
      (image-name
       (let* ((image (or (bknr.images:store-image-with-name image-name)
                        (error #?"image $(image-name) not found")))
              (images-in-category (images-in-category (list (quickhoney-image-category image)
                                                            (quickhoney-image-subcategory image))))
              (previous (loop for rest on images-in-category
                              when (equal image (cadr rest))
                              do (return (car rest))))
              (next (cadr (member image images-in-category))))
         (with-navigation (:previous (and previous (image-url previous))
                           :up (format nil "/~(~A/~A~)"
                                       (quickhoney-image-category image)
                                       (quickhoney-image-subcategory image))
                           :next (and next (image-url next)))
           (show-image image))))
      (subcategory
       (with-navigation (:up (format nil "/~(~A~)" category))
         (html
          (:h1 (:princ #?"$(category) / $(subcategory)"))
          (:ul
           (dolist (image (all-images-sorted-by-date category subcategory))
             (html
              (:li ((:a :href #?"/$(category)/$(subcategory)/$((bknr.images:store-image-name image))")
                    (:princ (bknr.images:store-image-name image))))))))))
      (category
       (with-navigation (:up "/")
         (html
          (:h1 "Category" (:br)
               (:princ category))
          (:h2 "Subcategories" (:br)
               (dolist (subcategory (subcategories-of (make-keyword-from-string category)))
                 (html ((:a :href (format nil "/~A/~(~A~)" category subcategory))
                        (:princ subcategory)) " | ")))
          (:h2 "Latest entries")
          (loop
             for i below 20
             for image in (all-images-sorted-by-date category)
             do (html
                 (:p
                  (show-image image)))))))
      (t
       (emit-tag-children)))))

(define-bknr-tag login-status ()
  (with-query-params (login)
    (cond
      ((bknr.web::admin-p (bknr-session-user))
       (redirect (or login "/")))
      ((and (bknr.web::anonymous-p (bknr-session-user))
            (query-param "__username"))
       (html (:h1 "Login failed, please try again")))
      (t
       (html (:h1 "Please login"))))))

(define-bknr-tag twitter-account-checkboxes ()
  (dolist (account (tweet:all-twitter-accounts))
    (html (:label ((:input :type "checkbox" :name "account-name" :value (tweet:twitter-name account)))
                  " " (:princ (tweet:twitter-name account)))
          (:br))))
