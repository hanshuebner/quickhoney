(in-package :quickhoney.tags)

(cl-interpol:enable-interpol-syntax)

(define-bknr-tag version-and-last-change ()
  (html "v2.0 | updated "
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
  `(prog1
       (html ((:div :id "html-content-foo")
              (navigation :previous ,previous :up ,up :next ,next)
              ,@body))
     (html ((:script :type "text/javascript")
            "document.getElementById('html-content-foo').style.visibility = 'hidden'"))))

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
           (html
            (:h1 (:princ #?"$(category) / $(subcategory)"))
            ((:img :src #?"/image/$(image-name)"
                   :class "image"
                   :alt ""
                   :width (bknr.images:store-image-width image)
                   :height (bknr.images:store-image-height image)))
            ((:div :class "metadata" :style #?"height: $((bknr.images:store-image-height image))px")
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
               (when (quickhoney-image-spider-keywords image)
                 (html (:tr (:th "keywords")
                          (:td (:princ (quickhoney-image-spider-keywords image)))))))))))))
      (subcategory
       (with-navigation (:up (format nil "/~(~A~)" category))
         (html
          (:h1 (:princ #?"$(category) / $(subcategory)"))
          (:ul
           (dolist (image (sort (copy-list (quickhoney:images-in-category
                                            (mapcar #'make-keyword-from-string (list category subcategory))))
                                #'>
                                :key #'bknr.datastore:blob-timestamp))
             (html
              (:li ((:a :href #?"/$(category)/$(subcategory)/$((bknr.images:store-image-name image))")
                    (:princ (bknr.images:store-image-name image))))))))))
      (category
       (with-navigation (:up "/")
         (html
          (:h1 (:princ category))
          (:ul
           (dolist (subcategory (subcategories-of (make-keyword-from-string category)))
             (html
              (:li ((:a :href (format nil "/~A/~(~A~)" category subcategory)))
                   (:princ subcategory))))))))
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

