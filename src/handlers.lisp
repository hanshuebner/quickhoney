(in-package :quickhoney)

(enable-interpol-syntax)

(defparameter *editable-keywords* '(:explicit :buy-file :buy-print :buy-t-shirt)
  "List of keywords that are image keywords which can be edited through the CMS")

(defclass quickhoney-image-dependent-handler (page-handler)
  ()
  (:documentation "Mixin for handlers whose response only depend on
  quickhoney images.  The HANDLE :AROUND method of this handler
  performs if-modified-since handling, defaulting the last-modified
  date of the response to the timestamp of the newest QUICKHONEY-IMAGE
  instance.  This mixin is suitable for handlers that query the
  database for multiple images.  It should not be used for
  single-object handlers.  Deletion is not properly handled
  presently."))

(defmethod handle :around ((handler quickhoney-image-dependent-handler))
  (let ((time (last-image-upload-timestamp)))
    (handle-if-modified-since time)
    (setf (header-out :last-modified) (rfc-1123-date time)
          (header-out :cache-control) "max-age=15"))
  (call-next-method))

(defclass random-image-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler random-image-handler))
  (random-elt (images-in-category (mapcar #'make-keyword-from-string (decoded-handler-path handler)))))

(defmethod handle-object ((handler random-image-handler) store-image)
  (redirect (format nil "/image/~A" (store-object-id store-image))))

(defclass animation-handler (object-handler)
  ())

(defmethod handle-object ((handler animation-handler) animation)
  (let ((content-type (blob-type (quickhoney-animation-image-animation animation))))
    (with-http-response (:content-type content-type)
      (blob-to-stream (quickhoney-animation-image-animation animation)
                      (send-headers)))))

(defclass json-image-info-handler (object-handler quickhoney-image-dependent-handler)
  ()
  (:default-initargs :query-function #'store-image-with-name))

(defmethod json:encode ((object symbol) &optional stream)
  (json:encode (string-downcase (symbol-name object))
               stream))

(defmethod image-to-json ((image quickhoney-image))
  (json:with-object ()
    (json:encode-object-element "class"
                                (string-downcase (cl-ppcre:regex-replace "^QUICKHONEY-"
                                                                         (symbol-name (class-name (class-of image)))
                                                                         "")))
    (json:encode-object-element "name" (store-image-name image))
    (when (quickhoney-image-category image)
      (json:encode-object-element "category" (quickhoney-image-category image))
      (when (quickhoney-image-subcategory image)
        (json:encode-object-element "subcategory" (quickhoney-image-subcategory image))))
    (json:encode-object-element "id" (store-object-id image))
    (json:encode-object-element "type" (image-content-type (blob-mime-type image)))
    (json:encode-object-element "width" (store-image-width image))
    (json:encode-object-element "height" (store-image-height image))
    (json:encode-object-element "client" (or (quickhoney-image-client image) ""))
    (when (typep image 'quickhoney-animation-image)
      (json:encode-object-element "animation_type"
                             (image-content-type (blob-mime-type (quickhoney-animation-image-animation image)))))
    (when (quickhoney-image-spider-keywords image)
      (json:encode-object-element "spider_keywords" (quickhoney-image-spider-keywords image)))
    (json:with-object-element ("keywords")
      (json:with-object ()
        (dolist (keyword (intersection *editable-keywords* (store-image-keywords image)))
          (json:encode-object-element (string-downcase (symbol-name keyword)) t))))))

(defmethod handle-object ((handler json-image-info-handler) image)
  (with-json-response ()
    (json:with-object-element ("image")
      (image-to-json image))))

(defclass json-image-query-handler (object-handler quickhoney-image-dependent-handler)
  ())

(defun images-in-category-sorted-by-time (cat-sub)
  (sort (copy-list (images-in-category cat-sub))
        #'> :key #'blob-timestamp))

(defmethod object-handler-get-object ((handler json-image-query-handler))
  (images-in-category-sorted-by-time (mapcar #'make-keyword-from-string (decoded-handler-path handler))))

(defmethod layout-to-json ((layout layout))
  (json:with-array ()
    (dolist (page (layout-pages layout))
      (json:with-array ()
        (dolist (row (page-rows page))
          (json:with-array ()
            (json:encode-array-element (row-cell-width row))
            (json:encode-array-element (row-cell-height row))
            (dolist (image (row-images row))
              (image-to-json image))))))))

(defmethod handle-object ((handler json-image-query-handler) images)
  (with-json-response ()
    (json:with-object-element ("queryResult")
      (with-query-params (layout)
        (layout-to-json (make-instance (case (make-keyword-from-string layout)
                                         (:smallworld 'quickhoney-name-layout)
                                         (t 'quickhoney-standard-layout))
                                       :objects images))))))

(defclass json-login-handler (page-handler)
  ())

(defmethod handle ((handler json-login-handler))
  (with-json-response ()
    (json:encode-object-element "admin" (admin-p (bknr-session-user)))
    (when (and (anonymous-p (bknr-session-user))
               (query-param "__username"))
      (json:encode-object-element "login_failed" t))
    (json:encode-object-element "login" (user-login (bknr-session-user)))))

(defclass json-logout-handler (page-handler)
  ())

(defmethod handle ((handler json-logout-handler))
  (setf (session-value 'bknr-session) nil)
  (with-json-response ()
    (json:encode-object-element "logged_out" t)))

(defclass json-clients-handler (page-handler)
  ())

(defmethod handle ((handler json-clients-handler))
  (with-json-response ()
    (json:with-object-element ("clients")
      (json:with-array ()
        (dolist (client (sort (remove "" (all-clients) :test #'equal)
                              #'string-lessp))
          (json:encode-array-element client))))))

(defclass json-edit-image-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-image))

(defmethod handle-object-form ((handler json-edit-image-handler) action image)
  (with-json-response ()
    (json:encode-object-element "result" "error")
    (json:encode-object-element "message" (format nil "; invalid action ~A or invalid object ~A~%" action image))))

(defun image-keywords-from-request-parameters ()
  (let (retval)
    (dolist (keyword *editable-keywords*
             retval)
      (when (query-param (string-downcase (symbol-name keyword)))
        (push keyword retval)))))
  
(defmethod handle-object-form ((handler json-edit-image-handler) (action (eql :edit)) image)
  (with-query-params (client spider-keywords)
    (with-transaction (:edit-image)
      (setf (quickhoney-image-client image) client
            (quickhoney-image-spider-keywords image) spider-keywords
            (store-image-keywords image) (append (set-difference (store-image-keywords image) *editable-keywords*)
                                                 (image-keywords-from-request-parameters)))))
  (setf *last-image-upload-timestamp* (get-universal-time))
  (with-json-response ()
    (json:encode-object-element "result" "edited")))

(defmethod handle-object-form ((handler json-edit-image-handler) (action (eql :delete)) (image quickhoney-image))
  (delete-object image)
  (setf *last-image-upload-timestamp* (get-universal-time))
  (with-json-response ()
    (json:encode-object-element "result" "deleted")))

(defclass json-edit-news-item-handler (json-edit-image-handler)
  ()
  (:default-initargs :object-class 'quickhoney-news-item))
  
(defmethod handle-object-form ((handler json-edit-news-item-handler) (action (eql :edit)) item)
  (with-query-params (title text)
    (with-transaction (:edit-news-item)
      (setf (quickhoney-news-item-title item) title
            (quickhoney-news-item-text item) text)))
  (setf *last-image-upload-timestamp* (get-universal-time))
  (with-json-response ()
    (json:encode-object-element "result" "edited")))

(defclass digg-image-handler (object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-image))

(defmethod handle-object ((handler digg-image-handler) (image quickhoney-image))
  (with-query-params (from to text)
    (cl-smtp:with-smtp-mail (smtp "localhost"
                                  "webserver@quickhoney.com"
                                  (cond
                                    ((and to (length to))
                                     (list to))
                                    ((owned-object-owner image)
                                     (list (user-email (owned-object-owner image))))
                                    (t
                                     (mapcar (alexandria:compose #'user-email #'find-user) (list "n" "p")))))
      (cl-mime:print-mime
       smtp
       (make-instance
        'cl-mime:multipart-mime
        :subtype "mixed"
        :content (list
                  (make-instance
                   'cl-mime:mime
                   :type "text" :subtype "html"
                   :content (with-output-to-string (s)
                              (html-stream s
                                           (:html
                                            (:head
                                             (:title "Picture comment"))
                                            (:body
                                             (:table
                                              (:tbody
                                               (:tr
                                                ((:td :colspan "2")
                                                 "Comment on picture "
                                                 ((:a :href (make-image-link image))
                                                  (:princ-safe (store-image-name image)))))
                                               (:tr
                                                (:td (:b "From"))
                                                (:td (:princ-safe from))))
                                               (:tr
                                                ((:td :valign "top") (:b "Text"))
                                                (:td (:princ-safe text)))))))))
                  (make-instance
                   'cl-mime:mime
                   :type "image"
                   :subtype (string-downcase (symbol-name (blob-type image)))
                   :encoding :base64
                   :content (flexi-streams:with-output-to-sequence (s)
                              (blob-to-stream image s)))))
       t t))))

(defclass json-buttons-handler (prefix-handler quickhoney-image-dependent-handler)
  ())

(defun preproduced-buttons (category subcategory)
  (let ((images (get-keywords-intersection-store-images (list category subcategory :button))))
    (when images
      (cons :buttons images))))

(defun images-in-all-subcategories-sorted-by-time (category)
  (sort (apply #'append (mapcar (lambda (cat-sub)
                                  (when (eq category (car cat-sub))
                                    (copy-list (images-in-category cat-sub))))
                                (all-categories)))
        #'> :key #'blob-timestamp))

(defun newest-images (category subcategory)
  (let ((images (if (eq :home category)
                    (remove-if (lambda (image)
                                 (or (eq :nudes (quickhoney-image-subcategory image))
                                     (find :explicit (store-image-keywords image))))
                               (images-in-all-subcategories-sorted-by-time subcategory))
                    (images-in-category-sorted-by-time (list category subcategory)))))
    (when images
      (cons :images (loop with since = (- (get-universal-time) (* 60 60 24 14))
                          for i from 0
                          for image in images
                          when (or (< i 10)
                                   (> (blob-timestamp image) since))
                          collect image)))))

(defmethod handle ((handler json-buttons-handler))
  (with-json-response ()
    (json:with-object-element ("buttons")
      (json:with-object ()
        (loop
           for (category subcategories-string) on (decoded-handler-path handler) by #'cddr
           do (dolist (subcategory (split "," subcategories-string))
                (json:with-object-element ((format nil "~(~A/~A~)" category subcategory))
                  (json:with-array ()
                    ;; For each subcategory, an array of buttons is
                    ;; generated.  The first element of the array is
                    ;; either "buttons" or "images", indicating
                    ;; whether the object ids that follow represent
                    ;; preproduced buttons or images.  Preproduced
                    ;; buttons are already in the required 208x208
                    ;; format and come with the caption rendered into
                    ;; them.
                    (let ((category (make-keyword-from-string category))
                          (subcategory (make-keyword-from-string subcategory)))
                      (destructuring-bind (&optional type &rest images)
                          (or (preproduced-buttons category subcategory)
                              (newest-images category subcategory)
                              (warn "No images for ~A ~A found" category subcategory))
                        (json:encode-array-element type)
                        (dolist (image (or images
                                           (list (store-image-with-name "button-dummy"))))
                          (json:encode-array-element (store-object-id image)))))))))))))

(defclass upload-image-handler (admin-only-handler prefix-handler)
  ())

(defun count-colors-used (&optional (image cl-gd:*default-image*))
  (let ((color-table (make-hash-table :test #'eql)))
    (cl-gd:do-pixels (image)
      (setf (gethash (cl-gd:raw-pixel) color-table) t))
    (hash-table-count color-table)))

(defun maybe-convert-to-palette (&optional (image cl-gd:*default-image*))
  (when (and (cl-gd:true-color-p image)
             (<= (count-colors-used image) 256))
    (cl-gd:true-color-to-palette :image image)
    t))

(defmethod handle ((handler upload-image-handler))
  (with-query-params (client spider-keywords)
    (let ((uploaded-file (request-uploaded-file "image-file")))
      (handler-case
	  (progn
	    (unless uploaded-file
	      (error "no file uploaded"))
	    (with-image-from-upload* (uploaded-file)
	      (let* ((width (cl-gd:image-width))
		     (height (cl-gd:image-height))
		     (ratio (/ 1 (max (/ width 300) (/ height 200))))
                     (image-name (pathname-name (upload-original-filename uploaded-file))))
                (maybe-convert-to-palette)
		(let* ((image (make-store-image :name image-name
						:class-name 'quickhoney-image
						:keywords (cons :upload (image-keywords-from-request-parameters))
						:initargs (list :owner (bknr-session-user)
                                                                :cat-sub (mapcar #'make-keyword-from-string
                                                                                 (decoded-handler-path handler))
                                                                :client client
                                                                :spider-keywords spider-keywords))))
		  (with-http-response ()
		    (with-http-body ()
		      (html (:html
			     (:head
			      (:title "Upload successful")
			      ((:script :type "text/javascript" :language "JavaScript")
			       "function done() { window.opener.do_query(); window.close(); }"))
			     (:body
			      (:p "Image " (:princ-safe (store-image-name image)) " uploaded")
			      (:p ((:img :src (format nil "/image/~D" (store-object-id image))
					 :width (round (* ratio width)) :height (round (* ratio height)))))
			      (:p ((:a :href "javascript:done()") "ok")))))))))))
	(error (e)
	  (with-http-response ()
	    (with-http-body ()
	      (html (:html
		     (:head
		      (:title "Error during upload"))
		     (:body
		      (:h2 "Error during upload")
		      (:p "Error during upload:")
		      (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		      (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

(defclass upload-news-handler (admin-only-handler page-handler)
  ())

(defun normalize-news-title (title)
  (string-downcase (cl-ppcre:regex-replace-all "(?i)[^a-z0-9_]+" title "_")))

(defconstant +news-image-width+ 486
  "Width of a news image.  Uploaded images that are wider are scaled down.")

(defmethod handle ((handler upload-news-handler))
  (with-query-params (title text)
    (let ((uploaded-file (or (request-uploaded-file "image-file"))))
      (unless uploaded-file
        (error "no file uploaded"))
      (handler-case
          (with-image-from-upload (uploaded-image uploaded-file)
            (let* ((processed (when (> (cl-gd:image-width uploaded-image) +news-image-width+)
                                (let* ((scaled-height (floor (* (/ +news-image-width+ (cl-gd:image-width uploaded-image))
                                                                (cl-gd:image-height uploaded-image))))
                                       (scaled-image (cl-gd:create-image +news-image-width+ scaled-height
                                                                         (cl-gd:true-color-p uploaded-image))))
                                  (cl-gd:copy-image uploaded-image scaled-image
                                                    0 0 0 0
                                                    (cl-gd:image-width uploaded-image) (cl-gd:image-height uploaded-image)
                                                    :resample t :resize t
                                                    :dest-width +news-image-width+ :dest-height scaled-height)
                                  (cl-gd:destroy-image uploaded-image)
                                  (setf uploaded-image scaled-image))
                                t))
                   (name (normalize-news-title title))
                   (args (list :name name
                               :type (if (cl-gd:true-color-p uploaded-image) :jpg :png)
                               :class-name 'quickhoney-news-item
                               :keywords (list :upload)
                               :initargs (list :cat-sub (list :news)
                                               :title title
                                               :text text
                                               :owner (bknr-session-user))))
                   (item (if processed
                             (apply #'make-store-image :image uploaded-image args)
                             (apply #'import-image (upload-pathname uploaded-file) args))))
              (declare (ignore item))   ; for now
              (with-http-response ()
                (with-http-body ()
                  (html (:html
                         (:head
                          (:title "News article created")
                          ((:script :type "text/javascript" :language "JavaScript")
                           "function done() { window.opener.reload_news(); window.close(); }"))
                         (:body
                          (:p "News article created")
                          (:p ((:a :href "javascript:done()") "ok")))))))))
	(error (e)
	  (with-http-response ()
	    (with-http-body ()
	      (html (:html
		     (:head
		      (:title "Error during upload"))
		     (:body
		      (:h2 "Error during upload")
		      (:p "Error during upload:")
		      (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		      (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

(defclass upload-animation-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler upload-animation-handler))
  (with-query-params (client)
    (let* ((uploaded-files (request-uploaded-files))
	   (uploaded-image (find "image-file" uploaded-files :test #'equal :key #'upload-name))
	   (uploaded-animation (find "animation-file" uploaded-files :test #'equal :key #'upload-name)))
      (handler-case
	  (progn
	    (unless (and uploaded-image uploaded-animation)
	      (error "files not uploaded"))
	    (unless (find (upload-content-type uploaded-animation)
                          '("application/x-shockwave-flash" "video/quicktime" "application/x-director")
                          :test #'equal)
	      (error "Invalid content type ~A - Please upload a Flash, Shockwave or Quicktime file"
                     (upload-content-type uploaded-animation)))
	    (with-image-from-upload* (uploaded-image)
	      (let* ((animation-blob (make-blob-from-file (upload-pathname uploaded-animation) 'blob
							  :type (upload-content-type uploaded-animation)))
		     (image (make-store-image :name (pathname-name (upload-original-filename uploaded-image))
                                              :type (make-keyword-from-string (pathname-type
                                                                               (upload-original-filename uploaded-image)))
					      :class-name 'quickhoney-animation-image
					      :keywords (list :upload)
					      :initargs (list :cat-sub (list :pixel :animation)
                                                              :client client
                                                              :animation animation-blob))))
		(with-http-response ()
		  (with-http-body ()
		    (html (:html
			   (:head
			    (:title "Upload successful")
			    ((:script :type "text/javascript" :language "JavaScript")
			     "function done() { window.opener.do_query(); window.close(); }"))
			   (:body
			    (:p "Animation uploaded")
			    (:p ((:img :src (format nil "/image/~D" (store-object-id image)))))
			    (:p ((:a :href "javascript:done()") "ok"))))))))))
	(error (e)
	  (with-http-response ()
	    (with-http-body ()
	      (html (:html
		     (:head
		      (:title "Error during upload"))
		     (:body
		      (:h2 "Error during upload")
		      (:p "Error during upload:")
		      (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		      (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

(defclass upload-button-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler upload-button-handler))
  (with-query-params (directory subdirectory)
    (let ((uploaded-file (request-uploaded-file "image-file")))
      (handler-case
	  (progn
	    (unless (and directory
			 (not (equal "" directory)))
	      (error "no category selected, upload not accepted"))
	    (unless (and subdirectory
			 (not (equal "" subdirectory)))
	      (error "no subcategory selected, upload not accepted"))
	    (unless uploaded-file
	      (error "no file uploaded"))
	    (with-image-from-upload* (uploaded-file)
	      (unless (and (eql 208 (cl-gd:image-width))
			   (eql 208 (cl-gd:image-height)))
		(error "invalid image size, button size must be 208 by 208 pixels"))
	      (let* ((image (make-store-image :name (pathname-name (upload-original-filename uploaded-file))
                                              :type (make-keyword-from-string (pathname-type (upload-original-filename uploaded-file)))
					      :class-name 'store-image
					      :keywords (list :button)
                                              :initargs (list :cat-sub (list (make-keyword-from-string directory)
                                                                             (make-keyword-from-string subdirectory))))))
		(with-http-response ()
		  (with-http-body ()
		    (html (:html
			   (:head
			    (:title "Upload successful")
			    ((:script :type "text/javascript" :language "JavaScript")
			     "function done() { window.opener.do_query(); window.close(); }"))
			   (:body
			    (:p "Image " (:princ-safe (store-image-name image)) " uploaded")
			    (:p ((:img :src (format nil "/image/~D" (store-object-id image))
				       :width 208 :height 208)))
			    (:p ((:a :href "javascript:done()") "ok"))))))))))
	(error (e)
	  (with-http-response ()
	    (with-http-body ()
	      (html (:html
		     (:head
		      (:title "Error during upload"))
		     (:body
		      (:h2 "Error during upload")
		      (:p "Error during upload:")
		      (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		      (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

(defclass json-news-handler (object-handler)
  ()
  (:default-initargs  :query-function (lambda (string) (or (find-rss-channel string)
                                                           (store-image-with-name string)
                                                           (find-store-object string)))))

(defgeneric json-encode-news-item (item)
  (:method ((item t))
    ; do nothing
    )
  (:method :before ((item store-object))
    (json:encode-object-element "id" (store-object-id item)))
  (:method :before ((image quickhoney-image))
    (when (owned-object-owner image)
      (json:encode-object-element "owner" (user-login (owned-object-owner image))))
    (json:encode-object-element "date" (format-date-time (blob-timestamp image) :vms-style t :show-time nil))
    (json:encode-object-element "name" (store-image-name image)))
  (:method ((image quickhoney-image))
    (json:encode-object-element "type" "upload")
    (json:encode-object-element "category" (quickhoney-image-category image))
    (json:encode-object-element "subcategory" (quickhoney-image-subcategory image))
    (json:with-object-element ("keywords")
      (json:with-array ()
        (dolist (keyword (store-image-keywords image))
          (json:encode-array-element (string-downcase (symbol-name keyword)))))))
  (:method ((item quickhoney-news-item))
    (json:encode-object-element "type" "news")
    (json:encode-object-element "title" (quickhoney-news-item-title item))
    (json:encode-object-element "text" (quickhoney-news-item-text item))
    (json:encode-object-element "width" (store-image-width item))
    (json:encode-object-element "height" (store-image-height item))))

(defun json-encode-news-items (items)
  (with-json-response ()
    (json:with-object-element ("items")
      (json:with-array ()
        (dolist (item items)
          (json:with-object ()
            (json-encode-news-item item)))))))

(defmethod handle-object ((handler json-news-handler) (channel rss-channel))
  (json-encode-news-items (rss-channel-items channel)))

(defmethod handle-object ((handler json-news-handler) (item quickhoney-news-item))
  (json-encode-news-items (list item)))

(defclass json-news-archive-handler (object-handler)
  ()
  (:default-initargs :object-class 'rss-channel :query-function #'find-rss-channel))

(defmethod handle-object ((handler json-news-archive-handler) (channel rss-channel))
  (with-json-response ()
    (json:with-object-element ("months")
      (json:with-array ()
        (dolist (month (sort (rss-channel-archived-months channel)
                             (lambda (a b)
                               (if (= (first a) (first b))
                                   (> (second a) (second b))
                                   (> (first a) (first b))))))
          (json:with-array ()
            (json:encode-array-element (first month))
            (json:encode-array-element (second month))))))))

(defclass shutdown-handler (admin-only-handler page-handler)
  ())

(defvar *acceptor* nil)

(defmethod handle ((handler shutdown-handler))
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  "Shutting down HTTP server")
