(in-package :quickhoney)

(defvar *dumped-image* nil)
(defvar *cron-actor* nil)

(defun dump-executable ()
  #+openmcl
  (ccl:save-application "quickhoney" :prepend-kernel t))

(defun reopen-store ()
  (close-store)
  (make-instance 'store
                 :directory *store-directory*
                 :subsystems (list (make-instance 'store-object-subsystem)
                                   (make-instance 'blob-subsystem
                                                  :n-blobs-per-directory 1000))))

(defun startup ()
  (cond
    (*dumped-image*
     (asdf:oos 'asdf:load-op :quickhoney)
     (format t "; starting from dumped image, skipping store initialization~%"))
    
    (t
     ;; lookup xml catalog
     (setq cxml::*default-catalog* (list (namestring (merge-pathnames #p"catalog" quickhoney.config:*xml-catalog-directory*))))
     (unless (probe-file (first cxml::*default-catalog*))
       (error "Could not find XML catalog.~%
Please make sure that ~A points to the correct location, or create
it if it is missing."
              (first cxml::*default-catalog*)))
     (setf cxml::*catalog* (cxml:make-catalog))

     (setf *hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf))

     (reopen-store)

     (unless (class-instances 'bknr.cron::cron-job)
       (bknr.cron:make-cron-job "daily statistics" 'make-yesterdays-statistics 1 0 :every :every)
       (bknr.cron:make-cron-job "snapshot" 'snapshot-store 0 5 :every :every))
     (unless (class-instances 'rss-channel)
       (make-rss-channel "quickhoney" "QuickHoney" "QuickHoney Illustrations" "rss/quickhoney"
                         :items (class-instances 'quickhoney-image)))

     ;; Update spring 2018
     (unless (store-image-with-name "type-honey-draw")
       (dolist (name (directory #P "../update-2018/type*.png"))
         (when-let (old (store-image-with-name (pathname-name name)))
           (delete-object old))
         (import-image name)))

     (unless (images-in-category '(:pen :quick-portraits))
       (with-transaction (:update-spring-2018)
         (dolist (image (images-in-category '(:pen :portraits)))
           (setf (quickhoney-image-cat-sub image) '(:pen :quick-portraits)))
         (dolist (image (images-in-category '(:pen :nudes)))
           (setf (quickhoney-image-cat-sub image) '(:pen :quick-draw)))
         (dolist (image (images-in-category '(:pen :stuff)))
           (setf (quickhoney-image-cat-sub image) '(:pen :quick-draw)))))
     
     (cl-gd::load-gd-glue)))

  (ensure-directories-exist
   (setf tbnl:*tmp-directory* (merge-pathnames "hunchentoot-tmp/" *store-directory*)))
  (actor-start (setf *cron-actor* (make-instance 'cron-actor)))

  (when (probe-file "site-config.lisp")
    (format t "; loading site configuration file~%")
    (let ((*package* (find-package :quickhoney.config)))
      (load "site-config.lisp")))

  (start-http-server))

(defun shutdown-qh ()
  (actor-stop *cron-actor*)
  (stop-http-server)
  (close-store))

(defvar *acceptor* nil
  "Stores the HUNCHENTOOT acceptor used to serve the website.")
(defvar *ht-thread* nil
  "Stores the thread used to run HUNCHENTOOT.")

(defun stop-http-server ()
  "Stop the running webserver, and destroy the thread it was running in."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (bt:destroy-thread *ht-thread*)
    (setf *acceptor* nil)))

(defun start-http-server ()
  "Restart the webserver, republishing the website as well."
  (stop-http-server)

  (publish-quickhoney)
  
  (setf *acceptor* (make-instance 'bknr.web:bknr-acceptor
                                  :port *webserver-port*
                                  :persistent-connections-p nil))
  
  ;; XXX store thread to stop later on
  (setf *ht-thread*
        (bt:make-thread (curry #'hunchentoot:start *acceptor*)
                        :name (format nil "HTTP server on port ~A" *webserver-port*))))
