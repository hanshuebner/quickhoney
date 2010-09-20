(in-package :quickhoney)

(defvar *dumped-image* nil)
(defvar *cron-actor* nil)

(defun dump-executable ()
  #+openmcl
  (ccl:save-application "quickhoney" :prepend-kernel t))

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

     (close-store)
     (make-instance 'store
                    :directory *store-directory*
                    :subsystems (list (make-instance 'store-object-subsystem)
                                      (make-instance 'blob-subsystem
                                                     :n-blobs-per-directory 1000)))

     (unless (class-instances 'bknr.cron::cron-job)
       (bknr.cron:make-cron-job "daily statistics" 'make-yesterdays-statistics 1 0 :every :every)
       (bknr.cron:make-cron-job "snapshot" 'snapshot-store 0 5 :every :every))
     (unless (class-instances 'rss-channel)
       (make-rss-channel "quickhoney" "QuickHoney" "QuickHoney Illustrations" "rss/quickhoney"
                         :items (class-instances 'quickhoney-image)))
     
     (cl-gd::load-gd-glue)))

  (ensure-directories-exist
   (setf tbnl:*tmp-directory* (merge-pathnames "hunchentoot-tmp/" *store-directory*)))
  (actor-start (setf *cron-actor* (make-instance 'cron-actor)))

  (paypal-init :user *paypal-user* :password *paypal-password* :signature *paypal-signature*)
  ;; XXX for test purposes
  (setf cl-paypal:*paypal-max-transaction-per-ip* 100)
  
  (when (probe-file "site-config.lisp")
    (format t "; loading site configuration file~%")
    (let ((*package* (find-package :quickhoney.config)))
      (load "site-config.lisp")))

  (start-http-server))

(defun shutdown-qh ()
  (actor-stop *cron-actor*)
  (stop-http-server)
  (close-store))

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
  
  (setf *acceptor* (make-instance 'hunchentoot:acceptor
                                  :port *webserver-port*
                                  :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
                                  :persistent-connections-p nil
                                  :request-dispatcher 'bknr.web:bknr-dispatch))
  
  ;; XXX store thread to stop later on
  (setf *ht-thread*
        (bt:make-thread (curry #'hunchentoot:start *acceptor*)
                        :name (format nil "HTTP server on port ~A" *webserver-port*))))
