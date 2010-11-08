(in-package :bknr.web)

(defclass request-processor ()
  ((requests :initarg :requests
	     :accessor request-processor-requests
	     :documentation "List storing the requests to be processed. Needs to be protected by LOCK for the access")
   (lock :initarg :lock
	 :accessor request-processor-lock
	 :documentation "Lock protecting the request pool")
   (cv :initarg :cv
       :accessor request-processor-cv
       :documentation "Condition variable to wake up the request-processor threads when new requests have arrived")
   (name :initarg :name
	 :accessor request-processor-name)
   (thread :initarg :thread
	   :accessor request-processor-thread
	   :documentation "Thread processing the requests"))
  (:default-initargs
   :requests (list)
   :lock (bt:make-lock)
    :cv (bt:make-condition-variable)))

(defvar *in-request-processor-p* nil)

(defmethod initialize-instance :after ((proc request-processor) &rest initargs)
  (setf (request-processor-thread proc)
	(bt:make-thread #'(lambda ()
			    (with-slots (requests lock cv) proc
			      (let ((*in-request-processor-p* t))
				(do () ()
				  (bt:condition-wait cv lock)
				  (do () ((emptyp requests))
				    (let ((request (pop requests)))
				      (process-request request)))))))
			:name (format nil "REQUEST PROCESSOR ~A" (request-processor-name proc)))))

(defclass bknr-user-request-pool ()
  ((hash :initarg :hash
	 :accessor bknr-user-request-pool-hash
	 :documentation "Hash mapping BKNR-USER to a REQUEST-PROCESSOR"))
  (:default-initargs
   :hash (make-hash-table :test #'equal)))

(defmethod dispatch-request ((pool bknr-user-request-pool) (request bknr-user-request))
  (with-slots (hash) pool
    (let* ((user (bknr-session-user))
	   (proc (gethash user pool)))
      (when (null proc)
	(setf proc (make-instance 'request-processor :name (user-login user)))
	(setf (gethash user pool) proc))
      (with-slots (requests lock cv) proc
	(bt:with-lock-held (lock)
	  (format t "enqueued request ~A for user ~A~%" request user)
	  (setf requests (append requests (list request)))
	  (bt:condition-notify cv))))))

(defclass bknr-user-acceptor (hunchentoot:acceptor)
  ((request-pool :initarg :request-pool
		 :accessor bknr-user-acceptor-request-pool
		 :documentation "Request pool to send requests from the main HT thread to the per-user processing threads"))
  (:default-initargs
   :request-class 'bknr-user-request
    :request-pool (make-instance 'bknr-user-request-pool)))

(defclass bknr-user-request (hunchentoot:request)
  ())

(defmethod hunchentoot:process-request ((request bknr-user-request))
  (if *in-request-processor-p*
      (call-next-method)
      (dispatch-request (bknr-user-acceptor-request-pool *acceptor*) request)))
	 