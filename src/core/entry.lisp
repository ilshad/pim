(in-package #:pim-core)

(defun max-entry-id ()
  (loop for k being the hash-keys in *entries*
	maximizing k into max
	finally (return max)))

(defclass entry ()
  ((id :initarg :id :initform (1+ (max-entry-id)) :reader id)
   (load? :initarg :load? :initform nil)
   (content :initform nil)))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id content load?) object
      (if load?
	  (format stream "~d (not yet loaded)" id)
	  (multiple-value-bind (string cut?) (string-cut content 20)
	    (format stream "~d: \"~a~:[\"~;...\" (~d chars)~]"
		    id string cut? (length content)))))))

(defgeneric (setf content) (string entry))

(defmethod (setf content) (string (entry entry))
  (with-slots (id content) entry
    (setf content string)
    (save-entry id content)))

(defgeneric content (entry))

(defmethod content ((entry entry))
  (with-slots (id content load?) entry
    (if load?
	(let ((string (load-entry id)))
	  (setf content string)
	  (setf load? nil)
	  string)
	content)))

(defun make-entry (content &key short?)
  (let ((entry (make-instance 'entry)))
    (setf (content entry) content)
    (setf (gethash (id entry) *entries*) entry)
    (values entry (run-handlers entry :add (list :short? short?)))))

(defun get-entry (id)
  (gethash id *entries*))

(defun edit-entry (entry new-content)
  (with-accessors ((content content)) entry
    (let ((content-before (copy-seq content)))
      (setf content new-content)
      (run-handlers entry :edit (list :content-before content-before)))))

(defun del-entry (entry)
  (let ((interactions (run-handlers entry :delete)))
    (remhash (id entry) *entries*)
    (delete-entry-file (id entry))
    interactions))
