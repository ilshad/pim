(in-package #:pkm)

(defvar *shorts* (make-hash-table :test 'equalp))

(defun short-content? (string)
  (and string
       (null (find #\Newline string))
       (or (null (find-urls string))
	   (url? string))))

(defun short? (entry)
  (not (null (gethash (content entry) *shorts*))))

(defun get-short (content)
  (when (short-content? content)
    (let ((id (gethash content *shorts*)))
      (when id
	(let ((entry (get-entry id)))
	  (format t "(i) Found short: ~s~%" entry)
	  entry)))))

(defun ensure-short (string)
  (let ((content (string-trim '(#\Space #\Newline) string)))
    (or (get-short content) (make-entry content :short? t))))

(defun set-short (entry)
  (setf (gethash (content entry) *shorts*) (id entry))
  (save-shorts *shorts*)
  (format t "(i) Added to shorts: ~s~%" entry))

(defun del-short (entry &optional content)
  (let ((content (or content (content entry))))
    (when (remhash content *shorts*)
      (save-shorts *shorts*)
      (format t "(i) Removed from shorts: ~s~%" (string-cut content 20)))))
