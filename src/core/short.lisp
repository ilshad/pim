(in-package #:pkm)

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
      (when id (get-entry id)))))

(defun ensure-short (string)
  (let ((content (string-trim '(#\Space #\Newline) string)))
    (or (get-short content) (make-entry content :short? t))))

(defun set-short (entry)
  (setf (gethash (content entry) *shorts*) (id entry))
  (save-shorts))

(defun del-short (entry &optional content)
  (let ((content (or content (content entry))))
    (when (remhash content *shorts*)
      (save-shorts))))
