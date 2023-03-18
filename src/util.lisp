(in-package #:pim-util)

(defun string-cut (string length &optional (suffix ""))
  (let ((string (if-let (newline-position (position #\Newline string))
		  (subseq string 0 newline-position)
		  string)))
    (if (< length (length string))
	(let ((string (subseq string 0 (- length (length suffix)))))
          (values (format nil "~a~a" string suffix) t))
	string)))

(defun url? (string)
  (not (zerop (ppcre:count-matches "^https?:\\/\\/\\S+$" string))))

(defun find-urls (string)
  (ppcre:all-matches-as-strings "(https?:\\/\\/\\S+\\w)+" string))

(defun in-emacs? ()
  (when-let* ((package (find-package "SWANK"))
	      (symbol (find-symbol "*GLOBALLY-REDIRECT-IO*" package)))
    (eq (symbol-value symbol) :started-from-emacs)))
