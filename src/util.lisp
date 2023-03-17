(in-package #:pim-util)

(defun string-cut (string length)
  (let ((length (min length (or (position #\Newline string) (1+ length)))))
    (if (< length (length string))
	(values (subseq string 0 length) t)
	string)))

(defun url? (string)
  (not (zerop (ppcre:count-matches "^https?:\\/\\/\\S+$" string))))

(defun find-urls (string)
  (ppcre:all-matches-as-strings "(https?:\\/\\/\\S+\\w)+" string))

(defun in-emacs? ()
  (when-let* ((package (find-package "SWANK"))
	      (symbol (find-symbol "*GLOBALLY-REDIRECT-IO*" package)))
    (eq (symbol-value symbol) :started-from-emacs)))
