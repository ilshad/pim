(in-package #:pkm-util)

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
  (let ((package (find-package "SWANK")))
    (when package
      (let ((symbol (find-symbol "*GLOBALLY-REDIRECT-IO*" package)))
	(when symbol
	  (eq (symbol-value symbol) :started-from-emacs))))))
