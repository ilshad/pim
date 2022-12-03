(in-package #:pkm)

(defun del-entry (entry)
  (dolist (triple (search-triples nil nil nil (id entry)))
    (del-triple triple))
  (when (get-short (content entry))
    (del-short entry))
  (remhash (id entry) *entries*))
