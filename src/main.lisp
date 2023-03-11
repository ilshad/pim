(in-package #:pim)

(defun run ()
  (init-db)
  (pim-cli:start))
