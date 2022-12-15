(in-package #:pkm)

(defun run ()
  (init-db)
  (route :main))
