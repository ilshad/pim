(in-package #:pkm)

(defun run ()
  (init-db)
  (pkm-cli:start))
