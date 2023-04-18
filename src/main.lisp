(in-package #:pim)

(defun run ()
  (pim-core:init-db)
  (pim-cli:start))
