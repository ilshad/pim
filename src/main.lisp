(in-package #:pkm)

(defun run ()
  (init-db :entries *entries*
	   :shorts *shorts*
	   :triples *triples*)
  (route :main))
