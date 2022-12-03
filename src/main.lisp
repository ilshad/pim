(in-package #:pkm)

(defun run ()
  (init-db :entries *entries*
	   :entry-id-pointer *entry-id-pointer*
	   :shorts *shorts*
	   :triples *triples*)
  (route :main))
