(defpackage #:pim-util
  (:use #:common-lisp #:alexandria)
  (:export #:string-cut
	   #:url?
	   #:find-urls
	   #:in-emacs?))

(defpackage #:pim-core
  (:use #:common-lisp #:alexandria #:pim-util)
  (:export #:*entries*
	   #:*shorts*
	   #:*triples*

	   ;; Database
	   #:init-db

	   ;; Entries
	   #:id
	   #:content
	   #:make-entry
	   #:get-entry
	   #:edit-entry
	   #:del-entry

	   ;; Shorts
	   #:short-content?
	   #:short?
	   #:ensure-short
	   #:set-short
	   #:del-short

	   ;; Triples
	   #:add-triple
	   #:del-triple
	   #:ensure-triple
	   #:subj
	   #:pred
	   #:obj
	   #:search-triples
	   #:complement-id
	   #:complement-entry
	   #:add-property-triple
	   #:get-property-triple
	   #:get-property-triples
	   #:del-property-triple
	   #:set-property-triple
	   #:del-orphan
	   #:entry-title
	   #:format-triple

	   ;; Behavior
	   #:define-handler
	   #:define-action
	   #:view-actions
	   #:*status-output*))

(defpackage #:pim-defaults
  (:use #:common-lisp #:alexandria #:pim-core #:pim-util))

(defpackage #:pim-cli
  (:use #:common-lisp #:alexandria #:pim-core #:pim-util)
  (:export #:start))

(defpackage #:pim
  (:use #:common-lisp)
  (:export #:run))
