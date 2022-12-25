(defpackage #:pkm-util
  (:use #:cl)
  (:export #:string-cut
	   #:url?
	   #:find-urls))

(defpackage #:pkm-core
  (:use #:cl #:pkm-util)
  (:export #:*entries*
	   #:*shorts*
	   #:*triples*

	   #:init-db

	   #:id
	   #:content
	   #:make-entry
	   #:get-entry
	   #:edit-entry
	   #:del-entry

	   #:short-content?
	   #:short?
	   #:ensure-short
	   #:set-short
	   #:del-short

	   #:add-triple
	   #:del-triple
	   #:ensure-triple
	   #:format-triple
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

	   #:define-handler
	   #:define-action
	   #:view-actions))

(defpackage #:pkm-defaults
  (:use #:cl #:pkm-core	#:pkm-util))

(defpackage #:pkm-cli
  (:use #:cl #:pkm-core	#:pkm-util)
  (:export #:route))

(defpackage #:pkm
  (:use #:cl #:pkm-core)
  (:export #:run))
