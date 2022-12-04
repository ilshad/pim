(asdf:defsystem "pkm"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "src/package")
	       (:file "src/util")

	       (:file "src/core/db")
	       (:file "src/core/handler")
	       (:file "src/core/action")
	       (:file "src/core/entry")
	       (:file "src/core/short")
	       (:file "src/core/triple")

	       (:file "src/defaults/handlers")
	       (:file "src/defaults/actions")

	       (:file "src/ui/cli")

	       (:file "src/main")))
