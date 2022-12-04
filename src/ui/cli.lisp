(in-package #:pkm)

;;
;; Editor
;;

(defparameter *content-filename* "/Users/ilshad/tmp/pkm.tmp")

(defparameter *editor-program-cmd*
  (list "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"
	"-w"
	:content-filename))

(defun editor-program-cmd ()
  (substitute *content-filename* :content-filename *editor-program-cmd*))

(defun edit-string-in-program (&optional (string ""))
  (with-open-file (out *content-filename* :direction :output :if-exists :supersede)
    (write-string string out))
  (uiop:run-program (editor-program-cmd))
  (uiop:read-file-string *content-filename*))

;;
;; Actions & Interactions
;;

(defun prompt (&optional message)
  (when message (format t message))
  (format t "> ")
  (force-output))

(defun read-multiline (newlines-submit)
  (let ((empty-lines-counter 0))
    (with-output-to-string (out)
      (loop
	(let ((char (read-char)))
	  (if (char= char #\Newline)
	      (progn (incf empty-lines-counter)
		     (when (= empty-lines-counter newlines-submit)
		       (return)))
	      (when (not (zerop empty-lines-counter))
		(setf empty-lines-counter 0)))
	  (write-char char out))))))

(defun cli-interaction-read-input (interaction state)
  (let ((message (getf interaction :message)))
    (case (getf interaction :type)
      (:boolean
       (funcall #'y-or-n-p message))

      (:integer
       (prompt message)
       (parse-integer (read-line) :junk-allowed t))

      (:string
       (prompt message)
       (let ((newlines-submit (getf interaction :newlines-submit 0)))
	 (if (zerop newlines-submit)
	     (read-line)
	     (string-trim '(#\Space #\Newline)
			  (read-multiline newlines-submit)))))

      (:editor
       (edit-string-in-program
	(let ((content-function (getf interaction :content)))
	  (if content-function
	      (funcall content-function state)
	      "")))))))

(defun cli-interaction-input (interaction state)
  (let ((input (cli-interaction-read-input interaction state))
	(validate (getf interaction :validate)))
    (if validate
	(if (funcall validate input state)
	    input
	    (progn (format t "Invalid input~%")
		   (cli-interaction-input interaction state)))
	input)))

(defun cli-interactions (interactions)
  (let ((state))
    (dolist (interaction interactions)
      (let ((when-prop (getf interaction :when)))
	(when (or (null when-prop)
		  (cond
		    ((keywordp when-prop) (cdr (assoc when-prop state)))
		    ((functionp when-prop) (funcall when-prop state))))
	  (let* ((message (getf interaction :message))
		 (message (cond
			    ((functionp message) (funcall message state))
			    ((stringp message) message))))
	    (if (getf interaction :type)
		(let ((input (cli-interaction-input interaction state))
		      (key (getf interaction :key))
		      (function (getf interaction :function)))
		  (setf state
			(cond
			  (key (acons key input state))
			  (function (funcall function input state)))))
		(format t message))
	    (let ((k (getf interaction :interactions)))
	      (when k
		(let ((nested (cdr (assoc k state))))
		  (when nested
		    (cli-interactions nested)))))))))
    state))

(defun cli-actions (view-name &optional context)
  (let ((actions (view-actions view-name context)))
    (list :menu-options
	  (mapcar #'(lambda (action)
		      (cons (getf action :command)
			    (or (getf action :description)
				(getf action :label))))
		  actions)

	  :action-runners
	  (mapcar #'(lambda (action)
		      (cons (getf action :command)
			    #'(lambda ()
				(let ((interactions (getf action :interactions))
				      (function (getf action :function))
				      (route (getf action :route)))
				  (if interactions
				      (let ((state (cli-interactions interactions)))
					(when function
					  (funcall function state))
					(when route
					  (if (functionp route)
					      (funcall route state)
					      route)))
				      (progn
					(when function
					  (funcall function))
					(when route
					  (if (functionp route)
					      (funcall route)
					      route))))))))
		  actions))))

(defun run-cli-action (input actions)
  (let ((function (cdr (assoc input
			      (getf actions :action-runners)
			      :test #'string-equal))))
    (when function (funcall function))))

;;
;; Views
;;

(defun hr ()
  (format t "~&~v{~c~:*~}~%" 66 '(#\-)))

(defun read-menu-input (options)
  (prompt)
  (let* ((input (string-trim '(#\Space) (read-line)))
	 (result (car (assoc input options :test #'string-equal))))
    (if result
	(progn
	  (terpri)
	  (when (not (string= result ""))
	    result))
	(progn (format t "Unexpected option: ~a~%" input)
	       (read-menu-input options)))))

(defun cli-menu (options &key empty-option)
  (fresh-line)
  (hr)
  (let ((options (append options
			 (when empty-option
			   (list (cons "" empty-option))))))
    (dolist (item options)
      (if (eq :separator (car item))
	  (hr)
	  (format t "[ ~a ] ~a~%"
		  (let ((option (car item)))
		    (if (string= option "")
			" "
			option))
		  (cdr item))))
    (read-menu-input options)))

(defun menu-items-triples (entry)
  (let ((indexed-triples
	  (loop for triple in (search-triples nil nil nil (id entry))
		counting 1 into index
		collect (cons index triple))))

    (list :indexed-triples
	  indexed-triples

	  :menu-items-triples
	  (loop for item in indexed-triples
		collect (cons (prin1-to-string (car item))
			      (format-triple nil (cdr item) entry))))))

(defun cli-entry-view (entry)
  (terpri)
  (hr)
  (write-string (content entry))
  (hr)
  (format t "ID: ~d~:[~;, short.~]~%" (id entry) (short? entry))
  (destructuring-bind (&key indexed-triples menu-items-triples) (menu-items-triples entry)
    (let* ((actions (cli-actions :entry (list :entry entry
					      :indexed-triples indexed-triples
					      :menu-items-triples menu-items-triples)))
	   (options (append menu-items-triples
			    (when menu-items-triples '((:separator)))
			    (getf actions :menu-options)))
	   (input (cli-menu options))
	   (index (read-from-string input)))
      (if (integerp index)
	  (let ((triple (cdr (assoc index indexed-triples :test #'eql))))
	    (when triple
	      (route (list :entry (id (complement-entry entry triple))))))
	  (route (run-cli-action input actions)
		 (list :entry (id entry)))))))

(defun list-entries-ids ()
  (loop for id being the hash-keys in *entries* using (hash-value entry)
	when (not (short? entry))
	collect id))

(defun entry-title (entry)
  (let ((property (first (get-property-triples entry "title"))))
    (if property
	(content (get-entry (obj property)))
	(string-cut (content entry) 80))))

(defparameter *page-size* 10)

(defun cli-list-entries (ids)
  (loop
    (if ids
	(let* ((page? (> (length ids) *page-size*))
	       (page (if page? (subseq ids 0 *page-size*) ids))
	       (indexed-ids (loop for id in page
				  counting 1 into index
				  collect (cons index id)))
	       (options (append
			 (loop for item in indexed-ids
			       collect (cons (prin1-to-string (car item))
					     (entry-title (get-entry (cdr item)))))
			 (when page? (list (cons "C" "Cancel")))))
	       (input (cli-menu options :empty-option (if page? "...more" "Done."))))
	  (if input
	      (if (string= input "C")
		  (return)
		  (let* ((index (read-from-string input))
			 (id (cdr (assoc index indexed-ids :test #'eql))))
		    (return (get-entry id))))
	      (when (not page?)	(return)))
	  (setf ids (when page? (subseq ids *page-size*))))
	(return))))

(defun cli-search-view ()
  (let ((ids (list-entries-ids)))
    (if ids
	(let ((entry (cli-list-entries ids)))
	  (if entry
	      (route (list :entry (id entry)))
	      (route :main)))
	(progn (format t "Nothing to show")
	       (route :main)))))

(defun cli-main-view ()
  (let* ((actions (cli-actions :main))
	 (input (cli-menu (getf actions :menu-options))))
    (route (run-cli-action input actions) :main)))

(defun route (route &optional default)
  (cond
    ((eql route :main)
     (cli-main-view))

    ((eql route :search)
     (cli-search-view))

    ((and (listp route)
	  (eql (first route) :entry)
	  (integerp (second route)))
     (cli-entry-view (get-entry (second route))))

    ((eql route :exit) t)

    (t (when default (route default)))))
