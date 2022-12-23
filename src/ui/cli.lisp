(in-package #:pkm-cli)

;;
;; Editor
;;

(defparameter *content-tmp-namestring* "~/.pkm/tmp/entry.md")

(defparameter *editor-program-cmd*
  '("/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"
    "-w"
    :tmp))

(defun edit-string-in-program (&optional (string ""))
  (let ((tmp (uiop:native-namestring *content-tmp-namestring*)))
    (ensure-directories-exist tmp)
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (write-string string out))
    (uiop:run-program (substitute tmp :tmp *editor-program-cmd*))
    (uiop:read-file-string tmp)))

;;
;; Actions & Interactions
;;

(defun interaction-when (interaction state)
  (let ((x (getf interaction :when)))
    (or (null x)
	(cond
	  ((keywordp x) (cdr (assoc x state)))
	  ((functionp x) (funcall x state))))))

(defun interaction-message (interaction state)
  (let ((x (getf interaction :message)))
    (cond
      ((stringp x) x)
      ((functionp x) (funcall x state)))))

(defun interaction-input-function (interaction)
  (let ((key (getf interaction :key))
	(function (getf interaction :function)))
    (cond
      (key #'(lambda (input state) (acons key input state)))
      (function function))))

(defun interaction-listing-function (interaction)
  (let ((x (getf interaction :listing)))
    (cond
      ((keywordp x) #'(lambda (state) (cdr (assoc x state))))
      ((functionp x) x))))

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

(defun interaction-read-input (interaction state)
  (case (getf interaction :input)
    (:boolean
     (funcall #'y-or-n-p (interaction-message interaction state)))

    (:integer
     (prompt (interaction-message interaction state))
     (parse-integer (read-line) :junk-allowed t))

    (:string
     (prompt (interaction-message interaction state))
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
	    ""))))))

(defun interaction-input (interaction state)
  (let ((input (interaction-read-input interaction state))
	(validate (getf interaction :validate)))
    (if validate
	(if (funcall validate input state)
	    input
	    (progn (format t "Invalid input~%")
		   (interaction-input interaction state)))
	input)))

(defun cli-interactions (interactions)
  (let ((index 0) state)
    (loop
      (let ((interaction (elt interactions index)))
	(incf index)
	(when (interaction-when interaction state)
	  (case (getf interaction :type)
	    (:input
	     (setf state (funcall (interaction-input-function interaction)
				  (interaction-input interaction state)
				  state)))

	    (:listing
	     (dolist (item (funcall (interaction-listing-function interaction) state))
	       (format t "[ ~a ] ~a~%" (getf item :index) (getf item :label))))

	    (:message
	     (format t (interaction-message interaction state)))

	    (:function
	     (setf state (funcall (getf interaction :function) state)))

	    (:goto
	     (setf index (position (getf interaction :goto)
				   interactions
				   :key #'(lambda (interaction)
					    (getf interaction :name))))))

	  (let ((k (getf interaction :interactions)))
	    (when k
	      (let ((nested (cdr (assoc k state))))
		(when nested
		  (cli-interactions nested)))))))

      (when (= index (length interactions))
	(return)))
    state))

(defun action-menu-option (action)
  (cons (getf action :command)
	(or (getf action :description)
	    (getf action :label))))

(defun action-runner (action)
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

(defun cli-actions (view-name &optional context)
  (let ((actions (view-actions view-name context)))
    (list :menu-options (mapcar #'action-menu-option actions)
	  :action-runners (mapcar #'action-runner actions))))

(defun run-action (input actions)
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

(defun menu (options &key empty-option)
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

(defun entry-view (entry)
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
	   (input (menu options))
	   (index (read-from-string input)))
      (if (integerp index)
	  (let ((triple (cdr (assoc index indexed-triples :test #'eql))))
	    (when triple
	      (route (list :entry (id (complement-entry entry triple))))))
	  (route (or (run-action input actions)
		     (list :entry (id entry))))))))

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

(defun list-entries (ids)
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
	       (input (menu options :empty-option (if page? "...more" "Done."))))
	  (if input
	      (if (string= input "C")
		  (return)
		  (let* ((index (read-from-string input))
			 (id (cdr (assoc index indexed-ids :test #'eql))))
		    (return (get-entry id))))
	      (when (not page?)	(return)))
	  (setf ids (when page? (subseq ids *page-size*))))
	(return))))

(defun search-view ()
  (let ((ids (list-entries-ids)))
    (if ids
	(let ((entry (list-entries ids)))
	  (if entry
	      (route (list :entry (id entry)))
	      (route :main)))
	(progn (format t "Nothing to show")
	       (route :main)))))

(defun main-view ()
  (let* ((actions (cli-actions :main))
	 (input (menu (getf actions :menu-options))))
    (route (or (run-action input actions) :main))))

(defun route (route)
  (cond
    ((eql route :main)
     (main-view))

    ((eql route :search)
     (search-view))

    ((and (listp route)
	  (eql (first route) :entry)
	  (integerp (second route)))
     (entry-view (get-entry (second route))))

    ((eql route :exit) t)))
