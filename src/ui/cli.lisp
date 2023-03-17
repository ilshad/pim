(in-package #:pim-cli)

(defvar *colorize-output-p* nil)

;;
;; Output
;;

(defparameter *colors*
  '(:error 1
    :warning 140
    :prompt 50
    :info 100
    :primary 110))

(defun rgb-code (r g b)
  (+ (* r 36) (* g 6) b 16))

(defun ansi-color-start (color)
  (format nil "~c[38;5;~dm" #\Escape color))

(defun ansi-color-end ()
  (format nil "~c[0m" #\Escape))

(defun colorize-string (string color)
  (format nil "~a~a~a" (ansi-color-start color) string (ansi-color-end)))

(defun out (type format-string &rest args)
  (format t (let ((string (apply #'format nil format-string args)))
	      (if *colorize-output-p*
		  (if-let (color (getf *colors* type))
                    (colorize-string string color)
		    string)
		  string))))

(defun prompt (&optional message)
  (when message (out :prompt message))
  (out :prompt "> ")
  (force-output))

(defun hr ()
  (out :primary "~%~v{~c~:*~}~%" 66 '(#\-)))

;;
;; Editor
;;

(defparameter *content-tmp-namestring* "~/.pim/tmp/entry.md")

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
;; Interactions
;;

(defun interaction-when (interaction state)
  (let ((p (getf interaction :when))
	(n (getf interaction :when-not)))
    (or (and (null p) (null n))
	(cond
	  ((keywordp p) (cdr (assoc p state)))
	  ((functionp p) (funcall p state))
	  ((keywordp n) (null (cdr (assoc n state))))
	  ((functionp n) (null (funcall n state)))))))

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

(defparameter *default-input-select-size* 9)

(defun input-select (interaction state)
  (let ((options (copy-seq (cdr (assoc (getf interaction :options) state))))
	(size (getf interaction :size *default-input-select-size*))
	(render (getf interaction :render #'identity)))
    (loop
      (when (null options) (return))
      (let* ((page? (> (length options) size))
	     (items (loop for option in (if page? (subseq options 0 size) options)
			  counting 1 into index
			  collect (list :index index
					:option option
					:render (funcall render option)))))
        (fresh-line)
	(dolist (item items)
	  (out :primary "[ ~a ] ~a~%" (getf item :index) (getf item :render)))
	(out :primary (if page?
			  "~%[ C ] Cancel~%[   ] more...~%"
			  "~%[   ] Done~%"))
	(prompt)
	(let* ((input (string-trim '(#\Space) (read-line))))
	  (when-let* ((index (parse-integer input :junk-allowed t))
		      (item (find index items :key #'(lambda (item) (getf item :index)))))
            (return (getf item :option)))
	  (when (string-equal input "C") (return)))
	(if page?
	    (setf options (subseq options size))
	    (return))))))

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
       (string-trim '(#\Space #\Newline)
		    (if (zerop newlines-submit)
			(read-line)
			(read-multiline newlines-submit)))))

    (:editor
     (string-left-trim
      '(#\Newline)
      (edit-string-in-program
       (let ((content-function (getf interaction :content)))
	 (if content-function
	     (funcall content-function state)
	     "")))))

    (:select
     (input-select interaction state))))

(defun interaction-input (interaction state)
  (let ((input (interaction-read-input interaction state)))
    (if-let (validate (getf interaction :validate)) 
      (if (funcall validate input state)
	  input
	  (progn (out :error "Invalid input~%")
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

	    (:message
	     (out :primary (interaction-message interaction state)))

	    (:function
	     (setf state (funcall (getf interaction :function) state)))

	    (:goto
	     (setf index (position (getf interaction :goto)
				   interactions
				   :key #'(lambda (interaction)
					    (getf interaction :name)))))

	    (:break (return)))

	  (when-let* ((k (getf interaction :interactions))
		      (nested (cdr (assoc k state))))
            (cli-interactions nested))))

      (when (= index (length interactions))
	(return)))
    state))

;;
;; Actions
;;

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
  (when-let (function (cdr (assoc input
				  (getf actions :action-runners)
				  :test #'string-equal)))
    (funcall function)))

;;
;; Menu
;;

(defun read-menu-input (options)
  (prompt)
  (let ((input (string-trim '(#\Space) (read-line))))
    (if-let (result (car (assoc input options :test #'string-equal)))
      (progn
	(terpri)
	(when (not (string= result ""))
	  result))
      (progn (out :error "Unexpected option: ~a~%" input)
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
	  (out :primary
	       "[ ~a ] ~a~%"
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

;;
;; Views
;;

(defun entry-view (entry)
  (terpri)
  (hr)
  (write-string (content entry))
  (hr)
  (out :info "ID: ~d~:[~;, short.~]~%" (id entry) (short? entry))
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
	  (when-let (triple (cdr (assoc index indexed-triples :test #'eql)))
            (route (list :entry (id (complement-entry entry triple)))))
	  (route (or (run-action input actions)
		     (list :entry (id entry))))))))

(defun main-view ()
  (let* ((actions (cli-actions :main))
	 (input (menu (getf actions :menu-options))))
    (route (or (run-action input actions) :main))))

(defun route (route)
  (cond
    ((eql route :main)
     (main-view))
    
    ((and (listp route)
	  (eql (first route) :entry)
	  (integerp (second route)))
     (entry-view (get-entry (second route))))

    ((eql route :exit) t)))

(defun start ()
  (let ((*status-output* *terminal-io*)
	(*colorize-output-p* (not (in-emacs?))))
    (route :main)))
