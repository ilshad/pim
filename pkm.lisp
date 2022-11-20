; (ql:quickload :cl-ppcre)

(defpackage #:pkm (:use :common-lisp))

(in-package #:pkm)

;;
;; Entry
;;

(defvar *entries* (make-hash-table))

(defvar *entry-id-pointer* 0)

(defclass entry ()
  ((id :initarg :id :initform (incf *entry-id-pointer*) :reader id)
   (content :initarg :content :accessor content)))

(defmethod print-object ((object entry) stream)
  (with-slots (id content) object
    (multiple-value-bind (string cut?) (string-cut content 20)
      (format stream
	      "~d: \"~a~:[\"~;...\" (~d chars)~]"
	      id string cut? (length content)))))

(defun string-cut (string length)
  (let ((length (min length (or (position #\Newline string) (1+ length)))))
    (if (< length (length string))
	(values (subseq string 0 length) t)
	string)))

(defun make-entry (content &key short?)
  (let ((entry (make-instance 'entry :content content)))
    (format t "(i) New entry ~s~%" entry)
    (setf (gethash (id entry) *entries*) entry)
    (values entry (run-handlers entry (list :short? short?)))))

(defun get-entry (id)
  (gethash id *entries*))

(defun edit-entry (entry new-content)
  (with-slots (content) entry
    (let ((content-before (copy-seq content)))
      (setf content new-content)
      (run-handlers entry (list :content-before content-before)))))

(defun del-entry (entry)
  (dolist (triple (search-triples nil nil nil (id entry)))
    (del-triple triple))
  (when (get-short (content entry))
    (del-short entry))
  (remhash (id entry) *entries*))

;;
;; Triples
;;

(defvar *triples* (list))

(defun get-triple (triple &optional (triples *triples*))
  (if (equalp triple (car triples))
      (car triples)
      (when (cdr triples) (get-triple triple (cdr triples)))))

(defun set-triple (old new &optional (triples *triples*))
  (if (equalp old (car triples))
      (progn (setf (car triples) new) t)
      (when (cdr triples) (set-triple old new (cdr triples)))))

(defun del-triple (triple)
  (format t "(i) Remove triple ~s~%" triple)
  (set-triple triple nil *triples*))

(defun add-triple (triple)
  (push triple *triples*)
  (format t "(i) New triple ~s~%" triple)
  triple)

(defun ensure-triple (triple)
  (or (get-triple triple) (add-triple triple)))

(defun subj (triple) (car triple))
(defun pred (triple) (cadr triple))
(defun obj (triple) (caddr triple))

(defun search-triples (s &optional p o s-or-o (triples *triples*))
  (remove-if-not
   (cond
     ((and (null p) s-or-o)
      #'(lambda (x)
	  (or (eql s-or-o (subj x))
	      (eql s-or-o (obj x)))))

     ((and p s-or-o)
      #'(lambda (x)
	  (and (equalp p (pred x))
	       (or (eql s-or-o (subj x))
		   (eql s-or-o (obj x))))))

     ((and s (null p) (null o))
      #'(lambda (x)
	  (eql s (subj x))))

     ((and (null s) p (null o))
      #'(lambda (x)
	  (equalp p (pred x))))

     ((and (null s) (null p) o)
      #'(lambda (x)
	  (eql o (obj x))))

     ((and (null s) p o)
      #'(lambda (x)
	  (and (equalp p (pred x))
	       (eql o (obj x)))))

     ((and s (null p) o)
      #'(lambda (x)
	  (and (eql s (subj x))
	       (eql o (obj x)))))

     ((and s p (null o))
      #'(lambda (x)
	  (and (eql s (subj x))
	       (equalp p (pred x))))))
   triples))

(defun complement-entry (entry triple)
  (cond
    ((= (id entry) (subj triple))
     (values (get-entry (obj triple)) :subj))
    ((= (id entry) (obj triple))
     (values (get-entry (subj triple)) :obj))))

(defun format-triple (stream triple entry)
  (multiple-value-bind (other-entry position) (complement-entry entry triple)
    (multiple-value-bind (content cut?) (string-cut (content other-entry) 80)
      (case position
	(:subj
	 (format stream "-> ~a -> ~a~:[~;...~]" (pred triple) content cut?))
	(:obj
	 (format stream "<- ~a <- ~a~:[~;...~]" (pred triple) content cut?))))))

;;
;; Properties
;;

(defun add-property-triple (entry key value)
  (ensure-triple (list (id entry) key (id (ensure-short value)))))

(defun get-property-triple (entry key value)
  (let ((property-entry (get-short value)))
    (when property-entry
      (get-triple (list (id entry) key (id property-entry))))))

(defun get-property-triples (entry key)
  (search-triples (id entry) key))

(defun del-property-triple (entry key value)
  (let ((property-entry (get-short value)))
    (when property-entry
      (del-triple (list (id entry) key (id property-entry))))))

;;
;; Handlers
;;

(defparameter *handlers-tmp* nil)
(defparameter *handlers* nil)

(defun sort-handlers (items &optional provided-item)
  (let ((item (or provided-item (first items))))
    (dolist (dep-name (cdr item))
      (let ((dep-item (assoc dep-name items)))
	(when (and dep-item (not (member dep-item *handlers*)))
	  (sort-handlers items dep-item))))
    (pushnew (car item) *handlers*)
    (when (and (null provided-item) (rest items))
      (sort-handlers (rest items)))))

(defun add-handler (name deps)
  (setf *handlers* nil)
  (sort-handlers (push (cons name deps) *handlers-tmp*))
  (setf *handlers* (reverse *handlers*)))

(defmacro define-handler (name deps args &body body)
  "Create a handler function that runs when the entry has been
   created or updated.

   For example, extract things from the content and create default
   triples and define additional interactions with user.

   A handler can rely on side effects of other handlers. For that,
   handlers declare dependencies between each other, so they will
   run sorted according to these dependencies.

   The function, created by this macro, takes the entry it runs for
   and 'context' plist. It returns nil or list of UI interactions.

   Context plist contains following keys:
   - :content-before - previous entry content string, if there was one.

   Interaction is a plist with following keys:

   - :type - keyword, one of :boolean, :string, :integer
   - :message - string or function that takes state and returns string
   - :key - keyword, to acons the input into the state
   - :function - function that takes input and state, returns state
   - :validation - function that takes input and state, returns boolean
   - :when - keyword or function that takes state and returns boolean

   As a result, interaction shows :message and asks for input depending
   on :type keyword:

   - :boolean - UI asks for a boolean value (e.g. yes / no),
   - :string - UI asks for one-line string input.

   :function takes the user input and state, performs side effects
   and returns the state (possibly updated). The state is a alist
   (association list), where interactions can put their results
   and get results of other interactions.

   :key simply puts the input into the state. This option can be used
   instead of :function if the only goal of the interaction is to provide
   input that can be used in subsequent interactions.

   :when - keyword or function. The function takes the state and returns
   boolean. If its result is NIL, UI skips this interaction.
   Keyword variant simply checks state for boolean value.

   If :type is not presented, interaction only shows the message.

   If :validation function returns nil, UI asks for the input again.

   All interactions from handlers run sequentially after all the
   handlers, using the state alist to exchange their results.
   
   Macro parameters:

   - handler symbol;
   - dependencies - list of handler symbols;
   - function arguments:
       - entry,
       - context.

   Returns: nil of list of interactions."
  `(progn
     (add-handler (quote ,name) (quote ,deps))
     (defun ,name ,args ,@body)))

(defun run-handlers (entry &optional context)
  (let ((interactions))
    (dolist (symbol *handlers*)
      (dolist (interacts (funcall (symbol-function symbol) entry context))
	(when interacts (push interacts interactions))))
    interactions))

;;
;; Default handlers
;;

(defvar *shorts* (make-hash-table :test 'equalp))

(defun short-content? (string)
  (and string
       (null (find #\Newline string))
       (or (null (find-urls string))
	   (url? string))))

(defun short? (entry)
  (not (null (gethash (content entry) *shorts*))))

(defun get-short (content)
  (when (short-content? content)
    (let ((id (gethash content *shorts*)))
      (when id
	(let ((entry (get-entry id)))
	  (format t "(i) Found short: ~s~%" entry)
	  entry)))))

(defun ensure-short (string)
  (let ((content (string-trim '(#\Space #\Newline) string)))
    (or (get-short content) (make-entry content :short? t))))

(defun set-short (entry)
  (setf (gethash (content entry) *shorts*) (id entry))
  (format t "(i) Added to shorts: ~s~%" entry))

(defun del-short (entry &optional content)
  (let ((content (or content (content entry))))
    (when (remhash content *shorts*)
      (format t "(i) Removed from shorts: ~s~%" (string-cut content 20)))))

(define-handler update-short () (entry context)
  "Shorts are entries, whose content can be used as identifiers, so
   they are indexed additionally. A short entry is a one-line string,
   usually a word or phrase, or URL. It's never created explicitly,
   but only with a triple."
  (let ((before (getf context :content-before))
	(after (content entry)))
    (when (not (string= before after))
      (if (getf context :short?)
	  (progn (when (short-content? before) (del-short entry before))
		 (when (short-content? after) (set-short entry)))
	  (when (and (short-content? before)
		     (short-content? after)
		     (not (string= before after)))
	    (del-short entry before)
	    (set-short entry))))))

(defun url? (string)
  (not (zerop (ppcre:count-matches "^https?:\\/\\/\\S+$" string))))

(define-handler type-url () (entry context)
  "If the whole content string is a URL, create property 'type' 'URL'."
  (declare (ignore context))
  (if (url? (content entry))
      (add-property-triple entry "type" "URL")
      (when (get-property-triple entry "type" "URL")
	(del-property-triple entry "type" "URL"))))

(defun find-urls (string)
  (ppcre:all-matches-as-strings "(https?:\\/\\/\\S+\\w)+" string))

(define-handler has-url (type-url) (entry context)
  "1. Extract all URLs from the content.
   2. Create or find entries for each URL.
   3. Create property 'url' [extraced URL].
   4. Remove old 'url' properties which are not applicable now."
  (declare (ignore context))
  (when (null (get-property-triple entry "type" "URL"))
    (let ((before (get-property-triples entry "has-url"))
	  (after (loop for url in (find-urls (content entry))
		       collect (list (id entry) "has-url" (id (ensure-short url))))))
      (dolist (triple (set-difference before after :test #'equalp))
	(del-triple triple))
      (dolist (triple (set-difference after before :test #'equalp))
	(add-triple triple)))))

(defun extract-title (content)
  (when (> (count #\Newline content) 1)
    (with-input-from-string (stream content)
      (let ((title (read-line stream)))
	(when (and (not (zerop (length title)))
		   (zerop (length (read-line stream))))
	  title)))))

(defun title-triples (entry)
  (loop for triple in (get-property-triples entry "title")
	collect (cons (content (get-entry (obj triple)))
		      triple)))

(define-handler set-title () (entry context)
  "If first line is separated from the rest content by an empty line,
   then interactively create 'title' property using this line.
   Otherwise prompt to user to set the title explicitly or to skip.
   Also interactively take care of old 'title' properties, if such
   triples are exist."
  (let ((title (extract-title (content entry)))
	(before (title-triples entry))
	(interactions))
    (if title
	(when (null (cdr (assoc title before :test #'string=)))
	  (push (list :type :boolean
		      :message (format nil "Set 'title' property ~s?" title)
		      :function #'(lambda (input state)
				    (when input
				      (add-property-triple entry "title" title))
				    state))
		interactions))
	(when (and (null before)
		   (not (zerop (count #\Newline (content entry))))
		   (not (string= (content entry) (getf context :content-before))))
	  (push (list :type :boolean
		      :message "Add title?"
		      :key :add-title?)
		interactions)
	  (push (list :when :add-title?
		      :type :string
		      :message (format nil "Enter title:")
		      :function #'(lambda (input state)
				    (add-property-triple entry "title" input)
				    state))
		interactions)))
    (dolist (cons before)
      (when (not (string= (car cons) title))
	(push (list :type :boolean
		    :message (format nil "Remove 'title' property ~s?" (car cons))
		    :function #'(lambda (input state)
				  (when input
				    (del-triple (cdr cons)))
				  state))
	      interactions)))
    interactions))

;;
;; Actions
;;

(defparameter *actions* nil)

(defun add-action (name view index)
  (setf (getf *actions* view)
	(sort (acons name index (copy-seq (getf *actions* view)))
	      #'<
	      :key #'cdr)))

(defmacro define-action (name (view index) args &body body)
  "Create action for particular view in UI in a way, that the action
   is abstracted from actual UI, so it will appear in all frontends.

   The function, created by this macro, takes context plist, passed
   from frontend. The schema of context is specific for each view.

   It returns plist with following keys:

   - :label
   - :description
   - :command
   - :interactions - see documentation for 'define-handler, it's the same.
   - :function
   - :when

  TBD"
  (let ((symbol (read-from-string (concatenate 'string (symbol-name name) "-action"))))
    `(progn
       (add-action (quote ,symbol) ,view ,index)
       (defun ,symbol ,args ,@body))))

(defun view-actions (view context)
  (let ((actions (mapcar #'(lambda (cons)
			     (funcall (car cons) context))
			 (getf *actions* view))))
    (remove-if-not
     #'(lambda (action)
	 (let ((when-function (getf action :when)))
	   (if when-function
	       (funcall when-function)
	       t)))
     actions)))

;;
;; Default actions
;;

(define-action delete-entry (:entry 10) (context)
  (list :label "Delete"
	:description "Delete entry"
	:command "D"
	:interactions '((:type :boolean
			 :message "Delete entry?"
			 :key :delete?))
	:function #'(lambda (state)
		      (when (cdr (assoc :delete? state))
			(del-entry (getf context :entry))))))

(define-action add-triple (:entry 20) (context)
  (list :label "Add triple"
	:description "Add triple for this subject"
	:command "+"
	:interactions '((:type :string
			 :message "Predicate:"
			 :key :predicate)
			(:type :string
			 :message "Object:"
			 :key :object))
	:function #'(lambda (state)
		      (ensure-triple
		       (list (id (getf context :entry))
			     (cdr (assoc :predicate state))
			     (let ((string (cdr (assoc :object state))))
			       (or (parse-entry-id string)
				   (id (ensure-short string)))))))))

(define-action del-triple (:entry 30) (context)
  (list :label "Delete triple"
	:command "-"
	:when #'(lambda () (getf context :indexed-triples))
	:interactions (list
		       (list :type :integer
			     :message "Select triple to delete:~%"
			     :key :index
			     :validation #'(lambda (input state)
					     (declare (ignore state))
					     (assoc input
						    (getf context :indexed-triples)
						    :test #'eql)))
		       (list :message
			     #'(lambda (state)
				 (format nil "Selected: ~s~%"
					 (format-triple
					  nil
					  (cdr (assoc (getf state :index)
						      (getf context :indexed-triples)
						      :test #'eql))
					  (getf context :entry)))))
		       (list :type :boolean
			     :message "Delete this triple?"
			     :key :delete?))
	:function #'(lambda (state)
		      (when (getf state :delete?)
			(let ((triple (cdr (assoc (getf state :index)
						  (getf context :indexed-triples)
						  :test #'eql))))
			  (when triple
			    (del-triple triple)))))))

;;
;; Edit content in CLI
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

(defun trim-if-one-liner (string)
  (let ((first-newline (position #\Newline string)))
    (if first-newline
	(let ((rest-string (subseq string first-newline)))
	  (if (zerop (length (string-trim '(#\Newline #\Space) rest-string)))
	      (string-right-trim '(#\Newline #\Space) string)
	      string))
	string)))

(defun read-multiline ()
  (let ((empty-lines-counter 0))
    (with-output-to-string (out)
      (loop
	(let ((char (read-char)))
	  (if (char= char #\Newline)
	      (progn (incf empty-lines-counter)
		     (when (= empty-lines-counter 2)
		       (return)))
	      (when (not (zerop empty-lines-counter))
		(setf empty-lines-counter 0)))
	  (write-char char out))))))

;;
;; CLI
;;

(defun hr ()
  (format t "~&~v{~c~:*~}~%" 66 '(#\-)))

(defun prompt (&optional message)
  (when message (format t message))
  (format t "> ")
  (force-output))

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

(defmacro cli-menu-case (&body cases)
  `(case (cli-menu ',(loop for x in cases collect (cons (caar x) (cadar x))))
     ,@(loop for x in cases collect `(,(caar x) ,@(cdr x)))))

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

(defun parse-entry-id (string)
  (let ((found (ppcre:all-matches-as-strings "^#\\d+$" string)))
    (when found (parse-integer (subseq (first found) 1)))))

(defun cli-interactions (interactions)
  (let ((state))
    (dolist (interaction interactions)
      (terpri)
      (destructuring-bind (&key when message type key function) interaction
       	(when (or (null when)
		  (cond
		    ((keywordp when) (cdr (assoc when state)))
		    ((functionp when) (funcall when state))))
	  (let* ((message (cond
			    ((functionp message) (funcall message state))
			    ((stringp message) message)))
		 (input (case type
			  (:boolean
			   (funcall #'y-or-n-p message))

			  (:string
			   (prompt message)
			   (read-line))

			  (:integer
			   (prompt message)
			   (let ((input (read-from-string
					 (string-trim '(#\Space)
						      (read-line)))))
			     (when (integerp input)
			       input))))))
	    (setf state
		  (cond
		    (key (acons key input state))
		    (function (funcall function input state))))))))
    state))

(defun cli-actions (view context)
  (let ((actions (view-actions view context)))
    (list :options
	  (mapcar #'(lambda (action)
		      (cons (getf action :command)
			    (or (getf action :description)
				(getf action :label))))
		  actions)

	  :cases
	  (mapcar #'(lambda (action)
		      (cons (getf action :command)
			    #'(lambda ()
				(let ((interactions (getf action :interactions))
				      (function (getf action :function)))
				  (if interactions
				      (let ((state (cli-interactions interactions)))
					(when function (funcall function state)))
				      (when function (funcall function)))))))
		  actions))))

(defun cli-action-case (input actions)
  (let ((function (cdr (assoc input (getf actions :cases) :test #'string-equal))))
    (when function
      (funcall function)
      t)))

(defun cli-entry (entry)
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
			    '(("E" . "Edit entry"))
			    (getf actions :options)
			    '(("Q" . "Quit"))))
	   (input (cli-menu options))
	   (index (read-from-string input)))
      (if (integerp index)
	  (let ((triple (cdr (assoc index indexed-triples :test #'eql))))
	    (when triple
	      (cli-entry (complement-entry entry triple))))
	  (or (cli-action-case input actions)
	      (case input
		("E"
		 (let ((string (edit-string-in-program (content entry))))
		   (cli-interactions (edit-entry entry (trim-if-one-liner string))))
		 (cli-entry entry))))))))

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

(defun cli-create-entry (content)
  (multiple-value-bind (entry interactions) (make-entry content)
    (cli-interactions interactions)
    (cli-entry entry)))

(defun cli-main ()
  (cli-menu-case

    (("I" "Create entry here")
     (prompt "New entry:~%")
     (cli-create-entry (string-trim '(#\Space #\Newline) (read-multiline)))
     (cli-main))

    (("E" "Create entry in editor")
     (cli-create-entry (trim-if-one-liner (edit-string-in-program)))
     (cli-main))

    (("S" "Search / list entries")
     (let ((ids (list-entries-ids)))
       (if ids
	   (let ((entry (cli-list-entries (list-entries-ids))))
	     (when entry (cli-entry entry)))
	   (format t "Nothing to show")))
     (cli-main))

    (("Q" "Quit")
     (format t "Bye-bye.~%"))))

(defun run ()
  (cli-main))
