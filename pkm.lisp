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

(defun make-entry (content)
  (let ((entry (make-instance 'entry :content content)))
    (format t "(i) New entry ~s~%" entry)
    (setf (gethash (id entry) *entries*) entry)
    (values entry (run-handlers entry))))

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
    (del-short (content entry)))
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
	 (format stream "-> ~a : ~a~:[~;...~]" (pred triple) content cut?))
	(:obj
	 (format stream "~a~:[~;...~] : ~a ->" content cut? (pred triple)))))))

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

(defparameter *handlers-deps* nil)
(defparameter *sorted-handlers* nil)

(defun sort-handlers (items &optional provided-item)
  (let ((item (or provided-item (first items))))
    (dolist (dep-name (cdr item))
      (let ((dep-item (assoc dep-name items)))
	(when (and dep-item (not (member dep-item *sorted-handlers*)))
	  (sort-handlers items dep-item))))
    (pushnew (car item) *sorted-handlers*)
    (when (and (null provided-item) (rest items))
      (sort-handlers (rest items)))))

(defun add-handler (name deps)
  (setf *sorted-handlers* nil)
  (sort-handlers (push (cons name deps) *handlers-deps*))
  (setf *sorted-handlers* (reverse *sorted-handlers*)))

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

   - :type - keyword, one of :boolean, :string.
   - :msg - string or function that takes state and returns string
   - :fn - function that takes input and state, returns state
   - :when (optional) - function that takes state and returns boolean

   As a result, interaction shows :msg and asks for input depending
   on :type keyword:

   - :boolean - UI asks for a boolean value (e.g. yes / no),
   - :string - UI asks for one-line string input.

   :fn function takes the user input and state, performs side effects
   and returns the state (possibly updated). The state is a alist
   (association list), where interactions can put their results
   and get results of other interactions.

   :when function takes the state and return boolean. If its
   result is NIL, UI skips this interaction.
        
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
    (dolist (symbol *sorted-handlers*)
      (dolist (interacts (funcall (symbol-function symbol) entry context))
	(when interacts (push interacts interactions))))
    interactions))

;;
;; Default handlers
;;

(defvar *shorts* (make-hash-table :test 'equalp))

(defun short-content? (string)
  (and (null (find #\Newline string))
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
    (or (get-short content) (make-entry content))))

(defun del-short (content)
  (remhash content *shorts*))

(define-handler short () (entry context)
  "Shorts are entries, whose content can be used as identifiers, so
   they are indexed additionally. A short entry is a one-line string,
   usually a word or phrase, or URL. It's never created explicitly,
   but only with a triple."
  (with-slots (id (after content)) entry
    (let ((before (getf context :content-before)))
      (when (not (string= after before))
	(when (short-content? after)
	  (setf (gethash after *shorts*) id)
	  (format t "(i) Added to shorts: ~s~%" entry))
	(when (short-content? before)
	  (when (remhash before *shorts*)
	    (format t "(i) Removed from shorts: ~s~%" entry)))))))

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
		      :msg (format nil "Set 'title' property ~s?" title)
		      :fn #'(lambda (input state)
			      (when input
				(add-property-triple entry "title" title))
			      state))
		interactions))
	(when (and (null before)
		   (not (zerop (count #\Newline (content entry))))
		   (not (string= (content entry) (getf context :content-before))))
	  (push (list :type :boolean
		      :msg "Add title?"
		      :fn #'(lambda (input state)
			      (acons :add-title? input state)))
		interactions)
	  (push (list :type :string
		      :msg (format nil "Enter title:")
		      :fn #'(lambda (input state)
			      (add-property-triple entry "title" input)
			      state)
		      :when #'(lambda (state)
				(cdr (assoc :add-title? state))))
		interactions)))
    (dolist (cons before)
      (when (not (string= (car cons) title))
	(push (list :type :boolean
		    :msg (format nil "Remove 'title' property ~s?" (car cons))
		    :fn #'(lambda (input state)
			    (when input
			      (del-triple (cdr cons)))
			    state))
	      interactions)))
    interactions))

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

(defun selected-triple (index indexed-triples)
  (cdr (assoc index indexed-triples :test #'eql)))

(defun parse-entry-id (string)
  (let ((found (ppcre:all-matches-as-strings "^#\\d+$" string)))
    (when found (parse-integer (subseq (first found) 1)))))

(defun cli-message (msg state)
  (if (functionp msg)
      (funcall msg state)
      msg))

(defun cli-input (type message)
  (case type
    (:boolean (funcall #'y-or-n-p message))
    (:string (prompt (format t "~a~%" message)) (read-line))))

(defun cli-interactions (interactions)
  (let ((state))
    (dolist (interaction interactions)
      (terpri)
      (destructuring-bind (&key when fn msg type) interaction
       	(when (or (null when) (funcall when state))
	  (let ((message (cli-message msg state)))
	    (setf state (funcall fn (cli-input type message) state))))))))

(defun cli-entry (entry)
  (terpri)
  (hr)
  (write-string (content entry))
  (hr)
  (format t "ID: ~d~:[~;, short.~]~%" (id entry) (short? entry))
  (destructuring-bind (&key indexed-triples menu-items-triples) (menu-items-triples entry)
    (let* ((actions (list (cons "E" "Edit entry")
			  (cons "D" "Delete entry")
			  (cons "+" "Add triple for this subject")
			  (when indexed-triples (cons "-" "Delete triple"))
			  (cons "Q" "Quit")))
	   (input (cli-menu (append menu-items-triples
				    (when menu-items-triples '((:separator)))
				    (remove nil actions))))
	   (index (read-from-string input)))
      (if (integerp index)
	  (let ((triple (selected-triple index indexed-triples)))
	    (when triple
	      (cli-entry (complement-entry entry triple))))
	  (case input
	    ("E"
	     (let ((string (edit-string-in-program (content entry))))
	       (cli-interactions (edit-entry entry (trim-if-one-liner string))))
	     (cli-entry entry))

	    ("D"
	     (when (y-or-n-p "Delete entry?")
	       (del-entry entry)))
	    
	    ("+"
	     (ensure-triple
	      (list (id entry)
		    (progn
		      (prompt "Predicate:")
		      (read-line))
		    (progn
		      (prompt "Object:")
		      (let ((string (read-line)))
			(terpri)
			(or (parse-entry-id string)
			    (id (ensure-short string)))))))
	     (cli-entry entry))

	    ("-"
	     (hr)
	     (format t "Select triple to delete:~%")
	     (let ((index (read-from-string (read-menu-input menu-items-triples))))
	       (if (integerp index)
		   (let ((triple (selected-triple index indexed-triples)))
		     (when triple
		       (hr)
		       (format t "Selected: ~s~%" (format-triple nil triple entry))
		       (hr)
		       (when (y-or-n-p "Delete this triple?")
			 (del-triple triple))))))
	     (cli-entry entry)))))))

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

(defun cli-main ()
  (cli-menu-case

    (("I" "Create entry here")
     (prompt "New entry:~%")
     (let ((input (string-trim '(#\Space #\Newline) (read-multiline))))
       (multiple-value-bind (entry interactions) (make-entry input)
	 (cli-interactions interactions)
	 (cli-entry entry)))
     (cli-main))

    (("E" "Create entry in editor")
     (let ((content (trim-if-one-liner (edit-string-in-program))))
       (multiple-value-bind (entry interactions) (make-entry content)
	 (cli-interactions interactions)
	 (cli-entry entry)))
     (cli-main))

    (("S" "Search / list entries")
     (let ((ids (list-entries-ids)))
       (if ids
	   (let ((entry (cli-list-entries (list-entries-ids))))
	     (when entry
	       (cli-entry entry)))
	   (format t "Nothing to show")))
     (cli-main))

    (("Q" "Quit")
     (format t "Bye-bye.~%"))))

(defun run ()
  (cli-main))
