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
   triples, or define additional interactions with user.

   A handler can rely on side effects of other handlers. For that,
   handlers declare dependencies between each other, so they will
   run sorted according to these dependencies.

   The function, created by this macro, takes the entry it runs for
   and returns nil or list of UI interactions.

   Interaction is a plist with following keys:

   - :type (required) - keyword, one of :boolean, :string.
   - :prompt (required) - string
   - :function (required) - function
   - :key - keyword
   - :condition - function

   As a result, interaction shows :prompt and asks for the input.

   For :type :boolean, user interface asks boolean value (e.g. yes or no).
   If yes, it calls the :function without arguments.

   For :type :string, user interface asks one text line input and calls
   :function with this string.

   If :key is defined, the result of the :function will be put into
   accumulated interactions results plist under this key.

   If :condition function is defined, it is called with accumulated
   interactions results plist. If it returns NIL, then skip this
   interaction.

   All interactions from handlers run sequentially after all the
   handlers. With :key and :condition, interactions can check the
   results of other interactions and run conditionally.
   
   Macro parameters:

   - handler symbol;
   - dependencies - list of handler symbols;
   - function arguments - entry.

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

(defun ensure-short (string)
  (let ((content (string-trim '(#\Space #\Newline) string)))
    (or (get-short content) (make-entry content))))

(defun short? (string)
  (and (null (find #\Newline string))
       (or (null (find-urls string))
	   (url? string))))

(defun get-short (content)
  (when (short? content)
    (let ((id (gethash content *shorts*)))
      (when id
	(let ((entry (get-entry id)))
	  (format t "(i) Found short: ~s~%" entry)
	  entry)))))

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
	(when (short? after)
	  (setf (gethash after *shorts*) id)
	  (format t "(i) Added to shorts: ~s~%" entry))
	(when (short? before)
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
   4. Remove old 'url' properties which are not applied now."
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
   Otherwise prompt to user to set the title explicitly or skip.
   Also interactively take care of old 'title' properties, if such
   triples are exist."
  (let ((title (extract-title (content entry)))
	(before (title-triples entry))
	(interactions))
    (if title
	(when (null (cdr (assoc title before :test #'string=)))
	  (push (list :type :boolean
		      :prompt (format nil "Set 'title' property ~s?" title)
		      :function #'(lambda () (add-property-triple entry "title" title)))
		interactions))
	(when (and (null before)
		   (not (zerop (count #\Newline (content entry))))
		   (not (string= (content entry) (getf context :content-before))))
	  (push (list :type :boolean
		      :prompt "Add title?"
		      :key :add-title?
		      :function (constantly t))
		interactions)
	  (push (list :type :string
		      :prompt (format nil "Enter title:")
		      :function #'(lambda (title) (add-property-triple entry "title" title))
		      :condition #'(lambda (results) (getf results :add-title?)))
		interactions)))
    (dolist (cons before)
      (when (not (string= (car cons) title))
	(push (list :type :boolean
		    :prompt (format nil "Remove 'title' property ~s?" (car cons))
		    :function #'(lambda () (del-triple (cdr cons))))
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

(defmacro case-menu (&body cases)
  `(case (menu ',(loop for x in cases collect (cons (caar x) (cadar x))))
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

(defun cli-interactions (interactions)
  (let ((results))
    (dolist (interaction interactions)
      (terpri)
      (destructuring-bind (&key condition type prompt function key) interaction
       	(when (or (null condition) (funcall condition results))
	  (case type

	    (:boolean
	     (when (funcall #'y-or-n-p prompt)
	       (let ((result (funcall function)))
		 (when key (setf (getf results key) result)))))

	    (:string
	     (prompt (format t "~a~%" prompt))
	     (let ((result (funcall function (read-line))))
	       (when key (setf (getf results key) result))))))))))

(defun cli-entry (entry)
  (terpri)
  (hr)
  (write-string (content entry))
  (hr)
  (format t "ID: ~d~%" (id entry))
  (destructuring-bind (&key indexed-triples menu-items-triples) (menu-items-triples entry)
    (let* ((actions (list (cons "E" "Edit entry")
			  (cons "D" "Delete entry")
			  (cons "+" "Add triple for this subject")
			  (when indexed-triples (cons "-" "Delete triple"))
			  (cons "Q" "Quit")))
	   (input (menu (append menu-items-triples
				(when menu-items-triples '((:separator)))
				(remove nil actions))))
	   (index (read-from-string input)))
      (if (integerp index)
	  (let ((triple (selected-triple index indexed-triples)))
	    (when triple
	      (cli-entry (complement-entry entry triple))))
	  (case input
	    ("E"
	     (cli-interactions (edit-entry entry (edit-string-in-program (content entry))))
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

(defparameter *page-size* 10)

(defun list-entries-menu ()
  (let ((ids (loop for id being the hash-keys in *entries* collect id)))
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
					       (string-cut
						(content (get-entry (cdr item)))
						80)))
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
	  (return)))))

(defun cli-main ()
  (case-menu

    (("I" "Create entry here")
     (prompt "New entry:~%")
     (multiple-value-bind (entry interactions) (make-entry (read-multiline))
       (cli-interactions interactions)
       (cli-entry entry))
     (cli-main))

    (("E" "Create entry in editor")
     (multiple-value-bind (entry interactions) (make-entry (edit-string-in-program))
       (cli-interactions interactions)
       (cli-entry entry))
     (cli-main))

    (("S" "Search / list entries")
     (let ((entry (list-entries-menu)))
       (when entry (cli-entry entry)))
     (cli-main))

    (("Q" "Quit")
     (format t "Bye-bye.~%"))))

(defun run ()
  (cli-main))
