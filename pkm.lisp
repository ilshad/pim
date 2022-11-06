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
  (print-unreadable-object (object stream :type t)
    (with-slots (id content) object
      (multiple-value-bind (string cut?) (string-cut content 20)
	(format stream
		"~d: \"~a~:[\"~;...\" (~d chars)~]"
		id string cut? (length content))))))

(defun string-cut (string length)
  (let ((length (min length (or (position #\Newline string) (1+ length)))))
    (if (< length (length string))
	(values (subseq string 0 length) t)
	string)))

(defun prepare-content (string)
  (string-trim '(#\Space #\Newline) string))

(defun make-entry (content)
  (let ((entry (make-instance 'entry :content content)))
    (format t "(i) New entry ~s~%" entry)
    (setf (gethash (id entry) *entries*) entry)
    (update-short entry)
    (run-handlers entry)
    entry))

(defun ensure-entry (string &key triples)
  (let* ((content (prepare-content string))
	 (entry (or (find-short content) (make-entry content))))
    (dolist (cons triples)
      (ensure-triple (list (id entry)
			   (car cons)
			   (id (ensure-entry (cdr cons))))))
    entry))

(defun edit-entry (entry)
  (with-slots (id content) entry
    (let ((triples-before (search-triples id nil nil))
	  (short-before (and (short? content) (copy-seq content))))
      (setf content (edit-string-in-program content))
      (update-short entry short-before)
      (run-handlers entry triples-before))))

(defun get-entry-by-id (id)
  (gethash id *entries*))

;;
;; Shorts
;;

(defvar *shorts* (make-hash-table :test 'equalp))

(defun short? (string)
  (not (find #\Newline string)))

(defun find-short (content)
  (when (short? content)
    (let ((id (gethash content *shorts*)))
      (when id
	(let ((entry (get-entry-by-id id)))
	  (format t "(i) Found short: ~s~%" entry)
	  entry)))))

(defun update-short (entry &optional short-content-before)
  (with-slots (id content) entry
    (let ((short? (short? content)))
      (cond
	((and short? (not short-content-before))
	 (setf (gethash content *shorts*) id)
	 (format t "(i) New short: ~s~%" entry))

	((and (not short?) short-content-before)
	 (when (remhash short-content-before *shorts*)
	   (format t "(i) Remove short: ~s~%" entry)))))))

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
(defun tag (triple) (cadddr triple))

(defun search-triples (s p o &optional (triples *triples*))
  (remove-if-not
   (cond
     ((and s (not p) (not o)) #'(lambda (x) (eql s (subj x))))
     ((and (not s) p (not o)) #'(lambda (x) (equalp p (pred x))))
     ((and (not s) (not p) o) #'(lambda (x) (eql o (obj x))))
     ((and (not s) p o) #'(lambda (x) (and (equalp p (pred x)) (eql o (obj x)))))
     ((and s (not p) o) #'(lambda (x) (and (eql s (subj x)) (eql o (obj x)))))
     ((and s p (not o)) #'(lambda (x) (and (eql s (subj x)) (equalp p (pred x))))))
   triples))

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
  "Create a function that runs when an entry has been created or updated.

   For example, extract things from the content and create default triples.

   A handler can rely on the results of other handlers. For example,
   if one handler has decided that the entry is a URL string, then
   another handler, which is dedicated to extracting URLs from the content,
   must not even try to do its job.

   In order to accomplish that, handlers declare their dependencies.

   Handlers can do many things. In particular:

   - define new triples, so they will be created if not exist already;
   - change or delete triples, created by other handlers, before the
     actual triples were created;
   - add response components to UI;
   - perform side effects on its own.

   Macro Parameters:

   - handler name (a symbol);
   - dependencies - list of handler names (symbols);
   - lambda list of function parameters:
     - the entry,
     - list of triples that had already been defined by other handlers.

   The triples in the second argument have 4th element - tag (in addition
   to subject, predicate, object). This tag is a symbol referencing
   the handler, where the triple was defined. This allows you to find
   the triples, defined by particular handlers (there is a helper function
   'triples-by-handler'). The tag is temporary, it exists only in
   'run-handlers' loop, and it's dismissed when the triple is actually
   created.

   Return plist with following optional keys:

   - :triple  - define single triple
   - :triples - define a list of triples
   - :replace - entirely replace the triples that have already been defined
                by other handlers.
   - :respond - response to UI.
  "
  `(progn
     (add-handler (quote ,name) (quote ,deps))
     (defun ,name ,args ,@body)))

(defun run-handlers (entry &optional triples-before)
  (let ((triples-after))
    (dolist (symbol *sorted-handlers*)
      (flet ((add-tag (triple) (append triple (list symbol))))
	(let ((handler-result (funcall (symbol-function symbol) entry triples-after)))
	  (destructuring-bind (&key triple triples replace) handler-result
	    (when replace
	      (setf triples-after replace))
	    (when triple
	      (setf triples-after (cons (add-tag triple) triples-after)))
	    (when triples
	      (setf triples-after (append (mapcar #'add-tag triples) triples-after)))))))
    (let ((triples-after (mapcar #'butlast triples-after)))
      (dolist (triple (set-difference triples-before triples-after :test #'equalp))
	(del-triple triple))
      (dolist (triple triples-after)
	(ensure-triple triple)))))

(defun triples-by-handler (symbol triples)
  (loop for x in triples when (eq symbol (tag x)) collect x))

;;
;; Default handlers
;;

(defun url? (string)
  (not (zerop (ppcre:count-matches "^https?:\\/\\/\\S+$" string))))

(define-handler type-url () (entry triples)
  (declare (ignore triples))
  "If the content is a URL string, then create triple:
   - this entry,
   - predicate 'type',
   - entry with content 'URL' (create if it doesn't exist yet)."
  (when (url? (content entry))
    (list :triple
	  (list (id entry)
		"type"
		(id (ensure-entry "URL"))))))

(defun find-urls (string)
  (ppcre:all-matches-as-strings "(https?:\\/\\/\\S+\\w)+" string))

(define-handler extract-urls (type-url) (entry triples)
  "1. Extract all URLs from the content.
   2. Create or find entries for each URL.
   3. Create triples:
      - this entry
      - predicate 'url',
      - the entry of the extracted URL."
  (when (not (triples-by-handler 'type-url triples))
    (list :triples
	  (loop for url in (find-urls (content entry))
		collect (list (id entry)
			      "url"
			      (id (ensure-entry url)))))))

;;
;; Edit content with command-line interface
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
;; Command-line interface
;;

(defun hr () (format t "~v{~c~:*~}~%" 50 '(#\-)))
(defun exit () (format t "Bye-bye.~%"))

(defun prompt (&optional message)
  (when message (format t message))
  (format t "> ")
  (force-output))

(defun menu (alist)
  (hr)
  (dolist (item alist) (format t "[ ~a ] ~a~%" (car item) (cdr item)))
  (prompt)
  (let ((input (string-trim '(#\Space) (read-line))))
    (or (car (assoc input alist :test #'string-equal))
	(progn (format t "Unexpected option: ~a~%" input)
	       (menu alist)))))

(defmacro case-menu (&body cases)
  `(case (menu ',(loop for x in cases collect (cons (caar x) (cadar x))))
     ,@(loop for x in cases collect `(,(caar x) ,@(cdr x)))))

(defun entry-menu (entry)
  (case-menu
    (("E" "Edit entry") (edit-entry entry) (entry-menu entry))
    (("M" "Main menu") (main-menu))
    (("Q" "Quit") (exit))))

(defun main-menu ()
  (case-menu

    (("I" "Create entry here")
     (prompt "NEW ENTRY:~%")
     (entry-menu (ensure-entry (read-multiline))))

    (("E" "Create entry in editor")
     (entry-menu (ensure-entry (edit-string-in-program))))

    (("Q" "Quit") (exit))))

(defun run () (main-menu))
