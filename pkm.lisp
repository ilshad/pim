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
	 (format t "(i) Added to shorts: ~s~%" entry))

	((and (not short?) short-content-before)
	 (when (remhash short-content-before *shorts*)
	   (format t "(i) Removed from shorts: ~s~%" entry)))))))

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
  "Create handler function that runs when an entry has been created or updated.

   For example, extract things from the content and create default triples,
   or define additional interaction with user.

   A handler can rely on the results of other handlers. For that, handlers
   declare dependencies between each other, so they will run sorted according
   to these dependencies.

   The function, created by this macro, takes:

   - alist (association list), which accumulates the results of all handlers,
   - the entry.

   It returns results alist, possibly updated.

   In other words, the results alist is passing through all the handlers.
   Each handler can update the results alist, adding its own result and
   even modifying or dismissing the results of other handlers.

   The result is a plist (property list):

   - :triple   - single triple
   - :triples  - list of triples
   - :response - response to UI.

   Assoc it into results alist using handler's name as a key:

   (acons 'my-handler
          (list :triple my-triple
                :response my-response)
          results)

   Macro parameters:

   - handler name (a symbol);
   - dependencies - list of handler names (symbols);
   - lambda list of function parameters:
     - results alist,
     - the entry.

   Return: results alist."
  `(progn
     (add-handler (quote ,name) (quote ,deps))
     (defun ,name ,args ,@body)))

(defun reduce-over-handlers (entry)
  (reduce #'(lambda (results symbol)
	      (funcall (symbol-function symbol)
		       results
		       entry))
	  *sorted-handlers*
	  :initial-value nil))

(defun run-handlers (entry &optional triples-before)
  (let ((triples-after))
    (dolist (result (reduce-over-handlers entry))
      (format t "(i) ~s for #~s: ~s~%" (car result) (id entry) (cdr result))
      (destructuring-bind (&key triple triples) (cdr result)
	(when triple
	  (setf triples-after (append triples-after (list triple))))
	(when triples
	  (setf triples-after (append triples-after triples)))))
    (dolist (triple (set-difference triples-before triples-after :test #'equalp))
      (del-triple triple))
    (dolist (triple triples-after)
      (ensure-triple triple))))

(defun find-triples-by-handler (symbol results)
  (or (getf (cdr (assoc symbol results)) :triple)
      (getf (cdr (assoc symbol results)) :triples)))

;;
;; Default handlers
;;

(defun url? (string)
  (not (zerop (ppcre:count-matches "^https?:\\/\\/\\S+$" string))))

(define-handler type-url () (results entry)
  "If the content is a URL string, then create triple:
   - this entry,
   - predicate 'type',
   - entry with content 'URL' (create if it doesn't exist yet)."
  (if (url? (content entry))
      (let ((triple (list (id entry) "type" (id (ensure-entry "URL")))))
	(acons 'type-url (list :triple triple) results))
      results))

(defun find-urls (string)
  (ppcre:all-matches-as-strings "(https?:\\/\\/\\S+\\w)+" string))

(define-handler extract-urls (type-url) (results entry)
  "1. Extract all URLs from the content.
   2. Create or find entries for each URL.
   3. Create triples:
      - this entry
      - predicate 'url',
      - the entry of the extracted URL."
  (if (null (find-triples-by-handler 'type-url results))
      (let ((triples (loop for url in (find-urls (content entry))
			   collect (list (id entry)
					 "url"
					 (id (ensure-entry url))))))
	(if triples
	    (acons 'extract-urls (list :triples triples) results)
	    results))
      results))

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
