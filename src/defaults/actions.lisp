(in-package #:pkm-defaults)

;;
;; Main view
;;

(defun create-entry-interaction (input state)
  (when (not (zerop (length input)))
    (multiple-value-bind (entry interactions) (make-entry input)
      (acons :id (id entry) (acons :interactions interactions state)))))

(defun entry-route (state)
  (let ((id (cdr (assoc :id state))))
    (if id
	(list :entry id)
	:main)))

(define-action create-entry-with-string (:main 10) (context)
  (declare (ignore context))
  (list :label "Create entry here"
	:command "I"
	:route #'entry-route
	:interactions (list (list :type :input
				  :input :string
				  :newlines-submit 2
				  :message "New entry:~%"
				  :function #'create-entry-interaction
				  :interactions :interactions))))

(define-action create-entry-with-editor (:main 20) (context)
  (declare (ignore context))
  (list :label "Create entry in editor"
	:command "E"
	:route #'entry-route
	:interactions (list (list :type :input
				  :input :editor
				  :function #'create-entry-interaction
				  :interactions :interactions))))

(defun entry-title-by-id (id)
  (entry-title (get-entry id)))

(defun all-entries-not-short ()
  (loop for id being the hash-keys in *entries* using (hash-value entry)
	when (not (short? entry))
	collect id))

(defun search-shorts (input)
  (loop for content being the hash-keys in *shorts* using (hash-value id)
	when (search input content :test #'string-equal)
	collect id))

(defun search-entries-by-shorts (input)
  (let (ids)
    (dolist (id (search-shorts input))
      (dolist (triple (search-triples nil nil nil id))
	(let ((entry (get-entry (complement-id id triple))))
	  (when (not (short? entry))
	    (pushnew (id entry) ids)))))
    ids))

(defun search-entries (input state)
  (if (zerop (length input))
      (acons :ids (all-entries-not-short) state)
      (let ((ids (search-entries-by-shorts input)))
	(case (length ids)
	  (0 (acons :not-found? t state))
	  (1 (acons :id (first ids) state))
	  (t (acons :ids ids state))))))

(define-action search (:main 30) (context)
  (declare (ignore context))
  (list :label "Search"
	:command "S"
	:route #'entry-route
	:interactions (list (list :type :input
				  :input :string
				  :message "Search entry:"
				  :function #'search-entries)

			    (list :when :not-found?
				  :type :message
				  :message "~%NO MATCH FOUND~%")

			    (list :when :ids
				  :type :input
				  :input :select
				  :options :ids
				  :key :id
				  :render #'entry-title-by-id))))

(defun all-predicates ()
  (remove-duplicates (mapcar #'pred (remove nil *triples*)) :test #'string-equal))

(defun predicates-search (input state)
  (let ((all (all-predicates)))
    (if (zerop (length input))
	(acons :predicates all state)
	(let ((found (remove-if-not #'(lambda (pred) (search input pred)) all)))
          (case (length found)
	    (0 (acons :not-found? t state))
	    (1 (append (list (cons :predicate (first found))
			     (cons :single-match? t))
		       state))
	    (t (acons :predicates found state)))))))

(defun predicate-search-objects (state)
  (let ((pred (cdr (assoc :predicate state))))
    (acons :ids
	   (remove-duplicates (mapcar #'obj (search-triples nil pred)))
	   state)))

(define-action predicates (:main 40) (context)
  (declare (ignore context))
  (list :label "Predicates"
	:command "P"
	:route #'entry-route
	:interactions (list (list :type :input
				  :input :string
				  :message "Search predicate:"
				  :function #'predicates-search)

			    (list :when :not-found?
				  :type :message
				  :message "~%NO MATCH FOUND~%")
			    
			    (list :when :predicates
				  :type :input
				  :input :select
				  :options :predicates
				  :key :predicate
				  :render #'identity)
			    
			    (list :type :break
				  :when-not :predicate)

			    (list :type :message
				  :message
				  #'(lambda (state)
				      (format nil "~%~:[Selected predicate~;Found single match~]: ~a~%~%"
					      (cdr (assoc :single-match? state))
					      (cdr (assoc :predicate state)))))

			    (list :type :function
				  :function #'predicate-search-objects)

			    (list :type :input
				  :input :select
				  :options :ids
				  :key :id
				  :render #'entry-title-by-id))))

(define-action quit (:main 50) (context)
  (declare (ignore context))
  '(:label "Quit"
    :command "Q"
    :route :exit
    :interactions ((:type :message :message "Bye-bye.~%"))))

;;
;; Entry view
;;

(defun trim-if-one-liner (string)
  (let ((first-newline (position #\Newline string)))
    (if first-newline
	(let ((rest-string (subseq string first-newline)))
	  (if (zerop (length (string-trim '(#\Newline #\Space) rest-string)))
	      (string-right-trim '(#\Newline #\Space) string)
	      string))
	string)))

(define-action edit (:entry 10) (context)
  (list :label "Edit"
	:description "Edit entry"
	:command "E"
	:interactions
	(list (list :type :input
		    :input :editor
		    :content #'(lambda (state)
				 (declare (ignore state))
				 (content (getf context :entry)))
		    :function #'(lambda (input state)
				  (let* ((entry (getf context :entry))
					 (content (trim-if-one-liner input))
					 (interactions (edit-entry entry content)))
				    (acons :interactions interactions state)))
		    :interactions :interactions))))

(define-action delete (:entry 20) (context)
  (list :label "Delete"
	:description "Delete entry"
	:command "D"
	:route #'(lambda (state)
		   (when (cdr (assoc :delete? state))
		     :main))
	:interactions
	(list (list :type :input
		    :input :boolean
		    :message "Delete entry?"
		    :function #'(lambda (delete? state)
				  (when delete?
				    (let* ((entry (getf context :entry))
					   (interactions (del-entry entry)))
				      (acons :delete? t
					     (acons :interactions interactions
						    state)))))
		    :interactions :interactions))))

(defun parse-entry-id (string)
  (let ((found (ppcre:all-matches-as-strings "^#\\d+$" string)))
    (when found (parse-integer (subseq (first found) 1)))))

(defun string-not-empty-validation (input state)
  (declare (ignore state))
  (not (zerop (length input))))

(define-action add-triple (:entry 30) (context)
  (list :label "Add triple"
	:description "Add triple for this subject"
	:command "+"
	:function #'(lambda (state)
		      (ensure-triple
		       (list (id (getf context :entry))
			     (cdr (assoc :predicate state))
			     (let ((string (cdr (assoc :object state))))
			       (or (parse-entry-id string)
				   (id (ensure-short string)))))))
	:interactions (list (list :type :input
				  :input :string
				  :message "Predicate:"
				  :key :predicate
				  :validate #'string-not-empty-validation)
			    (list :type :input
				  :input :string
				  :message "Object:"
				  :key :object
				  :validate #'string-not-empty-validation))))

(define-action del-triple (:entry 40) (context)
  (when (getf context :indexed-triples)
    (flet ((selected-triple (state)
	     (let ((triples (getf context :indexed-triples))
		   (index (cdr (assoc :index state))))
	       (cdr (assoc index triples :test #'eql)))))
      (list :label "Delete triple"
	    :command "-"
	    :function #'(lambda (state)
			  (when (cdr (assoc :delete? state))
			    (let ((triple (selected-triple state)))
			      (when triple
				(del-triple triple)
				(del-orphan triple)))))
	    :interactions
	    (list (list :type :input
			:input :integer
			:message "Select triple to delete:~%"
			:key :index
			:validate #'(lambda (input state)
				      (declare (ignore state))
				      (assoc input
					     (getf context :indexed-triples)
					     :test #'eql)))
		  (list :type :message
			:message
			#'(lambda (state)
			    (format nil
				    "Selected: ~s~%"
				    (format-triple nil
						   (selected-triple state)
						   (getf context :entry)))))
		  (list :type :input
			:input :boolean
			:message "Delete this triple?"
			:key :delete?))))))

(define-action close (:entry 50) (context)
  (declare (ignore context))
  '(:label "Quit"
    :command "Q"
    :route :main))
