(in-package #:pkm-defaults)

;;
;; Main view
;;

(defun create-entry-interaction (input state)
  (multiple-value-bind (entry interactions) (make-entry input)
    (acons :id (id entry) (acons :interactions interactions state))))

(defun entry-route (state)
  (list :entry (cdr (assoc :id state))))

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

(defparameter *search-page-size* 10)

(defun entries-ids ()
  (loop for id being the hash-keys in *entries* using (hash-value entry)
	when (not (short? entry))
	collect id))

(defun search-entries (input state)
  (declare (ignore input))
  (acons :ids (entries-ids) state))

(defun entry-title (entry)
  (let ((property (first (get-property-triples entry "title"))))
    (if property
	(content (get-entry (obj property)))
	(string-cut (content entry) 80))))

(defun search-set-result-page (state)
  (let ((ids (cdr (assoc :ids state))))
    (if ids
      (let* ((page? (> (length ids) *search-page-size*))
	     (page (if page? (subseq ids 0 *search-page-size*) ids)))
	(append
	 (list (cons :listing
		     (append
		      (loop for id in page
			    counting 1 into index
			    collect (list :label (entry-title (get-entry id))
					  :index index
					  :id id))
		      (if page?
			  '((:label "Cancel" :index "C")
			    (:label "...more" :index " "))
			  '((:label "Done" :index " ")))))
	       (cons :ids (when page? (subseq ids *search-page-size*))))
	 state))
      state)))

(defun search-select-result-item (input state)
  (let ((index (parse-integer input :junk-allowed t)))
    (if index
	(let ((item (find index
			  (cdr (assoc :listing state))
			  :key #'(lambda (item) (getf item :index)))))
	  (append (list (cons :id (getf item :id))
			(cons :ids nil))
		  state))
	(if (string-equal (string-trim '(#\Space) input) "C")
	    (acons :ids nil state)
	    state))))

(defun search-next-page? (state)
  (cdr (assoc :ids state)))

(define-action search (:main 30) (context)
  (declare (ignore context))
  (list :label "Search"
	:command "S"
	:route #'entry-route
	:interactions (list (list :type :input
				  :input :string
				  :message "Search: "
				  :function #'search-entries)
			    (list :name :page
				  :type :function
				  :function #'search-set-result-page)
			    (list :type :listing
				  :listing :listing)
			    (list :type :input
				  :input :string
				  :function #'search-select-result-item)
			    (list :type :goto
				  :goto :page
				  :when #'search-next-page?))))

(define-action quit (:main 40) (context)
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
	:interactions (list
		       (list :type :input
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
	:interactions (list
		       (list :type :input
			     :input :boolean
			     :message "Delete entry?"
			     :function #'(lambda (delete? state)
					   (when delete?
					     (let* ((entry (getf context :entry))
						    (interactions (del-entry entry)))
					       (acons :delete? t
						      (acons :interactions interactions
							     state)))))
			     :interactions :interactions))
	:route #'(lambda (state)
		   (when (cdr (assoc :delete? state))
		     :main))))

(defun parse-entry-id (string)
  (let ((found (ppcre:all-matches-as-strings "^#\\d+$" string)))
    (when found (parse-integer (subseq (first found) 1)))))

(define-action add-triple (:entry 30) (context)
  (list :label "Add triple"
	:description "Add triple for this subject"
	:command "+"
	:interactions '((:type :input
			 :input :string
			 :message "Predicate:"
			 :key :predicate)
			(:type :input
			 :input :string
			 :message "Object:"
			 :key :object))
	:function #'(lambda (state)
		      (ensure-triple
		       (list (id (getf context :entry))
			     (cdr (assoc :predicate state))
			     (let ((string (cdr (assoc :object state))))
			       (or (parse-entry-id string)
				   (id (ensure-short string)))))))))

(define-action del-triple (:entry 40) (context)
  (when (getf context :indexed-triples)
    (flet ((selected-triple (state)
	     (let ((triples (getf context :indexed-triples))
		   (index (cdr (assoc :index state))))
	       (cdr (assoc index triples :test #'eql)))))
      (list :label "Delete triple"
	    :command "-"
	    :interactions (list
			   (list :type :input
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
				 :key :delete?))
	    :function #'(lambda (state)
			  (when (cdr (assoc :delete? state))
			    (let ((triple (selected-triple state)))
			      (when triple
				(del-triple triple)
				(del-orphan triple)))))))))

(define-action close (:entry 50) (context)
  (declare (ignore context))
  '(:label "Quit"
    :command "Q"
    :route :main))
