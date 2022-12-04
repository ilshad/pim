(in-package #:pkm)

;;
;; Main view
;;

(defun create-entry-interaction (input state)
  (multiple-value-bind (entry interactions) (make-entry input)
    (acons :id (id entry) (acons :interactions interactions state))))

(defun create-entry-route (state)
  (list :entry (cdr (assoc :id state))))

(define-action create-entry-with-string (:main 10) (context)
  (declare (ignore context))
  (list :label "Create entry here"
	:command "I"
	:route #'create-entry-route
	:interactions (list (list :type :string
				  :newlines-submit 2
				  :message "New entry:~%"
				  :function #'create-entry-interaction
				  :interactions :interactions))))

(define-action create-entry-with-editor (:main 20) (context)
  (declare (ignore context))
  (list :label "Create entry in editor"
	:command "E"
	:route #'create-entry-route
	:interactions (list (list :type :editor
				  :function #'create-entry-interaction
				  :interactions :interactions))))

(define-action search (:main 30) (context)
  (declare (ignore context))
  (list :label "Search / list entries"
	:command "S"
	:route :search))

(define-action quit (:main 40) (context)
  (declare (ignore context))
  (list :label "Quit"
	:command "Q"
	:route :exit
	:interactions '((:message "Bye-bye.~%"))))

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
		       (list :type :editor
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
	:route :main
	:interactions (list
		       (list :type :boolean
			     :message "Delete entry?"
			     :function #'(lambda (delete? state)
					   (when delete?
					     (let* ((entry (getf context :entry))
						    (interactions (del-entry entry)))
					       (acons :interactions interactions state))))
			     :interactions :interactions))))

(defun parse-entry-id (string)
  (let ((found (ppcre:all-matches-as-strings "^#\\d+$" string)))
    (when found (parse-integer (subseq (first found) 1)))))

(define-action add-triple (:entry 30) (context)
  (list :label "Add triple"
	:description "Add triple for this subject"
	:command "+"
	:interactions '((:type :string :message "Predicate:" :key :predicate)
			(:type :string :message "Object:" :key :object))
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
			   (list :type :integer
				 :message "Select triple to delete:~%"
				 :key :index
				 :validate #'(lambda (input state)
					       (declare (ignore state))
					       (assoc input
						      (getf context :indexed-triples)
						      :test #'eql)))
			   (list :message
				 #'(lambda (state)
				     (format nil
					     "Selected: ~s~%"
					     (format-triple nil
							    (selected-triple state)
							    (getf context :entry)))))
			   (list :type :boolean
				 :message "Delete this triple?"
				 :key :delete?))
	    :function #'(lambda (state)
			  (when (cdr (assoc :delete? state))
			    (let ((triple (selected-triple state)))
			      (when triple (del-triple triple)))))))))

(define-action close (:entry 50) (context)
  (declare (ignore context))
  '(:label "Quit" :command "Q" :route :main))
