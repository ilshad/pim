(in-package #:pkm-defaults)

(defparameter *default-listing-page-size* 10)

(defun listing-page (label-function
		     &key
		       ((:sources sources-key) :sources)
		       ((:listing listing-key) :listing)
		       (size *default-listing-page-size*))
  #'(lambda (state)
      (let ((sources (cdr (assoc sources-key state))))
	(if sources
	    (let* ((page? (> (length sources) size))
		   (page (if page? (subseq sources 0 size) sources)))
	      (append (list (cons listing-key
				  (append
				   (loop for source in page
					 counting 1 into index
					 collect
					 (list :index index
					       :source source
					       :label (funcall label-function
							       source)))
				   (if page?
				       '((:label "Cancel" :index "C")
					 (:label "...more" :index " "))
				       '((:label "Done" :index " ")))))
			    (cons sources-key
				  (when page? (subseq sources size))))
		      state))
	    state))))

(defun listing-select (&key
			 ((:sources sources-key) :sources)
			 ((:listing listing-key) :listing)
			 ((:result result-key) :result))
  #'(lambda (input state)
      (let ((index (parse-integer input :junk-allowed t)))
	(if index
	    (let ((item (find index
			      (cdr (assoc listing-key state))
			      :key #'(lambda (item) (getf item :index)))))
	      (append (list (cons result-key (getf item :source))
			    (cons sources-key nil))
		      state))
	    (if (string-equal (string-trim '(#\Space) input) "C")
		(acons sources-key nil state)
		state)))))
