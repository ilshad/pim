(in-package #:pkm-defaults)

(define-handler update-short (:add :edit) () (entry context)
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
	    (set-short entry)))
      nil)))

(define-handler type-url (:add :edit) () (entry context)
  "If the whole content string is a URL, create property 'type' 'URL'."
  (declare (ignore context))
  (if (url? (content entry))
      (add-property-triple entry "type" "URL")
      (when (get-property-triple entry "type" "URL")
	(del-property-triple entry "type" "URL")))
  nil)

(define-handler has-url (:add :edit) (type-url) (entry context)
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
	(add-triple triple)))
    nil))

(defun extract-title (content)
  (when (> (count #\Newline content) 1)
    (with-input-from-string (stream content)
      (let ((title (read-line stream)))
	(when (and (not (zerop (length title)))
		   (> 80 (length title))
		   (zerop (length (read-line stream))))
	  title)))))

(defun title-triples (entry)
  (loop for triple in (get-property-triples entry "title")
	collect (cons (content (get-entry (obj triple)))
		      triple)))

(define-handler set-title (:add :edit) () (entry context)
  "If first line is separated from the rest content by an empty line,
   then interactively create 'title' property using this line.
   Otherwise prompt the user to set the title explicitly or to skip.
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
    (list :interactions interactions)))

(define-handler delete-entry (:delete) () (entry context)
  "Do some cleanup job before deleting the entry:
   - delete corresponding triples;
   - delete short property objects (i.e. short entries which are objects of
     the triples where this entry is the subject), if after removing the
     triples they become orphans (i.e. they are not objects of any triples
     anymore);
   - clean up shorts index if needed."
  (declare (ignore context))
  (dolist (triple (search-triples nil nil nil (id entry)))
    (del-triple triple)
    (del-orphan triple))
  (when (short? entry)
    (del-short entry))
  nil)
