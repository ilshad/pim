(in-package #:pkm-core)

(defun get-triple (triple &optional (triples *triples*))
  (if (equalp triple (car triples))
      (car triples)
      (when (cdr triples) (get-triple triple (cdr triples)))))

(defun set-triple* (old new triples)
  (if (equalp old (car triples))
      (progn (setf (car triples) new) t)
      (when (cdr triples)
	(set-triple* old new (cdr triples)))))

(defun set-triple (old new)
  (set-triple* old new *triples*)
  (save-triples))

(defun del-triple (triple)
  (set-triple triple nil))

(defun add-triple (triple)
  (push triple *triples*)
  (save-triples)
  triple)

(defun ensure-triple (triple)
  (or (get-triple triple) (add-triple triple)))

(defun format-triple (stream triple entry)
  (multiple-value-bind (other-entry position) (complement-entry entry triple)
    (multiple-value-bind (content cut?) (string-cut (content other-entry) 80)
      (case position
	(:subj
	 (format stream "-> ~a -> ~a~:[~;...~]" (pred triple) content cut?))
	(:obj
	 (format stream "<- ~a <- ~a~:[~;...~]" (pred triple) content cut?))))))

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

(defun set-property-triple (entry key value)
  (let ((foundp))
    (dolist (triple (get-property-triples entry key))
      (if (string= value (content (get-entry (obj triple))))
	  (setf foundp t)
	  (del-triple (list (id entry) key (obj triple)))))
    (when (not foundp)
      (add-property-triple entry key value))))

(defun orphan? (entry)
  (and (short? entry) (null (search-triples nil nil (id entry)))))

(defun del-orphan (triple)
  (let ((obj (get-entry (obj triple))))
    (when (orphan? obj)
      (del-entry obj))))
