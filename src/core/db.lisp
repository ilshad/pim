(in-package #:pkm)

;;
;; Indexes
;;

(defvar *entries* (make-hash-table))
(defvar *shorts* (make-hash-table :test 'equalp))
(defvar *triples* (list))

;;
;; Pathnames
;;

(defparameter *base-directory-namestring* "~/.pkm/var")
(defparameter *db-directory-name* "db")
(defparameter *entries-directory-name* "entries")
(defparameter *triples-file-name* "triples.sexp")
(defparameter *shorts-file-name* "shorts.sexp")

(defun base-directory ()
  (uiop:ensure-directory-pathname
   (uiop:native-namestring *base-directory-namestring*)))

(defun db-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames *db-directory-name* (base-directory))))

(defun entries-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames *entries-directory-name* (db-directory))))

(defun entry-pathname (id)
  (merge-pathnames (prin1-to-string id) (entries-directory)))

(defun triples-pathname ()
  (merge-pathnames *triples-file-name* (db-directory)))

(defun shorts-pathname ()
  (merge-pathnames *shorts-file-name* (db-directory)))

;;
;; Entries
;;

(defun save-entry (id content)
  (with-open-file (out (entry-pathname id) :direction :output :if-exists :supersede)
    (write-string content out)))

(defun load-entry (id)
  (uiop:read-file-string (entry-pathname id)))

(defun load-entries-ids ()
  (loop for pathname in (uiop:directory-files (entries-directory))
	for id = (parse-integer (file-namestring pathname) :junk-allowed t)
	when id collect id))

(defun init-entries ()
  (dolist (id (load-entries-ids))
    (setf (gethash id *entries*) (make-instance 'entry :id id :load? t))))

(defun delete-entry-file (id)
  (uiop:delete-file-if-exists (entry-pathname id)))

;;
;; Shorts
;;

(defun shorts-alist ()
  (loop for content being the hash-keys in *shorts* using (hash-value id)
	collect (cons content id)))

(defun save-shorts ()
  (with-open-file (out (shorts-pathname) :direction :output :if-exists :supersede)
    (prin1 (shorts-alist) out)))

(defun load-shorts ()
  (with-open-file (in (shorts-pathname) :if-does-not-exist nil)
    (when in
      (dolist (cons (read in))
	(setf (gethash (car cons) *shorts*) (cdr cons))))))

;;
;; Triples
;;

(defun save-triples ()
  (with-open-file (out (triples-pathname) :direction :output :if-exists :supersede)
    (prin1 (remove nil *triples*) out)))

(defun load-triples ()
  (with-open-file (in (triples-pathname) :if-does-not-exist nil)
    (when in (setf *triples* (read in)))))

;;
;; Start DB
;;

(defun init-db ()
  (ensure-directories-exist (entries-directory))
  (init-entries)
  (load-shorts)
  (load-triples))
