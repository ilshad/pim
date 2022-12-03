(in-package #:pkm)

(defparameter *handlers-tmp* nil)
(defparameter *handlers-sorted* nil)
(defparameter *handlers-ops* nil)

(defun sort-handlers (items &optional provided-item)
  (let ((item (or provided-item (first items))))
    (dolist (dep-name (cdr item))
      (let ((dep-item (assoc dep-name items)))
	(when (and dep-item (not (member dep-item *handlers-sorted*)))
	  (sort-handlers items dep-item))))
    (pushnew (car item) *handlers-sorted*)
    (when (and (null provided-item) (rest items))
      (sort-handlers (rest items)))))

(defun add-handler (name ops deps)
  (setf *handlers-sorted* nil)
  (sort-handlers (push (cons name deps) *handlers-tmp*))
  (setf *handlers-sorted* (reverse *handlers-sorted*))
  (setf (getf *handlers-ops* name) ops))

(defmacro define-handler (name ops deps args &body body)
  "Create a Handler function that runs when the entry has been created,
   updated or deleted.

   For example, extract things from the content and create default triples
   and define additional interactions with user.

   Handler can rely on side effects of other handlers. For that, handlers
   declare dependencies between each other, so they will run sorted according
   to these dependencies.

   Macro parameters:

   - handler name (symbol).
   - ops - list of ops (keywords) the handler must run for. Possible ops are:
       - :add
       - :edit
       - :delete
   - dependencies - list of other handler names (symbols).
   - function arguments:
       - the entry it runs for,
       - context plist, which contains:
           - :content-before - previous entry content, if there was one.
           - :short? - the entry is in special category 'short'.

   Return NIL or result plist with:
   - :interactions - list of Interactions to run next to the handler.

   See examples of handlers in 'defaults/handlers.lisp'.

   For more details about Interactions see corresponding section in
   'define-action' macro documentaiton - they are exactly the same."
  `(progn
     (add-handler (quote ,name) (quote ,ops) (quote ,deps))
     (defun ,name ,args ,@body)))

(defun run-handlers (entry op &optional context)
  (let ((interactions))
    (dolist (symbol *handlers-sorted*)
      (when (member op (getf *handlers-ops* symbol))
	(let ((result (funcall (symbol-function symbol) entry context)))
	  (dolist (interaction (getf result :interactions))
	    (push interaction interactions)))))
    interactions))
