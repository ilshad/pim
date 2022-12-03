(in-package #:pkm)

(defparameter *actions* nil)

(defun add-action (name view index)
  (setf (getf *actions* view)
	(sort (acons name index (copy-seq (getf *actions* view)))
	      #'<
	      :key #'cdr)))

(defmacro define-action (name (view index) args &body body)
  "The way to abstract user actions, such as buttons, links, menu items,
   from concrete UI implementations. Once defined, action appears and works
   in each frontend.

   Macro params include a list containing a keyword (the name of the view
   where the action must appear) and integer (index sorting actions).

   The function, created by this macro, takes context plist from view.
   Each view defines its own specific context. For example, entry view
   passes plist with the entry and data structures for navigation on
   triples.

   Return NIL (which means to skip this action) or a plist with the
   following keys:

   - :label - string.
   - :description - string.
   - :command - for use in CLI frontend as a menu option, or as a part of
                a keyboard shortcut in other frontends.
   - :interactions - list of plists, see the corresponding section below.
   - :function - main function of the action.
   - :route - view spec or function that takes state and returns view spec,
              defining the view, that must be open after this action.

   If interactions are defined, the main function and route function take the
   state built from these interactions. Otherwise, the main function and route
   function take no arguments.

   Main function is for side effects only. Its return value is always ignored.

   Route can be specified as:

   - a view name keyword;
   - a list containing view name keyword and some identifier;
   - a function that returns any of the variants shown above.

   If :route is not defined (or the :route function returns NIL), UI stays in
   the same view, where action has been called.

   Overall, functions defined in action run in following order:

   - interactions,
   - main function,
   - route.

   See examples of actions in 'defaults/actions.lisp'.


   Interactions
   ************

   Interactions is a way to abstract interactive dialogs, step-by-step
   inputs and wizards, including conditional branching, from concrete
   UI implementations. Once defined, interaction works in each frontend.

   Interactions can be defined here, in actions, and in the handlers
   (see macro 'define-handler'), as a sequence of plists.

   Each interaction is a plist with following keys:

   - :type - keyword, input type
   - :message - string or function that takes state and returns string
   - :key - keyword, to acons the input into the state
   - :function - function that takes input and state, returns state
   - :validate - predicate function that takes input and state
   - :when - keyword or predicate function that takes state
   - :content - function, only with :editor input type
   - :newlines-submit - integer, only with :string input type
   - :interactions - keyword pointing to nested interactions in the state

   As a result, interaction shows :message and asks for input depending
   on :type keyword:

   - :boolean - UI asks for a boolean value (e.g. yes / no),
   - :integer - UI asks for integer number,
   - :string - UI asks for string input,
   - :editor - UI opens text editor.

   :content function takes state and return string, that becomes initial
   content in text editor.

   :newlines-submit is the number of subsequent newlines, after which
   input type :string submits the input (these newlines will be trimmed).
   Default is 0, which means single-line input. 

   If :validate predicate is defined and it returns NIL, UI asks for the
   input again.

   :function takes the user input and state, performs side effects and
   returns the state (possibly updated). The state is a alist (association
   list), where interactions can put their results and query results of
   other interactions.

   The function, in turn, can dynamically define new (i.e. nested)
   interactions. To perform them, it must put them into the state
   under some key. Then property :interactions must refer to that key. 

   :key simply puts the input into the state. This option can be used
   instead of :function if the only goal of the interaction is to provide
   input that can be used in subsequent interactions.

   :when - keyword or function. The function takes the state and returns
   boolean. If its result is NIL, UI skips this interaction. Keyword
   variant simply checks state for that key.

   If :type is not presented, interaction only shows the message."
  (let ((symbol (read-from-string
		 (concatenate 'string
			      (symbol-name name) "-"
			      (symbol-name view) "-action"))))
    `(progn
       (add-action (quote ,symbol) ,view ,index)
       (defun ,symbol ,args ,@body))))

(defun view-actions (view-name context)
  (remove nil (mapcar #'(lambda (cons)
			  (funcall (car cons) context))
		      (getf *actions* view-name))))
