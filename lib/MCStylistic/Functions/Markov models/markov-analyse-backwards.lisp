#| Copyright 2008-2013 Tom Collins
   Monday 4 October 2010
   Completed Monday 4 October 2010

\noindent The functions here are  very similar to
those contained in the file markov-analyse.lisp.
Whereas those functions are designed to analyse
data according to a Markov-chain model that runs
forward in time, these functions do the same for going
backwards in time.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "markov-analyse"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "spacing-states"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(accumulate-to-stm<-
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '((2) (((5) ("Piece A"))))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((2) (((4) ("Piece A")) ((5) ("Piece A"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent This function is similar to the function
accumulate-to-stm, the difference being $X_n$ is
looked up and $X_{n-1}$ accumulated to the state-
transition matrix. The first argument is a listed
state; the second is the relevant row of the state
transition matrix; the third is the state transition
matrix itself. This function is called when the state
of the second item of the listed state has appeared in
the state transition matrix before. The references of
the event are included. |#

(defun accumulate-to-stm<-
       (listed-state relevant-row stm)
  (cons (list (first (second listed-state))
              (cons (first listed-state)
		    (second relevant-row)))
	(remove relevant-row stm :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(add-to-stm<-
 '(((4) ("Piece A")) ((3) ("Piece A")))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((3) (((4) ("Piece A"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent This function is similar to the function
add-to-stm, the difference being $X_n$ is looked up
and $X_{n-1}$ added to the state-transition matrix.
The first argument is a listed state; the second is
the state transition matrix. This function is called
when the state of the first item of the listed state
has not appeared in the state transition matrix
before. It is added. |#

(defun add-to-stm<- (listed-state stm)
  (cons
   (list
    (first (second listed-state))
    (list (first listed-state))) stm))

#|
\noindent Example:
\begin{verbatim}
(setq
 variable-1
 '((0 60 60 2 0) (0 63 62 1 0) (1 67 64 2 0)
   (2 59 59 1 0)))
(setq
 variable-2
 '((0 64 62 1 0) (1 62 61 1 0) (1 69 65 2 0)
   (2 66 63 1 0)))
(setq *variable-names* '(variable-1 variable-2))
(setq *catalogue* '("variable-1" "variable-2"))
(construct-final-states
 *variable-names* *catalogue* "beat-spacing-states"
 10 3 3 1)
--> '(((3 (8))
       (NIL NIL "variable-1"
        ((2 59 59 1 0 3 3) (1 67 64 2 0 3 2))))
      ((3 (3))
       (NIL NIL "variable-2"
        ((2 66 63 1 0 3 3) (1 69 65 2 0 3 2))))).
\end{verbatim}

\noindent This function is similar to the function
construct-stm, the difference being $X_n$ is looked
up and $X_{n-1}$ is accumulated or added to a state-
transition matrix. This recursion analyses one
variable name at a time, taking a catalogue name from
the variable catalogue, and outputs final
states accordingly. If the state function is of the
kind ``beat-rel-MNN-states'', then it is assumed that
the beat-rel-MNN states have been defined already, and
that this is what variable-names refers to. |#

(defun construct-final-states
       (variable-names catalogue state-fn &optional
	(depth-check 10) (duration-index 3)
	(beats-in-bar 4) (sort-index 2))
  (if (null variable-names) ()
    (cons
     (my-last
      (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	 (lastn
	  depth-check
	  (symbol-value (first variable-names)))
	 (first catalogue) duration-index)
        (if (string= state-fn "beat-rel-MNN-states")
          (symbol-value (first variable-names))
          (beat-spacing-states<-
           (lastn
            depth-check
            (symbol-value (first variable-names)))
           (first catalogue) beats-in-bar sort-index
           duration-index))))
     (construct-final-states
      (rest variable-names) (rest catalogue) state-fn
      depth-check duration-index beats-in-bar
      sort-index))))

#|
\noindent Example:
\begin{verbatim}
(setq
 variable-1
 '((0 60 60 2 0) (0 63 62 1 0) (1 67 64 2 0)
   (2 59 59 1 0)))
(setq
 variable-2
 '((0 64 62 1 0) (1 62 61 1 0) (1 69 65 2 0)
   (2 66 63 1 0)))
(setq *variable-names* '(variable-1 variable-2))
(setq *catalogue* '("variable-1" "variable-2"))
(construct-stm<-
 *variable-names* *catalogue* "beat-spacing-states"
 3 3 1)
--> "Finished!".
\end{verbatim}

\noindent This function is similar to the function
construct-stm, the difference being $X_n$ is looked
up and $X_{n-1}$ is accumulated or added to a state-
transition matrix. This recursion analyses one
variable name at a time, taking a catalogue name from
the variable catalogue, and updates the transition
matrix accordingly. The output "Finished!" is
preferable to the transition matrix, which is large
enough that it can cause the Listener to crash. If the state function
is of the kind ``beat-rel-MNN-states'', then it is
assumed that the beat-rel-MNN states have been defined
already, and that this is what variable-names refers
to. |#

(defun construct-stm<-
       (variable-names catalogue state-fn &optional
	(duration-index 3) (beats-in-bar 4)
	(sort-index 2))
  (if (null variable-names) (print "Finished!")
    (progn
      (markov-analyse<-
       (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	  (symbol-value (first variable-names))
	  (first catalogue) duration-index)
         (if (string= state-fn "beat-rel-MNN-states")
           (symbol-value (first variable-names))
           (beat-spacing-states<-
            (symbol-value (first variable-names))
            (first catalogue) beats-in-bar sort-index
            duration-index))))
      (construct-stm<-
       (rest variable-names) (rest catalogue) state-fn
       duration-index beats-in-bar sort-index))))

#|
\noindent Example:
\begin{verbatim}
(markov-analyse<-
 '(((3) ("Piece A")) ((6) ("Piece A"))
   ((4) ("Piece A")) ((4) ("Piece A"))
   ((3) ("Piece A")) ((2) ("Piece A"))))
--> '(((2) (((3) ("Piece A"))))
      ((3) (((4) ("Piece A"))))
      ((4) (((4) ("Piece A")) ((6) ("Piece A"))))
      ((6) (((3) ("Piece A"))))), or "It's done!".
\end{verbatim}

\noindent This function is similar to the function
markov-analyse, the difference being $X_n$ is looked
up and $X_{n-1}$ is accumulated or added to a state-
transition matrix. This function has one argument -
some states which are to be analysed according to a
backwards first-order Markov model. Note the need to
define a variable here, *transition-matrix*. The
output "It's done!" is preferable to the transition
matrix, which is large enough that it can cause the
Listener to crash. |#

(defvar *transition-matrix* ())

(defun markov-analyse<- (states)
  (if (update-stm<- (firstn-list 2 states)) t)
  "It's done!")

#|
\noindent Example:
\begin{verbatim}
(present-to-stm<-
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((2) (((4) ("Piece A")) ((5) ("Piece A"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent This function is similar to the function
present-to-stm, the difference being $X_n$ is looked
up and $X_{n-1}$ is accumulated or added to a state-
transition matrix. The function calls either the
function accumulate-to-stm<-, or add-to-stm<-,
depending on whether the first argument, a listed-
state, has appeared in the second argument, a state-
transition matrix, before. The example above results
in accumulate-to-stm<- being called, as the state of
(2) has occurred before. However, changing this state
to (3) in the argument would result in add-to-stm<-
being called. |#

(defun present-to-stm<- (listed-state stm)
  (let ((relevant-row
	 (assoc
	  (first (second listed-state)) stm
	  :test #'equalp)))
    (if (identity relevant-row) 
      (accumulate-to-stm<-
       listed-state relevant-row stm)
      (add-to-stm<- listed-state stm))))

#|
\noindent Example:
\begin{verbatim}
(update-stm<-
 '((((3) ("Piece A")) ((6) ("Piece A")))
   (((6) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((3) ("Piece A")))
   (((3) ("Piece A")) ((2) ("Piece A"))))
 nil)
--> '(((2) (((3) ("Piece A"))))
      ((3) (((4) ("Piece A"))))
      ((4) (((4) ("Piece A")) ((6) ("Piece A"))))
      ((6) (((3) ("Piece A"))))).
\end{verbatim}

\noindent This function is similar to the function
update-stm, the difference being $X_n$ is looked
up and $X_{n-1}$ is accumulated or added to a state-
transition matrix. This function has as its argument
listed states, and it applies the function
present-to-stm<- recursively to these listed states.
The variable *transition-matrix* is updated as it
proceeds. |#

(defun update-stm<-
       (listed-states &optional
	(stm *transition-matrix*))
  (if (null listed-states)
    (setq *transition-matrix* stm)
    (update-stm<-
     (rest listed-states)
     (present-to-stm<-
      (first listed-states) stm))))
