#| Copyright 2008-2013 Tom Collins
   Tuesday 27 January 2009
   Completed Tuesday 24 August 2010

\noindent The functions here are designed to analyse
data according to a Markov-chain model. At present the
code handles a first-order analysis. The aim is to
build a state transition matrix for some variables
(referenced by variable-names and catalogue). Hence,
the variable variable-names points to some actual data
(note the use of the function symbol-value) which is
indexed by the variable catalogue. Using the function
write-to-file, the information can be sent to a text
file, to avoid the Listener having to display it.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Query staff notation")
   :name "artic-dynam-lyrics-utilities"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "kern-articulation"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
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
   :name "segmentation"
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
(accumulate-to-stm
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '((4) (((1) ("Piece B")) ((2) ("Piece C"))))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B"))
         ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((4) (((2) ("Piece A")) ((1) ("Piece B"))
	    ((2) ("Piece C"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent The first argument is a listed state; the
second is the relevant row of the state transition
matrix; the third is the state transition matrix
itself. This function is called when the state of the
first item of the listed state has appeared in the
state transition matrix  before. The references of the
event are included. |#

(defun accumulate-to-stm
       (listed-state relevant-row stm)
  (cons (list (first (first listed-state))
              (cons (second listed-state)
		    (second relevant-row)))
	(remove relevant-row stm :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(add-to-stm
 '(((3) ("Piece A")) ((4) ("Piece A")))
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

\noindent The first argument is a listed state; the
second is the state transition matrix. This function
is called when the state of the first item of the
listed state has not appeared in the state transition
matrix before. It is added. |#

(defun add-to-stm (listed-state stm)
  (cons
   (list
    (first (first listed-state))
    (list (second listed-state))) stm))

#|
\noindent Example:
\begin{verbatim}
(setq
 variable-1
 '((0 30 1 1 84) (0 33 1 1 84) (1 40 1 1 84)
   (1 41 1 1 84)))
(setq
 variable-2
 '((0 60 1 1 84) (0 63 1 1 84) (1 62 1 1 84)
   (1 63 1 1 84)))
(setq *variable-names* '(variable-1 variable-2))
(setq *catalogue* '("variable-1" "variable-2"))
(construct-initial-states
 *variable-names* *catalogue*)
--> '((((3) (0 0))
       (NIL 1 "variable-1"
	    ((0 30 1 1 84 1 0) (0 33 1 1 84 1 1))))
      (((3) (0 0))
       (NIL 1 "variable-2"
	    ((0 60 1 1 84 1 0) (0 63 1 1 84 1 1))))).
\end{verbatim}

\noindent This recursion analyses one variable name at
a time, taking a catalogue name from the variable
catalogue, and outputs initial states accordingly. If
the state function is of the kind
``beat-rel-MNN-states'', then it is assumed that the
beat-rel-MNN states have been defined already, and
that this is what variable-names refers to. |#

(defun construct-initial-states
       (variable-names catalogue state-fn &optional
	(depth-check 10) (duration-index 3)
	(beats-in-bar 4) (sort-index 2))
  (if (null variable-names) ()
    (cons
     (first
      (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	 (firstn
	  depth-check
	  (symbol-value (first variable-names)))
	 (first catalogue) duration-index)
        (if (string= state-fn "beat-rel-MNN-states")
          (symbol-value (first variable-names))
          (beat-spacing-states
           (firstn
            depth-check
            (symbol-value (first variable-names)))
           (first catalogue) beats-in-bar sort-index
           duration-index))))
     (construct-initial-states
      (rest variable-names) (rest catalogue) state-fn
      depth-check duration-index beats-in-bar
      sort-index))))

#|
\noindent Example:
\begin{verbatim}
(setq
 *catalogue*
 (list "bachChoraleBWV411R246" "bachChoraleBWV4p8R184"))
(setq
 *kern-path&names*
 (loop for s in *catalogue* collect
   (merge-pathnames
    (make-pathname
     :directory
     (list :relative "bachChorales" s "kern")
     :name s :type "krn")
    *MCStylistic-MonthYear-data-path*)))
(setq *too-close* 1)
(setq
 internal-initial-states
 (construct-internal-states
  *kern-path&names* *catalogue* "fermata ending"
  *too-close* "beat-rel-MNN-states" 4))
--> (((3 (-12 7 12 16))
      ("bachChoraleBWV411R246"
       ((6 43 50 2 3) (6 62 61 2 2) (6 67 64 2 1)
        (6 71 66 2 0)) (55 57) (1 0)))
     ((3 (-5 2 11 19))
      ("bachChoraleBWV411R246"
       ((14 50 54 2 3) (14 57 58 2 2) (14 66 63 2 1)
        (14 74 68 2 0)) (55 57) (1 0)))
     ...
     ((3 (-7 0 9))
      ("bachChoraleBWV4p8R184"
       ((54 46 52 2 3) (54 53 56 2 2) (54 62 61 2 0)
        (54 62 61 2 1)) (53 56) (2 5)))).
\end{verbatim}

\noindent This function identifies events in files
where there are (or likely to be) phrase beginnings and
endings. If the variable method is a string (e.g.,
"phrase beginning"), it parses kern files for places
where phrases begin/end or where there are fermata. If
the variable method is a number or list of ontimes,
it parses point sets for events at specific times,
assuming phrases begin/end at these times. It
calculates the state representations for these events
and outputs them in a list. |#

(defun construct-internal-states
       (file-pathnames catalogue &optional
        (method "notation") ;or "ontime"
        (phrase-string "phrase beginning")
        #| If method is ontime, phrase-string ought to
        be a list whose first element is a string, and
        whose remaining elements are one or more
        ontimes. If one ontime, that's the duration of
        spacing; if multiple ontimes, those are the 
        ontimes to use! |#
        (too-close 1)
        (state-fn "beat-rel-MNN-states")
        (beats-in-bar 4) (ontime-idx 0) (MNN-idx 1)
        (MPN-idx 2) (duration-idx 3) (staff-idx 4))
  (if (string= method "notation")
    (loop for i from 0
      to (- (length file-pathnames) 1) append
      #| Want to avoid including initial and final
      states here. It turns out calling butlast
      here is the appropriate thing to do either
      way. |#
      (butlast
       (kern-file2phrase-boundary-states
        (nth i file-pathnames) phrase-string too-close
        state-fn beats-in-bar (nth i catalogue)
        ontime-idx MNN-idx MPN-idx duration-idx
        staff-idx)))
    (loop for i from 0
      to (- (length file-pathnames) 1) append
      #| Want to avoid including initial and final
      states here. It turns out calling butlast
      here is the appropriate thing to do either
      way. |#
      (butlast
       (point-set2phrase-boundary-states
        (nth i file-pathnames) phrase-string
        state-fn beats-in-bar (nth i catalogue)
        ontime-idx MNN-idx MPN-idx duration-idx
        staff-idx)))))

#| Retired 29/11/2016 so we can also generate internal
states based on specified and/or regularly spaced
ontimes.
(defun construct-internal-states
       (kern-path&names catalogue &optional
        (phrase-str "phrase beginning") (too-close 1)
        (state-fn "beat-rel-MNN-states")
        (beats-in-bar 4) (ontime-idx 0) (MNN-idx 1)
        (MPN-idx 2) (duration-idx 3) (staff-idx 4))
  (loop for i from 0
    to (- (length kern-path&names) 1) append
    #| Want to avoid including initial and final
    states here. It turns out calling butlast
    here is the appropriate thing to do either
    way. |#
    (butlast
     (kern-file2phrase-boundary-states
      (nth i kern-path&names) phrase-str too-close
      state-fn beats-in-bar (nth i catalogue)
      ontime-idx MNN-idx MPN-idx duration-idx
      staff-idx))))
|#
#|
\noindent Example:
\begin{verbatim}
(setq
 variable-1
 '((0 30 1 1 84) (0 33 1 1 84) (1 40 1 1 84)
   (1 41 1 1 84)))
(setq
 variable-2
 '((0 60 1 1 84) (0 63 1 1 84) (1 62 1 1 84)
   (1 63 1 1 84)))
(setq *variable-names* '(variable-1 variable-2))
(setq *catalogue* '("variable-1" "variable-2"))
(construct-stm *variable-names* *catalogue*)
--> "Finished!".
\end{verbatim}

\noindent This recursion analyses one variable name at
a time, taking a catalogue name from the variable
catalogue, and updates the transition matrix
accordingly. The output "Finished!" is preferable to
the transition matrix, which is large enough that it
can cause the Listener to crash. If the state function
is of the kind ``beat-rel-MNN-states'', then it is
assumed that the beat-rel-MNN states have been defined
already, and that this is what variable-names refers
to. |#

(defun construct-stm
       (variable-names catalogue state-fn &optional
	(duration-index 3) (beats-in-bar 4)
	(sort-index 2))
  (if (null variable-names) (print "Finished!")
    (progn
      (markov-analyse
       (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	  (symbol-value (first variable-names))
	  (first catalogue) duration-index)
         (if (string= state-fn "beat-rel-MNN-states")
           (symbol-value (first variable-names))
           (beat-spacing-states
            (symbol-value (first variable-names))
            (first catalogue) beats-in-bar sort-index
            duration-index))))
      (construct-stm
       (rest variable-names) (rest catalogue) state-fn
       duration-index beats-in-bar sort-index))))

(defun foo
       (a-number b-number &key fn)
  (funcall fn a-number b-number))

#|
\noindent Example:
\begin{verbatim}
(firstn-list 3 '(1 2 3 4 5)
--> '((1 2 3) (2 3 4) (3 4 5)).
\end{verbatim}

\noindent This function applies the function firstn
recursively to a list. It is like producing an n-
gram, and is useful for building a first-order
Markov model. I call the output 'listed states'. |#

(defun firstn-list (n a-list)
  (if (equal (length a-list) (- n 1)) ()
    (cons (firstn n a-list)
          (firstn-list n (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 kern-path&name
 (merge-pathnames
  (make-pathname
   :name "C-6-1-small" :type "krn")
  *MCStylistic-MonthYear-example-files-data-path*))
(kern-file2phrase-boundary-states kern-path&name)
--> (((4 (9))
      ("C-6-1-small" ((-1 66 63 5/3 0)) (57 58) (6 5)))
     ((19/4 (-15 9))
      ("C-6-1-small" ((3 42 49 1 1) (15/4 66 63 1/4 0))
       (57 58) (6 5)))).
\end{verbatim}

\noindent This function imports a kern file and searches
for notes that have articulation associated with phrase
beginnings or endings, with the aim of returning states
that will be appropriate for using as initial or final
internal states. The first optional argument phrase-str
can be called with `phrase beginning', `phrase ending',
`fermata beginning', or `fermata ending'. Phrase
beginnings are indicated by \texttt{(} in a kern file,
endings by \texttt{)}, and fermata by \texttt{;}.
Fermata indicate the end of a phrase, and therefore the
next state will be the beginning of the next phrase. So
if this function is called with phrase-str equal to
`fermata beginning', the next state(s) following fermata
will be returned.

Sometimes fermata signs appear in close succession (for
instance imagine the tenor sings A3 held over from the
fourth beat of the bar to the first beat of the next
bar, resolving to G$\sharp$3 on beat two, whilst the
bass, alto, and soprano sing E3, E4, B4 resepctively on
beat one, and suppose further that fermata are written
on these three notes and the resolving G$\sharp$3. Then
there will be three fermata at ontime $x$ and one at
ontime $x + 1$. The end of the phrase is not at ontime
$x$ but at ontime $x + 1$. This function will remove
fermata ontimes that are too close together---in our
example removing the fermata ontime $x$. This
functionality is controlled by the optional argument
too-close. |#

(defun kern-file2phrase-boundary-states
       (kern-path&name &optional
        (phrase-str "phrase beginning")
        (too-close 1)
        (state-fn "beat-rel-MNN-states")
        (beats-in-bar 4)
        (catalogue-information
         (pathname-name kern-path&name))
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4)
        (artic-set
         (kern-file2points-artic-dynam-lyrics
          kern-path&name))
        (ordinary-point-set
         (mapcar
          #'(lambda (x)
              (list
               (nth ontime-idx x) (nth MNN-idx x)
               (nth MPN-idx x) (nth duration-idx x)
               (nth staff-idx x))) artic-set))
        (q-split&artic-event&rel-points
         (articulation-points phrase-str artic-set))
        (relevant-times
         (remove-duplicates&values-too-close
          (nth-list-of-lists
           ontime-idx
           (third q-split&artic-event&rel-points))
          too-close))
        (seg-ons
         (nth-list-of-lists
          ontime-idx
          (segments-strict
           ordinary-point-set MNN-idx duration-idx)))
        (relevant-idx
         (loop for time in relevant-times
           when (position time seg-ons)
           collect (position time seg-ons)))
        (states
         (cond
          ((string= state-fn "beat-rel-MNN-states")
           (beat-rel-MNN-states
            ordinary-point-set catalogue-information
            beats-in-bar MNN-idx MPN-idx
            duration-idx))
          
          )))
  (loop for i in relevant-idx
    when relevant-idx
    collect
    (if (and
         (string= phrase-str "fermata beginning")
         #| 29/11/2016. Bug identified and corrected on
         the next line. Previously the i was missing,
         causing the predicate to always be true, which
         I guess led to the inclusion of empty or nil
         states, which could have resulted in follow-on
         errors. |#
         (< i (- (length states) 1)))
      (nth (+ i 1) states) (nth i states))))

#|
\noindent Example:
\begin{verbatim}
(markov-analyse
 '(((3) ("Piece A")) ((6) ("Piece A"))
   ((4) ("Piece A")) ((4) ("Piece A"))
   ((3) ("Piece A")) ((2) ("Piece A"))))
--> '(((3) (((2) ("Piece A")) ((6) ("Piece A"))))
      ((4) (((3) ("Piece A")) ((4) ("Piece A"))))
      ((6) (((4) ("Piece A"))))), or "It's done!".
\end{verbatim}

\noindent This function has one argument - some states
which are to be analysed according to a first-order
Markov model. Note the need to define a variable here,
*transition-matrix*. The output "Finished!" is
preferable to the transition matrix, which is large
enough that it can cause the Listener to crash. |#

(defvar *transition-matrix* ())

(defun markov-analyse (states)
  (if (update-stm (firstn-list 2 states)) t)
  "It's done!")

#|
\noindent Example:
\begin{verbatim}
(present-to-stm
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((4) (((2) ("Piece A")) ((1) ("Piece B"))
	    ((2) ("Piece C"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent This function calls either the function
accumulate-to-stm, or add-to-stm, depending on whether
the first argument, a listed-state, has appeared in
the second argument, a state-transition matrix,
before. The example above results in accumulate-to-stm
being called, as the state of (4) has occurred before.
However, changing this state to (3) in the argument
would result in add-to-stm being called. |#

(defun present-to-stm (listed-state stm)
  (let ((relevant-row
	 (assoc
	  (first (first listed-state)) stm
	  :test #'equalp)))
    (if (identity relevant-row) 
      (accumulate-to-stm
       listed-state relevant-row stm)
      (add-to-stm listed-state stm))))

#|
\noindent Example:
\begin{verbatim}
(setq times '(2 2 4 7 7.5 7.75 8))
(remove-duplicates&values-too-close times)
--> (2 4 8).
\end{verbatim}

\noindent This function takes a list of floats as
input, assumed to be ordered ascending. It will remove
any duplicates from that list, as well as removing
any elements that are too close in value (controlled by
the second optional argument), leaving the largest. |#

(defun remove-duplicates&values-too-close
       (times &optional (too-close 1)
        (times
         (remove-duplicates times :test #'equalp)))
  (if (null times) ()
    (if (or
         (equalp (length times) 1)
         (> (abs (- (second times) (first times))) 1))
      (cons
       (first times)
       (remove-duplicates&values-too-close
        nil too-close (rest times)))
      (remove-duplicates&values-too-close
       nil too-close (rest times)))))
         
#|
\noindent Example:
\begin{verbatim}
(update-stm
 '((((3) ("Piece A")) ((6) ("Piece A")))
   (((6) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((3) ("Piece A")))
   (((3) ("Piece A")) ((2) ("Piece A")))))
--> '(((3) (((2) ("Piece A")) ((6) ("Piece A"))))
      ((4) (((3) ("Piece A")) ((4) ("Piece A"))))
      ((6) (((4) ("Piece A"))))).
\end{verbatim}

\noindent This function has as its argument listed
states, and it applies the function present-to-stm
recursively to these listed states. The variable
*transition-matrix* is updated as it proceeds. |#

(defun update-stm
       (listed-states &optional
	(stm *transition-matrix*))
  (if (null listed-states)
    (setq *transition-matrix* stm)
    (update-stm (rest listed-states)
                (present-to-stm
                 (first listed-states) stm))))
