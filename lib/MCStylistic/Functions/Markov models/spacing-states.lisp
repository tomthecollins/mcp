#| Copyright 2008-2013 Tom Collins
   Tuesday 24 August 2010
   Incomplete

\noindent The functions here use segmentations to
build different types of states. One, output by the
function spacing-holding-states, consists of chord
spacing and holding types. Another, output by the
function beat-spacing-states, consists of beat-of-bar
and chord spacing.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "segmentation"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(bass-steps
 '(((56 0 0) (60 1 1) (72 1 2))
   ((58 1 3) (60 2 1) (72 3 2))
   ((58 2 3) (65 1 4) (72 3 2))
   ((56 0 5) (65 2 4) (72 2 2))
   ((55 0 6) (64 0 7) (73 1 8)) ((NIL NIL NIL))
   ((54 2 9) (70 2 10) (74 0 11))
   ((59 0 12) (63 1 13) (75 1 14))))
--> '(2 0 -2 -1 NIL NIL 5).
\end{verbatim}

\noindent This function takes a list of sorted holding
types and returns the intervals between the bass notes
of adjacent segments. It handles null entries, but
these will have been removed if it is being called
by the function spacing-holding-states. |#

(defun bass-steps
       (sorted-holdings-list &optional
	(note-number-1
	 (first
	  (first (first sorted-holdings-list)))))
  (let ((note-number-2
	 (first
	  (first (second sorted-holdings-list)))))
    (if (equal (length sorted-holdings-list) 1) ()
      (cons (if (or (null note-number-1)
		    (null note-number-2))
	      (identity NIL)
	      (- note-number-2 note-number-1))
	    (bass-steps (rest sorted-holdings-list)
			note-number-2)))))

#|
\noindent Example:
\begin{verbatim}
(bass-steps-with-rests
 '((3 ((3 48 53 3 1 6 0) (3 67 64 3/4 0 15/4 1)
       (3 76 69 3/4 0 15/4 2)))
   (15/4 ((3 48 53 3 1 6 0) (15/4 65 63 1/4 0 4 3)
	  (15/4 74 68 1/4 0 4 4)))
   (4 ((3 48 53 3 1 6 0) (4 64 62 2 0 6 5)
       (4 72 67 2 0 6 6)))
   (6 NIL)
   (13/2 ((13/2 61 60 1/2 0 7 7)))
   (7 ((7 62 61 1/2 0 15/2 8)))
   (15/2 ((15/2 64 62 1/2 0 8 9)))
   (8 ((8 50 54 1 1 9 10) (8 65 63 1 0 9 11)))
   (9 NIL)))
--> '(0 0 NIL 13 1 2 -14).
\end{verbatim}

\noindent This function. |#

(defun bass-steps-with-rests
       (segments &optional (bass-index 1)
	(current-bass-note
	 (nth
	  bass-index
	  (first (second (first segments)))))
	(next-bass-note
	 (nth
	  bass-index
	  (first (second (second segments))))))
  (if (<= (length segments) 1) ()
    (cons
     (if (and current-bass-note next-bass-note)
       (- next-bass-note current-bass-note))
     (bass-steps-with-rests
      (rest segments) bass-index
      (if (null next-bass-note)
	(identity current-bass-note)
	(identity next-bass-note))))))

#|
\noindent Example:
\begin{verbatim}
(beat-spacing-states
 '((3 48 53 3 1) (3 67 64 3/4 0) (3 76 69 3/4 0)
   (15/4 65 63 1/4 0) (15/4 74 68 1/4 0) (4 64 62 2 0)
   (4 72 67 2 0) (13/2 61 60 1/2 0) (7 62 61 1/2 0)
   (15/2 64 62 1/2 0) (8 50 54 1 1) (8 65 63 1 0))
 "C-68-3-mini" 3 1 3)
--> (((1 (19 9))
      (NIL NIL "C-68-3-mini"
           ((3 48 53 3 1 6 0) (3 67 64 3/4 0 15/4 1)
            (3 76 69 3/4 0 15/4 2))))
     ((7/4 (17 9))
      (0 0 "C-68-3-mini"
         ((3 48 53 3 1 6 0) (15/4 65 63 1/4 0 4 3)
          (15/4 74 68 1/4 0 4 4))))
     ((2 (16 8))
      (0 0 "C-68-3-mini"
         ((3 48 53 3 1 6 0) (4 64 62 2 0 6 5)
          (4 72 67 2 0 6 6))))
     ((1 "rest")
      (NIL NIL "C-68-3-mini" NIL))
     ((3/2 NIL)
      (13 7 "C-68-3-mini"
          ((13/2 61 60 1/2 0 7 7))))
     ((2 NIL)
      (1 1 "C-68-3-mini"
         ((7 62 61 1/2 0 15/2 8))))
     ((5/2 NIL)
      (2 1 "C-68-3-mini"
         ((15/2 64 62 1/2 0 8 9))))
     ((3 (15))
      (-14 -8 "C-68-3-mini"
           ((8 50 54 1 1 9 10) (8 65 63 1 0 9 11))))).
\end{verbatim}

\noindent Suppose you have three states $X_{n-1}, X_n,
X_{n+1}$. The function beat-spacing-states looks at
the beat and spacing of $X_n$, and also records the
difference between the bass notes of $X_n$ and
$X_{n-1}$. |#

(defun beat-spacing-states
       (datapoints &optional
	(catalogue-information "no information")
	(beats-in-bar 4) (sort-index 1)
	(duration-index 2)
	(datapoints-with-offs&enums
	 (enumerate-append
	  (append-offtimes
	   datapoints duration-index)))
	(unique-times
	 (remove-duplicates
	  (sort
	   (append
	    (nth-list-of-lists
	     0 datapoints-with-offs&enums)
	    (nth-list-of-lists
	     5 datapoints-with-offs&enums)) #'<)
	  :test #'equalp))
	(segmented
	 (segments-strict
	  datapoints sort-index duration-index
	  datapoints-with-offs&enums unique-times))
	(bass-steps-MIDI
	 (cons
	  NIL (bass-steps-with-rests segmented)))
        (bass-steps-morphetic
	 (cons
	  NIL (bass-steps-with-rests segmented 2))))
  (let ((ontime (first (first segmented)))
	(chord (second (first segmented))))
    (if (<= (length unique-times) 1) ()
      (cons
       (list
	(append
	 (list
	  (+ (mod ontime beats-in-bar) 1))
	 (list
	  (if chord (spacing 1 chord) "rest")))
	(list
	 (first bass-steps-MIDI)
         (first bass-steps-morphetic)
         catalogue-information
	 chord))
       (beat-spacing-states
	datapoints catalogue-information beats-in-bar
	sort-index duration-index
        datapoints-with-offs&enums (rest unique-times)
	(rest segmented) (rest bass-steps-MIDI)
        (rest bass-steps-morphetic))))))

#|
\noindent Example:
\begin{verbatim}
(beat-spacing-states<-
 '((3 48 53 3 1) (3 67 64 3/4 0) (3 76 69 3/4 0)
   (15/4 65 63 1/4 0) (15/4 74 68 1/4 0) (4 64 62 2 0)
   (4 72 67 2 0) (13/2 61 60 1/2 0) (7 62 61 1/2 0)
   (15/2 64 62 1/2 0) (8 50 54 1 1) (8 65 63 1 0))
 "C-68-3-mini" 3 1 3)
--> (((1 (19 9))
      (NIL NIL "C-68-3-mini"
           ((3 48 53 3 1 6 0) (3 67 64 3/4 0 15/4 1)
            (3 76 69 3/4 0 15/4 2))))
     ((7/4 (17 9))
      (0 0 "C-68-3-mini"
         ((3 48 53 3 1 6 0) (15/4 65 63 1/4 0 4 3)
          (15/4 74 68 1/4 0 4 4))))
     ((2 (16 8))
      (0 0 "C-68-3-mini"
         ((3 48 53 3 1 6 0) (4 64 62 2 0 6 5)
          (4 72 67 2 0 6 6))))
     ((1 "rest")
      (NIL NIL "C-68-3-mini" NIL))
     ((3/2 NIL)
      (13 7 "C-68-3-mini"
          ((13/2 61 60 1/2 0 7 7))))
     ((2 NIL)
      (1 1 "C-68-3-mini"
         ((7 62 61 1/2 0 15/2 8))))
     ((5/2 NIL)
      (2 1 "C-68-3-mini"
         ((15/2 64 62 1/2 0 8 9))))
     ((3 (15))
      (-14 -8 "C-68-3-mini"
           ((8 50 54 1 1 9 10) (8 65 63 1 0 9 11))))).
\end{verbatim}

\noindent This function is very similar to the
function beat-spacing-states. Suppose you have three
states $X_{n-1}, X_n, X_{n+1}$. The function
beat-spacing-states looks at the beat and spacing of
$X_n$, and also records the difference between the
bass notes of $X_n$ and $X_{n-1}$. The function
beat-spacing-states looks at the beat and spacing of
$X_n$, and also records the difference between the
bass notes of $X_{n+1}$ and $X_n$. |#

(defun beat-spacing-states<-
       (datapoints &optional
	(catalogue-information "no information")
	(beats-in-bar 4) (sort-index 1)
	(duration-index 2)
	(datapoints-with-offs&enums
	 (enumerate-append
	  (append-offtimes
	   datapoints duration-index)))
	(unique-times
	 (remove-duplicates
	  (sort
	   (append
	    (nth-list-of-lists
	     0 datapoints-with-offs&enums)
	    (nth-list-of-lists
	     5 datapoints-with-offs&enums)) #'<)
	  :test #'equalp))
	(segmented
	 (segments-strict
	  datapoints sort-index duration-index
	  datapoints-with-offs&enums unique-times))
	(bass-steps-MIDI
         (bass-steps-with-rests segmented))
        (bass-steps-morphetic
	 (bass-steps-with-rests segmented 2)))
  (let ((ontime (first (first segmented)))
	(chord (second (first segmented))))
    (if (<= (length unique-times) 1) ()
      (cons
       (list
	(append
	 (list
	  (+ (mod ontime beats-in-bar) 1))
	 (list
	  (if chord (spacing 1 chord) "rest")))
	(list
	 (first bass-steps-MIDI)
         (first bass-steps-morphetic)
         catalogue-information
	 chord))
       (beat-spacing-states<-
	datapoints catalogue-information beats-in-bar
	sort-index duration-index
        datapoints-with-offs&enums (rest unique-times)
	(rest segmented) (rest bass-steps-MIDI)
        (rest bass-steps-morphetic))))))

#|
\noindent Example:
\begin{verbatim}
(holding-type
 '(1/2
   ((1/2 65 1/2 1 58 1 4) (1/3 58 1/3 1 69 2/3 3)
    (0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1)))
 '(2/3
   ((2/3 56 1/3 1 60 1 5) (1/2 65 1/2 1 58 1 4)
    (1/3 58 1/3 1 69 2/3 3) (0 72 1 1 71 1 2)))
 '(1
   ((1 73 3/2 1 69 5/2 8) (1 64 1 1 69 2 7)
    (1 55 1 1 66 2 6) (2/3 56 1/3 1 60 1 5)
    (1/2 65 1/2 1 58 1 4) (0 72 1 1 71 1 2))))
--> ((56 0 5) (65 2 4) (72 2 2)).
\end{verbatim}

\noindent The function holding-type calls to one of
holding-type-start, holding-type-normal or holding-
type-finish, according to the emptiness of its
arguments. |#

(defun holding-type
       (previous-segment current-segment next-segment)
  (if (or (null previous-segment)
	  (null next-segment))
    (if (null previous-segment)
      (holding-type-start
       current-segment next-segment)
      (holding-type-finish
       previous-segment current-segment))
    (holding-type-normal
     previous-segment current-segment next-segment)))

#|
\noindent Example:
\begin{verbatim}
(holding-type-finish
 '(11/2 ((4 67 2 1 55 6 20) (4 76 3/2 1 69 11/2 18)))
 '(6 ((4 67 2 1 55 6 20))))
--> (NIL NIL NIL).
\end{verbatim}

\noindent The function holding-type-finish is called
by the function holding-type in the event that the
variable next-segment is empty. This only happens at
the end of a list of segments. I am yet to think of an
example where something other than an empty list
should be returned. |#

(defun holding-type-finish
       (previous-segment current-segment &optional
	(previous-list
	 (nth-list-of-lists
	  6 (second previous-segment)))
	(current-list
	 (nth-list-of-lists
	  6 (second current-segment)))
	(n (length current-list)) (j 0))
  (let* ((i (nth j current-list)))
    (if (equal j n) ()
      (if (equalp
           (nth
	    5
	    (nth
	     (index-item-1st-occurs i current-list)
	     (second current-segment)))
           (first current-segment))
	(cons
         (list NIL NIL NIL)
         (holding-type-finish
          previous-segment current-segment
	  previous-list current-list n (+ j 1)))
	(identity
	 "error - datapoint beyond final offtime")))))

#|
\noindent Example:
\begin{verbatim}
(holding-type-normal
 '(0 ((0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1)
      (0 56 1/3 1 47 1/3 0)))
 '(1/3 ((1/3 58 1/3 1 69 2/3 3) (0 72 1 1 71 1 2)
	(0 60 1/2 1 66 1/2 1) (0 56 1/3 1 47 1/3 0)))
 '(1/2 ((1/2 65 1/2 1 58 1 4) (1/3 58 1/3 1 69 2/3 3)
	(0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1))))
--> ((58 1 3) (72 3 2) (60 2 1)).
\end{verbatim}

\noindent The function holding-type-normal is called
by the function holding-type, in 'non-boundary
circumstances'. Holding types are assigned
appropriately, and returned as the second item in a
sublist of lists, along with MIDI note numbers and
identifiers. |#

(defun holding-type-normal
       (previous-segment current-segment next-segment
	&optional
	(previous-list
	 (nth-list-of-lists
	  6 (second previous-segment)))
	(current-list
	 (nth-list-of-lists
	  6 (second current-segment)))
	(next-list
	 (nth-list-of-lists 6 (second next-segment)))
	(n (length current-list)) (j 0))
  (let* ((i (nth j current-list)))
    (if (equal j n) ()
      (if (equalp
           (nth
	    5
	    (nth
	     (index-item-1st-occurs i current-list)
	     (second current-segment)))
           (first current-segment))
	(append
	 '()
	 (holding-type-normal
	  previous-segment current-segment
	  next-segment previous-list current-list
	  next-list n (+ j 1)))
	(cons
	 (list
	  (second
	   (nth
	    (index-item-1st-occurs
	     i current-list)
	    (second current-segment)))
	  (if (not (member i previous-list))
	    (if (equalp
		 (nth
		  5
		  (nth
		   (index-item-1st-occurs i next-list)
		   (second next-segment)))
		 (first next-segment))
	      (identity 0)
	      (identity 1))
	    (if (equalp
		 (nth
		  5
		  (nth
		   (index-item-1st-occurs i next-list)
		   (second next-segment)))
		 (first next-segment))
	      (identity 2)
	      (identity 3)))
	  (my-last
	   (nth
	    (index-item-1st-occurs i current-list)
	    (second current-segment))))
	 (holding-type-normal
	  previous-segment current-segment
	  next-segment previous-list current-list
	  next-list n (+ j 1)))))))

#|
\noindent Example:
\begin{verbatim}
(holding-type-start
 '(0 ((0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1)
      (0 56 1/3 1 47 1/3 0)))
 '(1/3 ((1/3 58 1/3 1 69 2/3 3) (0 72 1 1 71 1 2)
	(0 60 1/2 1 66 1/2 1) (0 56 1/3 1 47 1/3 0))))
--> ((72 1) (60 1) (56 0)).
\end{verbatim}

\noindent The function holding-type-start is called by
the function holding-type in the datapoint that the
variable previous-segment is empty. This only happens
at the beginning of a list of segments. Holding types
are assigned appropriately, and returned as the second
item in a sublist of lists, along with MIDI note
numbers and identifiers. It is possible to generate an
error using this function, if there are ontimes less
than the initial ontime present. |#

(defun holding-type-start
       (current-segment	next-segment &optional
        (current-list
	 (nth-list-of-lists
	  6 (second current-segment)))
	(next-list
	 (nth-list-of-lists 6 (second next-segment))))
  (let* ((i (first current-list)))
    (if (null current-list) ()
      (if (equalp
           (nth
	    5
	    (nth
	     (index-item-1st-occurs i next-list)
	     (second next-segment)))
           (first next-segment))
        (cons
         (list
	  (second
	   (nth
	    (index-item-1st-occurs i next-list)
	    (second next-segment)))
	  0
	  (my-last
	   (nth
	    (index-item-1st-occurs i next-list)
	    (second next-segment))))
         (holding-type-start
          current-segment next-segment
	  (rest current-list) next-list))
        (cons
         (list
	  (second
	   (nth
	    (index-item-1st-occurs i next-list)
	    (second next-segment)))
	  1
	  (my-last
	   (nth
	    (index-item-1st-occurs i next-list)
	    (second next-segment))))
         (holding-type-start
	  current-segment next-segment
          (rest current-list) next-list))))))

#|
\noindent Example:
\begin{verbatim}
(holding-types
 '((0 ((0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1)
       (0 56 1/3 1 47 1/3 0)))
   (1/3 ((1/3 58 1/3 1 69 2/3 3) (0 72 1 1 71 1 2)
	 (0 60 1/2 1 66 1/2 1) (0 56 1/3 1 47 1/3 0)))
   (1/2 ((1/2 65 1/2 1 58 1 4) (1/3 58 1/3 1 69 2/3 3)
	 (0 72 1 1 71 1 2) (0 60 1/2 1 66 1/2 1)))
   (2/3 ((2/3 56 1/3 1 60 1 5) (1/2 65 1/2 1 58 1 4)
	 (1/3 58 1/3 1 69 2/3 3) (0 72 1 1 71 1 2)))
   (1 ((2/3 56 1/3 1 60 1 5) (1/2 65 1/2 1 58 1 4)
       (0 72 1 1 71 1 2)))))
--> (((72 1 2) (60 1 1) (56 0 0))
     ((58 1 3) (72 3 2) (60 2 1))
     ((65 1 4) (58 2 3) (72 3 2))
     ((56 0 5) (65 2 4) (72 2 2))
     ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))).
\end{verbatim}

\noindent This function assigns holding-types to the
datapoints at each segment: 0 for unheld; 1 for held-
forward; 2 for held-backward; 3 for held-both. This
information is returned, along with the MIDI note
numbers and identifiers. |#

(defun holding-types
       (segments-list &optional
	(n (length segments-list)) (i 0)
	(previous-segment '())
	(next-segment (second segments-list)))
  (let ((current-segment (first segments-list)))
    (if (null segments-list) ()
      (cons
       (holding-type
	previous-segment current-segment next-segment)
       (holding-types
	(rest segments-list) n (+ i 1)
	current-segment)))))

#|
\noindent Example:
\begin{verbatim}
(index-rests
 '(((59 0 12) (63 0 13) (75 0 14)) NIL
   ((60 1 15) (63 0 16)) ((60 2 15)) (NIL NIL NIL)))
--> (1).
\end{verbatim}

\noindent A list of sorted holdings is the only
argument to this function. The output is the indices
of those sub-lists which are empty (excluding the last
sub-list) and therefore harbour rests. |#

(defun index-rests
       (sorted-holdings-list &optional
	(n (- (length sorted-holdings-list) 1)) (j 0))
  (if (equal j n) ()
    (append
     (if (null
	  (first
	   (first (first sorted-holdings-list))))
       (list (identity j)))
     (index-rests
      (rest sorted-holdings-list) n (+ j 1)))))

#|
\noindent Example:
\begin{verbatim}
(intervals-above-bass
 0 '((59 0 12) (63 1 13) (75 1 14)))
--> (0 4 16).
\end{verbatim}

\noindent An index n is provided as first argument; a
list of lists is the second argument. The nth item of
each sub-list is a MIDI note number, and these sub-
lists are in order of ascending MIDI note number. The
intervals above the bass are returned. It is possible
to produce nonsense output if null values are
interspersed with non-null values. I use the function
chord-spacing in preference to the function
intervals-above-bass. |#

(defun intervals-above-bass
       (index ascending-list-of-lists
	&optional
	(min-note-number
	 (nth
	  index (first ascending-list-of-lists))))
  (if (or (null min-note-number)
	  (null ascending-list-of-lists)) ()
    (cons (- (nth index
		  (first ascending-list-of-lists))
	     min-note-number)
	  (intervals-above-bass
	   index (rest ascending-list-of-lists)
	   min-note-number))))

#|
\noindent Example:
\begin{verbatim}
(sort-holding-types
 '(((72 1 2) (60 1 1) (56 0 0)) ((NIL NIL NIL))
   ((58 1 3) (72 3 2) (58 3 1))))
--> (((56 0 0) (60 1 1) (72 1 2)) ((NIL NIL NIL))
     ((58 1 3) (58 3 1) (72 3 2))).
\end{verbatim}

\noindent The sub-lists are returned, ordered by MIDI
note number and then holding-type (both ascending).
The function checks for empty chords to avoid errors
occurring in the sort function. |#

(defun sort-holding-types (holdings-list)
  (let ((first-holdings (first holdings-list)))
    (if (null holdings-list) ()
      (cons
       (if (equalp (first (first first-holdings)) NIL)
	 (identity first-holdings)
	 (sort-by
	  '((0 "asc") (1 "asc")) first-holdings))
       (sort-holding-types (rest holdings-list))))))

#|
\noindent Example:
\begin{verbatim}
(spacing 0 '((59 0 12) (63 1 13) (75 1 14)))
--> (4 12).
\end{verbatim}

\noindent An index n is provided as first argument; a
list of lists is the second argument. The nth item of
each sub-list is a MIDI note number, and these sub-
lists are in order of ascending MIDI note number. The
intervals between adjacent notes (chord spacing) are
returned. It is possible to produce nonsense output
if null values are interspersed with non-null
values. |#

(defun spacing (index list-of-lists)
  (if (equal (length list-of-lists) 1) ()
    (cons
     (- (nth index (second list-of-lists))
	(nth index (first list-of-lists)))
     (spacing index (rest list-of-lists)))))

#|
\noindent Example:
\begin{verbatim}
(spacing-holding-states
 '((0 74 1/2 1 84) (0 52 1 2 84) (1/2 76 1/4 1 84)
   (3/4 78 1/4 1 84) (1 80 1/4 1 84) (5/4 81 1/4 1 84)
   (3/2 83 1/2 1 84) (4 67 1 2 84) (4 64 1 2 84)
   (4 79 1/2 1 84) (9/2 78 1/2 1 84) (5 67 1 2 84)
   (5 64 1 2 84) (5 76 2 1 84)) "D Scarlatti L484" 2)
--> ((((22) (1 0)) (NIL 1/2 "D Scarlatti L484"
			((0 52 1 2 84 1 1)
			 (0 74 1/2 1 84 1/2 0))))
     (((24) (3 0)) (0 1/4 "D Scarlatti L484"
		      ((0 52 1 2 84 1 1)
		       (1/2 76 1/4 1 84 3/4 2))))
     (((26) (2 0)) (0 1/4 "D Scarlatti L484"
		      ((0 52 1 2 84 1 1)
		       (3/4 78 1/4 1 84 1 3))))
     ((NIL (0)) (28 1/4 "D Scarlatti L484"
		    ((1 80 1/4 1 84 5/4 4))))
     ((NIL (0)) (1 1/4 "D Scarlatti L484"
		   ((5/4 81 1/4 1 84 3/2 5))))
     ((NIL (0)) (2 5/2 "D Scarlatti L484"
		   ((3/2 83 1/2 1 84 2 6))))
     (((3 12) (1 1 0)) (-19 1/2 "D Scarlatti L484"
			    ((4 64 1 2 84 5 8)
			     (4 67 1 2 84 5 7)
			     (4 79 1/2 1 84 9/2 9))))
     (((3 11) (2 2 0)) (0 1/2 "D Scarlatti L484"
			  ((4 64 1 2 84 5 8)
			   (4 67 1 2 84 5 7)
			   (9/2 78 1/2 1 84 5 10))))
     (((3 9) (0 0 1)) (0 1 "D Scarlatti L484"
			 ((5 64 1 2 84 6 12)
			  (5 67 1 2 84 6 11)
			  (5 76 2 1 84 7 13))))
     ((NIL (2)) (12 1 "D Scarlatti L484"
		    ((5 76 2 1 84 7 13))))).
\end{verbatim}

\noindent This function takes datapoints as its
argument, and some optional catalogue information
about those datapoints. It converts the input into a
list of sub-lists, with each sub-list consisting of a
pair of lists. The first of the pair contains a chord
spacing, followed by holding types relating to the
notes of the chord. The second of the pair retains the
following information: the step (in semitones) between
the bass note of the previous chord and the current
state; the duration of the state (which can exceed the
minimum duration of the constituent datapoints if
rests are present); the catalogue information; the
relevant original datapoints, with offtimes and
enumeration appended. |#

(defun spacing-holding-states
       (datapoints &optional
	(catalogue-information "no information")
	(duration-index 2)
	(datapoints-with-offs&enums
	 (enumerate-append
	  (append-offtimes
	   datapoints duration-index)))
	(unique-times
	 (remove-duplicates
	  (sort
	   (append
	    (nth-list-of-lists
	     0 datapoints-with-offs&enums)
	    (nth-list-of-lists
	     5 datapoints-with-offs&enums)) #'<)
	  :test #'equalp))
	(sorted-holdings-list
	 (sort-holding-types
	  (holding-types
	   (segments
	    datapoints duration-index
	    datapoints-with-offs&enums
	    unique-times))))
	(indexed-rests
	 (index-rests sorted-holdings-list))
	(unique-times
	 (remove-nth-list indexed-rests unique-times))
	(sorted-holdings-list
	 (remove-nth-list
	  indexed-rests sorted-holdings-list))
	(bass-steps-list
	 (cons
	  NIL (bass-steps sorted-holdings-list))))
  (let ((sorted-holdings
	 (first sorted-holdings-list)))
    (if (equal (length unique-times) 1) ()
      (cons
       (list
	(append
	 (list (spacing 0 sorted-holdings))
	 (list (nth-list-of-lists 1 sorted-holdings)))
	(list
	 (first bass-steps-list)
	 (- (second unique-times)
	    (first unique-times))
	 catalogue-information
	 (nth-list
	  (nth-list-of-lists 2 sorted-holdings)
	  datapoints-with-offs&enums)))
       (spacing-holding-states
	datapoints catalogue-information
	duration-index datapoints-with-offs&enums
	(rest unique-times)
	(rest sorted-holdings-list) indexed-rests
	(rest unique-times)
	(rest sorted-holdings-list)
	(rest bass-steps-list))))))
