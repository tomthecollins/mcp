#| Copyright 2008-2013 Tom Collins
   Tuesday 27 January 2009
   Completed Tuesday 10 February 2009

\noindent These functions realise a sequence of states
in some given transition matrix. The states are
converted to datapoints so that they can be written to
a MIDI file. The global variable *rs* is defined here.

Here is an example demonstrating some of the
functionality of this code.

(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/File conversion"
  "/midi-export.lisp"))
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/Bach chorale model (score MIDI)"
  "/bach-transition-matrix.lisp"))
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/Bach chorale model (score MIDI)"
  "/bach-initial-states.lisp"))

(setq
 *states* (realise-states *initial-states* *stm* 5))
#| These functions help to show the process, but it's
possible to go straight for the function
states2datapoints.
(setq *state-durs* (state-durations *states*))
(setq
 *unique-times*
 (cons 0 (fibonacci-list *state-durs*)))
(setq
 *half-states* (create-MIDI-note-numbers *states* 48))
|#
(setq *datapoints* (states2datapoints *states* 48))
(setq
 *events*
 (scale-datapoints-by-factor 1 *datapoints*))
(saveit
 "/Users/tomcollins/Desktop/short.mid" *events*)

; REQUIRED PACKAGES
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
   :directory '(:relative "Maths foundation")
   :name "sort-by"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "stats-sampling"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "vector-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

;(defvar *rs* (make-random-state t))

#| Example:
(create-MIDI-note-numbers
 '((((12 3 4) (1 0 1 1))
    (NIL 500 "b707b"
     ((3000 57 1000 4 96 4000 0)
      (3000 69 500 3 96 3500 1)
      (3000 72 1000 2 96 4000 2)
      (3000 76 1000 1 96 4000 3))))
   (((10 5 4) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))
   (((15 7 5) (1 1 2 1))
    (-7 500 "b37800n"
     ((62000 50 1000 4 96 63000 62)
      (62000 65 1000 3 96 63000 127)
      (61000 72 1500 2 96 62500 189)
      (62000 77 1000 1 96 63000 244))))
   (((15 6 6) (2 2 0 2))
    (0 500 "b42600b"
     ((39000 45 1000 4 96 40000 47)
      (39000 60 1000 3 96 40000 119)
      (39500 66 500 2 96 40000 183)
      (39000 72 1000 1 96 40000 247))))
   (((12 9 7) (1 0 0 1))
    (1 500 "b39100b"
     ((35000 53 2000 4 96 37000 31)
      (35000 65 500 3 96 35500 95)
      (35000 74 500 2 96 35500 162)
      (35000 81 1000 1 96 36000 225))))) 48)
gives
((((48 60 63 67) (1 0 1 1))
  (NIL 500 "b707b"
   ((3000 57 1000 4 96 4000 0)
    (3000 69 500 3 96 3500 1)
    (3000 72 1000 2 96 4000 2)
    (3000 76 1000 1 96 4000 3))))
 (((48 58 63 67) (2 0 3 2))
  (0 500 "b41500b"
   ((43000 52 1000 4 96 44000 54)
    (43500 62 500 3 96 44000 134)
    (43000 67 1500 2 96 44500 201)
    (43000 71 1000 1 96 44000 256))))
 (((41 56 63 68) (1 1 2 1))
  (-7 500 "b37800n"
   ((62000 50 1000 4 96 63000 62)
    (62000 65 1000 3 96 63000 127)
    (61000 72 1500 2 96 62500 189)
    (62000 77 1000 1 96 63000 244))))
 (((41 56 62 68) (2 2 0 2))
  (0 500 "b42600b"
   ((39000 45 1000 4 96 40000 47)
    (39000 60 1000 3 96 40000 119)
    (39500 66 500 2 96 40000 183)
    (39000 72 1000 1 96 40000 247))))
 (((42 54 63 70) (1 0 0 1))
  (1 500 "b39100b"
   ((35000 53 2000 4 96 37000 31)
    (35000 65 500 3 96 35500 95)
    (35000 74 500 2 96 35500 162)
    (35000 81 1000 1 96 36000 225))))).

A list of realised states is provided, and this
function returns so-called `half-states': MIDI note
numbers have been created from the chord spacings. To
do this, we take the MIDI note number of the previous
bass note, and the variable bass-step, which is the
interval in semitones between the bass note of the
current state and previous state as they appeared in
the original data. If the current state is an initial
state in some original data, then this will be empty,
and so it is set to zero. |#

(defun create-MIDI-note-numbers
       (states &optional (previous-bass 60)
        (spacing-index 0) (other-index 1)
        (bass-step
	 (if (null (first (second (first states))))
	   (identity 0)
	   (first (second (first states))))))
  (if (null states) ()
    (let ((note-numbers
	   (spacing2note-numbers
	    (nth spacing-index (first (first states)))
	    (+ previous-bass bass-step))))
      (cons
       (cons
        (list note-numbers
              (nth
               other-index (first (first states))))
        (list (second (first states))))
       (create-MIDI-note-numbers
	(rest states) (first note-numbers)
        spacing-index other-index)))))

#| fibonacci-list moved to vector-operations! |#

#| Example:
(half-state2datapoints 0
 '((((48 60 63 67) (1 0 1 1))
    (NIL 500 "b707b"
     ((3000 57 1000 4 96 4000 0)
      (3000 69 500 3 96 3500 1)
      (3000 72 1000 2 96 4000 2)
      (3000 76 1000 1 96 4000 3))))
   (((48 58 63 67) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))
   (((41 56 63 68) (1 1 2 1))
    (-7 500 "b37800n"
     ((62000 50 1000 4 96 63000 62)
      (62000 65 1000 3 96 63000 127)
      (61000 72 1500 2 96 62500 189)
      (62000 77 1000 1 96 63000 244)))))
 '(500 500 500 500 500) '(0 500 1000 1500 2000 2500))
gives
((0 48 1000 4 96) (0 60 500 3 96)
 (0 63 1500 2 96) (0 67 1000 1 96)).

This function increments over the ith note of a half-
state, i = 0, 1,..., n-1. If the ith note in this
state is of tie-type 0 or 1 (corresponding to `untied'
or `tied-forward') then the function
state-note2datapoint is applied. |#

(defun half-state2datapoints
       (j half-states
	state-durs unique-times
	&optional
	 (i 0)
	 (current-state (nth j half-states))
	 (n (length (first (first current-state)))))
  (if (equal i n) ()
    (let ((tie-type
	   (nth i
		(second (first current-state)))))
      (append
       (if (or (equal tie-type 0) (equal tie-type 1))
	 (list
	  (state-note2datapoint i j half-states
				 state-durs
				 unique-times)))
       (half-state2datapoints
	 j half-states state-durs unique-times
	 (+ i 1) current-state n)))))

#| Example:
(index-of-offtime 0 63
  '((((48 60 63 67) (1 0 1 1))
    (NIL 500 "b707b"
     ((3000 57 1000 4 96 4000 0)
      (3000 69 500 3 96 3500 1)
      (3000 72 1000 2 96 4000 2)
      (3000 76 1000 1 96 4000 3))))
   (((48 58 63 67) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))
   (((41 56 63 68) (1 1 2 1))
    (-7 500 "b37800n"
     ((62000 50 1000 4 96 63000 62)
      (62000 65 1000 3 96 63000 127)
      (61000 72 1500 2 96 62500 189)
      (62000 77 1000 1 96 63000 244))))))
gives
2.

Given a starting index, a note-number and some half-
states to search through, this function returns the
index of the half-state where the note-number in
question comes to an end. This will be where its tie
type is first equal to 0 or 2, indicating `untied' and
`tied-back' respectively. |# 

(defun index-of-offtime (starting-index note-number
			 half-states &optional
			 (j starting-index)
			 (n (- (length half-states)
			       1)))
  (if (equal j n) (identity j)
    (let ((current-state (first (nth j half-states))))
      (if (evenp
	   (nth
	    (index-item-1st-occurs
	     note-number (first current-state))
	    (second current-state)))
	(identity j)
	(index-of-offtime starting-index note-number
			  half-states (+ j 1) n)))))

#| Example:
(realise-states *initial-states* *stm* 2)
gives
((((4 3 5) (0 0 1 1))
  (NIL 500 "b43800b"
   ((3000 48 500 4 96 3500 0)
    (3000 52 500 3 96 3500 1)
    (3000 55 1000 2 96 4000 2)
    (3000 60 1000 1 96 4000 3))))
 (((4 5 5) (0 0 2 2))
  (-2 500 "b42600b"
   ((46500 60 500 4 96 47000 56)
    (46500 64 500 3 96 47000 126)
    (46000 69 1000 2 96 47000 191)
    (46000 74 1000 1 96 47000 253))))).

Given some initial states and a transition matrix, and
an optional argument called count, this function
realises a total of count states in the transition
matrix. If a closed state is reached, then the process
is terminated, and however many states have been
generated by this stage are returned. |#

(defun realise-states
       (initial-states transition-matrix &optional
        (count 5)
        (initial-state (choose-one initial-states))
        (initial-tf T))
  (let ((new-state
	 (if initial-tf (identity initial-state)
	   (choose-one
	    (second
	     (assoc
              (first initial-state) transition-matrix
              :test #'equalp))))))
    (if (or (null new-state) (< count 1)) ()
      (cons new-state
	    (realise-states
	     initial-states transition-matrix
	     (- count 1) new-state NIL)))))

#| Example:
(scale-datapoints-by-factor 2
 '((0 48 1000 4 96) (0 60 500 3 96) (0 63 1500 2 96)
   (0 67 1000 1 96) (500 58 500 3 96)
   (1000 41 1000 4 96) (1000 56 1000 3 96)))
gives
'((0 48 2000 4 96) (0 60 1000 3 96) (0 63 3000 2 96)
  (0 67 2000 1 96) (1000 58 1000 3 96)
  (2000 41 2000 4 96) (2000 56 2000 3 96)).

The ontimes and durations of datapoints are scaled
up or down by a constant factor. |#

(defun scale-datapoints-by-factor (factor datapoints)
  (let ((current-event (first datapoints)))
    (if (null datapoints) ()
      (cons (list
	     (* factor (first current-event))
	     (second current-event)
	     (* factor (third current-event))
	     (fourth current-event)
	     (fifth current-event))
	    (scale-datapoints-by-factor
	     factor
	     (rest datapoints))))))

#| Example:
(spacing2note-numbers '(3 12) 64)
gives
'(64 67 79).

A chord spacing and note-number are inputs to this
function. Returned are the MIDI note numbers of the
chord whose lowest note is given by note-number, and
whose spacing is as provided. |#

(defun spacing2note-numbers (spacing note-number)
  (cons note-number (add-to-list
		     note-number
		     (fibonacci-list spacing))))

#| Example:
(state-durations
 '((((10 5 4) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))
   (((15 7 5) (1 1 2 1))
    (-7 500 "b37800n"
     ((62000 50 1000 4 96 63000 62)
      (62000 65 1000 3 96 63000 127)
      (61000 72 1500 2 96 62500 189)
      (62000 77 1000 1 96 63000 244))))
   (((15 6 6) (2 2 0 2))
    (0 500 "b42600b"
     ((39000 45 1000 4 96 40000 47)
      (39000 60 1000 3 96 40000 119)
      (39500 66 500 2 96 40000 183)
      (39000 72 1000 1 96 40000 247))))
   (((12 9 7) (1 0 0 1))
    (1 500 "b39100b"
     ((35000 53 2000 4 96 37000 31)
      (35000 65 500 3 96 35500 95)
      (35000 74 500 2 96 35500 162)
      (35000 81 1000 1 96 36000 225))))))
gives
(500 500 500 500).

This function takes a list of states as its argument,
and returns a list containing the duration of each
state in a list. The set of so-called `partition
points' can be generated easily by applying the
function fibonacci-list. |#

(defun state-durations (states)
  (if (null states) ()
    (cons (second (second (first states)))
	  (state-durations (rest states)))))

#| Example:
(state-note2datapoint 2 0
 '((((48 60 63 67) (1 0 1 1))
    (NIL 500 "b707b"
     ((3000 57 1000 4 96 4000 0)
      (3000 69 500 3 96 3500 1)
      (3000 72 1000 2 96 4000 2)
      (3000 76 1000 1 96 4000 3))))
   (((48 58 63 67) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))
   (((41 56 63 68) (1 1 2 1))
    (-7 500 "b37800n"
     ((62000 50 1000 4 96 63000 62)
      (62000 65 1000 3 96 63000 127)
      (61000 72 1500 2 96 62500 189)
      (62000 77 1000 1 96 63000 244)))))
 '(500 500 500) '(0 500 1000 1500))
gives
(0 63 1500 2 96).

The ith note of the jth half-state is transformed into
a so-called `datapoint', meaning we find its ontime
(the jth element of the partition points), its MIDI
note number, its offtime, which is trickier. Other
information, such as voicing and relative dynamics,
are drawn from the initial occurence of the half-state
in question.

Returning to the calculation of offtime, we find the
half-state k where the note ends and use this to
calculate the duration of the note up until the kth
state. Whichever is less---the kth state duration or
the duration of this note within the kth state---is
added to give the total duration. This encapsulates
the implicit encoding of rests. |#

(defun state-note2datapoint
       (i j half-states state-durs unique-times)
  (let* ((current-state (nth j half-states))
	 (note-ontime (nth j unique-times))
	 (note-number
          (nth i (first (first current-state))))
	 (offtime-state
	  (index-of-offtime
	   j note-number half-states))
	 (offtime-index
	  (index-item-1st-occurs
	   note-number
	   (first
	    (first
	     (nth offtime-state half-states))))))
    (list
     note-ontime
     note-number
     (+
      (- (nth offtime-state unique-times)
	 (nth j unique-times))
      (min-item
       (list
	(third (nth 
		offtime-index
		(fourth
		 (second
		  (nth offtime-state
		       half-states)))))
	(nth offtime-state state-durs))))
     (fourth (nth i (fourth (second current-state))))
     (fifth
      (nth i (fourth (second current-state)))))))

#| Example:
(states2datapoints
 '((((12 3 4) (1 0 1 1))
    (NIL 500 "b707b"
     ((3000 57 1000 4 96 4000 0)
      (3000 69 500 3 96 3500 1)
      (3000 72 1000 2 96 4000 2)
      (3000 76 1000 1 96 4000 3))))
   (((10 5 4) (2 0 3 2))
    (0 500 "b41500b"
     ((43000 52 1000 4 96 44000 54)
      (43500 62 500 3 96 44000 134)
      (43000 67 1500 2 96 44500 201)
      (43000 71 1000 1 96 44000 256))))) 48)
gives
'((0 48 1000 4 96) (0 60 500 3 96) (0 63 1000 2 96)
  (0 67 1000 1 96) (500 58 500 3 96)).

This function applies the function
half-state2datapoint recursively to a list of states.
Some initial caluclations are performed to obtain
half-states, state durations and partition points. |#

(defun states2datapoints
       (states
	&optional
	 (initial-note-number 60)
	 (half-states
	  (create-MIDI-note-numbers
	   states initial-note-number))
	 (j 0) (n (length half-states))
	 (state-durs
	  (state-durations states))
	 (unique-times
	  (cons 0
		(fibonacci-list state-durs))))
  (if (equal j n) ()
    (append
     (half-state2datapoints
      j half-states state-durs unique-times)
     (states2datapoints
      states initial-note-number half-states
      (+ j 1) n state-durs unique-times))))
