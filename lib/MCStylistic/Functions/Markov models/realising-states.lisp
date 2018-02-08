#| Copyright 2008-2013 Tom Collins
   Wednesday 1 September 2010
   Incomplete

\noindent These functions are used to convert states
generated using Markov-chain Monte Carlo into
datapoints. A lot of the functions have similar
versions in the file markov-compose.lisp.

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
   :directory '(:relative "Maths foundation")
   :name "sort-by"
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

#|
\noindent Example:
\begin{verbatim}
(setq
 states
 '(((1 (12 7 5 4))
    (NIL NIL "C-63-1"
         ((0 35 45 1/2 1 1/2 0) (0 47 52 1/2 1 1/2 1)
          (0 54 56 1/2 0 1/2 2) (0 59 59 1/2 0 1/2 3)
          (0 63 61 1/2 0 1/2 4))))
   ((3/2 "rest")
    (NIL NIL "C-63-1" NIL))
   ((7/4 NIL)
    (-6 -3 "C-63-1"
        ((831/4 56 57 1/4 0 208 701))))
   ((2 (12 14 6 3))
    (-26 -15 "C-63-1"
         ((208 30 42 1 1 209 702)
          (208 42 49 1 1 209 703)
          (208 56 57 1 0 209 704)
          (208 62 60 1 0 209 705)
          (208 65 62 1 0 209 706))))
   ((3 (12 12 9 3))
    (0 0 "C-63-1"
       ((209 30 42 1 1 210 707)
        (209 42 49 1 1 210 708)
        (209 54 56 1 0 210 709)
        (209 63 61 1 0 210 710)
        (209 66 63 1 0 210 711))))
   ((1 (12 16 6 4))
    (0 0 "C-63-1"
       ((216 30 42 1 1 217 740)
        (216 42 49 1 1 217 741)
        (216 58 58 1 0 217 742)
        (216 64 62 1 0 217 743)
        (216 68 64 1 0 217 744))))
   ((2 (5 4 3))
    (19 11 "C-63-1"
        ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
         (43 65 62 1/2 0 87/2 197)
         (43 68 64 1 1 44 198))))
   ((5/2 (5 4 8 4))
    (0 0 "C-63-1"
       ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
        (25 63 61 1 1 26 103)
        (51/2 71 66 1/2 0 26 105)
        (51/2 75 68 1/2 0 26 106))))
   ((3 (3))
    (19 11 "C-63-1"
        ((230 73 67 3/4 0 923/4 809)
         (230 76 69 3/4 0 923/4 810))))
   ((7/2 (4))
    (3 2 "C-67-1"
       ((41/2 72 67 1/2 0 21 94)
        (41/2 76 69 1/2 0 21 95))))))
(create-MIDI&morphetic-numbers states)
--> (((1 (60 72 79 84 88) (60 67 71 74 76))
      (NIL NIL "C-63-1"
           ((0 35 45 1/2 1 1/2 0)
            (0 47 52 1/2 1 1/2 1)
            (0 54 56 1/2 0 1/2 2)
            (0 59 59 1/2 0 1/2 3)
            (0 63 61 1/2 0 1/2 4))))
     ((3/2 NIL NIL)
      (NIL NIL "C-63-1" NIL))
     ((7/4 (54) (57))
      (-6 -3 "C-63-1"
          ((831/4 56 57 1/4 0 208 701))))
     ((2 (28 40 54 60 63) (42 49 57 60 62))
      (-26 -15 "C-63-1"
           ((208 30 42 1 1 209 702)
            (208 42 49 1 1 209 703)
            (208 56 57 1 0 209 704)
            (208 62 60 1 0 209 705)
            (208 65 62 1 0 209 706))))
     ((3 (28 40 52 61 64) (42 49 56 61 63))
      (0 0 "C-63-1"
         ((209 30 42 1 1 210 707)
          (209 42 49 1 1 210 708)
          (209 54 56 1 0 210 709)
          (209 63 61 1 0 210 710)
          (209 66 63 1 0 210 711))))
     ((1 (28 40 56 62 66) (42 49 58 62 64))
      (0 0 "C-63-1"
         ((216 30 42 1 1 217 740)
          (216 42 49 1 1 217 741)
          (216 58 58 1 0 217 742)
          (216 64 62 1 0 217 743)
          (216 68 64 1 0 217 744))))
     ((2 (47 52 56 59) (53 56 58 60))
      (19 11 "C-63-1" 
          ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
           (43 65 62 1/2 0 87/2 197)
           (43 68 64 1 1 44 198))))
     ((5/2 (47 52 56 64 68) (53 56 58 63 65))
      (0 0 "C-63-1"
         ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
          (25 63 61 1 1 26 103)
          (51/2 71 66 1/2 0 26 105)
          (51/2 75 68 1/2 0 26 106))))
     ((3 (66 69) (64 66))
      (19 11 "C-63-1"
          ((230 73 67 3/4 0 923/4 809)
           (230 76 69 3/4 0 923/4 810))))
     ((7/2 (69 73) (66 68))
      (3 2 "C-67-1"
         ((41/2 72 67 1/2 0 21 94)
          (41/2 76 69 1/2 0 21 95)))))
\end{verbatim}

\noindent This function is meant to take generated
states and realise MIDI note numbers and morphetic
pitch numbers for each state. 2/9/2010 The spelling
seems to be getting out of hand, however, so this will
need addressing. |#

(defun create-MIDI&morphetic-numbers
       (states &optional (previous-MIDI 60)
        (previous-morphetic 60) (MIDI-step-index 0)
        (morphetic-step-index 1)
        (MNNs
         (nth-list-of-lists
          1 (fourth (second (first states)))))
        (MPNs
         (nth-list-of-lists
          2 (fourth (second (first states)))))
        (MNN-trans
         (if MNNs (- previous-MIDI (first MNNs))))
        (MPN-trans
         (if MPNs
           (- previous-morphetic (first MPNs))))
        (bass-step-MIDI
	 (if (null
              (nth
               MIDI-step-index
               (second (first states))))
	   (identity 0)
	   (nth
            MIDI-step-index (second (first states)))))
        (bass-step-morphetic
	 (if (null
              (nth
               morphetic-step-index
               (second (first states))))
	   (identity 0)
	   (nth
            morphetic-step-index
            (second (first states)))))
        (MNNs
         (if MNNs
           (add-to-list
            (+ MNN-trans bass-step-MIDI) MNNs)))
        (MPNs
         (if MPNs
           (add-to-list
            (+ MPN-trans bass-step-morphetic) MPNs))))
  (if (null states) ()
    (cons
     (cons
      (list
       (first (first (first states))) MNNs MPNs)
      (list (second (first states))))
     (create-MIDI&morphetic-numbers
      (rest states)
      (if MNNs (first MNNs) (identity previous-MIDI))
      (if MPNs
        (first MPNs) (identity previous-morphetic))
      MIDI-step-index morphetic-step-index))))

#|
\noindent Example:
\begin{verbatim}
(setq
 half-states
 '(((1 (60 72 79 84 88) (60 67 71 74 76))
    (NIL NIL "C-63-1"
         ((0 35 45 1/2 1 1/2 0) (0 47 52 1/2 1 1/2 1)
          (0 54 56 1/2 0 1/2 2) (0 59 59 1/2 0 1/2 3)
          (0 63 61 1/2 0 1/2 4))))
   ((3/2 NIL NIL)
    (NIL NIL "C-63-1" NIL))
   ((7/4 (54) (57))
    (-6 -3 "C-63-1"
        ((831/4 56 57 1/4 0 208 701))))
   ((2 (28 40 54 60 63) (42 49 57 60 62))
    (-26 -15 "C-63-1"
         ((208 30 42 1 1 209 702)
          (208 42 49 1 1 209 703)
          (208 56 57 1 0 209 704)
          (208 62 60 1 0 209 705)
          (208 65 62 1 0 209 706))))
   ((3 (28 40 52 61 64) (42 49 56 61 63))
    (0 0 "C-63-1"
       ((209 30 42 1 1 210 707)
        (209 42 49 1 1 210 708)
        (209 54 56 1 0 210 709)
        (209 63 61 1 0 210 710)
        (209 66 63 1 0 210 711))))
   ((1 (28 40 56 62 66) (42 49 58 62 64))
    (0 0 "C-63-1"
       ((216 30 42 1 1 217 740)
        (216 42 49 1 1 217 741)
        (216 58 58 1 0 217 742)
        (216 64 62 1 0 217 743)
        (216 68 64 1 0 217 744))))
   ((2 (47 52 56 59) (53 56 58 60))
    (19 11 "C-63-1" 
        ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
         (43 65 62 1/2 0 87/2 197)
         (43 68 64 1 1 44 198))))
   ((5/2 (47 52 56 64 68) (53 56 58 63 65))
    (0 0 "C-63-1"
       ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
        (25 63 61 1 1 26 103)
        (51/2 71 66 1/2 0 26 105)
        (51/2 75 68 1/2 0 26 106))))
   ((3 (66 69) (64 66))
    (19 11 "C-63-1"
        ((230 73 67 3/4 0 923/4 809)
         (230 76 69 3/4 0 923/4 810))))
   ((7/2 (69 73) (66 68))
    (3 2 "C-67-1"
       ((41/2 72 67 1/2 0 21 94)
        (41/2 76 69 1/2 0 21 95))))))
(half-state2datapoints-by-lookup
 0 half-states
 '(500 500 500 500 500) '(0 500 1000 1500 2000 2500))
--> ((0 60 60 500 1) (0 72 67 500 1) (0 79 71 500 0)
     (0 84 74 500 0) (0 88 76 500 0))
\end{verbatim}

\noindent This function increments over the $i$th note
of a half-state, $i = 0, 1,\ldots, n-1$. The function
\nameref{fun:state-note2datapoint-by-lookup} is
applied. |#

(defun half-state2datapoints-by-lookup
       (j half-states state-durs unique-times
	&optional (point-set-idx 3) (i 0)
	(current-state (nth j half-states))
	(n (length (second (first current-state))))
        (datapoint
         (if (< i n)
           (state-note2datapoint-by-lookup
            i j half-states state-durs
            unique-times point-set-idx))))
  (if (equal i n) ()
    (if datapoint
      (cons
       datapoint
       (half-state2datapoints-by-lookup
        j half-states state-durs unique-times
        point-set-idx (+ i 1) current-state n))
      (half-state2datapoints-by-lookup
       j half-states state-durs unique-times
       point-set-idx (+ i 1) current-state n))))

#| Old version (for Racchman/Racchmaninoff-Oct2010),
but new version should be backwards-compatible.
(defun half-state2datapoints-by-lookup
       (j half-states state-durs unique-times
	&optional
	(i 0)
	(current-state (nth j half-states))
	(n (length (second (first current-state))))
        (datapoint
         (if (< i n)
           (state-note2datapoint-by-lookup
            i j half-states state-durs
            unique-times))))
  (if (equal i n) ()
    (if datapoint
      (cons
       datapoint
       (half-state2datapoints-by-lookup
        j half-states state-durs unique-times
        (+ i 1) current-state n))
      (half-state2datapoints-by-lookup
       j half-states state-durs unique-times
       (+ i 1) current-state n))))
|#

#|
\noindent Example:
\begin{verbatim}
(index-of-offtime-by-lookup
 3 28
 '(((1 (60 72 79 84 88) (60 67 71 74 76))
      (NIL NIL "C-63-1"
           ((0 35 45 1/2 1 1/2 0)
            (0 47 52 1/2 1 1/2 1)
            (0 54 56 1/2 0 1/2 2)
            (0 59 59 1/2 0 1/2 3)
            (0 63 61 1/2 0 1/2 4))))
     ((3/2 NIL NIL)
      (NIL NIL "C-63-1" NIL))
     ((7/4 (54) (57))
      (-6 -3 "C-63-1"
          ((831/4 56 57 1/4 0 208 701))))
     ((2 (28 40 54 60 63) (42 49 57 60 62))
      (-26 -15 "C-63-1"
           ((208 30 42 2 1 209 702)
            (208 42 49 1 1 209 703)
            (208 56 57 1 0 209 704)
            (208 62 60 1 0 209 705)
            (208 65 62 1 0 209 706))))
     ((3 (28 40 52 61 64) (42 49 56 61 63))
      (0 0 "C-63-1"
         ((209 30 42 1 1 210 707)
          (209 42 49 1 1 210 708)
          (209 54 56 1 0 210 709)
          (209 63 61 1 0 210 710)
          (209 66 63 1 0 210 711)))))
 '(1/2 1/4 1/4 1) 3)
--> 4
\end{verbatim}

\noindent Given a starting index, a note-index and
some half-states to search through, this function
returns the index of the half-state where the note-
number in question comes to an end. |#

(defun index-of-offtime-by-lookup
       (starting-index note-number half-states
        state-durations &optional
        (point-set-idx 3)
        (j starting-index)
        (n (- (length half-states) 1))
        (note-index
         (position
          note-number
          (second (first (nth j half-states)))))
        (context-durations
         (nth-list-of-lists
          3
          (nth
           point-set-idx
           (second (nth j half-states))))))
  (if (or
       (>= j n)
       (not
        (find
         note-number
         (second (first (nth (+ j 1) half-states)))))
       (<= (nth note-index context-durations)
           (nth j state-durations)))
    (identity j)
    (index-of-offtime-by-lookup
     starting-index note-number half-states
     state-durations point-set-idx (+ j 1) n)))

#| Old version (for Racchman/Racchmaninoff-Oct2010),
but new version should be backwards-compatible.
(defun index-of-offtime-by-lookup
       (starting-index note-number half-states
        state-durations &optional (j starting-index)
        (n (- (length half-states) 1))
        (note-index
         (position
          note-number
          (second (first (nth j half-states)))))
        (context-durations
         (nth-list-of-lists
          3 (fourth (second (nth j half-states))))))
  (if (or
       (>= j n)
       (not
        (find
         note-number
         (second (first (nth (+ j 1) half-states)))))
       (<= (nth note-index context-durations)
           (nth j state-durations)))
    (identity j)
    (index-of-offtime-by-lookup
     starting-index note-number half-states
     state-durations (+ j 1) n)))
|#

#| Older version.
(defun index-of-offtime-by-lookup
       (starting-index note-number half-states
        &optional (beats-in-bar 4)
        (j starting-index)
        (n (- (length half-states) 1))
        (note-index
         (position
          note-number
          (first (first (nth j half-states)))))
        (context-durations
         (nth-list-of-lists
          3 (third (second (nth j half-states)))))
        (min-duration
         (if context-durations
           (min-item context-durations)
           (identity 0))))
  (if (or
       (>= j n)
       (not
        (find
         note-number
         (first (first (nth (+ j 1) half-states)))))
       (<= (nth note-index context-durations)
           (max
            (mod
             (-
              (second
               (first
                (nth (+ j 1) half-states)))
              (second (first (nth j half-states))))
             beats-in-bar)
            min-duration)))
    (identity j)
    (index-of-offtime-by-lookup
     starting-index note-number half-states
     beats-in-bar (+ j 1) n)))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 states
 '(((1 (12 7 5 4))
    (NIL NIL "C-63-1"
         ((0 35 45 1/2 1 1/2 0) (0 47 52 1/2 1 1/2 1)
          (0 54 56 1/2 0 1/2 2) (0 59 59 1/2 0 1/2 3)
          (0 63 61 1/2 0 1/2 4))))
   ((3/2 "rest")
    (NIL NIL "C-63-1" NIL))
   ((7/4 NIL)
    (-6 -3 "C-63-1"
        ((831/4 56 57 1/4 0 208 701))))
   ((2 (12 14 6 3))
    (-26 -15 "C-63-1"
         ((208 30 42 1 1 209 702)
          (208 42 49 1 1 209 703)
          (208 56 57 1 0 209 704)
          (208 62 60 1 0 209 705)
          (208 65 62 1 0 209 706))))
   ((3 (12 12 9 3))
    (0 0 "C-63-1"
       ((209 30 42 1 1 210 707)
        (209 42 49 1 1 210 708)
        (209 54 56 1 0 210 709)
        (209 63 61 1 0 210 710)
        (209 66 63 1 0 210 711))))
   ((1 (12 16 6 4))
    (0 0 "C-63-1"
       ((216 30 42 1 1 217 740)
        (216 42 49 1 1 217 741)
        (216 58 58 1 0 217 742)
        (216 64 62 1 0 217 743)
        (216 68 64 1 0 217 744))))
   ((2 (5 4 3))
    (19 11 "C-63-1"
        ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
         (43 65 62 1/2 0 87/2 197)
         (43 68 64 1 1 44 198))))
   ((5/2 (5 4 8 4))
    (0 0 "C-63-1"
       ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
        (25 63 61 1 1 26 103)
        (51/2 71 66 1/2 0 26 105)
        (51/2 75 68 1/2 0 26 106))))
   ((3 (3))
    (19 11 "C-63-1"
        ((230 73 67 3/4 0 923/4 809)
         (230 76 69 3/4 0 923/4 810))))
   ((7/2 (4))
    (3 2 "C-67-1"
       ((41/2 72 67 1/2 0 21 94)
        (41/2 76 69 1/2 0 21 95))))))
(state-durations-by-beat states 3)
--> (1/2 1/4 1/4 1 1 1 1/2 1/2 1/2).
\end{verbatim}

\noindent This function takes a list of states as its
argument, and returns a list containing the duration
of each state. It is a little more involved than the
function state-durations, because of the nature of
the beat-spacing states. |#

(defun state-durations-by-beat
       (states &optional (beats-in-bar 4)
        (point-set-idx 3)
        (n (length states))
        (result nil)
        (context-durations
         (nth-list-of-lists
          3
          (nth
           point-set-idx (second (first states)))))
        (min-duration
         (if context-durations
           (min-item context-durations)
           (identity 0))) 
        (inter-state-duration
         (if (> n 1)
           (if (equalp
                (first (first (first states)))
                (first (first (second states))))
             beats-in-bar
             (mod
              (-
               (first (first (second states)))
               (first (first (first states))))
              beats-in-bar))))
        (mod-state-duration
         (if (> min-duration beats-in-bar)
           (identity min-duration)
           (identity inter-state-duration))))
  (if (null inter-state-duration)
    (append result (list min-duration))
    (state-durations-by-beat
     (rest states) beats-in-bar point-set-idx (- n 1)
     (append result (list mod-state-duration)))))

#| Old version (for Racchman/Racchmaninoff-Oct2010),
but new version should be backwards-compatible.
(defun state-durations-by-beat
       (states &optional (beats-in-bar 4)
        (n (length states))
        (result nil)
        (context-durations
         (nth-list-of-lists
          3 (fourth (second (first states)))))
        (min-duration
         (if context-durations
           (min-item context-durations)
           (identity 0))) 
        (inter-state-duration
         (if (> n 1)
           (mod
            (-
             (first (first (second states)))
             (first (first (first states))))
            beats-in-bar)))
        (mod-state-duration
         (if (> min-duration beats-in-bar)
           (identity min-duration)
           (identity inter-state-duration))))
  (if (null inter-state-duration)
    (append result (list min-duration))
    (state-durations-by-beat
     (rest states) beats-in-bar (- n 1)
     (append result (list mod-state-duration)))))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 half-states
 '(((1 (60 72 79 84 88) (60 67 71 74 76))
      (NIL NIL "C-63-1"
           ((0 35 45 1/2 1 1/2 0)
            (0 47 52 1/2 1 1/2 1)
            (0 54 56 1/2 0 1/2 2)
            (0 59 59 1/2 0 1/2 3)
            (0 63 61 1/2 0 1/2 4))))
     ((3/2 NIL NIL)
      (NIL NIL "C-63-1" NIL))
     ((7/4 (54) (57))
      (-6 -3 "C-63-1"
          ((831/4 56 57 1/4 0 208 701))))
     ((2 (28 40 54 60 63) (42 49 57 60 62))
      (-26 -15 "C-63-1"
           ((208 30 42 1 1 209 702)
            (208 42 49 1 1 209 703)
            (208 56 57 1 0 209 704)
            (208 62 60 1 0 209 705)
            (208 65 62 1 0 209 706))))
     ((3 (28 40 52 61 64) (42 49 56 61 63))
      (0 0 "C-63-1"
         ((209 30 42 1 1 210 707)
          (209 42 49 1 1 210 708)
          (209 54 56 1 0 210 709)
          (209 63 61 1 0 210 710)
          (209 66 63 1 0 210 711))))
     ((1 (28 40 56 62 66) (42 49 58 62 64))
      (0 0 "C-63-1"
         ((216 30 42 1 1 217 740)
          (216 42 49 1 1 217 741)
          (216 58 58 1 0 217 742)
          (216 64 62 1 0 217 743)
          (216 68 64 1 0 217 744))))
     ((2 (47 52 56 59) (53 56 58 60))
      (19 11 "C-63-1" 
          ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
           (43 65 62 1/2 0 87/2 197)
           (43 68 64 1 1 44 198))))
     ((5/2 (47 52 56 64 68) (53 56 58 63 65))
      (0 0 "C-63-1"
         ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
          (25 63 61 1 1 26 103)
          (51/2 71 66 1/2 0 26 105)
          (51/2 75 68 1/2 0 26 106))))
     ((3 (66 69) (64 66))
      (19 11 "C-63-1"
          ((230 73 67 3/4 0 923/4 809)
           (230 76 69 3/4 0 923/4 810))))
     ((7/2 (69 73) (66 68))
      (3 2 "C-67-1"
         ((41/2 72 67 1/2 0 21 94)
          (41/2 76 69 1/2 0 21 95))))))
(state-note2datapoint-by-lookup
 4 0 half-states '(1/2 1/4 1/4 1 1 1 1/2 1/2 1/2 1/2)
 '(0 1/2 3/4 1 2 3 4 9/2 5 11/2 6))
--> (0 88 76 1/2 0)
\end{verbatim}

\noindent The $i$th note of the $j$th half-state is transformed
into a so-called `datapoint', meaning we find its
ontime (the $j$th element of the partition points),
its MIDI note number, morphetic pitch number,
duration, and voice. |#

(defun state-note2datapoint-by-lookup
       (i j half-states state-durs unique-times
        &optional (point-set-idx 3)
        (current-state (nth j half-states))
        (note-ontime (nth j unique-times))
        (MNN
         (nth i (second (first current-state))))
        (MPN
         (nth i (third (first current-state))))
        (index-in-previous-chord
         (if (and (> j 0) MNN)
           (position
            MNN
            (second
             (first (nth (- j 1) half-states))))))
        (held-over
         (if index-in-previous-chord
           (>=
            (index-of-offtime-by-lookup
             (- j 1) MNN half-states state-durs
             point-set-idx) j)))
        (offtime-state
         (if MNN
           (index-of-offtime-by-lookup
            j MNN half-states state-durs
            point-set-idx)))
        (voice
         (if MNN
           (fifth
            (nth
             i
             (nth
              point-set-idx
              (second current-state)))))))
  (if (and MNN (not held-over)) 
    (list
     note-ontime MNN MPN
     (- (nth (+ offtime-state 1) unique-times)
        (nth j unique-times))
     voice)))

#| Old version (for Racchman/Racchmaninoff-Oct2010),
but new version should be backwards-compatible.
(defun state-note2datapoint-by-lookup
       (i j half-states state-durs unique-times
        &optional
        (current-state (nth j half-states))
        (note-ontime (nth j unique-times))
        (MNN
         (nth i (second (first current-state))))
        (MPN
         (nth i (third (first current-state))))
        (index-in-previous-chord
         (if (and (> j 0) MNN)
           (position
            MNN
            (second
             (first (nth (- j 1) half-states))))))
        (held-over
         (if index-in-previous-chord
           (>=
            (index-of-offtime-by-lookup
             (- j 1) MNN half-states state-durs) j)))
        (offtime-state
         (if MNN
           (index-of-offtime-by-lookup
            j MNN half-states state-durs)))
        (voice
         (if MNN
           (fifth
            (nth
             i (fourth (second current-state)))))))
  (if (and MNN (not held-over)) 
    (list
     note-ontime MNN MPN
     (- (nth (+ offtime-state 1) unique-times)
        (nth j unique-times))
     voice)))
|#

#|
\noindent Example:
\begin{verbatim}
(setq 
 states
 '(((1 (12 7 5 4))
    (NIL NIL "C-63-1"
         ((0 35 45 1/2 1 1/2 0) (0 47 52 1/2 1 1/2 1)
          (0 54 56 1/2 0 1/2 2) (0 59 59 1/2 0 1/2 3)
          (0 63 61 1/2 0 1/2 4))))
   ((3/2 "rest")
    (NIL NIL "C-63-1" NIL))
   ((7/4 NIL)
    (-6 -3 "C-63-1"
        ((831/4 56 57 1/4 0 208 701))))
   ((2 (12 14 6 3))
    (-26 -15 "C-63-1"
         ((208 30 42 1 1 209 702)
          (208 42 49 1 1 209 703)
          (208 56 57 1 0 209 704)
          (208 62 60 1 0 209 705)
          (208 65 62 1 0 209 706))))
   ((3 (12 12 9 3))
    (0 0 "C-63-1"
       ((209 30 42 1 1 210 707)
        (209 42 49 1 1 210 708)
        (209 54 56 1 0 210 709)
        (209 63 61 1 0 210 710)
        (209 66 63 1 0 210 711))))
   ((1 (12 16 6 4))
    (0 0 "C-63-1"
       ((216 30 42 1 1 217 740)
        (216 42 49 1 1 217 741)
        (216 58 58 1 0 217 742)
        (216 64 62 1 0 217 743)
        (216 68 64 1 0 217 744))))
   ((2 (5 4 3))
    (19 11 "C-63-1"
        ((43 56 57 1 1 44 195) (43 61 60 1 1 44 196)
         (43 65 62 1/2 0 87/2 197)
         (43 68 64 1 1 44 198))))
   ((5/2 (5 4 8 4))
    (0 0 "C-63-1"
       ((25 54 56 1 1 26 101) (25 59 59 1 1 26 102)
        (25 63 61 1 1 26 103)
        (51/2 71 66 1/2 0 26 105)
        (51/2 75 68 1/2 0 26 106))))
   ((3 (3))
    (19 11 "C-63-1"
        ((230 73 67 3/4 0 923/4 809)
         (230 76 69 3/4 0 923/4 810))))
   ((7/2 (4))
    (3 2 "C-67-1"
       ((41/2 72 67 1/2 0 21 94)
        (41/2 76 69 1/2 0 21 95))))))
(states2datapoints-by-lookup states 3 60 60)
--> ((0 60 60 1/2 1) (0 72 67 1/2 1) (0 79 71 1/2 0)
     (0 84 74 1/2 0) (0 88 76 1/2 0) (3/4 54 57 1/4 0)
     (1 28 42 1 1) (1 40 49 1 1) (1 54 57 1 0)
     (1 60 60 1 0) (1 63 62 1 0) (2 28 42 1 1)
     (2 40 49 1 1) (2 52 56 1 0) (2 61 61 1 0)
     (2 64 63 1 0) (3 28 42 1 1) (3 40 49 1 1)
     (3 56 58 1 0) (3 62 62 1 0) (3 66 64 1 0)
     (4 47 53 1 1) (4 52 56 1 1) (4 56 58 1/2 0)
     (4 59 60 1/2 1) (9/2 56 58 1/2 1)
     (9/2 64 63 1/2 0) (9/2 68 65 1/2 0)
     (5 66 64 1/2 0) (5 69 66 1 0)
     (11/2 73 68 1/2 0)).
\end{verbatim}

This function applies the function
half-state2datapoint-by-lookup recursively to a list
of states. It is very similar to the function
states2datapoints |#

(defun states2datapoints-by-lookup
       (states &optional (beats-in-bar 4)
        (initial-MNN 60) (initial-MPN 60)
        (half-states
         (create-MIDI&morphetic-numbers
          states initial-MNN initial-MPN))
        (j 0) (n (length half-states))
        (state-durs
         (state-durations-by-beat
          states beats-in-bar))
        (unique-times
         (cons
          0 (fibonacci-list state-durs))))
  (if (equal j n) ()
    (append
     (half-state2datapoints-by-lookup
      j half-states state-durs unique-times)
     (states2datapoints-by-lookup
      states beats-in-bar initial-MNN initial-MPN
      half-states (+ j 1) n state-durs
      unique-times))))
