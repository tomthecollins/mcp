#| Copyright 2008-2013 Tom Collins
   Friday 22 October 2010
   Incomplete

\noindent Yes.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
#|
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/generating-beat-MNN-spacing-backwards.lisp"))
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/generating-beat-MNN-spacing-forwards.lisp"))
|#
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "hash-tables"
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
   :directory '(:relative "Maths foundation")
   :name "real-interval-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-merge"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
--> .
\end{verbatim}

\noindent This function. |#

(defun car< (a-list b-list)
  (< (car a-list) (car b-list)))

#|
\noindent Example:
\begin{verbatim}
--> .
\end{verbatim}

\noindent This function. |#

(defun car> (a-list b-list)
  (> (car a-list) (car b-list)))

#|
\noindent Example:
\begin{verbatim}
(setq existing-intervals '((11 17) (20 26) (26 30)))
(setq floor-ontime 1)
(setq ceiling-ontime 40)
(generate-intervals
 floor-ontime ceiling-ontime existing-intervals)
--> ((1 11) (17 20) (30 40)).
\end{verbatim}

\noindent This function.
21/1/2016. Altered to accepted a new optional
beats-in-bar argument. If present, will be used to
filter out any generated intervals that are less than
one bar in length. |#

(defun generate-intervals
       (floor-ontime ceiling-ontime existing-intervals
        &optional (beats-in-bar nil)
        (relevant-index
         (interval-intersectionsp
          (list floor-ontime ceiling-ontime)
          existing-intervals))
        (new-interval
         (if (not
              (interval-subsetsp
               (list floor-ontime ceiling-ontime)
               existing-intervals))
           (if relevant-index
             (if (< floor-ontime
                    (first
                     (nth
                      relevant-index
                      existing-intervals)))
               (list
                floor-ontime
                (first
                 (nth
                  relevant-index existing-intervals)))
               (list
                (second
                 (nth
                  relevant-index existing-intervals))
                (if (nth
                     (+ relevant-index 1)
                     existing-intervals)
                  (min ceiling-ontime
                       (first
                        (nth
                         (+ relevant-index 1)
                         existing-intervals)))
                  ceiling-ontime)))
             (list floor-ontime ceiling-ontime)))))
  (if (or
       (null new-interval)
       (equalp floor-ontime ceiling-ontime)) ()
    (if beats-in-bar
      (if (<
           (-
            (second new-interval) (first new-interval))
           beats-in-bar)
        (generate-intervals
         (second
          (nth (+ relevant-index 1) existing-intervals))
         ceiling-ontime existing-intervals)
        (cons
         new-interval
         (generate-intervals
          (second new-interval) ceiling-ontime
          existing-intervals)))
      (if (equalp
           (first new-interval) (second new-interval))
        (generate-intervals
         (second
          (nth (+ relevant-index 1) existing-intervals))
         ceiling-ontime existing-intervals)
        (cons
         new-interval
         (generate-intervals
          (second new-interval) ceiling-ontime
          existing-intervals))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash2.txt")))
(indices-of-max-subset-score patterns-hash)
--> (3 4).
\end{verbatim}

\noindent Yes. |#

(defun indices-of-max-subset-score
       (patterns-hash &optional
        (subset-score-indices-pair-rating-list
         (unaddressed-patterns-subset-scores
          patterns-hash))
        (top-subset-scores-for-each-pattern
         (mapcar
            #'(lambda (x)
                (list
                 (first
                  (merge-sort-by-car> (first x)))
                 (second x)))
            subset-score-indices-pair-rating-list))
        (scores-rearranged
         (mapcar
            #'(lambda (x)
                (list
                 (list
                  (first (first x))
                  (second x))
                 (second (first x))))
            top-subset-scores-for-each-pattern)))
  (second
   (my-last
    (merge-sort-by-vector<vector-car
     scores-rearranged))))

#|
\noindent Example:
\begin{verbatim}
(merge-sort-by-car<
 '((2 "b") (6 "j") (0 "a") (3 "i") (6 "h")))
--> ((0 "a") (2 "b") (3 "i") (6 "j") (6 "h")).
\end{verbatim}

\noindent Yes. |#

(defun merge-sort-by-car<
       (sequence)
  (if (null (cdr sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge
         'list
         (merge-sort-by-car< (subseq sequence 0 half))
         (merge-sort-by-car< (subseq sequence half))
         #'car<))))

#|
\noindent Example:
\begin{verbatim}
(merge-sort-by-car>
 '((2 "b") (6 "j") (0 "a") (3 "i") (6 "h")))
--> ((6 "j") (6 "h") (3 "i") (2 "b") (0 "a")).
\end{verbatim}

\noindent Yes. |#

(defun merge-sort-by-car>
       (sequence)
  (if (null (cdr sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge
         'list
         (merge-sort-by-car> (subseq sequence 0 half))
         (merge-sort-by-car> (subseq sequence half))
         #'car>))))

#|
\noindent Example:
\begin{verbatim}
(merge-sort-by-vector<vector-car
 '(((1 8.968646) (0 0)) ((0 8.957496) (1 0))
   ((0 8.167285) (2 0)) ((2 3.8855853 (3 4)))))
--> ((6 "j") (6 "h") (3 "i") (2 "b") (0 "a")).
\end{verbatim}

\noindent Yes. |#

(defun merge-sort-by-vector<vector-car
       (sequence)
  (if (null (cdr sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge
         'list
         (merge-sort-by-vector<vector-car
          (subseq sequence 0 half))
         (merge-sort-by-vector<vector-car
          (subseq sequence half))
         #'vector<vector-car))))

#|
\noindent Example:
\begin{verbatim}
(setq input-datapoints '((0 60) (0 72) (8 86)))
(setq MNN-min 41)
(setq MNN-max 79)
(transpose-to-sensible-range
 input-datapoints MNN-min MNN-max)
--> ((0 42) (0 54) (8 68)).
\end{verbatim}

\noindent This function identifies the lowest and
highest MIDI note numbers in the input datapoints.
It compares these to the minimum and maximum MIDI
note numbers specified in the second and third
arguments. If it's possible to transpose the input
datapoints, given the range of the specified
minimum and maximum datapoints, then the function
choses a transposition at random and outputs the
transposed datapoints. |#

(defun transpose-to-sensible-range
       (input-datapoints MNN-min MNN-max
        &optional
        (MNN-idx 1)
        (inp-MNNs
         (nth-list-of-lists
          MNN-idx input-datapoints))
        (inp-min
         (min-item inp-MNNs))
        (inp-max
         (max-item inp-MNNs))
        (range-begin (- MNN-min inp-min))
        (range-end (- MNN-max inp-max))
        (poss-transpositions
         (loop for i from range-begin to range-end
           collect i))
        (rand-trans
         (if poss-transpositions
             (choose-one poss-transpositions)))
        (trans-vec
         (if rand-trans
           (add-to-nth
            rand-trans (+ MNN-idx 1)
            (constant-vector
             0
             (length (first input-datapoints)))))))
  (if trans-vec
    (translation input-datapoints trans-vec)
    (concatenate 'string
     "Could not transpose the datapoints given"
     " range constraints.")))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash2.txt")))
(unaddressed-patterns-subset-scores patterns-hash)
--> ((((1 (0 0)) (1 (0 1)) (1 (0 2)) (1 (0 3)))
      8.968646)
     (((0 (1 0)) (0 (1 1))) 8.957496)
     (((0 (2 0)) (0 (2 1))) 8.167285)
     (((1 (3 0)) (1 (3 1)) (1 (3 2)) (1 (3 3))
       (2 (3 4)) (0 (3 5)) (1 (3 6)) (2 (3 7))
       (2 (3 8)) (1 (3 9)) (2 (3 10))) 3.8855853)).
\end{verbatim}

\noindent Yes. |#

(defun unaddressed-patterns-subset-scores
       (patterns-hash &optional
        (pattern-hash (first patterns-hash)))
  (if (null patterns-hash) ()
    (if (string=
         (gethash
          '"inheritance addressed" pattern-hash) "No")
      (cons
       (list
        (pair-off-lists
         (gethash
          '"subset scores" pattern-hash)
         (gethash
          '"inheritance indices" pattern-hash))
        (gethash '"rating" pattern-hash))
       (unaddressed-patterns-subset-scores
        (rest patterns-hash)))
      (unaddressed-patterns-subset-scores
       (rest patterns-hash)))))
