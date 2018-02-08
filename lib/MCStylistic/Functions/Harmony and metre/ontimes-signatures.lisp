#| Copyright 2008-2014 Tom Collins
   Monday 15 March 2010
   Incomplete

These lisp functions help with converting between
ontimes and bar/beat numbers, when time signatures
change according to the variable time-sigs-with-
ontimes, which is a list of time signatures in a
piece. In this list of lists: the first item of a
list is a bar number; the second item is the upper
number of the time signature in that bar (specifying
number of beats per bar); the third item is the
lower number of the time signature (specifying the
division used to count time, with 4 for crotchet,
etc.); the fourth item is the corresponding
ontime.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(append-ontimes-to-time-signatures
 '((1 2 4) (2 3 8) (4 3 4) (7 5 8)))
--> ((1 2 4 0) (2 3 8 2) (4 3 4 5) (7 5 8 14))
\end{verbatim}

\noindent This function appends ontimes to rows of
the time-signature table. |#

(defun append-ontimes-to-time-signatures
       (time-signatures &optional
        (ontime 0)
        (i (length time-signatures))
        (ontimes-appended
         (list
          (append
           (first time-signatures) (list 0)))))
  (if (equal i 1)
    (identity ontimes-appended)
    (append-ontimes-to-time-signatures
     (rest time-signatures)
     ontime (- i 1)
     (append
      ontimes-appended
      (list
       (append
        (second time-signatures)
        (list
         (+ (my-last (my-last ontimes-appended))
            (* (- (first (second time-signatures))
                  (first (first time-signatures)))
               (second (first time-signatures))
               (/ 4
                  (third
                   (first
                    time-signatures))))))))))))

#|
\noindent Example:
\begin{verbatim}
(bar&beat-number-of-ontime
 10 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (7 5 8 14)))
--> (5 3 10)
(bar&beat-number-of-ontime
 2 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (7 5 8 14)))
--> (2 1 2)
(bar&beat-number-of-ontime
 2.5 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (7 5 8 14)))
--> (2 1.5 2.5)
\end{verbatim}

\noindent Given an ontime and a time-signature table
(with ontimes appended), this function returns the
bar number and beat number of that ontime. Beat
numbers are expressed in crotchets counting from one
(see the second and third examples above). |#

(defun bar&beat-number-of-ontime
       (ontime time-sigs-with-ontimes &optional
        (n (length time-sigs-with-ontimes))
        (relevant-row
         (if (>= ontime 0)
           (row-of-max-ontime<=ontime-arg
            ontime time-sigs-with-ontimes n)))
        (excess
         (if (>= ontime 0)
           (- ontime (fourth relevant-row))))
        (local-beat*bar
         (if (>= ontime 0)
           (* (second relevant-row)
              (/ 4 (third relevant-row)))))
        (anacrusis-beat
         (if (< ontime 0)
           (-
            (- ontime)
            (*
             (second (first time-sigs-with-ontimes))
             (/
              4
              (third
               (first time-sigs-with-ontimes))))))))
  (if (>= ontime 0)
    (list
     (+ (first relevant-row)
        (floor excess local-beat*bar))
     (+ (rem excess local-beat*bar) 1)
     ontime)
    (list 0 anacrusis-beat ontime)))

#| This version did not work so well for an
anacrusis when the bottom no. was something other
than 4.
(defun bar&beat-number-of-ontime
       (ontime time-sigs-with-ontimes &optional
        (n (length time-sigs-with-ontimes))
        (relevant-row
         (if (>= ontime 0)
           (row-of-max-ontime<=ontime-arg
            ontime time-sigs-with-ontimes n)))
        (excess
         (if (>= ontime 0)
           (- ontime (fourth relevant-row))))
        (local-beat*bar
         (if (>= ontime 0)
           (* (second relevant-row)
              (/ 4 (third relevant-row)))))
        (anacrusis-beat
         (if (< ontime 0)
           (+
            (+
             (second (first time-sigs-with-ontimes))
             ontime)
            1))))
  (if (>= ontime 0)
    (list
     (+ (first relevant-row)
        (floor excess local-beat*bar))
     (+ (rem excess local-beat*bar) 1)
     ontime)
    (list 0 anacrusis-beat ontime)))
|#

#| Old version could not handle ontimes less than
zero.
(defun bar&beat-number-of-ontime
       (ontime time-sigs-with-ontimes &optional
        (n (length time-sigs-with-ontimes))
        (relevant-row
         (row-of-max-ontime<=ontime-arg
          ontime time-sigs-with-ontimes n))
        (excess (- ontime (fourth relevant-row)))
        (local-beat*bar
         (* (second relevant-row)
            (/ 4 (third relevant-row)))))
  (list
   (+ (first relevant-row)
      (floor excess local-beat*bar))
   (+ (rem excess local-beat*bar) 1)
   ontime))
|#

#|
\noindent Example:
\begin{verbatim}
(bar-beat-ontimes
 0 1 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (7 5 8 14)))
--> ((1 1 0) (1 2 1) (2 1 2) (2 2 3) (3 3/2 4)
     (4 1 5) (4 2 6) (4 3 7) (5 1 8) (5 2 9)
     (5 3 10) (6 1 11) (6 2 12) (6 3 13) (7 1 14)
     (7 2 15))
\end{verbatim}

\noindent Given an ontime start, a subdivision of
the crotchet beat and a time-signature table (with
ontimes appended), this function returns the bar and
beat numbers that will be displayed at the foot of
the MIDI table, and the corresponding ontimes. |#

(defun bar-beat-ontimes
       (ontime-start subdivision
	time-sigs-with-ontimes
        &optional (n-times 15)
        (increment (/ 1 subdivision))
        (ontimes
         (increment-by-x-n-times
          increment n-times ontime-start))
        (result nil))
  (if (null ontimes) (identity result)
    (bar-beat-ontimes
     ontime-start subdivision time-sigs-with-ontimes
     n-times increment (rest ontimes)
     (append
      result
      (list
       (bar&beat-number-of-ontime
        (first ontimes) time-sigs-with-ontimes))))))

#|
\noindent Example:
\begin{verbatim}
(increment-by-x-n-times 1/2 3 7.5)
--> (7.5 8.0 8.5 9.0)
\end{verbatim}

\noindent Adds the first argument to the third
(default zero), and continues to do so until the
second argument is exceeded. |#

(defun increment-by-x-n-times
       (x n &optional (initial 0) (i 0)
        (growing-list (list initial)))
  (if (>= i n) (identity growing-list)
    (increment-by-x-n-times
     x n initial (+ i 1)
     (append
      growing-list
      (list
       (+ x (my-last growing-list)))))))

#|
\noindent Example:
\begin{verbatim}
(ontime-of-bar&beat-number
 5 2 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (8 5 8 17)))
--> (5 2 9)
\end{verbatim}

\noindent Given a bar and beat number, and a
time-signature table (with ontimes appended), this
function returns the ontime of that bar and beat
number. |#

(defun ontime-of-bar&beat-number
       (bar beat time-sigs-with-ontimes &optional
        (n (length time-sigs-with-ontimes))
        (relevant-row
         (row-of-max-bar<=bar-arg
          bar time-sigs-with-ontimes n))
        (excess (- bar (first relevant-row)))
        (local-beat*bar
         (* (second relevant-row)
            (/ 4 (third relevant-row)))))
  (list bar beat
        (+ (fourth relevant-row)
           (* excess local-beat*bar)
           (- beat 1))))

#|
\noindent Example:
\begin{verbatim}
(row-of-max-bar<=bar-arg
 4 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (8 5 8 17)))
--> (4 3 4 5)
\end{verbatim}

\noindent Returns the row (in a list of time
signatures) of the maximal bar number less than or
equal to the bar number argument. |#

(defun row-of-max-bar<=bar-arg
       (bar time-sigs-with-ontimes &optional
        (n (length time-sigs-with-ontimes))
        (i 0))
  (if (or (>= i n)
          (< bar
             (first
              (nth i time-sigs-with-ontimes))))
    (if (equalp bar
                (first
		 (first time-sigs-with-ontimes)))
      (first time-sigs-with-ontimes)
      (nth (- i 1) time-sigs-with-ontimes))
    (row-of-max-bar<=bar-arg
     bar time-sigs-with-ontimes n (+ i 1))))

#|
\noindent Example:
\begin{verbatim}
(row-of-max-ontime<=ontime-arg
 7 '((1 2 4 0) (2 3 8 2) (4 3 4 5) (5 5 8 8)))
--> (4 3 4 5)
\end{verbatim}

\noindent Returns the row (in a list of time
signatures) of the maximal ontime less than or equal
to the ontime argument.

2/1/2015. Added handling of negative ontimes
(e.g., representing an anacrusis). |#

(defun row-of-max-ontime<=ontime-arg
       (ontime time-sigs-with-ontimes
        &optional
        (n (length time-sigs-with-ontimes)) (i 0))
  (if (or (>= i n)
          (< ontime
             (fourth
              (nth i time-sigs-with-ontimes))))
    (if (or
         (equalp ontime
                 (fourth
                  (first time-sigs-with-ontimes)))
         (zerop i))
      (first time-sigs-with-ontimes)
      (nth (- i 1) time-sigs-with-ontimes))
    (row-of-max-ontime<=ontime-arg
     ontime time-sigs-with-ontimes n (+ i 1))))
