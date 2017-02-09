#| Copyright 2008-2015 Tom Collins
   Monday 15/6/2015
   Incomplete

\noindent These functions support operations on
real-valued intervals. The documentation needs
finishing. They are used by functions in
generating-with-patterns-preliminaries and
c@merata-processing.

DOCUMENTATION REQUIRES COMPLETION. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(combine-contiguous-interval '((0 1) (2 3)) '(3 4))
--> ((0 1) (2 4))
(combine-contiguous-interval '((0 1)) '(3 4))
--> ((0 1) (3 4))
(combine-contiguous-interval '((0 1)) nil)
--> ((0 1))
\end{verbatim}

\noindent Two intervals $(a, b), (c, d)$ are contiguous
if and only if $b = c$. This function takes a pair of
intervals $(e, f), (g, h)$ and returns $(e, h)$ if and
only if $(e, f), (g, h)$ are contiguous, and returns
$(e, f), (g, h)$ otherwise. |#

(defun combine-contiguous-interval
       (intervals new-interval &optional
        (old-end-interval (my-last intervals)))
  (if (null new-interval) intervals
    (if (equalp
         (first new-interval) (second old-end-interval))
      (append
       (butlast intervals)
       (list
        (list
         (first old-end-interval)
         (second new-interval))))
      (append intervals (list new-interval)))))

#|
\noindent Example:
\begin{verbatim}
(combine-contiguous-intervals
 '((0 4) (5 6) (6 7) (9 11.5) (12 12.4) (12.4 16)
   (16 17)))
--> ((0 4) (5 7) (9 11.5) (12 17))
\end{verbatim}

\noindent This function applies the function
\nameref{fun:combine-contiguous-interval} to a list
of intervals. |#

(defun combine-contiguous-intervals
       (intervals &optional (curr-out nil))
  (if (null intervals) curr-out
    (combine-contiguous-intervals
     (rest intervals)
     (combine-contiguous-interval
      curr-out (first intervals)))))

#|
\noindent Example:
\begin{verbatim}
(interval-intersectionp '(0 1) '(1 2))
--> T
\end{verbatim}

\noindent This function returns T if its first argument
$[a, b]$ and its second argument $[c, d]$ have a
nonempty intersection, and nil otherwise. |#

(defun interval-intersectionp (interval1 interval2)
  (or
   (and
    (<= (first interval1) (first interval2))
    (>= (second interval1) (first interval2)))
   (and
    (<= (first interval1) (second interval2))
    (>= (second interval1) (second interval2)))))

#|
\noindent Example:
\begin{verbatim}
--> .
\end{verbatim}

\noindent This function. |#

(defun interval-intersectionsp
       (single-interval intervals &optional (i 0))
  (if (null intervals) ()
    (if (interval-intersectionp
         single-interval (first intervals)) i
      (interval-intersectionsp
       single-interval (rest intervals) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(interval-strict-intersection '(0 1) '(1 2))
--> NIL.
(interval-strict-intersection '(0 1.5) '(1 2))
--> (1 1.5).
(interval-strict-intersection '(0 4) '(1 2))
--> (1 2).

\noindent If the two intervals $[a_i, b_i)$ and
$[a_j, b_j)$ intersect then this function returns
the endpoints of the intersection, e.g.,
$[\max{a_i, a_j}, \min{b_i, b_j}]$, and nil
otherwise. |#

(defun interval-strict-intersection
       (interval1 interval2 &optional
        (the-intersection
         (list
          (max (first interval1) (first interval2))
          (min
           (second interval1) (second interval2)))))
  (if (<
       (first the-intersection)
       (second the-intersection))
    the-intersection nil))

#|
\noindent Example:
\begin{verbatim}
(interval-strict-intersectionp '(24 48) '(18 25))
--> T.
(interval-strict-intersectionp '(18 25) '(24 48))
--> T.
(interval-strict-intersectionp '(18 25) '(18 25))
--> T.
(interval-strict-intersectionp '(18 25) '(18 25))
--> T.
(interval-strict-intersectionp '(18 25) '(25 26))
--> NIL.
(interval-strict-intersectionp '(18 25) '(26 27))
--> NIL.
\end{verbatim}

\noindent If the two intervals $[a_i, b_i)$ and
$[a_j, b_j)$ intersect then this function returns T
and NIL otherwise. |#

(defun interval-strict-intersectionp
       (interval1 interval2)
  (and
   (< (first interval1) (second interval2))
   (> (second interval1) (first interval2))))

#|
\noindent Example:
\begin{verbatim}
(setq intervals '((0 4) (2 3) (-1 2.5)))
(interval-strict-intersections intervals)
--> (2 2.5)
\end{verbatim}

\noindent \noindent If the intervals $[a_1, b_1),
[a_2, b_2),\ldots, [a_n, b_n)$ intersect then this
function returns the endpoints of the intersection,
and nil if it is empty. |#

(defun interval-strict-intersections
       (intervals &optional
        (the-intersection (first intervals)))
  (if (null the-intersection) ()
    (if (<= (length intervals) 1) the-intersection
      (interval-strict-intersections
       (rest intervals)
       (interval-strict-intersection
        the-intersection (second intervals))))))

#|
\noindent Example:
\begin{verbatim}
--> .
\end{verbatim}

\noindent This function. |#

(defun interval-subsetp (interval1 interval2)
  (and
   (>= (first interval1) (first interval2))
   (<= (second interval1) (second interval2))))

#|
\noindent Example:
\begin{verbatim}
--> .
\end{verbatim}

\noindent This function. |#

(defun interval-subsetsp (single-interval intervals)
  (if (null intervals) ()
    (if (interval-subsetp
         single-interval (first intervals)) T
      (interval-subsetsp
       single-interval (rest intervals)))))

