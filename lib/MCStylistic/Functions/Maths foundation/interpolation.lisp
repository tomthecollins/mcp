#| Copyright 2008-2013 Tom Collins
   Friday 1 October 2010
   Incomplete

\noindent Functions for interpolation.

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
(setq knot-value-pairs1 '((0 0) (1.5 1) (2 3) (4 2)))
(setq knot-value-pairs2 '((0 0) (1 1) (2 3) (4 3)))
(abs-differences-for-curves-at-points
 knot-value-pairs1 knot-value-pairs2)
--> (0 1.0 0 1).
\end{verbatim}

\noindent Two lists of knot-value pairs are provided
as arguments. The `x-values' of the first argument are
interpolated using the second argument. The absolute
difference between these interpolated values and the
actual `y-values' of the first argument is
returned. |#

(defun abs-differences-for-curves-at-points
       (knot-value-pairs1 knot-value-pairs2)
  (mapcar
   #'(lambda (x)
       (abs
        (-
         (linearly-interpolate
          (first x) knot-value-pairs2)
         (second x))))
   knot-value-pairs1))

#|
\noindent Example:
\begin{verbatim}
(index-1st-sublist-item<
 0 '(14 14 14 11 0 0 -1 -2 -2))
--> 6
\end{verbatim}

\noindent This function takes two arguments: a real
number $x$ and a list $L$ of real numbers. It returns
the index of the first element of $L$ which is less
than $x$. |#

(defun index-1st-sublist-item<
       (item a-list &optional (i 0))
  (if (null a-list) ()
    (if (< (first a-list) item) (identity i)
      (index-1st-sublist-item<
       item (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(index-1st-sublist-item>
 0 '(-2 -2 -1 0 0 11 14 14 14))
--> 5.
\end{verbatim}

\noindent This function takes two arguments: a real
number x and a list L of real numbers. It returns the
index of the first element of L which is greater than
x. |#

(defun index-1st-sublist-item>
       (item a-list &optional (i 0))
  (if (null a-list) ()
    (if (> (first a-list) item) (identity i)
      (index-1st-sublist-item>
       item (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq knot-value-pairs '((0 0) (1 1) (2 3) (4 3)))
(linearly-interpolate 1.5 knot-value-pairs)
--> 2.0.
\end{verbatim}

\noindent The second argument is a list of knot-value
pairs. The `x-value' of the first argument is
interpolated to give a `y-value' using the knot-value
pairs. If the first argument exceeds the endpoints,
the appropriate endpoint value is returned. |#

(defun linearly-interpolate
       (x knot-value-pairs &optional
        (result
         (if (<= x (first (first knot-value-pairs)))
           (second (first knot-value-pairs))
           (if (>= x
                   (first (my-last knot-value-pairs)))
             (second (my-last knot-value-pairs)))))
        (index
         (if (not result)
           (index-1st-sublist-item>
            x
            (nth-list-of-lists 0 knot-value-pairs)))))
  (if index
    (let ((x0
           (first (nth (- index 1) knot-value-pairs)))
          (x1 (first (nth index knot-value-pairs)))
          (f0
           (second
            (nth (- index 1) knot-value-pairs)))
          (f1 (second (nth index knot-value-pairs))))
      (+ f0 (* (/ (- x x0) (- x1 x0)) (- f1 f0))))
    result))

#|
\noindent Example:
\begin{verbatim}
(setq knot-value-pairs '((0 0) (1 1) (2 3) (4 3)))
(linearly-interpolate-x-values
 '(1.5 2 1.75) knot-value-pairs)
--> (2.0 3 2.5).
\end{verbatim}

\noindent The second argument is a list of knot-value
pairs. The first argument is a list of `x-values' that
are interpolated to give `y-values' using the knot-
value pairs. If any members of the first argument
exceeds the endpoints, the appropriate endpoint value
is returned. |#

(defun linearly-interpolate-x-values
       (x-values knot-value-pairs)
  (mapcar
   #'(lambda (x)
       (linearly-interpolate x knot-value-pairs))
   x-values))


