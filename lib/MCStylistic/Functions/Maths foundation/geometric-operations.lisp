#| Copyright 2008-2013 Tom Collins
   Tuesday 11 May 2010
   Incomplete

\noindent The functions below aim towards computing
the convex hull of a set of points in the plane
according to the Graham scan algorithm.

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
   :name "set-operations"
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
(setq a-list '((-4 4) (-2 -2) (-2 2) (0 0) (1 1)
               (2 -2) (2 4) (6 2)))
(convex-hull a-list)
--> ((-2 -2) (2 -2) (6 2) (2 4) (-4 4))
\end{verbatim}

\noindent For a set of points in the plane, this
function returns those points that lie on the convex
hull, using the Graham scan algorithm. Passing an
empty set of points to this function will result in
an error. |#

(defun convex-hull
       (a-list &optional
        (first-point
         (min-y-coord a-list))
        (sorted-points
         (append
          (rest
           (sort-by
            '((2 "asc") (1 "asc") (0 "asc"))
            (mapcar
             #'(lambda (x)
                 (append
                  x
                  (list
                   (atan
                    (- (second x)
                       (second first-point))
                    (- (first x)
                       (first first-point))))))
             a-list)))
          (list first-point)))
        (result (list first-point))
        (i 0))
  (if (<= (length sorted-points) 1)
    (identity result)
    (if (minusp
         (counter-clockwisep
          (list
           (nth i result)
           (first sorted-points)
           (second sorted-points))))
      (convex-hull
       a-list first-point (rest sorted-points)
       result i)
      (convex-hull
       a-list first-point (rest sorted-points)
       (append
        result
        (list
         (butlast (first sorted-points))))
       (+ i 1)))))
#|
\noindent Example:
\begin{verbatim}
(counter-clockwisep '((-2 -2) (2 -2) (1 1)))
--> -1
\end{verbatim}

\noindent This function takes three points in the
plane as its argument, p1, p2, and p3, arranged in a
single list. If travelling along the line from p1 to
p2, turning next to p3 means turning counter-
clockwise, then 1 is the value returned. If clockwise
then -1 is returned, and if collinear then 0 is
returned. |#

(defun counter-clockwisep (a-list3pairs)
  (signum
   (-
    (*
     (- (first (second a-list3pairs))
        (first (first a-list3pairs)))
     (- (second (third a-list3pairs))
        (second (first a-list3pairs))))
    (*
     (- (second (second a-list3pairs))
        (second (first a-list3pairs)))
     (- (first (third a-list3pairs))
        (first (first a-list3pairs)))))))

#|
\noindent Example:
\begin{verbatim}
(dot-adjacent-points
 '((-1 -3) (1 1) (-2 -1) (-1 -3)))
--> (-4 -3 5)
\end{verbatim}

This function takes adjacent pairs from the argument
list and computes their dot product. |#

(defun dot-adjacent-points
       (a-list)
  (if (<= (length a-list) 1) ()
    (cons
      (+ (* (first (first a-list))
            (first (second a-list)))
         (* (second (first a-list))
            (second (second a-list))))
      (dot-adjacent-points (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(setq closed-vertices
      '((-2 -2) (2 -2) (6 2) (2 4) (-4 4) (-2 -2)))
(in-polygonp '(1 1) closed-vertices)
--> T
\end{verbatim}

\noindent A point in the plane and a list of closed,
adjacent vertices are supplied as arguments. T is
returned if the point is inside or on the polygon, and
nil otherwise. |#

(defun in-polygonp
       (probe-pair closed-vertices &optional
        (translated-vertices
         (translation
          closed-vertices
          (multiply-list-by-constant probe-pair -1)))
        (quad
         (mapcar
          #'(lambda (x)
              (quadrant-number x))
          translated-vertices))
        (sign-adj-detsx2
         (multiply-list-by-constant
          (signum-adjacent-determinants
           translated-vertices) 2))
        (dot-adj-points
         (dot-adjacent-points translated-vertices))
        (diff-quad
         (substitute-index-by-index-abs-x
          (substitute
           1 -3
           (substitute -1 3 (spacing-items quad)))
          sign-adj-detsx2 2))
        (in-result
         (not
          (zerop
           (my-last (fibonacci-list diff-quad)))))
        (on-result
         (if (find
              1
              (multiply-two-lists
               (mapcar
                #'(lambda (x)
                    (if (zerop x)
                      (identity 1) (identity 0)))
                sign-adj-detsx2)
               (mapcar
                #'(lambda (x)
                    (if (<= x 0)
                      (identity 1) (identity 0)))
                dot-adj-points)))
           (identity T))))
  (or in-result on-result))

#|
\noindent Example:
\begin{verbatim}
(min-y-coord '((-4 4) (-2 -2) (-2 2) (2 -2) (6 2)))
--> (-2 -2)
\end{verbatim}

This function returns the point with the minimum
y-coordinate, where the argument is assumed to be in
the form '((x1 y1) ... (xn yn)). Ties are broken
using the x-coordinate. |#

(defun min-y-coord (a-list)
  (if (equal 1 (length a-list)) (first a-list)
    (if (or
         (< (second (first a-list))
            (second (second a-list)))
         (and
          (equal (second (first a-list))
                 (second (second a-list)))
          (< (first (first a-list))
             (first (second a-list)))))
      (min-y-coord (cons (first a-list)
                    (rest (rest a-list))))
      (min-y-coord (rest a-list))
      )))

#|
\noindent Example:
\begin{verbatim}
(points-in-convex-hull
 '((-1.71 -1.13) (1.27 -3.95) (3.66 -2.05)
   (-2.65 -3.48) (1.4 -2.94) (1.53 0.51) (-2.67 0.32))
 '((-1.33 0.3) (-1.3 -4.0) (0.83 1.41) (1.83 2.89)
   (1.85 -0.94) (2.22 -2.93) (2.34 2.81) (2.4 -0.15)
   (2.49 -2.71)))
--> ((-1.33 0.3) (1.85 -0.94) (2.22 -2.93)
     (2.49 -2.71))
\end{verbatim}

This function takes two sets of points in the plane
as its arguments. The convex hull is found for the
first set. It is then determined for each member of
the second set whether or not that member is inside 
(or on) the convex hull or not. The points in the
convex hull are returned. There is a plot for the
above example in the Example Files folder, entitled
convex_hull.pdf. |#

(defun points-in-convex-hull
       (a-list b-list &optional
        (K (convex-hull a-list))
        (closed-vertices
         (append K (list (first K))))
        (in-verdicts
         (mapcar
          #'(lambda (x)
              (in-polygonp x closed-vertices))
          b-list)))
  (if (null b-list) ()
   (append
    (if (first in-verdicts)
      (list (first b-list)))
    (points-in-convex-hull
     a-list (rest b-list) K closed-vertices
     (rest in-verdicts)))))

#|
\noindent Example:
\begin{verbatim}
(quadrant-number '(-4 4))
--> (-2 -2)
\end{verbatim}

This function returns the quadrant number of the
plane point (x y) supplied as argument. |#

(defun quadrant-number (pair)
  (if (> (first pair) 0)
    (if (> (second pair) 0) (identity 1) (identity 4))
    (if (> (second pair) 0)
      (identity 2) (identity 3))))

#|
\noindent Example:
\begin{verbatim}
(signum-adjacent-determinants
 '((-1 -3) (1 1) (-2 -1) (-1 -3)))
--> (1 1 1)
\end{verbatim}

This function takes adjacent pairs from the argument
list and computes the sign of the determinant, as
though the pairs were in a 2x2 matrix. |#

(defun signum-adjacent-determinants (a-list)
  (if (<= (length a-list) 1) ()
    (cons
     (signum
      (- (* (first (first a-list))
            (second (second a-list)))
         (* (second (first a-list))
            (first (second a-list)))))
     (signum-adjacent-determinants (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(spacing-items '(0 12 1 7 4))
--> (12 -11 6 -3)

A list of numbers is the only argument. The intervals
between adjacent numbers are returned. It is possible
to produce nonsense output if null values are
interspersed with non-null values. |#

(defun spacing-items (a-list)
  (if (equal (length a-list) 1) ()
    (cons
     (- (second a-list)
	(first a-list))
     (spacing-items (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(substitute-index-by-index-abs-x
 '(-4 4 -2 2 6 2) '(3 5 10 12 7 13) 2) 
--> (-4 4 10 12 6 13)
\end{verbatim}

This function is very specific. When the absolute
value of the ith item of the first argument is equal
to the third argument, that item is replaced in the
output with the ith item of the second argument. |#

(defun substitute-index-by-index-abs-x
       (a-list b-list x)
  (if (null a-list) ()
    (cons
     (if (equal (abs (first a-list)) x)
       (first b-list) (first a-list))
     (substitute-index-by-index-abs-x
      (rest a-list) (rest b-list) x))))
