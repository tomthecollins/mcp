#| Copyright 2008-2013 Tom Collins
   Friday 29 January 2010
   Incomplete

Common things we do to vectors, such as taking
norms, calculating dot products and distance
functions.

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
|#

#|
\noindent Example:
\begin{verbatim}
(dot-product '(0 1 2) '(3 -1 4))
--> 7.
\end{verbatim}

\noindent The dot product of two lists is returned. If
$x = (x_1, x_2,\ldots, x_n)$ and
$y = (y_1, y_2,\ldots, y_n)$, then the output is
$\sum_{i = 1}^n x_i y_i$. Passing lists of different
lengths may lead to errors. |#

(defun dot-product (a-list b-list)
  (my-last
   (fibonacci-list
    (multiply-two-lists a-list b-list))))

#|
\noindent Example:
\begin{verbatim}
(fibonacci-list '(0 1 2 4 8))
--> (0 1 3 7 15)
\end{verbatim}

\noindent The $n$th element of the list returned is
the sum of the previous $n-1$ elements, with the
convention that a sum over an empty set is zero. |#

(defun fibonacci-list (a-list &optional (last-sum 0))
  (if (null a-list) ()
    (let ((next-sum (+ (first a-list) last-sum)))
      (cons
       next-sum
       (fibonacci-list (rest a-list) next-sum)))))

#|
\noindent Example:
\begin{verbatim}
(max-matrix '((4 0 -3) (-2 3 5) (0 0 0) (0 -1 3)))
--> 5
\end{verbatim}

\noindent This function returns the maximum element in
a matrix represented as a list of lists. |#

(defun max-matrix
       (a-matrix-list &optional
        (row-max
         (mapcar
          #'(lambda (x)
              (max-item x)) a-matrix-list)))
  (max-item row-max))

#|
\noindent Example:
\begin{verbatim}
(min-matrix '((4 0 -3) (-2 3 5) (0 0 0) (0 -1 3)))
--> -3
\end{verbatim}

\noindent This function returns the minimum element in
a matrix represented as a list of lists. |#

(defun min-matrix
       (a-matrix-list &optional
        (row-min
         (mapcar
          #'(lambda (x)
              (min-item x)) a-matrix-list)))
  (min-item row-min))

#|
\noindent Example:
\begin{verbatim}
(multiply-two-lists '(4 7 -3) '(8 -2 -3))
--> (32 -14 9)
\end{verbatim}

\noindent Multiplies two lists element-by-element. It
is assumed that elements of list arguments are
numbers, and the list arguments are of the same
length. An empty first (but not second) argument will
be tolerated. |#

(defun multiply-two-lists (a-list b-list)
  (if (null a-list) ()
    (cons (* (first a-list) (first b-list))
          (multiply-two-lists (rest a-list)
                         (rest b-list)))))

#|
\noindent Example:
\begin{verbatim}
(normalise-0-1 '(4 7 -3 2))
--> (7/10 1 0 1/2)
\end{verbatim}

\noindent Normalises data (linearly) to [0 1]. |#

(defun normalise-0-1
       (a-list &optional
        (min-a-list (reduce #'min a-list))
        (max-a-list (reduce #'max a-list)))
  (if (or
       (equal min-a-list max-a-list)
       (and (equal min-a-list 0)
             (equal max-a-list 1)))
    (identity a-list)
    (normalise-0-1-checks-done
     a-list min-a-list max-a-list)))

#|
\noindent Example:
\begin{verbatim}
(normalise-0-1-checks-done '(4 7 -3 2))
--> (7/10 1 0 1/2)
\end{verbatim}

\noindent Normalises data (linearly) to [0 1],
assuming that the data is not constant and that the
min and max are not already 0, 1 respectively. |#

(defun normalise-0-1-checks-done
       (a-list min-a-list max-a-list &optional
        (denom (- max-a-list min-a-list)))
  (mapcar
    #'(lambda (x)
        (/ (- x min-a-list) denom))
    a-list))

#|
\noindent Example:
\begin{verbatim}
(replace-nth-in-list-with-x 3 '(0 52 55 0.5 1) 5.4)
--> (0 52 55 5.4 1)
\end{verbatim}

\noindent This function replaces the nth item of a
list with whatever is supplied as the third
variable. Passing a value for n less than zero or
greater than m, where m is one less than the length of
the list, will result in an error. |#

(defun replace-nth-in-list-with-x
       (n a-list x)
  (append
   (subseq a-list 0 n)
   (list x) (subseq a-list (+ n 1))))
