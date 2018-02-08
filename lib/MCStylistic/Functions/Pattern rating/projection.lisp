#| Copyright 2008-2013 Tom Collins
   Thursday 23 July 2009

The main functions of use here are for creating
projections of datasets \citep{meredith2002}, which
is a precursor to pattern discovery.

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
   :name "set-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(difference-list '((8 -2 -3) (4 6 6) (4 7 -3)))
--> ((-4 8 9) (-4 9 0) (4 -8 -9) (0 1 -9) (4 -9 0)
     (0 -1 9))
\end{verbatim}

\noindent The argument to this function is a list
consisting of sublists of equal lengths. For
$i = 1, 2,\ldots, n$, the $i$th sublist $S$ is removed
from the argument to give a list $L$, and the function
subtract-list-from-each-list is applied. |#

(defun difference-list
       (a-list &optional (n (length a-list)) (i 0))
  (if (equal i n) ()
    (append
     (subtract-list-from-each-list
      (remove-nth i a-list)
      (nth i a-list))
     (difference-list a-list n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(difference-lists
 '((2 1) (2 2)) '((8 -2) (4 6) (4 7)))
--> ((6 -3) (2 5) (2 6) (6 -4) (2 4) (2 5))
\end{verbatim}

\noindent The arguments to this function are two
lists, assumed to be of the same dimension (that is,
each sublist has the same length), but possibly of
different lengths. The difference between each pair of
sublist items is computed and output as a single list. |#

(defun difference-lists
       (a-list b-list &optional (m (length a-list))
        (n (length b-list)) (i 0) (j 0))
  (if (>= i m) ()
    (if (>= j n)
      (difference-lists a-list b-list m n (+ i 1) 0)
      (cons
       (subtract-two-lists
       (nth j b-list) (nth i a-list))
       (difference-lists
        a-list b-list m n i (+ j 1))))))

#|
\noindent Example:
\begin{verbatim}
(index-1st-sublist-item<=
 6 '(14 14 14 11 7 7 6 6 4 1 1 0 0))
--> 6
\end{verbatim}

\noindent This function takes two arguments: a real
number $x$ and a list $L$ of real numbers. It returns
the index of the first element of $L$ which is less
than or equal to $x$. |#

(defun index-1st-sublist-item<=
       (item a-list &optional (i 0))
  (if (null a-list) ()
    (if (<= (first a-list) item) (identity i)
      (index-1st-sublist-item<=
       item (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(index-1st-sublist-item>=
 4 '(0 0 0 1 1 4 6 6 7 7 11 14 14 14))
--> 5
\end{verbatim}

\noindent This function takes two arguments: a real
number $x$ and a list $L$ of real numbers. It returns
the index of the first element of $L$ which is greater
than or equal to $x$. |#

(defun index-1st-sublist-item>=
       (item a-list &optional (i 0))
  (if (null a-list) ()
    (if (>= (first a-list) item) (identity i)
      (index-1st-sublist-item>=
       item (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(is-maximal-translatable-pattern
 '((0 1/2) (1/2 1/2))
 '((0 1/2) (1/2 1/2) (1 1/2) (1 1) (2 3) (5 1/2)
   (11/2 1/2)))
--> (5 0)
\end{verbatim}

\noindent Two arguments are supplied to this function:
a pattern $P$ and a dataset $D$. If $P$ is a maximal
translatable pattern in $D$ for some vector
$\mathbf{u}$, then $\mathbf{u}$ is returned. NIL is
returned otherwise. |#

(defun is-maximal-translatable-pattern
       (pattern dataset &optional
        (vector-differences
         (difference-list dataset)))
  (if (null vector-differences) ()
    (if (equal
         pattern
         (maximal-translatable-pattern
          (first vector-differences) dataset))
      (identity (first vector-differences))
      (is-maximal-translatable-pattern
       pattern dataset
       (rest vector-differences)))))

#|
\noindent Example:
\begin{verbatim}
(maximal-translatable-pattern
 '(2 0)
 '((0 1/2) (0 1) (1 1) (2 1/2) (2 1) (3 2)))
--> ((0 1) (0 1/2))
\end{verbatim}

\noindent This function assumes that the dataset is
sorted ascending. This enables a more efficient search
for the maximal translatable pattern of an arbitrary
vector u, searching in some dataset $D$, defined by
$\MTP(\mathbf{u}, D) = \{ \mathbf{d} \in D :
\mathbf{d} + \mathbf{u} \in D \}$. |#

(defun maximal-translatable-pattern
       (vector dataset &optional
        (dataset-translated
         (mapcar
          #'(lambda (x)
              (add-two-lists x vector))
          dataset))
        (verdict
         (if (and dataset dataset-translated)
           (vector<vector
            (first dataset-translated)
            (first dataset)))))
  (if (null dataset) ()
    (if verdict
      (if (string= verdict "equal")
        (cons
         (subtract-two-lists (first dataset) vector)
         (maximal-translatable-pattern
          vector (rest dataset)
          (rest dataset-translated)))
        (maximal-translatable-pattern
         vector dataset (rest dataset-translated)))
      (maximal-translatable-pattern
       vector (rest dataset) dataset-translated))))

#|
\noindent Example:
\begin{verbatim}
(nth-list-index '(1 1 0 1 0))
--> (0 1 3)
\end{verbatim}

\noindent This function returns the value of the
increment $i$ if the $i$th element of the input list
is equal to 1. |#

(defun nth-list-index
       (indicator-vector &optional (i 0)
        (n (length indicator-vector)))
  (if (null indicator-vector) ()
    (if (equal (first indicator-vector) 1)
      (cons
       (identity i)
       (nth-list-index
        (rest indicator-vector) (+ i 1) n))
      (nth-list-index
       (rest indicator-vector) (+ i 1) n))))

#|
\noindent Example:
\begin{verbatim}
(orthogonal-projection-not-unique-equalp
 '((2 4 -1 6 9) (0 0 4 2 -7) (-3 -2 -1 -1 1)
   (12 0 -7 5 3) (1 2 3 4 3) (1 2 5 4 5))
 '(1 1 0 1 0))
--> ((2 4 6) (0 0 2) (-3 -2 -1) (12 0 5) (1 2 4)
     (1 2 4))
\end{verbatim}

\noindent Given a set of vectors (all members of the
same $n$-dimensional vector space), and an $n$-tuple
of zeros and ones indicating a particular orthogonal
projection, this function returns the projected set of
vectors. |#

(defun orthogonal-projection-not-unique-equalp
       (vector-set projection-indicator &optional
        (nth-list-indexed
         (nth-list-index projection-indicator)))
  (if (null vector-set) ()
    (cons
     (nth-list nth-list-indexed (first vector-set))
     (orthogonal-projection-not-unique-equalp
      (rest vector-set) projection-indicator
      nth-list-indexed))))

#|
\noindent Example:
\begin{verbatim}
(orthogonal-projection-unique-equalp
 '((2 4 -1 6 9) (0 0 4 2 -7) (-3 -2 -1 -1 1)
   (12 0 -7 5 3) (1 2 3 4 3) (1 2 5 4 5)
   (12 0 -6 5 4) (-3 -2 1 -1 0) (12 0 -7 5 4))
 '(1 1 0 1 0))
--> ((2 4 6) (0 0 2) (-3 -2 -1) (12 0 5) (1 2 4))
\end{verbatim}

\noindent Given a set of vectors (all members of the
same $n$-dimensional vector space), and an $n$-tuple
of zeros and ones indicating a particular orthogonal
projection, this function returns the projected set of
vectors. Coincidences are reduced to single
vectors. |#

(defun orthogonal-projection-unique-equalp
       (vector-set projection-indicator &optional
        (projected-not-unique
         (orthogonal-projection-not-unique-equalp
          vector-set projection-indicator)))
  (remove-duplicates
   (sort-dataset-asc projected-not-unique)
    :test #'equalp))

#|
\noindent Example:
\begin{verbatim}
(pair-off-lists '("asc" "asc" "asc") '(0 1 2))
--> (("asc" 0) ("asc" 1) ("asc" 2))
\end{verbatim}

\noindent Two lists A and B of equal length are
provided as arguments to this function. The first
element $a_1$ of $A$ is paired off with the first
element $b_1$ of $B$ to become the first sublist of a
new list, and so on for $a_2$ and $b_2$, $a_3$ and
$b_3$. |#

(defun pair-off-lists (a-list b-list)
  (if (null a-list) ()
    (cons
     (list (first a-list) (first b-list))
     (pair-off-lists
      (rest a-list) (rest b-list)))))

#|
\noindent Example:
\begin{verbatim}
(test-equal<subset '((4 6) (6 5) (6 5) (6 7))
 '((0 1) (0 2) (1 3) (1 4) (4 6) (6 5) (6 7)
   (7 9) (7 10) (11 11) (14 1) (14 3) (14 14)))
--> T
\end{verbatim}

\noindent There are two arguments to this function,
both lists of $n$-tuples. If when written as sets, the
first argument is a subset of the second, then T is
returned. Otherwise NIL is returned (and an empty
first argument is permissible). The $<$ in the
function name indicates that a subfunction,
test-equal$<$list-elements, assumes an argument has
been sorted ascending by each of its elements. |#    

(defun test-equal<subset
       (a-list b-list &optional
        (b-1st-elements
         (nth-list-of-lists 0 b-list))
        (start-search
	 (if (null a-list) ()
	   (index-1st-sublist-item>=
	    (first (first a-list))
	    b-1st-elements)))
        (end-search
	 (if (null a-list) ()
	   (index-1st-sublist-item<=
	    (first (my-last a-list))
	    (reverse b-1st-elements))))
        (c-list
         (if (and (not (null a-list))
                  (not (null start-search))
                  (not (null end-search)))
           (subseq
            b-list
            start-search
            (- (length b-list) end-search)))))
  (if (null a-list) (identity T)
    (if (test-equal<list-elements
         c-list (first a-list))
      (test-equal<subset
       (rest a-list) b-list
       b-1st-elements start-search
       end-search c-list))))
