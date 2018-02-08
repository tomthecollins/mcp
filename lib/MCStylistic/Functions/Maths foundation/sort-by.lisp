#| Copyright 2008-2013 Tom Collins
   Tuesday 5 August 2008
   Completed Friday 8 August 2008

\noindent The functions in this file culminate in the
function sort-by.


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
(break-fixed-with-sort-by-col '(1 2)
  '((3 4 0 0) (3 4 5 2) (0 4 5 -3)
    (-1 4 5 9) (1 3 6 1) (-1 2 7 0))
  3 "desc")
--> ((3 4 0 0) (-1 4 5 9) (3 4 5 2)
     (0 4 5 -3) (1 3 6 1) (-1 2 7 0))
\end{verbatim}

\noindent This function has as its second argument a
list whose items are themselves lists of m items, and
it is assumed that this list has already been sorted
by certain 'columns' (ascending or descending)
specified in the first argument; a list called fixed.
In these specified 'columns', any ties which persist
between consecutive 'rows' are (potentially) broken,
with a sort by the argument col (default direction is
ascending).

We see in the example above that a sort has already
been conducted by 'columns' one and two, hence the
argument fixed is '(1 2). The function breaks the
specified ties in the given list with a descending
sort by column three. |#

(defun break-fixed-with-sort-by-col
       (fixed a-list col &optional (direction "asc"))
  (let ((a-partition
	 (rows-with-fixed-same-as-1st-row
	  fixed a-list)))
    (if (null a-list) ()
      (append
       (sort-by-col col a-partition direction)
       (break-fixed-with-sort-by-col
        fixed
        (lastn
         (- (length a-list)
            (length a-partition)) a-list)
        col direction)))))

#|
\noindent Example:
\begin{verbatim}
(index-item-1st-doesnt-occur 0 '(0 0 0 -2 4 2))
--> 3
\end{verbatim}

\noindent Taking an item and a list of items as its
arguments, this function returns the index at which
the given item first does not occur, counting from
zero. If the list is constant and equal to the item
then the function returns NIL. |#

(defun index-item-1st-doesnt-occur
       (item a-list &optional (index 0))
  (if (null a-list) ()
    (if (equalp (first a-list) item)
      (index-item-1st-doesnt-occur
       item (rest a-list) (1+ index))
      (identity index))))

#|
\noindent Example:
\begin{verbatim}
(index-equalps-for-pair-list
 '(1 3 4) '((1 7 9 2 1 1) (0 7 9 9 1 0)))
--> (T NIL T)
\end{verbatim}

This function looks for equality (using the function
equalp) in a pair of lists at those indices specified
by a second variable index. |#

(defun index-equalps-for-pair-list (index pair-list)
  (if (null index) ()
    (cons
     (equalp
      (nth
       (first index) (first pair-list))
      (nth (first index) (second pair-list)))
     (index-equalps-for-pair-list
      (rest index) pair-list))))

#|
\noindent Example:
\begin{verbatim}
(max-argmax '(2 4 -2 7/2 4))
--> (4 1)
\end{verbatim}

\noindent This function returns the maximum item in a
nonempty list (assuming all items are of the same
type), as well as the index of that maximum item, 
counting from zero. |#

(defun max-argmax (a-list &optional (i 0) (j 0))
  (if (equal 1 (length a-list))
    (list (first a-list) j)
    (if (< (first a-list) (second a-list))
      (max-argmax (rest a-list) (+ i 1) (+ i 1))
      (max-argmax
       (cons
        (first a-list)
        (rest (rest a-list))) (+ i 1) j))))

#|
\noindent Example:
\begin{verbatim}
(max-item '(2/3 -3 15 2))
--> 15
\end{verbatim}

\noindent This function finds the maximum item in a
nonempty list. It assumes all items are of the same
type; otherwise nonsense output can be produced. |#

(defun max-item (a-list)
  (if (equal 1 (length a-list)) (first a-list)
    (if (< (first a-list) (second a-list))
      (max-item (rest a-list))
      (max-item
       (cons (first a-list) (rest (rest a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(max-nth 0 '((0 3 10) (1 5 2) (0 4 9)))
--> (1 5 2)
\end{verbatim}

\noindent This function returns the maximum item in a
list where all items are themselves lists of $m$
items. In order to find the maximum therefore, it is
necessary to specify the `column' (counting from zero)
by which the search ought to be conducted. |#

(defun max-nth (col a-list)
  (if (equal 1 (length a-list)) (first a-list)
    (if (< (nth col (first a-list))
	   (nth col (second a-list)))
      (max-nth col (rest a-list))
      (max-nth
       col
       (cons (first a-list) (rest (rest a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(max-nth-argmax 2 '((0 3 2) (0 4 8) (0 5 -2) (9 9 2)))
--> ((0 4 8) 1)
\end{verbatim}

\noindent Here we return the maximum item in a list
where all items are themselves lists of m items,
searching by the $n$th `column' counting from zero.
Also returned is the index of the maximum item. |#

(defun max-nth-argmax (col a-list)
  (let* ((*max-nth* (max-nth col a-list))
	 (*argmax*
          (index-item-1st-occurs *max-nth* a-list)))
    (list *max-nth* *argmax*)))

#|
\noindent Example:
\begin{verbatim}
(min-argmin '(0 4 -2 7/2 -2))
--> (-2 2)
\end{verbatim}

\noindent This function returns the minimum item in a
nonempty list (assuming all items are of the same
type), as well as the index of that minimum item, 
counting from zero. |#

(defun min-argmin (a-list)
  (let* ((*min* (min-item a-list))
	 (*argmin*
          (index-item-1st-occurs *min* a-list)))
    (list *min* *argmin*)))

#|
\noindent Example:
\begin{verbatim}
(min-item '(2/3 -3 15 2))
--> -3
\end{verbatim}

\noindent This function finds the minimum item in a
nonempty list. It assumes all items are of the same
type; otherwise nonsense output can be produced. |#

(defun min-item (a-list)
  (if (equal 1 (length a-list)) (first a-list)
    (if (> (first a-list) (second a-list))
      (min-item (rest a-list))
      (min-item
       (cons (first a-list) (rest (rest a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(min-nth 2 '((0 3 10) (1 5 7) (0 4 9)))
--> (1 5 7)
\end{verbatim}

\noindent This function returns the minimum item in a
list where all items are themselves lists of $m$
items. In order to find the minimum therefore, it is
necessary to specify the `column' (counting from zero)
by which the search ought to be conducted. |#

(defun min-nth (col a-list)
  (if (equal 1 (length a-list)) (first a-list)
    (if (> (nth col (first a-list))
	   (nth col (second a-list)))
      (min-nth col (rest a-list))
      (min-nth
       col
       (cons
        (first a-list) (rest (rest a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(min-nth-argmin 1 '((0 3 2) (0 5 -2) (0 0 8) (9 9 2)))
--> ((0 0 8) 2)
\end{verbatim}

\noindent Here we return the minimum item in a list
where all items are themselves lists of $m$ items,
searching by the $n$th `column' counting from zero.
Also returned is the index of the minimum item. |#

(defun min-nth-argmin (col a-list)
  (let* ((*min-nth* (min-nth col a-list))
	 (*argmin*
          (index-item-1st-occurs *min-nth* a-list)))
    (list *min-nth* *argmin*)))

#|
\noindent Example:
\begin{verbatim}
(nos-consecutives-with-fixed
 '(1 3)
 '((7 4 0 2 1) (1 4 -1 2 -9) (3 4 4 1 1)
   (2 -5 0 3 -9) (2 4 5 2 -2) (3 4 4 2 1)
   (1 1 1 1 1)))
--> 2
\end{verbatim}

\noindent Suppose that the items indexed by
$I = (i_1, i_2,\ldots, i_m)$ in a list are $x_1$,
$x_2,\ldots$, $x_m$, and that this is the case for
several such lists, appearing as the first $n$ entries
in some list of lists. Then this function will return
the value $n$. If index is empty, then the length of
the list is returned. This has a bearing on
higher-level functions. |#

(defun nos-consecutives-with-fixed
       (fixed a-list &optional (i 1))
  (if (null fixed) (length a-list)
    (nos-consecutives-with-nonempty-fixed
     fixed a-list i)))

#|
\noindent Example:
\begin{verbatim}
(nos-consecutives-with-nonempty-fixed
 '(1 3)
 '((7 4 0 2 1) (1 4 -1 2 -9) (3 4 4 2 1)
   (2 -5 0 2 -9) (2 4 5 2 -2) (3 4 4 2 1)
   (1 1 1 1 1)))
--> 2
\end{verbatim}

\noindent Here the function assumes a nonempty
$I = (i_1, i_2,\ldots, i_m)$, indexing items $x_1,
x_2,\ldots, x_m$ in some list. This is supposed to be
the case for several such lists, appearing as the
first $n$ entries in some list of lists. This function
will return the value $n$. It may seem unnecessary to
have both the functions nos-consecutives-with-fixed
and nos-consecutives-with-nonempty-fixed, but writing
the two as a single function results in a less
efficient algorithm. |#

(defun nos-consecutives-with-nonempty-fixed
       (nonempty-fixed a-list &optional (i 1))
  (if (or
       (not (test-all-true
             (index-equalps-for-pair-list
              nonempty-fixed (firstn 2 a-list))))
       (equal 1 (length a-list))) (identity i)
    (nos-consecutives-with-nonempty-fixed
     nonempty-fixed (rest a-list) (1+ i))))

#|
\noindent Example:
\begin{verbatim}
(rows-with-fixed-same-as-1st-row
 '(1 3)
 '((7 4 0 2 1) (1 4 -1 2 -9) (3 4 4 1 1)
   (2 -5 0 3 -9) (2 4 5 2 -2) (3 4 4 2 1)
   (1 1 1 1 1)))
--> ((7 4 0 2 1) (1 4 -1 2 -9))
\end{verbatim}

\noindent Suppose that the $i_1$th, $i_2$th,$\ldots$,
$i_m$th items in a list are $x_1, x_2,\ldots, x_m$,
and that this is the case for several such lists
appearing as the first $n$ entries in some list of
lists. Then this function will return those first $n$
entries. |#

(defun rows-with-fixed-same-as-1st-row (fixed a-list)
  (if (null a-list) ()
    (firstn
     (nos-consecutives-with-fixed fixed a-list)
     a-list)))

#|
\noindent Example:
\begin{verbatim}
(sort-by
 '((5 "asc") (0 "asc") (1 "asc") (3 "desc"))
 '((1000 41 500 1 84 1500) (1000 36 500 1 84 1500)
   (1000 41 500 2 84 1500) (0 60 1000 1 84 1000)
   (2500 61 500 1 84 3000) (3000 62 1000 1 84 4000)
   (1500 60 1500 1 84 3000)))
--> ((0 60 1000 1 84 1000) (1000 36 500 1 84 1500)
     (1000 41 500 2 84 1500) (1000 41 500 1 84 1500)
     (1500 60 1500 1 84 3000) (2500 61 500 1 84 3000)
     (3000 62 1000 1 84 4000))
\end{verbatim}

\noindent This code sorts a list of items (where each
item is itself a list of $m$ items). It does so
according to an index (of arbitrary length) consisting
of `column' numbers and the direction in which each
column ought to be sorted. If for example,
\texttt{(0 "asc")} appears before \texttt{(3 "desc")}
in the index, then the list is sorted first by
`column' 0 ascending. And then any ties which emerge
might be broken by sorting among tied sets according
to `column' 3 descending. |#

(defun sort-by
       (by-list	a-list &optional (fixed NIL))
  (if (null by-list) (identity a-list)
    (sort-by
     (rest by-list)
     (break-fixed-with-sort-by-col
      fixed a-list (first (first by-list))
      (second (first by-list)))
     (cons (first (first by-list)) fixed))))

#|
\noindent Example:
\begin{verbatim}
(sort-by-col
 2 '((3 -2 5 0) (4 1 -8 1) (4 1 0 -2) (3 0 0 -1))
 "desc")
--> ((3 -2 5 0) (4 1 0 -2) (3 0 0 -1) (4 1 -8 1))
(sort-by-col
 2 '((3 -2 5 0) (4 1 -8 1) (4 1 0 -2) (3 0 0 -1)))
--> ((4 1 -8 1) (4 1 0 -2) (3 0 0 -1) (3 -2 5 0))
\end{verbatim}

\noindent This code sorts a list of items (where each
item is itself a list of $m$ items) in a specified
direction (e.g. `desc'). If this direction is not
provided, the function sorts in an ascending order.
It assumes all items are of the same type; otherwise
nonsense output can be produced. |#

(defun sort-by-col (col a-list &optional direction)
  (if (equalp direction "desc")
    (sort-by-col-desc col a-list)
    (sort-by-col-asc col a-list)))

#|
\noindent Example:
\begin{verbatim}
(sort-by-col-asc
 2 '((3 -2 5 0) (4 1 -8 1) (4 1 0 -2) (3 0 0 -1)))
--> ((4 1 -8 1) (4 1 0 -2) (3 0 0 -1) (3 -2 5 0))
\end{verbatim}

\noindent This function returns a list (where each
item is itself a list of $m$ items) which is ordered
ascending by a particular `column'. It assumes all
items are of the same type; otherwise nonsense output
can be produced. |#

(defun sort-by-col-asc (col a-list)
  (let ((next-min-argmin (min-nth-argmin col a-list)))
    (if (equal 1 (length a-list))
      (list (first a-list))
      (cons (first next-min-argmin)
	    (sort-by-col-asc
	     col
	     (remove-nth
	      (second next-min-argmin) a-list))))))

#|
\noindent Example:
\begin{verbatim}
(sort-by-col-desc
 2 '((3 -2 -5 0) (4 1 8 1) (4 1 0 -2) (3 0 0 -1)))
--> ((4 1 8 1) (4 1 0 -2) (3 0 0 -1) (3 -2 -5 0))
\end{verbatim}

\noindent This function returns a list (where each
item is itself a list of $m$ items) which is ordered
descending by a particular `column'. It assumes all
items are of the same type; otherwise nonsense output
can be produced. |#

(defun sort-by-col-desc (col a-list)
  (let ((next-max-argmax (max-nth-argmax col a-list)))
    (if (equal 1 (length a-list))
      (list (first a-list))
      (cons (first next-max-argmax)
	    (sort-by-col-desc
	     col
	     (remove-nth
	      (second next-max-argmax) a-list))))))

#|
\noindent Example:
\begin{verbatim}
(sort-items '(8 2 5 0 -6 2) "desc")
--> (8 5 2 2 0 -6)
(sort-items '(8 2 5 0 -6 2))
--> (-6 0 2 2 5 8)
\end{verbatim}

\noindent This code sorts a list of items (non-
destructively) in a specified direction (e.g. `desc').
If this direction is not provided, the function sorts
in an ascending order. It assumes all items are of the
same type; otherwise nonsense output can be
produced. |#

(defun sort-items (a-list &optional direction)
  (if (equalp direction "desc")
    (sort (copy-list a-list) #'>)
    (sort (copy-list a-list) #'<)))

#|
\noindent Example:
\begin{verbatim}
(sort-items-asc '(0 2 4 6 -1 2))
--> (-1 0 2 2 4 6)
\end{verbatim}

\noindent This code sorts a list of items (non-
destructively) in ascending order. It assumes all
items are of the same type; otherwise nonsense output
can be produced. |#

(defun sort-items-asc (a-list)
  (let ((next-min-argmin (min-argmin a-list)))
    (if (equal 1 (length a-list))
      (list (first a-list))
      (cons (first next-min-argmin)
	    (sort-items-asc
	     (remove-nth
	      (second next-min-argmin) a-list))))))

#|
\noindent Example:
\begin{verbatim}
(sort-items-desc '(0 2 4 6 -1 2))
--> (6 4 2 2 0 -1)
\end{verbatim}

\noindent This code sorts a list of items (non-
destructively) in descending order. It assumes all
items are of the same type; otherwise nonsense output
can be produced. |#

(defun sort-items-desc (a-list)
  (let ((next-max-argmax (max-argmax a-list)))
    (if (equal 1 (length a-list))
      (list (first a-list))
      (cons (first next-max-argmax)
	    (sort-items-desc
	     (remove-nth
	      (second next-max-argmax) a-list))))))

#|
\noindent Example:
\begin{verbatim}
(test-all-true '(T NIL T T NIL))
--> NIL
\end{verbatim}

\noindent This code tests whether all of the items in
a list (of Ts and NILs) are in fact Ts. |#

(defun test-all-true (T-NIL-list)
  (if (equalp (length T-NIL-list) 1)
    (if (equalp (first T-NIL-list) T)
      (identity T) (identity NIL))
    (if (equalp (first T-NIL-list) T)
      (and T (test-all-true (rest T-NIL-list)))
      (and NIL (test-all-true (rest T-NIL-list))))))
