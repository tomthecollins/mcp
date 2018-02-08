#| Copyright 2008-2013 Tom Collins
   Thursday 13/1/2010
   Incomplete

\noindent These functions enable the formation of
unions and intersections over lists that represent
finite sets in n-dimensional space. It is also
possible to find translators of a pattern in a
dataset. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

(defvar *equality-tolerance* 1e-5)

#|
\noindent Example:
\begin{verbatim}
(add-two-lists '(4 7 -3) '(8 -2 -3))
--> (12 5 -6)
\end{verbatim}

\noindent Adds two lists element-by-element. It is
assumed that elements of list arguments are numbers,
and the list arguments are of the same length. An
empty first (but not second) argument will be
tolerated. |#

(defun add-two-lists (a-list b-list)
  (if (null a-list) ()
    (cons (+ (first a-list) (first b-list))
          (add-two-lists (rest a-list)
                         (rest b-list)))))

#|
\noindent Example:
\begin{verbatim}
(check-potential-translators
 '(3 52) '((0 0) (1 2) (1 5) (2 7))
 '((0 60) (3 52) (4 57) (5 59)))
--> ((0 0) (1 5) (2 7))
\end{verbatim}

\noindent Checks whether the first argument, when
translated by each member of the second argument, is a
member of the third argument. Members of the second
argument that satisfy this property are returned. |#

(defun check-potential-translators
       (patternpoint potential-translators dataset)
  (if (null potential-translators) ()
    (append
     (test-equal<potential-translator
      dataset patternpoint
      (first potential-translators))
     (check-potential-translators
      patternpoint (rest potential-translators)
      dataset))))

#|
\noindent Example:
\begin{verbatim}
(equal-up-to-tol '(2 2 4 5) '(2 2 4 4.501) 1/2)
--> T
(equal-up-to-tol '(2 2.5 4 5) '(2 2 4 5) 1/2)
--> T
(equal-up-to-tol '(2 2 4.5 5) '(2 2 4 5) 1/3)
--> NIL
\end{verbatim}

\noindent This function compares two lists for
equality, up to a given tolerance. |#

(defun equal-up-to-tol
       (a-list b-list &optional
        (tol *equality-tolerance*))
  (if (null a-list) t
    (if (<=
         (abs (- (first b-list) (first a-list))) tol)
      (equal-up-to-tol
       (rest a-list) (rest b-list) tol) ())))

#|
\noidnent Example:
\begin{verbatim}
(insert-retaining-sorted-asc
 '(5 0) '((-6 2) (-4 1) (8 0)))
--> ((-6 2) (-4 1) (5 0) (8 0))
\end{verbatim}

\noindent Two arguments are supplied to this function:
a (real) vector and a strictly-ascending list of
(real) vectors (of the same dimension). The first 
argument is included in the second and output, so that
it remains a strictly-ascending list of vectors. (Note
this means that if the first argument is already in
the list, then this list is output unchanged.) |#

(defun insert-retaining-sorted-asc
       (insert a-list &optional
	(front-list nil)
	(insert-state
	 (if (null a-list)
	   (identity "maximal-element")
	   (vector<vector
	    insert (first a-list)))))
  (if insert-state
    (append
     front-list
     (if (not (equalp insert-state "equal"))
       (list insert))
     a-list)
    (insert-retaining-sorted-asc
     insert (rest a-list)
     (append front-list (list (first a-list))))))

#|
\noindent Example:
\begin{verbatim}
(intersection-multidimensional
 '((4 8 8) (4 7 6) (5 -1 0) (2 0 0))
 '((4 6 7) (2 0 0) (4 7 6)))
--> ((4 7 6) (2 0 0))
\end{verbatim}

\noindent Like the built-in Lisp function
intersection, this function returns the intersection
of two lists. Unlike the built-in Lisp function, this
function handles lists of lists. |#

(defun intersection-multidimensional
       (a-list b-list)
  (if (null a-list) ()
    (append
     (if (test-equal<list-elements
          b-list
          (first a-list))
       (list (first a-list)))
     (intersection-multidimensional
      (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(intersections-multidimensional
 '(((4 8 8) (4 7 6) (5 -1 0) (2 0 0))
   ((4 6 7) (2 0 0) (4 7 6))
   ((4 7 6) (2 1 0) (5 -1 0) (5 0 5))))
--> ((4 7 6))
\end{verbatim}

\noindent The single argument to this function
consists of n lists of lists (of varying length).
Their intersection is calculated and returned. |#

(defun intersections-multidimensional
       (datasets &optional
        (is-nullity
         (null-list-of-lists datasets))
        (dataset (first datasets))
        (current-intersection dataset))
  (if (or (identity is-nullity)
          (null current-intersection)) ()
    (if (null dataset)
      (identity current-intersection)
      (intersections-multidimensional
       (rest datasets)
       is-nullity
       (second datasets)
       (intersection-multidimensional
        current-intersection
        dataset)))))

#|
\noindent Example:
\begin{verbatim}
(null-list-of-lists
 '(((4 8 8) (4 7 6) (5 -1 0) (2 0 0))
   ()
   ((4 7 6) (2 1 0) (5 -1 0) (5 0 5))))
--> T
\end{verbatim}

\noindent The single argument to this function
consists of n lists of lists (of varying length). If
any one of these lists is empty then the value T is
returned. Otherwise the value NIL is returned. Note
that a null argument gives the output NIL. |#

(defun null-list-of-lists
       (list-of-lists &optional
        (n (length list-of-lists))
        (i 0))
  (if (equal i n) ()
    (if (null (first list-of-lists))
      (identity t)
      (null-list-of-lists
       (rest list-of-lists)
       n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(set-difference-multidimensional-sorted-asc
 '((-1 1) (0 1) (1 1) (2 3) (4 -4) (4 3))
 '((-1 1) (0 1) (2 3) (3 2) (4 3)))
--> ((1 1) (4 -4))
\end{verbatim}

\noindent This function computes the set difference
$A\backslash B = \{ a \in A \mid a \notin B \}$ for
point sets. |#

(defun set-difference-multidimensional-sorted-asc
       (a-list b-list)
  (if (null a-list) ()
    (append
     (if (not (test-equal<list-elements
	       b-list (first a-list)))
       (list (first a-list)))
     (set-difference-multidimensional-sorted-asc
      (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(sort-dataset-asc
 '((1 1) (0 1) (4 4) (0 1) (1 1) (-2 3) (4 4) (4 3)))
--> ((-2 3) (0 1) (0 1) (1 1)
     (1 1) (4 3) (4 4) (4 4))
\end{verbatim}

\noindent This function takes one argument: a dataset.
It sorts the dataset ascending by each dimension in
turn. By the definition of `dataset', the dataset
should not contain repeated values. If it does these
will not be removed. |#

(defun sort-dataset-asc
       (sequence &optional
        (fn #'vector<vector-t-or-nil))
  (if (null (cdr sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge
         'list
         (sort-dataset-asc (subseq sequence 0 half))
         (sort-dataset-asc (subseq sequence half))
         fn))))

#|
\noindent Example:
\begin{verbatim}
(subset-multidimensional
 '((2 56) (6 60)) '((0 62) (2 56) (6 60) (6 72)))
--> T
\end{verbatim}

\noindent This function returns T if and only if the
first argument is a subset of the second, and it is
assumed that the second list is sorted ascending. |#

(defun subset-multidimensional
       (a-list b-list)
  (if (null a-list) (identity t)
    (if (test-equal<list-elements
	 b-list (first a-list))
      (subset-multidimensional
       (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(subtract-list-from-each-list
 '((8 -2 -3) (4 6 6) (0 0 0) (4 7 -3)) '(4 7 -3))
--> ((4 -9 0) (0 -1 9) (-4 -7 3) (0 0 0))
\end{verbatim}

\noindent The function subtract-two-lists is applied
recursively to each sublist in the first list
argument, and the second argument. |#

(defun subtract-list-from-each-list (a-list b-list)
  (if (null a-list) ()
    (cons
     (subtract-two-lists (first a-list) b-list)
     (subtract-list-from-each-list
      (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(subtract-two-lists '(4 7 -3) '(8 -2 -3))
--> (-4 9 0)
\end{verbatim}

\noindent Subtracts the second list from the first,
element-by-element. It is assumed that elements of
list arguments are numbers, and the list arguments are
of the same length. An empty first (but not second)
argument will be tolerated. |#

(defun subtract-two-lists (a-list b-list)
  (mapcar #'(lambda (x y) (- x y)) a-list b-list))

#|
Old version.
(defun subtract-two-lists (a-list b-list)
  (if (null a-list) ()
    (cons
     (- (first a-list) (first b-list))
     (subtract-two-lists
      (rest a-list) (rest b-list)))))
|#

#|
\noindent Example:
\begin{verbatim}
(test-equal<list-elements
 '((0 1) (0 2) (1 1) (3 1/4)) '(1 1))
--> T
\end{verbatim}

\noindent The first argument is a list of sublists,
assumed to be sorted ascending by each of its elements
in turn. We imagine it as a set of vectors (all
members of the same n-dimensional vector space). The
second argument v (another list) is also an n-
dimensional vector. If v1 is less than v2, the first
element of the first element of the first argument
then NIL is returned, since we know the list is sorted
ascending. Otherwise each item is checked for
equality. |#

(defun test-equal<list-elements
       (a-list a-vector &optional
        (i 0)
	(v1 (first a-vector))	
	(ith-a-list (nth i a-list))
	(v2 (if (null ith-a-list)
	      (identity v1)
	      (first (nth i a-list)))))
  (if (< v1 v2) ()
    (if (null ith-a-list) ()
      (if (equal a-vector ith-a-list)
	(identity T)
	(test-equal<list-elements
	 a-list a-vector (+ i 1) v1)))))

#|
\noindent Example:
\begin{verbatim}
(test-equal<potential-translator
 '((0 1) (0 2) (1 2) (3 1/4)) '(0 1) '(1 1))
--> ((1 1))
\end{verbatim}

\noindent This function is very similar in spirit to
test-equal<list-elements. The first argument here is a
dataset, the second is a member of some pattern (so
also a member of the dataset), and the third is a
potential translator of the patternpoint. If the
potential translator is really a translator, it is
returned, and NIL otherwise. |#

(defun test-equal<potential-translator
       (dataset patternpoint potential-translator
        &optional
        (sum
         (add-two-lists
          patternpoint potential-translator))
        (i 0)
	(v1 (first sum))	
	(ith-a-list (nth i dataset))
	(v2 (if (null ith-a-list)
	      (identity v1)
	      (first (nth i dataset)))))
  (if (< v1 v2) ()
    (if (null ith-a-list) ()
      (if (equal sum ith-a-list)
	(list potential-translator)
	(test-equal<potential-translator
	 dataset patternpoint potential-translator sum
         (+ i 1) v1)))))

#|
\noindent Example:
\begin{verbatim}
(test-translation
 '((2 2) (4 5)) '((11 6) (13 9)))
--> T
\end{verbatim}

\noindent If the first argument to this function, a
list, consists of vectors of uniform dimension that
are the pairwise translation of vectors in another
list (the functions second argument), then T is
returned, and nil otherwise. The length of the vectors
is checked first for equality, then passed to the
function test-translation-no-length-check if equal.

1/12/2009. NB There is an inefficiency in length
checking when using assoc/rassoc, as the length of the
probe has to be recalculated for each check. |#

(defun test-translation (a-list b-list)
  (if (equal (length a-list) (length b-list))
    (test-translation-no-length-check
     a-list b-list)))

#|
\noindent Example:
\begin{verbatim}
(test-translation-no-length-check
 '((2 2) (4 5)) '((11 6) (13 9)))
--> T
\end{verbatim}

\noindent If the first argument to this function, a
list, consists of vectors of uniform dimension that
are the pairwise translation of vectors in another
list (the function's second argument), then T is
returned, and nil otherwise. The length of the vectors
is not checked for equality. (At present the function
returns T if two empty lists are provided as
arguments.) |#

(defun test-translation-no-length-check
       (a-list b-list &optional
	(last-difference nil)
	(current-difference
	 (if (and a-list b-list)
	   (subtract-two-lists
	    (first a-list)
	    (first b-list)))))
  (if (null current-difference) (identity t)
    (if (or (null last-difference)
	    (equal current-difference
		   last-difference))
      (test-translation-no-length-check
       (rest a-list) (rest b-list)
       current-difference))))

#|
\noindent Example:
\begin{verbatim}
(translation
 '((8 -2 -3) (4 6 6) (4 7 -3)) '(3 1 0))
--> ((11 -1 -3) (7 7 6) (7 8 -3))
\end{verbatim}

\noindent The first argument is a list of sublists,
but we imagine it as a set of vectors (all members of
the same n-dimensional vector space). The second
argument---another list---is also an n-dimensional
vector, and this is added to each of the members of
the first argument. `Added' means vector addition,
that is element-wise, so the resulting set is a
translation of the first argument by the second. |#

(defun translation (a-list b-list)
  (if (or (null a-list) (null b-list)) ()
    (cons
     (add-two-lists (first a-list) b-list)
     (translation (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(translational-equivalence-class
 '((6 2) (7 1/2) (15/2 1/4) (31/4 1/4) (8 1) (9 1))
 '((0 1) (0 4/3) (0 2) (1 1) (4/3 1/3) (5/3 1/3)
   (2 1/2) (2 1) (5/2 1/2) (3 1/2) (3 2) (7/2 1/2)
   (4 1/2) (4 1) (9/2 1/2) (5 1) (6 1) (6 2)
   (7 1/2) (7 1) (15/2 1/4) (31/4 1/4) (8 1) (9 1)
   (9 2) (10 1/2) (10 1) (21/2 1/4) (43/4 1/4)
   (11 1) (12 1) (12 2) (13 1/2) (13 2) (27/2 1/4)
   (55/4 1/4) (14 1) (14 2) (15 1) (16 1/3) (16 2)
   (49/3 1/3) (50/3 1/3) (17 1)))
--> (((6 2) (7 1/2) (15/2 1/4)
      (31/4 1/4) (8 1) (9 1))
     ((9 2) (10 1/2) (21/2 1/4)
      (43/4 1/4) (11 1) (12 1))
     ((12 2) (13 1/2) (27/2 1/4)
      (55/4 1/4) (14 1) (15 1)))
\end{verbatim}

\noindent The function takes two arguments: a pattern
P and a dataset D. It returns the translational
equivalence class of P in D. |#

(defun translational-equivalence-class
       (pattern dataset &optional
	(translators
	 (translators-of-pattern-in-dataset
	  pattern dataset)))
  (if (null translators) ()
    (cons
     (translation pattern (first translators))
     (translational-equivalence-class
      pattern dataset (rest translators)))))

#|
\noindent Example:
\begin{verbatim}
(translations
 '((1 2) (2 4)) '((0 0) (1 2)))
--> (((1 2) (2 4)) ((2 4) (3 6)))
\end{verbatim}

\noindent There are two arguments to this function, a
pattern and some translators. The pattern is
translated by each translator and the results
returned. |#

(defun translations
       (pattern translators)
  (if (null translators) ()
    (cons
     (translation pattern (first translators))
     (translations pattern (rest translators)))))

#|
\noindent Example:
\begin{verbatim}
(translators-of-pattern-in-dataset
 '((8 3) (8 7))
 '((4 7) (8 -3) (8 3) (8 7) (9 -3) (10 7) (11 -3)
   (13 -3) (13 1)))
--> ((0 0) (5 -6))
\end{verbatim}

\noindent A pattern and dataset are provided. The
transaltors of the pattern in the dataset are
returned. I think this function and the functions it
calls are as efficient as possible, but it can still
take a long time to run in large datasets. |#

(defun translators-of-pattern-in-dataset
       (pattern dataset &optional
        (translators
         (subtract-list-from-each-list
          dataset
          (first pattern)))
        (next-translators
         (if (null (second pattern))
           (identity translators)
           (check-potential-translators
            (second pattern) translators dataset))))
  (if (or
       (equal (length translators) 1) 
       (null (first pattern)))
    (identity translators)
    (translators-of-pattern-in-dataset
     (rest pattern)
     dataset
     next-translators)))

#|
\noindent Example:
\begin{verbatim}
(union-multidimensional-sorted-asc
 '((-5 0 4) (-4 3 1) (8 5 3) (8 6 0))
 '((-4 3 1) (-6 2 2) (8 5 0) (8 6 0))
 T)
--> ((-6 2 2) (-5 0 4) (-4 3 1) (8 5 0)
     (8 5 3) (8 6 0))
\end{verbatim}

\noindent Two lists of (real) vectors of the same
dimension are supplied to this function. If the first
is sorted strictly ascending already, a third argument
of T should be supplied to prevent it being sorted so.
The union of these lists is output and remains
strictly ascending. |#

(defun union-multidimensional-sorted-asc
       (a-list b-list &optional
	(is-a-list-sorted nil)
	(a-cup-b
	 (if is-a-list-sorted
	   (identity a-list)
	   (sort-dataset-asc a-list)))
	(first-b (first b-list)))
  (if (null first-b) (identity a-cup-b)
    (union-multidimensional-sorted-asc
     a-list (rest b-list) t
     (insert-retaining-sorted-asc
      first-b a-cup-b))))

#|
\noindent Example:
\begin{verbatim}
(unions-multidimensional-sorted-asc
 '(((12 10) (0 0) (1 2)) ((0 0) (1 5)) ((6 6))))
--> ((0 0) (1 2) (1 5) (6 6) (12 10))
\end{verbatim}

\noindent The function union-multidimensional-sorted-
asc is applied recursively to a list of k-dimensional
vector sets. |#

(defun unions-multidimensional-sorted-asc
       (k-dimensional-sets &optional
	(first-sorted nil)
	(result
	 (if first-sorted
	   (first k-dimensional-sets)
	   (sort-dataset-asc
	    (first k-dimensional-sets)))))
  (if (<= (length k-dimensional-sets) 1)
    (identity result)
    (unions-multidimensional-sorted-asc
     (rest k-dimensional-sets) first-sorted
     (union-multidimensional-sorted-asc
      result (second k-dimensional-sets) t))))

#|
\noindent Example:
\begin{verbatim}
(vector<vector '(4 6 7) '(4 6 7.1))
--> T
\end{verbatim}

\noindent For d = (d1, d2,..., dk),
e = (e1, e2,..., ek), we say that d is less than e,
denoted d < e, if and only if there exists an integer
1 <= j <= k such that dj < ej, and di = ei for
1<= i < j. This function returns true if its first
argument is `less than' its second argument, `equal'
if the two arguments are equal, and nil otherwise. |#

(defun vector<vector (a-vector b-vector)
  (if (null a-vector) (identity "equal")
    (if (< (first a-vector) (first b-vector))
      (identity t)
      (if (equal (first a-vector) (first b-vector))
	(vector<vector
	 (rest a-vector) (rest b-vector))))))

#|
\noindent Example:
\begin{verbatim}
(vector<vector-realp '(4 6 ("yes" "no")) '(4 6 "no"))
--> T
\end{verbatim}

\noindent This function acts like
\nameref{fun:vector<vector-t-or-nil}, returning t if
the first vector is lexicographically less than the
second vector. Unlike
\nameref{fun:vector<vector-t-or-nil}, it will handle
junk (non-real) input. |#

(defun vector<vector-realp (a-vector b-vector)
  (if (null a-vector) ()
    (if (and
         (realp (first a-vector))
         (realp (first b-vector))
         (< (first a-vector) (first b-vector)))
      (identity t)
      (if (equal (first a-vector) (first b-vector))
	(vector<vector-realp
	 (rest a-vector) (rest b-vector))))))

#|
\noindent Example:
\begin{verbatim}
(vector<vector-t-or-nil '(4 6 7) '(4 6 7.1))
--> T
\end{verbatim}

\noindent The function vector<vector returned `equal'
if the arguments were equal. This function returns nil
in such a scenario. |#

(defun vector<vector-t-or-nil (a-vector b-vector)
  (if (null a-vector) ()
    (if (< (first a-vector) (first b-vector))
      (identity t)
      (if (equal (first a-vector) (first b-vector))
	(vector<vector-t-or-nil
	 (rest a-vector) (rest b-vector))))))
