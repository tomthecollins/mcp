#| Copyright 2008-2013 Tom Collins
   Monday 6 September 2010
   Incomplete

\noindent These functions implement a structural
induction algorithm using merge sort, and with
calculation of difference vectors restricted to the
first $r$ superdiagonals of the dataset's similarity
matrix.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
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
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-merge"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(difference-list-sorted-asc
 '((71 1/2) (143/2 1/2) (72 1/2) (145/2 1/2)))
--> ((1/2 0) (1/2 0) (1/2 0) (1 0) (1 0) (3/2 0))
\end{verbatim}

\noindent For a dataset $D = \{\mathbf{d}_1,\ldots,
\mathbf{d}_n\}$, this function calculates
$(\mathbf{d}_j - \mathbf{d}_i)_{i < j}$ and sorts
the output by lexicographic order. |#

(defun difference-list-sorted-asc (pattern)
  (if (<= (length pattern) 1) ()
    (merge
     'list
     (mapcar
      #'(lambda (x)
          (subtract-two-lists
           x (first pattern)))
      (rest pattern))
     (difference-list-sorted-asc (rest pattern))
     #'vector<vector-t-or-nil)))

#|
\noindent Example:
\begin{verbatim}
(merge-sort-by
 '(((1 -1) 0 60) ((2 5) 1 59) ((1 -3) 3 64)
   ((3 4) 4 61) ((3 4) 0 60) ((3 2) 1 59)
   ((4 1) 3 64)))
--> (((1 -3) 3 64) ((1 -1) 0 60) ((2 5) 1 59)
     ((3 2) 1 59) ((3 4) 0 60) ((3 4) 4 61)
     ((4 1) 3 64))
\end{verbatim}

\noindent As a default, this function applies the
predicate vector$<$vector-car-cdr to merge-sort the
list provided as an argument. |#

(defun merge-sort-by
       (sequence &key (test #'vector<vector-car-cdr))
  (if (null (cdr sequence))
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge
         'list
         (merge-sort-by (subseq sequence 0 half))
         (merge-sort-by (subseq sequence half))
         test))))

#|
\noindent Example:
\begin{verbatim}
(merge-sort-diff-sets-of-datapoints
 '(((1 0) (0 60) (0 64) (4 59)) ((4 7) (0 60) (0 64))
   ((0 16) (0 60) (0 62) (0 64))))
--> ((0 2) (0 4) (0 4) (0 4) (4 -5) (4 -1))
\end{verbatim}

\noindent The argument to this function is a list of
vector-datapoints pairs. The elements of the upper
triangle of the similarity matrix of each set of
datapoints are merge sorted. Duplicates are removed
within similarity matrices but may still occur
between similarity matrices. |#

(defun merge-sort-diff-sets-of-datapoints
       (vector-datapoints-pairs)
  (if (null vector-datapoints-pairs) ()
    (merge
     'list
     (remove-duplicates-sorted-asc
      (difference-list-sorted-asc
       (cdr (first vector-datapoints-pairs))))
     (merge-sort-diff-sets-of-datapoints
      (rest vector-datapoints-pairs))
     #'vector<vector-t-or-nil)))

#|
\noindent Example:
\begin{verbatim}
(remove-duplicates-sorted-asc '(0 0 6 7 8 8 8))
--> (0 6 7 8)
\end{verbatim}

\noindent Consequent elements of a list are checked
for equality to remove duplicates from an already-
sorted list. |#

(defun remove-duplicates-sorted-asc (a-list)
  (if (null a-list) ()
    (if (equalp (first a-list) (second a-list))
      (remove-duplicates-sorted-asc (rest a-list))
      (cons
       (first a-list)
       (remove-duplicates-sorted-asc
        (rest a-list))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 dataset
 '((0 60) (1 61) (2 62) (3 60) (5 60) (5 61) (6 59)
   (6 62) (7 60) (7 63) (8 61)))
(structure-induction-algorithm-r
 dataset 2
 (merge-pathnames
  (make-pathname
   :name "SIA_r example"
   :type "txt")
  *MCStylistic-MonthYear-example-files-results-path*))
--> Writes file to the specified location.
\end{verbatim}

\noindent The first $r$ superdiagonals of the
similarity matrix for the dataset are treated in a
SIA-like fashion to form patterns. For each pattern
$P_i = \{\mathbf{p}_{i,1},\ldots,
\mathbf{p}_{i,l_i}\}$, we calculate the
vector $\mathbf{v} =
\mathbf{p}_{i,j} - \mathbf{p}_{i,1}, 2 \leq j
\leq l_i$. If this vector is not in a growing list,
then its MTP is computed and appended to the
output. |#

(defun structure-induction-algorithm-r
       (dataset r path&name &optional
        (patterns
         (collect-by-cars
          (merge-sort-by
           (subtract&retain-at-fixed-distances
            dataset r))))
        (diff-sets-of-datapoints
         (frequency-count
          (merge-sort-diff-sets-of-datapoints
           patterns) t))
        (flat&freq-first
         (mapcar
          #'(lambda (x)
              (append
               (list (second x)) (first x)))
          diff-sets-of-datapoints))
        (L
         (mapcar
          #'(lambda (x) (rest x))
          (reverse
           (sort-dataset-asc flat&freq-first)))))
  (write-to-file
   (mapcar
    #'(lambda (x)
        (cons
         x (maximal-translatable-pattern x dataset)))
    L)
   path&name))

#|
\noindent Example:
\begin{verbatim}
(subtract&retain-at-fixed-distance
 '((0 60) (0 67) (0 72) (1 62) (2 66) (3 59) (3 62)
   (3 67) (4 69) (11/2 71) (6 60) (6 67) (6 72)) 1)
--> (((0 7) 0 60) ((0 5) 0 67) ((1 -10) 0 72)
     ((1 4) 1 62) ((1 -7) 2 66) ((0 3) 3 59)
     ((0 5) 3 62) ((1 2) 3 67) ((3/2 2) 4 69)
     ((1/2 -11) 11/2 71) ((0 7) 6 60) ((0 5) 6 67))
\end{verbatim}

\noindent A list $(\mathbf{e}_1, \mathbf{e}_2,
\ldots, \mathbf{e}_n)$ is the argument to this
function, and an optional fixed distance $k$. The
pairs $(\mathbf{e}_{k+i} - \mathbf{e}_i,
\mathbf{e}_i)$ are output for $i = 1, 2,\ldots,
n-k$. |#

(defun subtract&retain-at-fixed-distance
       (dataset &optional (fixed-distance 1)
        (first-dataset (first dataset)))
  (if (<= (length dataset) fixed-distance) ()
    (cons
     (cons
      (subtract-two-lists
       (nth fixed-distance dataset) first-dataset)
      first-dataset)
     (subtract&retain-at-fixed-distance
      (rest dataset) fixed-distance))))

#|
\noindent Example:
\begin{verbatim}
(subtract&retain-at-fixed-distances
 '((0 60) (0 67) (0 72) (1 62)) 2)
--> (((0 7) 0 60) ((0 5) 0 67) ((1 -10) 0 72)
     ((0 12) 0 60) ((1 -5) 0 67))
\end{verbatim}

\noindent The function subtract&retain-at-fixed-
distance is applied for $i = 1, 2,\ldots, k$, where
$k$ is the second argument. The output of each
application is appended. |#

(defun subtract&retain-at-fixed-distances
       (dataset &optional (k 1) (i 1))
  (if (> i k) ()
    (append
     (subtract&retain-at-fixed-distance dataset i)
     (subtract&retain-at-fixed-distances
      dataset k (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(vector<vector-car-cdr
 '((2 2) . (1 4)) '((2 2) . (1 3)))
--> NIL
\end{verbatim}

\noindent Applies the function vector$<$vector to the
car of each list (the two arguments), and if equal,
applies the function vector$<$vector-t-or-nil to the
cdr of each list. |#

(defun vector<vector-car-cdr
       (a-list b-list &optional
        (verdict
         (vector<vector (car a-list) (car b-list))))
  (if (string= verdict "equal")
    (vector<vector-t-or-nil (cdr a-list) (cdr b-list))
    verdict))
