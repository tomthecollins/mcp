#| Copyright 2008-2013 Tom Collins
   Tuesday 5 February 2013
   Incomplete

\noindent The functions below are metrics for
evaluating the extent to which different ontime-pitch
pairs have been output by an algorithm.

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
   :directory '(:relative "Pattern metrics")
   :name "matching-score"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "projection"
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
   :name "stats-sampling"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
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
(setq
 a-dataset
 '((25 53) (25 69) (53/2 53) (53/2 69) (27 48) (27 52)
   (27 60) (27 67) (28 50) (28 53) (28 65)))
(setq
 b-dataset
 '((25 53) (25 60) (25 69) (53/2 53) (53/2 69) (27 40)
   (27 48) (27 51) (27 60) (27 67) (28 50) (28 53)))
--> 9/12
(cardinality-score a-dataset b-dataset)
(setq
 b-dataset (translation b-dataset '(40 6)))
(cardinality-score a-dataset b-dataset)
--> 0
(cardinality-score a-dataset b-dataset nil t)
--> 9/12
(setq
 b-dataset (translation a-dataset '(1e-5 0)))
(cardinality-score a-dataset b-dataset t)
--> 12/12
\end{verbatim}

\noindent Returns the cardinality score for two point
sets. This is the number of occurrences of the most
frequent difference vector between two point sets
(possibly including an error tolerance), divided by
the maximum cardinality of the two point sets. If the
optional argument translationp is set to false, the
occurrences of the zero vector in the differences is
used as the numerator, i.e., not considering
translations to be equivalent. |#

(defun cardinality-score
       (a-dataset b-dataset &optional
        (tolp nil) (translationp nil)
        (vector&occ
         (if translationp
           (most-frequent-difference-vector
            a-dataset b-dataset tolp translationp)
           (list
            (constant-vector
             0 (length (first a-dataset)))
            (length
             (if tolp
               (intersection
                a-dataset b-dataset :test
                #'equal-up-to-tol)
               (intersection
                a-dataset b-dataset :test
                #'equalp))))))
        (denom
         (max-item
          (list
           (length a-dataset) (length b-dataset)))))
  (/ (second vector&occ) denom))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-dataset
 '((12 45 51 1/2 0) (12 48 53 1/2 0) (25/2 45 51 3/2)
   (25/2 52 55 1/2 0) (13 36 46 1 1) (13 41 49 1)
   (13 53 56 1 0) (14 29 42 1 1) (14 36 46 1 1)))
(setq
 b-dataset
 '((12 45 51 1/2 0) (12 48 53 1/2 0) (25/2 45 51 3/2)
   (25/2 52.0001 55 1/2 0) (13 36 46 1 1) (13 41 49 1)
   (13 53 56 1 0) (14 29 42 1 1) (14 36 46 1 1)))
(setq *equality-tolerance* 1e-2)
(equalp-score a-dataset b-dataset t)
--> T
\end{verbatim}

\noindent This function returns t if the two datasets
provided as arguments are equal. An optional argument
for equality up to tolerance specified by the
variable *equality-tolerance* allows for different
degrees of exactness in testing equality. |#

(defun equalp-score
       (a-dataset b-dataset &optional (tolp nil)
        (max-length
         (if tolp
           (max
            (length a-dataset) (length b-dataset)))))
  (if tolp
    (if (equalp
         (length
          (intersection
           a-dataset b-dataset :test
           #'equal-up-to-tol)) max-length) 1 0)
    (if (equalp a-dataset b-dataset) 1 0)))

#|
\noindent Example:
\begin{verbatim}
(setq P11 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq
 P12 '((21 60) (45/2 60) (23 47) (23 59) (25 57)))
(setq P13 '((41 53) (85/2 53) (43 52) (44 50)))
(setq P1 (list P11 P12 P13))

(setq P21 '((5/2 55) (3 57) (7/2 58) (4 60) (5 48)))
(setq
 P22 '((21/2 55) (43/4 57) (23/2 58) (12 60) (13 48)))
(setq
 P23 '((23/2 58) (12 60) (25/2 62) (13 64) (14 52)))
(setq
 P24 '((27/2 50) (14 52) (29/2 53) (15 55) (16 42)))
(setq P2 (list P21 P22 P23 P24))
(setq P-patt&occ (list P1 P2))

(setq Q11 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq Q12 (translation Q11 '(5 2)))
(setq Q13 '((0 59) (1/2 60) (1 60) (2 62)))
(setq Q14 (translation Q11 '(40 0)))
(setq Q1 (list Q11 Q12 Q13 Q14))

(setq Q21 (butlast (translation Q11 '(8 5))))
(setq Q22 (rest (translation Q12 '(8 5))))
(setq Q23 (rest (translation Q13 '(8 5))))
(setq Q24 (butlast (translation Q14 '(8 5))))
(setq Q2 (list Q21 Q22 Q23 Q24))

(setq
 Q31
 '((67/2 60) (67/2 64) (34 62) (34 65) (69/2 64)
   (69/2 67) (35 57) (35 65) (35 69) (35 72)))
(setq
 Q32
 '((97/2 60) (97/2 64) (49 62) (49 65) (99/2 64)
   (99/2 67) (50 58) (50 66) (50 70) (50 72)))
(setq Q3 (list Q31 Q32))
(setq Q-patt&occ (list Q1 Q2 Q3))

(establishment-matrix P-patt&occ Q-patt&occ)
--> ((1 0 0) (0 0 0))
\end{verbatim}

\noindent The establishment matrix indicates whether
an algorithm is strong at \emph{establishing} that a
pattern $P$ is repeated at least once during a piece.
It does not indicate whether the algorithm is strong
at retrieving all occurrences of $P$ (exact and
inexact). For such a matrix, please see the function
occurrence-matrix.

The first argument to the function establishment-
matrix is a list of patterns, and within each list,
all occurrences of that pattern. These are assumed to
be the ground truth. The second argument has the same
structure as the first, but for the output of some
algorithm. The number of each element in the output
establishment matrix indicates the extent to which
output pattern Qj (and all its occurrences)
constitutes the discovery of ground truth pattern Pi
(and all its occurrences). The are options for
comparison function, error tolerance, and accepting
discoveries up to translation.

For each pair of point sets in the first and
second arguments, this function computes the
cardinality score or matching score (or some other
function for computing symbolic musical similarity).
These scores are returned in a list of lists. Each
lists contains a row of scores for a point set from
P-occurrences, and so the $n$th element of each list
corresponds to a column of scores for a point set from
Q-occurrences. |#

(defun establishment-matrix
       (P-patt&occ Q-patt&occ &optional
        (compare-fn #'cardinality-score) (tolp nil)
        (translationp nil) (card-limit 100)
        (nP (length P-patt&occ)) (i 0)
        (matrix-row
         (if (< i nP)
           (mapcar
            #'(lambda (x)
                (max-matrix
                 (score-matrix
                  (nth i P-patt&occ) x compare-fn tolp
                  translationp card-limit)))
            Q-patt&occ))))
  (if (>= i nP) ()
    (cons
     matrix-row
     (establishment-matrix
      P-patt&occ Q-patt&occ compare-fn tolp
      translationp card-limit nP (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
P-patt&occ and Q-patt&occ defined as in the example
for the function establishment-matrix.
(establishment-metric
 P-patt&occ Q-patt&occ #'precision-matrix)
--> 11/15
(establishment-metric
 P-patt&occ Q-patt&occ #'recall-matrix)
--> 7/10
\end{verbatim}

\noindent This function is a wrapper for calculating
the precision or recall of an establishment matrix.
The establishment matrix indicates whether
an algorithm is strong at \emph{establishing} that a
pattern $P$ is repeated at least once during a piece.
The entry element $(i, j)$ is a scalar between 0 and 1
indicating the performance of algorith-output pattern
$j$ for ground truth pattern $i$. This metric does not
indicate whether the algorithm is strong at retrieving
all occurrences of $P$ (exact and inexact). For such a
metric, please see the function occurrence-metric. |#

(defun establishment-metric
       (P-patt&occ Q-patt&occ &optional
        (prec-rec #'precision-matrix)
        (compare-fn #'cardinality-score) (tolp nil)
        (translationp nil) (card-limit 100)
        (establish-mtx
         (establishment-matrix
          P-patt&occ Q-patt&occ compare-fn tolp
          translationp card-limit)))
  (if (equalp prec-rec #'precision-matrix)
    (precision-matrix establish-mtx)
    (recall-matrix establish-mtx)))

#|
\noindent Example:
\begin{verbatim}
(index-lookup
 '(1/2 60) '((0 59) (1/2 60) (1/2 60) (2 62)))
--> (1 2).
\end{verbatim}

\noindent Returns indices of the second list that are
equal to the first list. |#

(defun index-lookup
       (a b-proj &optional (n (length b-proj)) (i 0))
  (if (>= i n) ()
    (append
     (if (equalp (nth i b-proj) a)
       (list i))
     (index-lookup a b-proj n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(indices-lookup
 '((1/2 60) (2 63))
 '((0 59 59) (1/2 60 60) (1/2 61 60) (2 62 63))
 '(1 0 1))
--> ((1 2) (3))
\end{verbatim}

\noindent Returns indices of the second list that are
equal to members of the first list. If there is more
than one, both matches are returned. It is possible to
focus on certain dimensions by specifying a projection
vector. |#

(defun indices-lookup
       (a-list b-list &optional
        (projection-vector
         (constant-vector 1 (length (first b-list))))
        (b-proj
         (orthogonal-projection-not-unique-equalp
          b-list projection-vector))
        (n (length b-proj)))
  (mapcar
   #'(lambda (x) (index-lookup x b-proj n)) a-list))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-dataset
 '((-1 81) (-3/4 76) (-1/2 85) (-1/4 81) (0 88) (1 57)
   (1 61) (1 64) (2 73) (9/4 69) (5/2 76) (11/4 73)
   (3 81) (4 45) (4 49) (4 52) (4 57) (5 61) (21/4 57)
   (11/2 64) (23/4 61) (6 57) (6 69) (7 54) (7 59)
   (7 63) (7 69) (8 51) (8 59) (8 66) (8 69) (9 52)
   (9 59) (9 66) (9 69) (10 40) (10 64) (10 68)))
(setq
 b-dataset
 '((21 56) (21 62) (21 71) (22 57) (22 61) (22 69)
   (23 81) (93/4 76) (47/2 85) (95/4 81) (24 88)
   (25 57) (25 58) (25 64) (26 73) (105/4 69)
   (53/2 76) (107/4 73) (27 81) (28 45) (28 49)
   (28 52) (28 57) (29 61) (117/4 57) (59/2 64)
   (119/4 61) (30 57) (30 69) (31 54) (31 59) (31 63)
   (31 69) (32 51) (32 59) (32 66) (32 69) (33 52)
   (33 59) (33 66) (33 69) (34 40) (34 64) (34 68)
   (35 80) (141/4 76) (71/2 83) (143/4 80) (36 86)
   (37 52) (37 56) (37 59) (37 62) (38 68) (153/4 64)
   (77/2 71) (155/4 68) (39 74) (40 40) (40 44)
   (40 47) (40 52) (41 62) (165/4 59) (83/2 64)
   (167/4 62) (42 68) (43 52) (43 59) (43 62) (43 68)
   (44 52) (44 59) (44 62) (44 68) (45 45) (45 56)
   (45 62) (45 71) (46 57)))
(matching-score a-dataset b-dataset)
--> 0.6097795
\end{verbatim}

\noindent This function calculates the matching score
histogram defined by \cite{arzt2012}. En route it
calculates two collections of symbolic fingerprints
for the two input datasets. The maximum value in the
histogram corresponds to the time point at which the
two datasets are most similar. Divided by the maximum
possible number of matches (maximum number of symbolic
fingerprints), and taking the square root, this number
can be used to measure the symbolic musical similarity
of the two datasets. |#

(defun matching-score
       (a-dataset b-dataset &optional (card-limit 100)
        (n1 5) (n2 5) (d .05) (trans-invar nil)
        (res 1) (flexitime nil)
        (calcp
         (<=
          (max (length a-dataset) (length b-dataset))
          card-limit))
        (a-fgp
         (if calcp
           (symbolic-fingerprint
            a-dataset "a" n1 n2 d trans-invar)))
        (b-fgp
         (if calcp
           (symbolic-fingerprint
            b-dataset "b" n1 n2 d trans-invar)))
        (hist
         (if calcp
           (matching-score-histogram
            a-fgp b-fgp res flexitime))))
  (if calcp
    (sqrt
     (/
      (max-item hist)
      (max (length a-fgp) (length b-fgp))))
    (progn
      (format
       t
       (concatenate
        'string "Pattern exceeds ~D points, replacing"
        " matching score with cardinality score.~%")
       card-limit)
      (cardinality-score
       a-dataset b-dataset t trans-invar))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '((25 53) (25 69) (53/2 53) (53/2 69) (27 48) (27 52)
   (27 60) (27 67) (28 50) (28 53) (28 65.01)))
(setq
 b-list
 '((25 53) (25 69) (53/2 53) (53/2 69) (27 48) (27 52)
   (27 60) (27 67) (28 50) (28 53) (28 65)))
(most-frequent-difference-vector a-list b-list nil)
--> ((0 0) 10)
(setq
 a-list
 '((25 53) (25 69) (53/2 53) (53/2 69) (27 48) (27 52)
   (27 60) (27 67) (28 50) (28 53) (28 65.00001)))
(most-frequent-difference-vector a-list b-list t)
--> ((0 0) 11)
(setq a-list (translation a-list '(4 5)))
(most-frequent-difference-vector a-list b-list t t)
((-4 -5) 11)
\end{verbatim}

\noindent Returns the most frequent difference vector
(and its total number of occurrences) between two
lists of lists. There are options for allowing
equality up to a given tolerance (specified by the
variable *equality-tolerance*), and for whether the
sets are allowed to be translations of one another.
If not, the count of the zero vector is returned. |#

(defun most-frequent-difference-vector
       (a-list b-list &optional (tolp nil)
        (translationp nil)
        (a-sorted (sort-dataset-asc a-list))
        (b-sorted (sort-dataset-asc b-list))
        (delta-ab
         (difference-lists a-sorted b-sorted))
        (freq-count
         (frequency-count delta-ab tolp))
        (zero-vector
         (if (not translationp)
           (constant-vector
            0 (length (first a-list)))))
        (max-freq-or-index
         (if translationp
           (max-argmax
            (nth-list-of-lists 1 freq-count))
           (if tolp
             (position
              zero-vector
              (nth-list-of-lists 0 freq-count)
              :test #'equal-up-to-tol)
             (position
              zero-vector
              (nth-list-of-lists 0 freq-count)
              :test #'equalp)))))
  (if translationp
    (list
     (first
      (nth (second max-freq-or-index) freq-count))
     (first max-freq-or-index))
    (if max-freq-or-index
      (list
       (first
        (nth max-freq-or-index freq-count))
       (second
        (nth max-freq-or-index freq-count)))
      ;Situation where there are no zero vectors
      (list nil 0))))

#|
\noindent Example:
\begin{verbatim}
(setq P11 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq
 P12 '((21 60) (45/2 60) (23 47) (23 59) (25 57)))
(setq P13 '((41 53) (85/2 53) (43 52) (44 50)))
(setq P1 (list P11 P12 P13))

(setq P21 '((5/2 55) (3 57) (7/2 58) (4 60) (5 48)))
(setq
 P22 '((21/2 55) (43/4 57) (23/2 58) (12 60) (13 48)))
(setq
 P23 '((23/2 58) (12 60) (25/2 62) (13 64) (14 52)))
(setq
 P24 '((27/2 50) (14 52) (29/2 53) (15 55) (16 42)))
(setq P2 (list P21 P22 P23 P24))
(setq P-patt&occ (list P1 P2))

(setq Q11 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq Q12 (translation Q11 '(5 2)))
(setq Q13 '((0 59) (1/2 60) (1 60) (2 62)))
(setq Q14 (translation Q11 '(40 0)))
(setq Q1 (list Q11 Q12 Q13 Q14))

(setq Q21 (butlast (translation Q11 '(8 5))))
(setq Q22 (rest (translation Q12 '(8 5))))
(setq Q23 (rest (translation Q13 '(8 5))))
(setq Q24 (butlast (translation Q14 '(8 5))))
(setq Q2 (list Q21 Q22 Q23 Q24))

(setq
 Q31
 '((67/2 60) (67/2 64) (34 62) (34 65) (69/2 64)
   (69/2 67) (35 57) (35 65) (35 69) (35 72)))
(setq
 Q32
 '((97/2 60) (97/2 64) (49 62) (49 65) (99/2 64)
   (99/2 67) (50 58) (50 66) (50 70) (50 72)))
(setq Q3 (list Q31 Q32))
(setq Q-patt&occ (list Q1 Q2 Q3))

(occurrence-matrix&rel-idx
 P-patt&occ Q-patt&occ #'precision-matrix)
--> (((9/20 0 0) (0 0 0)) ((0 0) (0 1)))
(occurrence-matrix&rel-idx
 P-patt&occ Q-patt&occ #'recall-matrix)
--> (((3/5 0 0) (0 0 0)) ((0 0) (0 1)))
\end{verbatim}

\noindent The occurrence matrix indicates whether
an algorithm is strong at retrieving \emph{all
occurrences} of a pattern $P$, given that the
existence of at least one repetition of $P$ has been
established. So even if an algorithm fails to discover
many noticeable/important patterns in a piece, it can
still score well on the precision or recall of its
occurrence matrix.

The first argument to the function occurrence-matrix
is a list of patterns, and within each list, all
occurrences of that pattern. These are assumed to be
the ground truth. The second argument has the same
structure as the first, but for the output of some
algorithm. The first calculation is the establishment
matrix, indicating the extent to which output pattern
Qj (and all its occurrences) constitutes the discovery
of ground truth pattern Pi (and all its occurrences).
The are options for comparison function, error
tolerance, and accepting discoveries up to
translation.

The second calculation is restricted to elements of
the establishment matrix greater than a score
threshold (specifiable by the variable score-thresh),
thus putting failed discoveries to one side and
focusing on the retrieved occurrences of successful
discoveries. A score matrix is computed for each Pi
and Qj above the score threshold, which contains
information about the retrieval of occurrences. Either
the precision or recall of each matrix is returned and
forms a new sparser matrix called the occurrence
matrix. Also output are the indices of the
establishment matrix greater than the score
threshold. |#

(defun occurrence-matrix&rel-idx
       (P-patt&occ Q-patt&occ &optional
        (prec-rec #'precision-matrix)
        (compare-fn #'cardinality-score) (tolp nil)
        (translationp nil) (card-limit 100)
        (score-thresh 0.75)
        (establish-mtx
         (establishment-matrix
          P-patt&occ Q-patt&occ compare-fn tolp
          translationp))
        (rel-idx
         (indices-of-matrix-passing-tests
          establish-mtx (list #'>=)
          (list score-thresh)))
        (nrow (length establish-mtx)) (irow 0)
        (ncol (length (first establish-mtx)))
        (icol
         (add-to-list
          -1 (reverse (first-n-naturals ncol))))
        (occ-mtx nil))
  (if (>= irow nrow)
    (list occ-mtx rel-idx)
    (occurrence-matrix&rel-idx
     P-patt&occ Q-patt&occ prec-rec compare-fn tolp
     translationp card-limit score-thresh
     establish-mtx rel-idx nrow (+ irow 1) ncol icol
     (append
      occ-mtx
      (list
       (mapcar
        #'(lambda (x)
            (if (find
                 (list irow x) rel-idx :test
                 #'equalp)
              (funcall
               prec-rec
               (score-matrix
                (nth irow P-patt&occ)
                (nth x Q-patt&occ) compare-fn
                tolp translationp card-limit)) 0))
        icol))))))

#|
\noindent Example:
\begin{verbatim}
P-patt&occ and Q-patt&occ defined as in the example
for the function occurrence-matrix&rel-idx.
(occurrence-metric
 P-patt&occ Q-patt&occ #'precision-matrix)
--> 9/40
(occurrence-metric
 P-patt&occ Q-patt&occ #'recall-matrix)
--> 3/5
\end{verbatim}

\noindent The occurrence metrics indicate whether
an algorithm has strong precision/recall for
retrieving \emph{all occurrences} of a pattern $P$,
given that the existence of at least one repetition of
$P$ has been established. So even if an algorithm
fails to discover many noticeable/important patterns
in a piece, it can still score well on the precision
or recall occurrence metrics.

First this function calculates either the precision or
recall occurrence matrix. This is calculated by taking
the indices of those elements of the establishment
matrix that are greater than some score threshlold
(default .75), and calculating the precision or recall
of the score matrix for $\mathcal{P}_i$ (all
occurrences) and $\mathcal{Q}_j$ (all occurrences),
for each pair $(i, j)$ of indices. Then the precision
or recall of the occurrence matrix is calculated (over
this subset of columns/rows), to return a scalar
value. |#

(defun occurrence-metric
       (P-patt&occ Q-patt&occ &optional
        (prec-rec #'precision-matrix)
        (compare-fn #'cardinality-score) (tolp nil)
        (translationp nil) (card-limit 100)
        (score-thresh .75)
        (occ-mtx&rel-idx
         (occurrence-matrix&rel-idx
          P-patt&occ Q-patt&occ prec-rec compare-fn
          tolp translationp card-limit score-thresh))
        (prec-rec-idx
         (if (equalp prec-rec #'precision-matrix)
           1 0))
        (n
         (length
          (remove-duplicates
           (nth-list-of-lists
            prec-rec-idx
            (second occ-mtx&rel-idx))
           :test #'equalp))))
  (if (zerop n) 0
    (funcall
     prec-rec (first occ-mtx&rel-idx) n)))

#|
\noindent Example:
\begin{verbatim}
(precision-matrix '((1 4/5 1/5) (2/5 1/5 2/5)))
--> 11/15
\end{verbatim}

\noindent The precision of a matrix where rows
represent ground truth items and columns represent
retrieved items is defined as the mean of the column
maxima. |#

(defun precision-matrix
       (a-matrix-list &optional
        (n (length (first a-matrix-list)))
        (i
         (add-to-list
          -1
          (reverse
           (first-n-naturals
            (length (first a-matrix-list)))))))
  (/
   (my-last
    (fibonacci-list
     (mapcar
      #'(lambda (x)
          (max-item
           (nth-list-of-lists x a-matrix-list))) i)))
   n))

#|
\noindent Example:
\begin{verbatim}
(recall-matrix '((1 4/5 1/5) (2/5 1/5 2/5)))
--> 7/10
\end{verbatim}

\noindent The recall of a matrix where rows represent
ground truth items and columns represent retrieved
items is defined as the mean of the row maxima. |#

(defun recall-matrix
       (a-matrix-list &optional
        (n (length a-matrix-list)))
  (/
   (my-last
    (fibonacci-list
     (mapcar
      #'(lambda (x) (max-item x)) a-matrix-list))) n))

#|
\noindent Example:
\begin{verbatim}
(setq P1 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq P2 '((21 60) (45/2 60) (23 47) (23 59) (25 57)))
(setq P3 '((41 53) (85/2 53) (43 52) (44 50)))
(setq P-occurrences (list P1 P2 P3))
(setq Q1 '((1 53) (5/2 53) (3 40) (3 52) (4 50)))
(setq Q2 (translation Q1 '(5 2)))
(setq Q3 '((0 59) (1/2 60) (1 60) (2 62)))
(setq Q4 (translation Q1 '(40 0)))
(setq Q-occurrences (list Q1 Q2 Q3 Q4))
(score-matrix P-occurrences Q-occurrences)
--> ((1 0 0 0) (0 0 0 0) (0 0 0 4/5))
(score-matrix
 P-occurrences Q-occurrences #'cardinality-score nil
 t)
--> ((1 1 1/5 1) (4/5 4/5 1/5 4/5) (4/5 4/5 1/4 4/5))
\end{verbatim}

\noindent For each pair of point sets in the first and
second arguments, this function computes the
cardinality score or matching score (or some other
function for computing symbolic musical similarity).
These scores are returned in a list of lists. Each
lists contains a row of scores for a point set from
P-occurrences, and so the $n$th element of each list
corresponds to a column of scores for a point set from
Q-occurrences. |#

(defun score-matrix
       (P-occurrences Q-occurrences &optional
        (compare-fn #'cardinality-score) (tolp nil)
        (translationp nil) (card-limit 100)
        (mP (length P-occurrences)) (i 0)
        (matrix-row
         (if (< i mP)
           (mapcar
            #'(lambda (x)
                (if (equalp
                     compare-fn #'cardinality-score)
                  (cardinality-score 
                   (nth i P-occurrences) x tolp
                   translationp)
                  (if (equalp
                       compare-fn #'matching-score)
                    (matching-score
                     (nth i P-occurrences) x
                     card-limit)
                    (equalp-score
                     (nth i P-occurrences) x tolp))))
            Q-occurrences))))
  (if (>= i mP) ()
    (cons
     matrix-row
     (score-matrix
      P-occurrences Q-occurrences compare-fn tolp
      translationp card-limit mP (+ i 1)))))
