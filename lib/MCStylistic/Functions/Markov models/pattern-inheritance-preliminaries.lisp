#| Copyright 2008-2013 Tom Collins
   Wednesday 20 October 2010
   Incomplete

\noindent These functions filter the results of
applying SIACT to an excerpt. The result is a list
consisting of hash tables, where each hash table
consists of the keys: index, name, cardinality,
occurrences, MTP vectors, rating, compactness,
expected occurrences, compression ratio, pattern,
region, and translators. The most important function
in this file is called prepare-for-pattern-
inheritance.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "evaluation-for-SIACT"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
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
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-merge"
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
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(indices-of-patterns-equalp-trans&intersect
 (gethash '"pattern" (first patterns-hash))
 (gethash '"translators" (first patterns-hash))
 (rest patterns-hash))
--> (0 3 4)
\end{verbatim}

\noindent This function takes information about a
pattern from the first hash table in a list of
hash tables. It then compares this with each
pattern in the rest of the list. If a pair of
patterns have the same translation vectors and
their first occurrences have intersecting
datapoints, then the second pattern in the pair
will have to be removed. To this end, the index of
the second pattern is returned. |#

(defun indices-of-patterns-equalp-trans&intersect
       (head-pattern head-translators
        rest-patterns-hash &optional
        (n (length rest-patterns-hash)) (i 0)
        (pattern-hash
         (if (< i n) (nth i rest-patterns-hash)))
        (remove-patternp
         (if pattern-hash
           (and
            (equalp
             (gethash '"translators" pattern-hash)
             head-translators)
            (intersection-multidimensional
             (gethash '"pattern" pattern-hash)
             head-pattern)))))
  (if (>= i n) ()
    (append
     (if remove-patternp (list i))
     (indices-of-patterns-equalp-trans&intersect
      head-pattern head-translators
      rest-patterns-hash n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   SIACT-output
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp documentation"
     "/Example files/SIACT-output.txt")))
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-68-1-ed.txt"))
  (setq dataset-mini (subseq dataset-all 0 350))
  (setq
   projected-dataset
   (orthogonal-projection-unique-equalp
    dataset-mini '(1 1 1 0 0)))
  "Yes!")
(setq
 patterns-hash
 (prepare-for-pattern-inheritance
  SIACT-output projected-dataset 1))
--> gives a hash table called patterns-hash.
(write-to-file-balanced-hash-table
 patterns-hash
 (concatenate
  'string
  "/Applications/CCL/Lisp documentation"
  "/Example files/patterns-hash2.txt"))
\end{verbatim}

\noindent This function applies functions that
prepare the output of SIACT run on a dataset for
Markov-chain Monte Carlo generation with pattern
inheritance. |#

(defun prepare-for-pattern-inheritance
       (pattern-compact-vec-triples projected-dataset
        &optional (duration-threshold 3)
        (coefficients
	 (list 4.277867 3.422478734 -0.038536808
	       0.651073171))
        (norm-coeffs
	 (list 73.5383283152 0.02114878519)))
  (subset-scores-of-patterns+
   (remove-temporally-overlapping-points-of-patterns
    (remove-patterns-equalp-trans&intersect
     (remove-overlapping-translators-of-patterns
      (translate-patterns-to-1st-occurrences
       (evaluate-variables-of-patterns2hash
        (remove-patterns-shorter-than
         pattern-compact-vec-triples
         duration-threshold)
        projected-dataset coefficients
        norm-coeffs)))))))

#| Old version.
(defun prepare-for-pattern-inheritance
       (pattern-compact-vec-triples projected-dataset
        &optional (duration-threshold 3)
        (coefficients
	 (list 4.277867 3.422478734 -0.038536808
	       0.651073171))
        (norm-coeffs
	 (list 73.5383283152 0.02114878519)))
  (subset-scores-of-patterns+
   (remove-patterns-equalp-trans&intersect
    (remove-overlapping-translators-of-patterns
     (translate-patterns-to-1st-occurrences
      (evaluate-variables-of-patterns2hash
       (remove-patterns-shorter-than
        pattern-compact-vec-triples
        duration-threshold)
       projected-dataset coefficients
       norm-coeffs))))))
|#

#|
\noindent Example:
\begin{verbatim}
(remove-overlapping-translators
 3 '((0 0 0) (1 2 3) (4 0 0) (5 0 0) (7 0 0) (8 2 1)))
--> ((0 0 0) (4 0 0) (7 0 0))
\end{verbatim}

\noindent This function takes the duration of a
pattern and its translators as arguments, and returns
a list of those translators that do not produce
overlapping patterns (in the sense of the argument
pattern-duration). |#

(defun remove-overlapping-translators
       (pattern-duration translators &optional
        (result (list (first translators)))
        (vector1 (first translators))
        (vector2 (second translators))
        (overlapp
         (if vector2
           (<
            (- (first vector2) (first vector1))
            pattern-duration))))
  (if (null vector2) result
    (if overlapp
      (remove-overlapping-translators
       pattern-duration
       (cons vector1 (rest (rest translators)))
       result)
      (remove-overlapping-translators
       pattern-duration
       (cons vector2 (rest (rest translators)))
       (append result (list vector2))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(setq
 patterns-hash
 (remove-overlapping-translators-of-patterns
  patterns-hash))
--> gives a hash table called patterns-hash.
\end{verbatim}

\noindent This function applies the function
remove-overlapping-translators recursively to a list
consisting of hash tables. Each hash table contains
information about a discovered pattern, as returned by
the function evaluate-variables-of-patterns2hash. The
output is an updated hash table. |#

(defun remove-overlapping-translators-of-patterns
       (patterns-hash &optional
        (pattern-hash (first patterns-hash))
        (pattern
         (if pattern-hash
           (gethash '"pattern" pattern-hash)))
        (new-translators
         (if pattern-hash
           (remove-overlapping-translators
            (- (ceiling (first (my-last pattern)))
               (floor (first (first pattern))))
            (gethash '"translators" pattern-hash)))))
  (if (null patterns-hash) ()
    (if (> (length new-translators) 1)
      (cons
       (progn
         (setf
          (gethash '"translators" pattern-hash)
          new-translators)
         pattern-hash)
       (remove-overlapping-translators-of-patterns
        (rest patterns-hash)))
      (remove-overlapping-translators-of-patterns
        (rest patterns-hash)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(setq
 patterns-hash
 (remove-patterns-equalp-trans&intersect
  patterns-hash))
--> gives a hash table called patterns-hash.
\end{verbatim}

\noindent This function applies the function
indices-of-patterns-equalp-trans&intersect
recursively. The result is that the lower-rating of
any pair of patterns is removed if the two patterns
have the same translation vectors and their first
occurrences have intersecting datapoints. It is
assumed that each pattern has already been arranged so
that its first translation vector is the zero
vector. |#

(defun remove-patterns-equalp-trans&intersect
       (patterns-hash &optional
        (result (list (first patterns-hash)))
        (pattern-hash (first patterns-hash))
        (rest-patterns-hash (rest patterns-hash))
        (new-patterns-hash
         (if rest-patterns-hash
           (remove-nth-list
           (indices-of-patterns-equalp-trans&intersect
            (gethash '"pattern" pattern-hash)
            (gethash '"translators" pattern-hash)
            rest-patterns-hash)
           rest-patterns-hash))))
  (if (null patterns-hash) result
    (remove-patterns-equalp-trans&intersect
     new-patterns-hash
     (if new-patterns-hash
       (append
        result (list (first new-patterns-hash)))
       result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 lil-pattern&sources
 '((((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
     (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
     (7/2 60 60 1/2))
    16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0))
   (((1/2 72 67 1/2) (3/2 79 71 1/2))
    1 (130 0 0) 2/3 (100 0 1/2))))
(remove-patterns-shorter-than lil-pattern&sources 3)
--> ((((1/2 72 67 1/2) (1 76 69 1/2)
       (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2)
       (3 64 62 1/2) (7/2 60 60 1/2))
      16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0)))
\end{verbatim}

\noindent Let a be the floor of the first ontime
and b be the ceiling of the last offtime of a
pattern. If this is less than the optional variable
duration-threshold, then this pattern will not
appear in the output of this function. |#

(defun remove-patterns-shorter-than
       (pattern-compact-vec-triples &optional
        (duration-threshold 3)
        (first-triple
         (first pattern-compact-vec-triples))
        (first-pattern (first first-triple)))
  (if (null first-pattern) ()
    (if (>= (- (ceiling
                (first (my-last first-pattern)))
               (floor
                (first (first first-pattern))))
            duration-threshold)
      (cons
       first-triple
       (remove-patterns-shorter-than
        (rest pattern-compact-vec-triples)
        duration-threshold))
      (remove-patterns-shorter-than
        (rest pattern-compact-vec-triples)
        duration-threshold))))

#|
\noindent Example:
\begin{verbatim}
(setq pattern '((0 60 1) (1 58 1) (2 60 2) (4 63 1)))
(setq translators '((0 0 0) (2 -7 0) (4 0 0)))
(remove-temporally-overlapping-points
 pattern translators)
--> ((0 60 1) (1 58 1))
\end{verbatim}

\noindent This function determines whether points from
the end of a pattern will overlap temporally with
points from the beginning of any subsequent
occurences. Such overlapping points are removed. |#

(defun remove-temporally-overlapping-points
       (pattern translators &optional (ontime-index 0)
        (next-occ
         (if (> (length translators) 1)
           (translation
            pattern (second translators))))
        (pattern-trunc
         (if next-occ
           (loop for i from 0
             to (- (length pattern) 1)
             when
             (<
              (nth ontime-index (nth i pattern))
              (nth ontime-index (first next-occ)))
             collect (nth i pattern)))))
  (if (null next-occ) pattern
    (remove-temporally-overlapping-points
     pattern-trunc (rest translators) ontime-index)))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (merge-pathnames
   (make-pathname
   :name "patterns-hash" :type "txt")
  *MCStylistic-MonthYear-example-files-data-path*)))
(setq
 patterns-hash
 (remove-temporally-overlapping-points-of-patterns
  patterns-hash))
--> gives a hash table called patterns-hash.
\end{verbatim}

\noindent This function applies the function
remove-temporally-overlapping-points recursively to a
list consisting of hash tables. Each hash table
contains information about a discovered pattern, as
returned by the function
evaluate-variables-of-patterns2hash. The output is an
updated hash table.

The function also updates the region in the same way,
so that these do not overlap with subsequent
occurrences also. |#

(defun
    remove-temporally-overlapping-points-of-patterns
    (patterns-hash &optional
     (pattern-hash (first patterns-hash))
     (pattern
      (if pattern-hash
        (gethash '"pattern" pattern-hash)))
     (translators
      (if pattern-hash
        (gethash '"translators" pattern-hash)))
     (new-pattern
      (if pattern-hash
        (remove-temporally-overlapping-points
         pattern translators)))
     (new-region
      (if pattern-hash
        (remove-temporally-overlapping-points
         (gethash '"region" pattern-hash)
         translators))))
  (if (null patterns-hash) ()
    (if new-pattern
      (cons
       (progn
         (setf
          (gethash '"pattern" pattern-hash)
          new-pattern)
         (setf
          (gethash '"region" pattern-hash)
          new-region)
         pattern-hash)
     (remove-temporally-overlapping-points-of-patterns
        (rest patterns-hash)))
     (remove-temporally-overlapping-points-of-patterns
       (rest patterns-hash)))))

#| Old version focused on adjacent pairs of
occurrences and removing overlaps there.
(setq pattern '((0 60 1) (1 58 1) (2 60 2) (4 63 1)))
(setq translators '((0 0 0) (4 -7 0) (8 0 0)))
(remove-temporally-overlapping-points
 pattern translators)
--> (((0 60 1) (1 58 1) (2 60 2))
     ((4 67 1) (5 65 1) (6 67 2) (8 74 1))
     ((8 60 1) (9 58 1) (10 60 2) (12 67 1)))

(defun remove-temporally-overlapping-points
       (pattern translators &optional (ontime-index 0)
        (accumulated-occ nil)
        (curr-occ
         (if translators
           (translation pattern (first translators))))
        (next-occ
         (if (> (length translators) 1)
           (translation
            pattern (second translators))))
        (curr-occ-trunc
         (if next-occ
           (loop for i from 0
             to (- (length curr-occ) 1)
             when
             (<
              (nth ontime-index (nth i curr-occ))
              (nth ontime-index (first next-occ)))
             collect (nth i curr-occ))))
        (accumulated-occ
         (if accumulated-occ
           (if curr-occ-trunc
             (append
              accumulated-occ (list curr-occ-trunc))
             accumulated-occ)
           (list curr-occ-trunc))))
  (if (null next-occ)
    (append accumulated-occ (list curr-occ))
    (remove-temporally-overlapping-points
     pattern (rest translators) ontime-index
     accumulated-occ next-occ)))
|#


#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(subset-score-of-pattern
 (gethash '"pattern" (nth 6 patterns-hash))
 6 patterns-hash)
--> '(2 2 1)
\end{verbatim}

\noindent This function takes a pattern as its first
argument, called the probe pattern, and a hash table
of patterns as its second argument. It counts and
returns the number of patterns in the hash table
(including translations) of which the probe pattern is
a subset. |#

(defun subset-score-of-pattern
       (probe-pattern index-in-patterns-hash
        patterns-hash &optional
        (n (length patterns-hash)) (i 0)
        (growing-result 0)
        (pattern-hash
         (if (< i n) (nth i patterns-hash)))
        (result
         (if (and
              pattern-hash
              (not (equalp i index-in-patterns-hash)))
           (my-last
            (fibonacci-list
             (mapcar
              #'(lambda (x)
                  (if (subset-multidimensional
                       probe-pattern x) 1 0))
              (translations
               (gethash
                '"pattern" pattern-hash)
               (gethash
                '"translators" pattern-hash)))))
           0)))
  (if (>= i n) growing-result
    (subset-score-of-pattern
     probe-pattern index-in-patterns-hash
     patterns-hash n (+ i 1)
     (+ growing-result result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(setq
 patterns-hash
 (subset-scores-of-patterns+ patterns-hash))
--> gives a hash table called patterns-hash.
\end{verbatim}

\noindent This function applies the function subset-
score-of-pattern to each pattern (including
translations) listed in a hash table of patterns. It
also creates inheritance indices (for example the
first occurrence of the highest-rating pattern is
labelled $P_{0,0}$) and a variable called inheritance
addressed, set to "No" by default, but will revert to
"Yes" when patterns are incorporated into the
generated passage. This function is the last step in
preparing a hash table of patterns for generation with
pattern inheritance. |#

(defun subset-scores-of-patterns+
       (patterns-hash &optional
        (n (length patterns-hash)) (i 0)
        (pattern-hash
         (if (< i n) (nth i patterns-hash)))
        (pattern
         (if pattern-hash
           (gethash '"pattern" pattern-hash)))
        (translators
         (if pattern-hash
           (gethash '"translators" pattern-hash)))
        (m (length translators))
        (subset-scores
         (if pattern
           (mapcar
            #'(lambda (x)
                (subset-score-of-pattern
                 x i patterns-hash n))
            (translations pattern translators))))
        (inheritance-indices
         (if pattern
           (mapcar
            #'(lambda (x)
                (list i (- x 1)))
            (reverse (first-n-naturals m)))))
        (inheritance-addressed
         (if pattern
           "No"
           ;22/10/10 (constant-vector "No" m)
           )))
  (if (>= i n) ()
    (cons
     (progn
       (setf
        (gethash '"subset scores" pattern-hash)
        subset-scores)
       (setf
        (gethash '"inheritance indices" pattern-hash)
        inheritance-indices)
       (setf
        (gethash
         '"inheritance addressed" pattern-hash)
        inheritance-addressed)
       pattern-hash)
     (subset-scores-of-patterns+
      patterns-hash n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 pattern
 '((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
   (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
   (7/2 60 60 1/2)))
(setq translators '((-1/2 0 0 0) (0 0 0 0) (3 2 1 0)))
(translate-pattern-to-1st-occurrence
 pattern translators)
--> (((0 72 67 1/2) (1/2 76 69 1/2) (1 79 71 1/2)
      (3/2 84 74 2) (2 67 64 1/2) (5/2 64 62 1/2)
      (3 60 60 1/2))
     ((0 0 0 0) (1/2 0 0 0) (7/2 2 1 0)))
\end{verbatim}

\noindent Sometimes an occurrence of a pattern is
found, other than the first occurrence in a piece.
This function takes such instances and rearranges the
pattern and the translators, so it is the first
occurrence which is displayed. |#

(defun translate-pattern-to-1st-occurrence
       (pattern translators &optional
        (first-translator (first translators))
        (zero-vector
         (constant-vector
          0 (length first-translator))))
  (if (vector<vector-t-or-nil
       first-translator zero-vector)
    (list
     (translation pattern first-translator)
     (translation
      translators
      (multiply-list-by-constant
       first-translator -1)))
    (list pattern translators)))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   "/Applications/CCL/Lisp documentation"
   "/Example files/patterns-hash.txt")))
(setq
 patterns-hash
 (translate-patterns-to-1st-occurrences
  patterns-hash))
--> gives a hash table called pattern-hash.
\end{verbatim}

\noindent This function applies the function
translate-pattern-to-1st-occurrence recursively to a
list consisting of hash tables. Each hash table
contains information about a discovered pattern, as
returned by the function evaluate-variables-of-
patterns2hash. The output is an updated hash table. |#

(defun translate-patterns-to-1st-occurrences
       (patterns-hash &optional
        (pattern-hash (first patterns-hash))
        (pattern&translators
         (if pattern-hash
           (translate-pattern-to-1st-occurrence
            (gethash '"pattern" pattern-hash)
            (gethash '"translators" pattern-hash))))
        (region&translators
         (if pattern-hash
           (translate-pattern-to-1st-occurrence
            (gethash '"region" pattern-hash)
            (gethash '"translators" pattern-hash)))))
  (if (null patterns-hash) ()
    (cons
     (progn
       (setf
        (gethash '"pattern" pattern-hash)
        (first pattern&translators))
       (setf
        (gethash '"region" pattern-hash)
        (first region&translators))
       (setf
        (gethash '"translators" pattern-hash)
        (second pattern&translators))
       pattern-hash)
     (translate-patterns-to-1st-occurrences
      (rest patterns-hash)))))
