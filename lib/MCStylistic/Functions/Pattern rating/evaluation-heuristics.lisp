#| Copyright 2008-2013 Tom Collins
   Wednesday 13 January 2010

These functions implement definitions of coverage,
compactness, and compression ratio
\citep{meredith2003,forth2009}.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "geometric-operations"
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
(compactness
 '((1 2) (2 4)) '((1 2) (2 -1) (2 4) (3 6) (5 2))
 "lexicographic")
--> 2/3

(setq
 dataset-sorted
 '((-4.13 -2.62) (-3.89 -4.13) (-3.82 2.71)
   (-2.67 0.32) (-2.65 -3.48) (-1.71 -1.13)
   (-1.33 0.3) (-1.3 -4.0) (0.83 1.41) (1.27 -3.95)
   (1.4 -2.94) (1.53 0.51) (1.83 2.89) (1.85 -0.94)
   (2.22 -2.93) (2.34 2.81) (2.4 -0.15) (2.49 -2.71)
   (3.66 -2.05) (4.7 -3.99)))
(setq
 pattern-sorted
 '((-2.67 0.32) (-2.65 -3.48) (-1.71 -1.13)
   (1.27 -3.95) (1.4 -2.94) (1.53 0.51) (3.66 -2.05)))
(compactness
 pattern-sorted dataset-sorted "lexicographic")
--> 7/16

(compactness
 pattern-sorted dataset-sorted "convex hull")
--> 7/11
\end{verbatim}

\noindent The ratio of the number of points in the
pattern to the number of points in the region spanned
by the pattern. Both pattern and dataset are assumed
to be sorted ascending. At present two definitions of
region ("lexicographic" and "convex hull") are
admissible.

There is a plot for the above example in the Example
Files folder, entitled convex_hull.pdf. NB we define
region as the lexicographic region first, as a point
is in the convex hull of the pattern only if it is in
its lexicographic region (ought to prove this
assertion). This avoids determining in-polygonp for
each point in the dataset, which would require more
time. |#

(defun compactness
       (pattern projected-dataset &optional
	(region-type "lexicographic")
        (length-pattern (length pattern))
        (region
         (subseq
          projected-dataset
          (index-item-1st-occurs
           (first pattern) projected-dataset)
          (+ (index-item-1st-occurs
              (my-last pattern) projected-dataset)
             1))))
  (if (string= region-type "lexicographic")
    (/ length-pattern (length region))
    (/
     length-pattern
     (+
      length-pattern
      (length
       (points-in-convex-hull
        pattern
        (set-difference-multidimensional-sorted-asc
         region pattern)))))))

#|
\noindent Example:
\begin{verbatim}
(compactness-max
 '((1 2) (2 4)) '((0 0) (1 2) (3 -2))
 '((1 2) (2 -1) (2 0) (2 4) (3 0) (3 1) (3 3) (3 6)
   (4 0) (5 1) (5 2))
 0.2 1 "straight down" 2)
--> 2/3
\end{verbatim}

\noindent The function compactness-min-max is applied
to each occurrence of a pattern and the maximum
compactness returned. |#

(defun compactness-max
       (pattern translators projected-dataset
	&optional (min 0.25) (max 1)
	(region "straight down")
        (length-pattern (length pattern))
	(overall 0)
	(current
	 (if translators
	   (compactness-min-max
	    (translation pattern (first translators))
	    projected-dataset min max region
            length-pattern))))
  (if (null current)
    (identity overall)
    (if (>= overall max)
      (identity max)
      (compactness-max
       pattern (rest translators) projected-dataset
       min max region length-pattern
       (max current overall)))))

#|
\noindent Example:
\begin{verbatim}
(compactness-min-max
 '((1 2) (2 4)) '((1 2) (2 -1) (2 4) (3 6) (5 2))
 0.2 1 "straight down")
--> 2/3
\end{verbatim}

\noindent The ratio of the number of points in the
pattern to the number of points in the region spanned
by the pattern. Both pattern and dataset are assumed
to be sorted ascending. At present the only admissible
definition of region is `straight down' (which means
`lexicographic', cf.~Def.~2.10 in
\citeauthor{collins2011b},
\citeyear{collins2011b}). |#

(defun compactness-min-max
       (pattern projected-dataset &optional
	(min 0.25) (max 1) (region "straight down")
        (length-pattern (length pattern))
	(span
	 (if (equalp region "straight down")
	   (+
	    (-
	     (index-item-1st-occurs
	      (my-last pattern) projected-dataset)
	     (index-item-1st-occurs
	      (first pattern) projected-dataset)) 1)))
	(result (/ length-pattern span)))
  (if (< result min)
    (identity 0)
    (if (< result max)
      (identity result)
      (identity max))))

#|
\noindent Example:
\begin{verbatim}
(compression-ratio
 '((1 2) (2 4)) '((0 0) (1 2) (3 -2))
 '((1 2) (2 -1) (2 0) (2 4) (3 0) (3 1) (3 3) (3 6)
   (4 0) (5 1) (5 2))
 0.2 1 5)
--> 1
\end{verbatim}

\noindent The compression ratio that can be achieved
by representing the set of points covered by all
occurrences of a pattern by specifying just one
occurrence of the pattern together with all the non-
zero vectors by which the pattern in translatable
within the dataset. |#

(defun compression-ratio
       (pattern translators projected-dataset
        &optional (min 0.25) (max 1)
        (membership-assumption nil)
        (first-sorted nil)
        (length-pattern (length pattern))
        (length-translators
         (length translators))
        (coverage-calculated
         (coverage
          pattern translators
          projected-dataset
          membership-assumption first-sorted))
        (result
         (/ coverage-calculated
            (-
             (+ length-pattern
                length-translators) 1))))
  (if (< result min)
    (identity 0)
    (if (< result max)
      (identity result)
      (identity max))))

#|
\noindent Example:
\begin{verbatim}
(cover-ratio
 '((1 2) (2 4)) '((0 0) (1 2))
 '((1 2) (2 4) (3 6) (5 2) (6 1)) 0.2 t t)
--> 3/5
\end{verbatim}

\noindent The ratio between the number of uncovered
datapoints in the dataset that are members of
occurrences of the pattern, to the total number of
uncovered datapoints in the dataset. |#

(defun cover-ratio
       (pattern translators projected-dataset
	&optional (min 0.2)
	(membership-assumption nil) (first-sorted nil)
	(coverage-calculated
	 (coverage
	  pattern translators projected-dataset
	  membership-assumption first-sorted))
	(length-dataset (length projected-dataset))
	(result
	 (/ coverage-calculated
	    length-dataset)))
  (if (< result min) (identity 0) (identity result)))

#|
\noindent Example:
\begin{verbatim}
(coverage
 '((1 2) (2 4)) '((0 0) (1 2))
 '((1 2) (2 4) (3 6) (5 2) (6 1)) t t)
--> 3
\end{verbatim}

\noindent The number of datapoints in the dataset that
are members of occurrences of the pattern. |#

(defun coverage
       (pattern translators
	projected-dataset &optional
	(membership-assumption nil)
	(first-sorted nil))
  (if membership-assumption
    (length
     (unions-multidimensional-sorted-asc
      (translations pattern translators)
      first-sorted))
    (length
     (intersection-multidimensional
      (unions-multidimensional-sorted-asc
       (translations pattern translators)
		     first-sorted)
      projected-dataset))))

#|
\noindent Example:
\begin{verbatim}
(coverage-mod-2nd-n
 '((1 2) (2 4)) '((0 0) (1 2))
 '((1 2) (2 4) (3 6) (5 2) (6 1)) 12 t t)
--> 3
\end{verbatim}

\noindent The number of datapoints in the dataset that
are members of occurrences of the pattern.
Translations are carried out modulo the fourth
argument. |#

(defun coverage-mod-2nd-n
       (pattern translators
	projected-dataset n &optional
	(membership-assumption nil)
	(first-sorted nil))
  (if membership-assumption
    (length
     (unions-multidimensional-sorted-asc
      (translations-mod-2nd-n pattern translators n)
      first-sorted))
    (length
     (intersection-multidimensional
      (unions-multidimensional-sorted-asc
       (translations-mod-2nd-n pattern translators n)
       first-sorted)
      projected-dataset))))

#|
\noindent Example:
\begin{verbatim}
(heuristics-pattern-translators-pair
 '((1 2) (2 4)) '((0 0) (1 2) (3 -2))
 '((1 2) (2 -1) (2 0) (2 4) (3 0) (3 1) (3 3) (3 6)
   (4 0) (5 1) (5 2)) '(t t t t t t)
   0.2 0.25 1 0.25 1 "straight down" 11)
--> (5 5/11 1 2/3 2 3)
\end{verbatim}

\noindent A pattern and its translators in a projected
dataset are supplied as arguments to this function,
along with an indicator vector that indicates which
heuristics out of coverage, cover ratio, compression
ratio, compactness, $|P|$ and $|T(P, D)|$ should be
calculated. |#

(defun heuristics-pattern-translators-pair
       (pattern translators projected-dataset
        heuristics-indicator &optional
        (cover-ratio-min 0.2)
        (compression-ratio-min 0.25)
        (compression-ratio-max 1)
        (compactness-max-min 0.25)
        (compactness-max-max 1)
        (region "straight down")
        (length-dataset
         (if (second heuristics-indicator)
           (length projected-dataset)))
        (coverage-calculated
         (if (or (first heuristics-indicator)
                 (second heuristics-indicator)
                 (third heuristics-indicator))
           (coverage pattern translators
                     projected-dataset nil t)))
        (length-pattern
         (if (or (third heuristics-indicator)
                 (fourth heuristics-indicator)
                 (fifth heuristics-indicator))
           (length pattern)))
        (length-translators
         (if (or (third heuristics-indicator)
                 (sixth heuristics-indicator))
           (length translators))))
  (append
   (if (first heuristics-indicator)
     (list coverage-calculated))
   (if (second heuristics-indicator)
     (list (cover-ratio
            pattern translators projected-dataset
            cover-ratio-min nil t
            coverage-calculated length-dataset)))
   (if (third heuristics-indicator)
     (list (compression-ratio
            pattern translators projected-dataset
            compression-ratio-min
            compression-ratio-max nil t
            length-pattern length-translators
            coverage-calculated)))
   (if (fourth heuristics-indicator)
     (list (compactness-max
            pattern translators projected-dataset
            compactness-max-min
            compactness-max-max region
            length-pattern)))
   (if (fifth heuristics-indicator)
     (list length-pattern))
   (if (sixth heuristics-indicator)
     (list length-translators))))

#|
\noindent Example:
\begin{verbatim}
(heuristics-pattern-translators-pairs
 '((((1 2) (2 4)) ((0 0) (1 2) (3 -2)))
   (((1 2) (2 0)) ((0 0) (2 0))))
 '((1 2) (2 -1) (2 0) (2 4) (3 0) (3 1) (3 2) (3 6)
   (4 0) (5 1) (5 2)) '(t t t t t t)
   0.2 0.25 1 0.25 1 "straight down" 11)
--> ((5 5/11 1 2/3 2 3) (4 4/11 1 2/3 2 2))

\end{verbatim}

\noindent The function
heuristics-pattern-translators-pair is applied
recursively to pairs of pattern-translators. |#

(defun heuristics-pattern-translators-pairs
       (pairs projected-dataset
        heuristics-indicator &optional
        (cover-ratio-min 0.2)
        (compression-ratio-min 0.25)
        (compression-ratio-max 1)
        (compactness-max-min 0.25)
        (compactness-max-max 1)
        (region "straight down")
        (length-dataset
         (length projected-dataset)))
  (if (null pairs) ()
    (cons
     (heuristics-pattern-translators-pair
      (first (first pairs)) (second (first pairs))
      projected-dataset heuristics-indicator
      cover-ratio-min compression-ratio-min
      compression-ratio-max compactness-max-min
      compactness-max-max region length-dataset)
     (heuristics-pattern-translators-pairs
      (rest pairs) projected-dataset
      heuristics-indicator cover-ratio-min
      compression-ratio-min compression-ratio-max
      compactness-max-min compactness-max-max
      region length-dataset))))

#|
\noindent Example:
\begin{verbatim}
(musicological-heuristics
 '((((1 2) (2 4)) ((0 0) (1 2) (3 -2)))
   (((1 2) (2 0)) ((0 0) (2 0)))
   (((1 2) (2 4) (4 0)) ((0 0) (1 2) (2 -4))))
 '((1 2) (2 -1) (2 0) (2 4) (3 -2) (3 0) (3 1)
   (3 2) (3 6) (4 0) (5 1) (5 2) (6 -4))
 0.25 1 0.25 1 "straight down" 11)
--> ((1 1 1) (1 1 0))

\end{verbatim}

\noindent The function
heuristics-pattern-translators-pairs is applied to
pattern-translator pairs with the heuristics indicator
set to compression ratio and compactness (max). The
values are normalised (linearly) to $[0, 1]$ and
returned as two lists. |#

(defun musicological-heuristics
       (pairs projected-dataset &optional
        (compression-ratio-min 0.25)
        (compression-ratio-max 1)
        (compactness-max-min 0.25)
        (compactness-max-max 1)
        (region "straight down")
        (length-dataset
         (length projected-dataset))
        (heuristics-calculated
         (heuristics-pattern-translators-pairs
          pairs projected-dataset
          '(nil nil t t nil nil) 0.2
          compression-ratio-min
          compression-ratio-max
          compactness-max-min compactness-max-max
          region length-dataset)))
  (list
   (normalise-0-1
    (nth-list-of-lists 0 heuristics-calculated))
   (normalise-0-1
    (nth-list-of-lists 1 heuristics-calculated))))
