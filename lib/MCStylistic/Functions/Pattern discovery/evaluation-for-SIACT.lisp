#| Copyright 2008-2013 Tom Collins
   Tuesday 19 January 2010

The purpose of these functions is to rate the trawled
patterns, according to the formula for perceived
pattern importance \citep{collins2011a}.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "empirical-preliminaries"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "evaluation-heuristics"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "hash-tables"
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
   :name "sort-by"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (list
  (make-hash-table :test #'equal)
  (make-hash-table :test #'equal)
  (make-hash-table :test #'equal)))
(setf (gethash '"index" (first patterns-hash)) 0)
(setf (gethash '"rating" (first patterns-hash)) 3.3)
(setf (gethash '"index" (second patterns-hash)) 1)
(setf (gethash '"rating" (second patterns-hash)) 8.0)
(setf (gethash '"index" (third patterns-hash)) 2)
(setf (gethash '"rating" (third patterns-hash)) 2.1)
(collect-indices&ratings patterns-hash)
--> ((0 3.3) (1 8.0) (2 2.1))
\end{verbatim}

\noindent This function collects the index and rating
from each sublist of a list, where the sublist is a
hash table consisting of information about a
pattern. |#

(defun collect-indices&ratings (patterns-hash)
  (if (null patterns-hash) ()
    (cons
     (list
      (gethash '"index" (first patterns-hash))
      (gethash '"rating" (first patterns-hash)))
     (collect-indices&ratings (rest patterns-hash)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 pattern&source
 '(((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
    (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
    (7/2 60 60 1/2))
   16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0)))
(setq
 dataset
 '((0 48 53 2) (1/2 72 67 1/2) (1 76 69 1/2)
   (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2) 
   (3 64 62 1/2) (7/2 60 60 1/2) (4 36 46 2)
   (9/2 72 67 1/2) (5 76 69 1/2) (11/2 79 71 1/2)
   (6 84 74 2) (13/2 67 64 1/2) (7 64 62 1/2)
   (15/2 60 60 1/2) (8 36 46 2) (17/2 72 67 1/2)
   (9 76 69 1/2) (19/2 79 71 1/2)))
(setq
 dataset-palette
 (orthogonal-projection-not-unique-equalp
  dataset
  (append
   (list 0)
   (constant-vector
    1
    (- (length
	(first (first pattern&source))) 1)))))
(setq
 empirical-mass
 (empirical-mass dataset-palette))
(setq
 pattern-hash
 (evaluate-variables-of-pattern2hash
  pattern&source dataset 20 dataset-palette
  empirical-mass
  '(4.277867 3.422478734 -0.038536808 0.651073171)
  '(73.5383283152 0.02114878519) 1))
--> #<HASH-TABLE
    :TEST EQUAL size 12/60 #x3000418C079D>
(disp-ht-el pattern-hash)
--> (("name" . "pattern 1") ("compactness" . 16/23)
     ("expected occurrences" . 62.352943)
     ("rating" . 5.3952165)
     ("pattern"
      (1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
      (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
      (7/2 60 60 1/2))
     ("translators" (0 0 0 0) (4 0 0 0)) ("index" . 1)
     ("cardinality" . 7)
     ("MTP vectors" (96 -5 0) (104 -5 0) (140 5 0))
     ("compression ratio" . 7/4)
     ("region"
      (1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
      (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
      (7/2 60 60 1/2)) ("occurrences" . 2))
\end{verbatim}

\noindent This function evaluates variables of the
supplied pattern, such as cardinality and expected
occurrences. |#

(defun evaluate-variables-of-pattern2hash
       (pattern&source dataset length-dataset
	dataset-palette empirical-mass coefficients
	norm-coeffs index &optional
	(pattern (car pattern&source))
	(compactness&vectors (cdr pattern&source))
	(name
	 (concatenate
	  'string "pattern " (write-to-string index)))
	(cardinality (length pattern))
	(translators
	 (translators-of-pattern-in-dataset
	  pattern dataset))
	(occurrences (length translators))
	(MTP-vectors
	 (nth-list
	  (add-to-list
	   -1
	   (multiply-list-by-constant
	    (first-n-naturals
	     (/ (length compactness&vectors) 2))
	    2)) compactness&vectors))
	(compactness (first compactness&vectors))
        (span (round (/ cardinality compactness)))
        (region
	 (subseq
	  dataset
	  (index-item-1st-occurs
	   (first pattern) dataset)
	  (+ (index-item-1st-occurs
	      (my-last pattern) dataset) 1)))
	(coverage
	 (coverage pattern translators dataset t t))
	(compression-ratio
	 (/ coverage
	    (- (+ cardinality occurrences) 1)))
	(K
	 (+
	  (choose span cardinality)
	  (*
	   (- length-dataset cardinality)
	   (choose
	    (- span 1) (- cardinality 1)))))
	(expected-occurrences
	 (* (first norm-coeffs)
	    (expt
	     (*
	      K
	      (likelihood-of-translations-reordered
	       pattern dataset-palette
	       empirical-mass))
	     (second norm-coeffs))))
	(rating
	 (+ (first coefficients)
	    (* (second coefficients) compactness)
	    (* (third coefficients)
	       expected-occurrences)
	    (* (fourth coefficients)
	       compression-ratio)))
	(pattern-hash
	 (make-hash-table :test #'equal)))
  (setf (gethash '"index" pattern-hash) index)
  (setf (gethash '"name" pattern-hash) name)
  (setf
   (gethash '"cardinality" pattern-hash) cardinality)
  (setf
   (gethash '"occurrences" pattern-hash) occurrences)
  (setf
   (gethash '"MTP vectors" pattern-hash) MTP-vectors)
  (setf (gethash '"rating" pattern-hash) rating)
  (setf
   (gethash '"compactness" pattern-hash) compactness)
  (setf
   (gethash '"expected occurrences" pattern-hash)
   expected-occurrences)
  (setf
   (gethash '"compression ratio" pattern-hash)
   compression-ratio)
  (setf (gethash '"pattern" pattern-hash) pattern)
  (setf (gethash '"region" pattern-hash) region)
  (setf
   (gethash '"translators" pattern-hash) translators)
  (identity pattern-hash))

#|
\noindent Example:
\begin{verbatim}
(setq
 pattern&sources
 '((((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
     (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
     (7/2 60 60 1/2))
    16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0))
   (((1/2 72 67 1/2) (3/2 79 71 1/2))
    1 (130 0 0) 2/3 (100 0 1/2))))
(setq
 dataset
 '((0 48 53 2) (1/2 72 67 1/2) (1 76 69 1/2)
   (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2) 
   (3 64 62 1/2) (7/2 60 60 1/2) (4 36 46 2)
   (9/2 72 67 1/2) (5 76 69 1/2) (11/2 79 71 1/2)
   (6 84 74 2) (13/2 67 64 1/2) (7 64 62 1/2)
   (15/2 60 60 1/2) (8 36 46 2) (17/2 72 67 1/2)
   (9 76 69 1/2) (19/2 79 71 1/2)))
(setq
 patterns-hash
 (evaluate-variables-of-patterns2hash
  pattern&sources dataset
  '(4.277867 3.422478734 -0.038536808 0.651073171)
  '(73.5383283152 0.02114878519)))
--> (#<HASH-TABLE
     :TEST EQUAL size 12/60 #x300041916ACD>
     #<HASH-TABLE
     :TEST EQUAL size 12/60 #x30004188107D>)
(disp-ht-el (first patterns-hash))
--> (("name" . "pattern 1") ("compactness" . 1)
     ("expected occurrences" . 72.79239)
     ("rating" . 5.8717685)
     ("pattern" (1/2 72 67 1/2) (3/2 79 71 1/2))
     ("translators" (0 0 0 0) (4 0 0 0) (8 0 0 0))
     ("index" . 1) ("cardinality" . 2)
     ("MTP vectors" (100 0 1/2) (130 0 0))
     ("compression ratio" . 3/2)
     ("region"
      (1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2))
     ("occurrences" . 3))
\end{verbatim}

\noindent This function applies the function
evaluate-variables-of-pattern2hash recursively. |#

(defun evaluate-variables-of-patterns2hash
       (SIACT-output dataset &optional
	(coefficients
	 (list 4.277867 3.422478734 -0.038536808
	       0.651073171))
	(norm-coeffs
	 (list 73.5383283152 0.02114878519))
	(length-dataset (length dataset))
	(dataset-palette
	 (orthogonal-projection-not-unique-equalp
	  dataset
	  (append
	   (list 0)
	   (constant-vector
	    1
	    (- (length (first dataset)) 1)))))
	(empirical-mass
	 (empirical-mass dataset-palette))
	(index 0)
	(patterns-hash nil))
  (if (null SIACT-output)
    (nth-list
     (nth-list-of-lists
      0
      (sort-by
       '((1 "desc"))
       (collect-indices&ratings patterns-hash)))
     patterns-hash)
    (evaluate-variables-of-patterns2hash
     (rest SIACT-output) dataset coefficients
     norm-coeffs length-dataset dataset-palette
     empirical-mass (+ index 1)
     (append
      patterns-hash
      (list
       (evaluate-variables-of-pattern2hash
	(first SIACT-output) dataset length-dataset
	dataset-palette empirical-mass coefficients
	norm-coeffs index))))))
