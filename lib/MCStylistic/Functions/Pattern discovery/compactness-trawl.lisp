#| Copyright 2008-2013 Tom Collins
   Tuesday 12 January 2010

These functions are designed to trawl through already-
discovered patterns (usually MTPs) from beginning to
end. They return subpatterns that have compactness
\citep{meredith2003} and cardinality greater than
thresholds that can be specified
\citep{collins2010b}.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(compact-subpatterns
 '((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
   (2 84 74 2) (7/2 60 60 1/2) (5 76 69 1/2)
   (11/2 79 71 1/2) (6 84 74 2) (13/2 67 64 1/2)
   (15/2 60 60 1/2))
 '((0 48 53 2) (1/2 72 67 1/2) (1 76 69 1/2)
   (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2)
   (3 64 62 1/2) (7/2 60 60 1/2) (4 36 46 2)
   (9/2 72 67 1/2) (5 76 69 1/2) (11/2 79 71 1/2)
   (6 84 74 2) (13/2 67 64 1/2) (7 64 62 1/2)
   (15/2 60 60 1/2) (8 36 46 2) (17/2 72 67 1/2))
 2/3 3)
--> (((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
      (2 84 74 2) (7/2 60 60 1/2))
     ((5 76 69 1/2) (11/2 79 71 1/2) (6 84 74 2)
      (13/2 67 64 1/2) (15/2 60 60 1/2)))
\end{verbatim}

\noindent This function takes a pattern and looks
within that pattern for subpatterns that have
compactness and cardinality greater than certain
thresholds, which are optional arguments. In this
version, just the subpatterns are returned. |#

(defun compact-subpatterns
       (pattern dataset &optional
        (compactness-threshold 2/3)
        (cardinality-threshold 5)
        (region-type "lexicographic")
        (result nil)
        (catch (list (first pattern)))
        (catch-compactness
         (if (my-last catch)
           (compactness
            catch dataset region-type))))
  (if (null pattern)
    (if (>= (length (butlast catch))
            cardinality-threshold)
      (append result (list (butlast catch)))
      (identity result))
    (if (>= catch-compactness compactness-threshold)
      (compact-subpatterns
       (rest pattern) dataset compactness-threshold
       cardinality-threshold region-type result
       (append catch (list (second pattern))))
      (compact-subpatterns
       pattern dataset compactness-threshold
       cardinality-threshold region-type
       (if (>= (length (butlast catch))
               cardinality-threshold)
         (append result (list (butlast catch)))
         (identity result))))))

#|
\noindent Example:
\begin{verbatim}
(compact-subpatterns-more-output
 '((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
   (2 84 74 2) (7/2 60 60 1/2) (5 76 69 1/2)
   (11/2 79 71 1/2) (6 84 74 2) (13/2 67 64 1/2)
   (15/2 60 60 1/2))
 '((0 48 53 2) (1/2 72 67 1/2) (1 76 69 1/2)
   (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2)
   (3 64 62 1/2) (7/2 60 60 1/2) (4 36 46 2)
   (9/2 72 67 1/2) (5 76 69 1/2) (11/2 79 71 1/2)
   (6 84 74 2) (13/2 67 64 1/2) (7 64 62 1/2)
   (15/2 60 60 1/2) (8 36 46 2) (17/2 72 67 1/2))
 2/3 3)
--> ((((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
       (2 84 74 2) (7/2 60 60 1/2))
      ((5 76 69 1/2) (11/2 79 71 1/2) (6 84 74 2)
       (13/2 67 64 1/2) (15/2 60 60 1/2)))
     (5/7 5/6))
\end{verbatim}

\noindent This function takes a pattern and looks
within that pattern for subpatterns that have
compactness and cardinality greater than certain
thresholds, which are optional arguments. In this
version, the subpatterns and corresponding compactness
values are returned. |#

(defun compact-subpatterns-more-output
       (pattern dataset &optional
        (compactness-threshold 2/3)
        (cardinality-threshold 5)
        (region-type "lexicographic")
        (subpatterns nil)
	(compactnesses nil)
	(last-compactness nil)
        (catch (list (first pattern)))
        (catch-compactness
         (if (my-last catch)
           (compactness
            catch dataset region-type))))
  (if (null pattern)
    (if (>= (length (butlast catch))
            cardinality-threshold)
      (append
       (list
	(append subpatterns (list (butlast catch))))
       (list
	(append
	 compactnesses (list last-compactness))))
      (append
       (list subpatterns) (list compactnesses)))
    (if (>= catch-compactness compactness-threshold)
      (compact-subpatterns-more-output
       (rest pattern) dataset compactness-threshold
       cardinality-threshold region-type subpatterns
       compactnesses catch-compactness
       (append catch (list (second pattern))))
      (compact-subpatterns-more-output
       pattern dataset compactness-threshold
       cardinality-threshold region-type
       (if (>= (length (butlast catch))
               cardinality-threshold)
         (append subpatterns (list (butlast catch)))
         (identity subpatterns))
       (if (>= (length (butlast catch))
               cardinality-threshold)
         (append
	  compactnesses (list last-compactness))
         (identity compactnesses))
       last-compactness))))

#|
\noindent Example: see Discovering and rating musical
patterns (Sec.~\ref{sec:disc&rate-patterns}),
especially lines 101-107). 
\vspace{0.5cm}

\noindent The compactness trawler \citep{collins2010b}
applies the function compact-subpatterns-more-output
recursively to the output of SIA (Structure Induction
Algorithm, \citeauthor{meredith2002},
\citeyear{meredith2002}), or any output with an
analogous list format. |#

(defun compactness-trawler
       (SIA-output dataset path&name &optional
	(compactness-threshold 2/3)
	(cardinality-threshold 5)
        (region-type "lexicographic")
	(growing-list nil)
	(subpatterns&compactness
	 (compact-subpatterns-more-output
	  (cdr (first SIA-output)) dataset
	  compactness-threshold
	  cardinality-threshold region-type))
	(probes
	 (first subpatterns&compactness))
	(compactness-values
	 (second subpatterns&compactness))
	(probe (first probes))
	(compactness-value (first compactness-values))
	(result
	 (if probe
	   (assoc
	    probe growing-list
	    :test #'test-translation))))
  (if (null SIA-output)
    (write-to-file growing-list path&name)
    (if (null probes)
      (compactness-trawler
       (rest SIA-output) dataset path&name
       compactness-threshold cardinality-threshold
       region-type growing-list)
      (if result
	(compactness-trawler
	 SIA-output dataset path&name
	 compactness-threshold cardinality-threshold
         region-type
	 (progn
	   (rplacd
	    (assoc
	     probe growing-list
	     :test #'test-translation)
	    (append
	     (cdr result)
	     (list compactness-value
		   (car (first SIA-output)))))
	   (identity growing-list))
	 subpatterns&compactness
	 (rest probes)
	 (rest compactness-values))
	(compactness-trawler
	 SIA-output dataset path&name
	 compactness-threshold cardinality-threshold
         region-type
	 (cons
	  (cons
	   probe
	   (list compactness-value
		 (car (first SIA-output))))
	  growing-list)
	 subpatterns&compactness (rest probes)
	 (rest compactness-values))))))
