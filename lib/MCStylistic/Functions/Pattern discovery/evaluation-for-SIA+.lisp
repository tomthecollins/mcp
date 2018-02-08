#| Copyright 2008-2013 Tom Collins
   Monday 10 August 2009

The aim of these functions is to provide support for
the implementation of COSIATEC
\citep{meredith2003,forth2009}.

; REQUIRED PACKAGES
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
(argmax-of-threeCs
 '((1 5 5/4) (1/10 4 4/3) (1 6 1) (4/5 12 2)))
--> 3
\end{verbatim}

\noindent The argument to this function is a list of
sublists, each containing three elements. Lists are
constructed from these elements along dimensions one
to three, and normalised linearly to $[0, 1]$. Then
the lists are multiplied element-wise, and the
resulting list is searched for the argument of the
maximum element. |#

(defun argmax-of-threeCs
       (threeCs-list-of-lists &optional
	(to-interrogate
	 (multiply-two-lists
	  (normalise-0-1
	   (mapcar
            #'(lambda (x) (nth 0 x))
            threeCs-list-of-lists))
	  (multiply-two-lists
	   (normalise-0-1
	    (mapcar
            #'(lambda (x) (nth 1 x))
            threeCs-list-of-lists))
	   (normalise-0-1
	    (mapcar
            #'(lambda (x) (nth 2 x))
            threeCs-list-of-lists))))))
  (second (max-argmax to-interrogate)))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '((((151/3 84 1/3) (152/3 83 1/3) (51 81 1/3))
    (-8 0 0) (-4 0 0) (0 0 0) (11/3 7 0) (19/3 0 0) 
    (23/3 0 0) (26/3 -5 0))
   (((143/3 84 1/3) (48 83 1/3) (146/3 86 1/3))
    (0 0 0) (23/3 0 0))
   (((52 43 2) (54 31 2) (56 36 2))
    (0 0 0) (8 7 0))
   (((5 76 1/2) (11/2 79 1/2))
    (0 0 0) (225/4 0 -1/4))))
(index-target-translation-in-list-assoc
 '((62 44 2) (64 32 2) (66 37 2)) a-list)
--> 2
\end{verbatim}

\noindent The sublists of the list each contain a
pattern and its translators. We want to know if any
of the patterns are translations of the first
argument, the target. The index of the first extant
translation is returned, and nil otherwise. This
function is used for checking the output of COSIATEC,
as it uses assoc. |#

(defun index-target-translation-in-list-assoc
       (target a-list &optional (i 0))
  (if (null a-list) ()
    (if (test-translation
	 target
	 (car (first a-list)))
      (identity i)
      (index-target-translation-in-list-assoc
       target (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(((1 0 0)
    (66 55 1) (66 65 1) (67 55 1) (68 55 1) (68 64 1)
    (69 55 1))
   ((11/3 -42 5/3)
    (163/3 90 1/3))
   ((10/3 -11 0)
    (163/3 90 1/3))
   ((3 -9 0)
    (163/3 90 1/3) (164/3 88 1/3) (56 88 1/3))))
(index-target-translation-in-list-rassoc
 '((166/3 60 1/3) (167/3 58 1/3) (57 58 1/3)) a-list)
--> 3
\end{verbatim}

\noindent The sublists of the list each contain a
pattern and its translators. We want to know if any
of the patterns are translations of the first
argument, the target. The index of the first extant
translation is returned, and nil otherwise. This
function is used for checking the output of SIA, as it
uses rassoc. |#

(defun index-target-translation-in-list-rassoc
       (target a-list &optional (i 0))
  (if (null a-list) ()
    (if (test-translation
	 target
	 (cdr (first a-list)))
      (identity i)
      (index-target-translation-in-list-rassoc
       target (rest a-list) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '((((151/3 0 1/3) (152/3 11 1/3) (51 10 1/3))
    (-8 0 0) (-4 0 0) (0 0 0) (11/3 7 0) (19/3 0 0) 
    (23/3 0 0) (26/3 -5 0))
   (((143/3 0 1/3) (48 11 1/3) (146/3 10 1/3))
    (0 0 0) (23/3 0 0))
   (((52 7 2) (54 7 2) (56 0 2))
    (0 0 0) (8 7 0))
   (((5 4 1/2) (11/2 7 1/2))
    (0 0 0) (225/4 0 -1/4))))
(index-target-translation-mod-in-list-assoc
 '((62 0 2) (64 0 2) (66 5 2)) a-list 12)
--> 2
\end{verbatim}

\noindent This function is very similar to the
function index-target-translation-in-list-assoc,
except that in the second dimension translations are
carried out modulo the third argument. The sublists of
the list each contain a pattern and its translators.
We want to know if any of the patterns are
translations of the first argument, the target. The
index of the first extant translation is returned, and
nil otherwise. This function is used for checking the
output of the function COSIATEC-mod-2nd-n, as it uses
assoc (when the dataset has been projected modulo
$n$). |#

(defun index-target-translation-mod-in-list-assoc
       (target a-list n &optional (i 0))
  (if (null a-list) ()
    (if (test-translation-mod-2nd-n
	 target (car (first a-list)) n)
      (identity i)
      (index-target-translation-mod-in-list-assoc
       target (rest a-list) n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(((1 0 0)
    (66 55 1) (66 65 1) (67 55 1) (68 55 1) (68 64 1)
    (69 55 1))
   ((11/3 -42 5/3)
    (163/3 90 1/3))
   ((10/3 -11 0)
    (163/3 90 1/3))
   ((3 -9 0)
    (163/3 90 1/3) (164/3 88 1/3) (56 88 1/3))))
(index-target-translation-mod-in-list-rassoc
 '((166/3 0 1/3) (167/3 10 1/3) (57 10 1/3))
 a-list 12)
--> 3
\end{verbatim}

\noindent This function is very similar to the
function index-target-translation-in-list-rassoc,
except that in the second dimension translations are
carried out modulo the third argument. The sublists of
the list each contain a pattern and its translators.
We want to know if any of the patterns are
translations of the first argument, the target. The
index of the first extant translation is returned, and
nil otherwise. This function is used for checking the
output of the function SIA-mod-2nd-n, as it uses
rassoc (when the dataset has been projected modulo
$n$). |#

(defun index-target-translation-mod-in-list-rassoc
       (target a-list n &optional (i 0))
  (if (null a-list) ()
    (if (test-translation-mod-2nd-n
	 target (cdr (first a-list)) n)
      (identity i)
      (index-target-translation-mod-in-list-rassoc
       target (rest a-list) n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '((((151/3 84 1/3) (152/3 83 1/3) (51 81 1/3))
    (-8 0 0) (-4 0 0) (0 0 0) (11/3 7 0) (19/3 0 0) 
    (23/3 0 0) (26/3 -5 0))
   (((143/3 84 1/3) (48 83 1/3) (146/3 86 1/3))
    (0 0 0) (23/3 0 0))
   (((52 43 2) (54 31 2) (56 36 2))
    (0 0 0) (8 7 0))
   (((5 76 1/2) (11/2 79 1/2))
    (0 0 0) (225/4 0 -1/4))))
(number-of-targets-translation-in-list-assoc
 '(((62 44 2) (64 32 2) (66 37 2))
   ((5 76 1/2) (11/2 79 1/2))
   ((5 76 1/2) (6 79 1/2)))
 a-list)
--> 2
\end{verbatim}

\noindent The function index-target-translation-in-
list-assoc is applied recursively to each member of
the first argument of this function. This argument is
a list of targets. Each time a translation of a target
is detected, the output (initially set to zero) is
incremented by one. |#

(defun number-of-targets-translation-in-list-assoc
       (targets a-list &optional (result 0))
  (if (null targets) (identity result)
    (if (index-target-translation-in-list-assoc
	 (first targets) a-list)
      (number-of-targets-translation-in-list-assoc
       (rest targets) a-list (+ result 1))
      (number-of-targets-translation-in-list-assoc
       (rest targets) a-list result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(((1 0 0)
    (66 55 1) (66 65 1) (67 55 1) (68 55 1) (68 64 1)
    (69 55 1))
   ((11/3 -42 5/3)
    (163/3 90 1/3))
   ((10/3 -11 0)
    (163/3 90 1/3))
   ((3 -9 0)
    (163/3 90 1/3) (164/3 88 1/3) (56 88 1/3))))
(number-of-targets-translation-in-list-rassoc
 '(((166/3 60 1/3) (167/3 58 1/3) (57 58 1/3))
   ((66 55 1) (66 65 1) (67 55 1) (68 55 1) (68 64 1)
    (69 55 1))
   ((163/3 90 1/3)))
 a-list)
--> 3
\end{verbatim}

\noindent The function index-target-translation-in-
list-rassoc is applied recursively to each member of
the first argument of this function. This argument is
a list of targets. Each time a translation of a target
is detected, the output (initially set to zero) is
incremented by one. |#

(defun number-of-targets-translation-in-list-rassoc
       (targets a-list &optional (result 0))
  (if (null targets) (identity result)
    (if (index-target-translation-in-list-rassoc
	 (first targets) a-list)
      (number-of-targets-translation-in-list-rassoc
       (rest targets) a-list (+ result 1))
      (number-of-targets-translation-in-list-rassoc
       (rest targets) a-list result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '((((151/3 84 1/3) (152/3 83 1/3) (51 81 1/3))
    (-8 0 0) (-4 0 0) (0 0 0) (11/3 7 0) (19/3 0 0) 
    (23/3 0 0) (26/3 -5 0))
   (((143/3 84 1/3) (48 83 1/3) (146/3 86 1/3))
    (0 0 0) (23/3 0 0))
   (((52 43 2) (54 31 2) (56 36 2))
    (0 0 0) (8 7 0))
   (((5 76 1/2) (11/2 79 1/2))
    (0 0 0) (225/4 0 -1/4))))
(number-of-targets-trans-mod-in-list-assoc
 '(((62 8 2) (64 8 2) (66 1 2))
   ((5 4 1/2) (11/2 7 1/2))
   ((5 4 1/2) (6 7 1/2)))
 a-list 12)
--> 2
\end{verbatim}

\noindent The function index-target-translation-mod-
in-list-assoc is applied recursively to each member of
the first argument of this function. This argument is
a list of targets. Each time a translation (modulo the
third argument) of a target is detected, the output
(initially set to zero) is incremented by one. |#

(defun number-of-targets-trans-mod-in-list-assoc
       (targets a-list n &optional (result 0))
  (if (null targets) (identity result)
    (if (index-target-translation-mod-in-list-assoc
	 (first targets) a-list n)
      (number-of-targets-trans-mod-in-list-assoc
       (rest targets) a-list n (+ result 1))
      (number-of-targets-trans-mod-in-list-assoc
       (rest targets) a-list n result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(((1 0 0)
    (66 55 1) (66 65 1) (67 55 1) (68 55 1) (68 64 1)
    (69 55 1))
   ((11/3 -42 5/3)
    (163/3 90 1/3))
   ((10/3 -11 0)
    (163/3 90 1/3))
   ((3 -9 0)
    (163/3 90 1/3) (164/3 88 1/3) (56 88 1/3))))
(number-of-targets-trans-mod-in-list-rassoc
 '(((166/3 1 1/3) (167/3 11 1/3) (57 11 1/3))
   ((66 8 1) (66 6 1) (67 8 1) (68 8 1) (68 11 1)
    (69 8 1))
   ((163/3 6 1/3)))
 a-list 12)
--> 2
\end{verbatim}

\noindent The function index-target-translation-mod-
in-list-rassoc is applied recursively to each member
of the first argument of this function. This argument
is a list of targets. Each time a translation (modulo
the third argument) of a target is detected, the
output (initially set to zero) is incremented by
one. |#

(defun number-of-targets-trans-mod-in-list-rassoc
       (targets a-list n &optional (result 0))
  (if (null targets) (identity result)
    (if (index-target-translation-mod-in-list-rassoc
	 (first targets) a-list n)
      (number-of-targets-trans-mod-in-list-rassoc
       (rest targets) a-list n (+ result 1))
      (number-of-targets-trans-mod-in-list-rassoc
       (rest targets) a-list n result))))

#|
\noindent Example:
\begin{verbatim}
(threeCs-pattern-trans-pair-mod-2nd-n
 '((0 0) (1 1)) '((0 0) (1 1) (3 0))
 '((0 0) (1 1) (2 2) (3 0) (5/2 7) (4 1)) 12)
--> (1 5 5/4)
\end{verbatim}

\noindent A pattern and its translators in a projected
dataset are supplied as arguments to this function.
The output is the compactness, coverage and
compression ratio, with translations in the second
dimension being carried out modulo the fourth
argument. |#

(defun threeCs-pattern-trans-pair-mod-2nd-n
       (pattern translators dataset n &optional
	(cardinality (length pattern))
	(occurrences (length translators))
	(region
	 (subseq
	  dataset
	  (index-item-1st-occurs
	   (first pattern) dataset)
	  (+ (index-item-1st-occurs
	      (my-last pattern) dataset) 1)))
	(span (length region))
	(compactness (/ cardinality span))
	(coverage
	 (coverage-mod-2nd-n
	  pattern translators dataset n t t))
	(compression-ratio
	 (/ coverage
	    (- (+ cardinality occurrences) 1))))
  (list compactness coverage compression-ratio))

#|
\noindent Example:
\begin{verbatim}
(threeCs-pattern-trans-pairs-mod-2nd-n
 '((((0 11) (1 0) (3 11)) (0 0) (1 1))
   (((0 11)) (0 0) (1 1) (2 2) (3 0) (5/2 8) (4 1)))
 '((0 11) (1 0) (2 1) (3 11) (5/2 7) (4 0))
 12)
--> ((3/4 5 5/4) (1 6 1))
\end{verbatim}

\noindent Pairs (consisting of patterns and their
translators and sometimes referred to as SIATEC-
output) in a projected dataset are supplied as
arguments to this function. The output is a list of
lists, each of which contains the compactness,
coverage and compression ratio of the corresponding
pattern. Translations in the second dimension are
carried out modulo the third argument. |#

(defun threeCs-pattern-trans-pairs-mod-2nd-n
       (SIATEC-output dataset n)
  (if (null SIATEC-output) ()
    (cons
     (threeCs-pattern-trans-pair-mod-2nd-n
      (car (first SIATEC-output))
      (cdr (first SIATEC-output)) dataset n)
     (threeCs-pattern-trans-pairs-mod-2nd-n
      (rest SIATEC-output) dataset n))))

#|
\noindent Example:
\begin{verbatim}
(threeCs-pattern-translators-pair
 '((0 60) (1 61)) '((0 0) (1 1) (3 0))
 '((0 60) (1 61) (2 62) (3 60) (5/2 67) (4 61)))
--> (1 5 5/4)
\end{verbatim}

\noindent A pattern and its translators in a projected
dataset are supplied as arguments to this function.
The output is the compactness, coverage and
compression ratio. |#

(defun threeCs-pattern-translators-pair
       (pattern translators dataset &optional
	(cardinality (length pattern))
	(occurrences (length translators))
	(region
	 (subseq
	  dataset
	  (index-item-1st-occurs
	   (first pattern) dataset)
	  (+ (index-item-1st-occurs
	      (my-last pattern) dataset) 1)))
	(span (length region))
	(compactness (/ cardinality span))
	(coverage
	 (coverage pattern translators dataset t t))
	(compression-ratio
	 (/ coverage
	    (- (+ cardinality occurrences) 1))))
  (list compactness coverage compression-ratio))

#|
\noindent Example:
\begin{verbatim}
(threeCs-pattern-translators-pairs
 '((((0 60) (1 61)) (0 0) (1 1) (3 0))
   (((0 60)) (0 0) (1 1) (2 2) (3 0) (5/2 7) (4 1)))
 '((0 60) (1 61) (2 62) (3 60) (5/2 67) (4 61)))
--> ((1 5 5/4) (1 6 1))
\end{verbatim}

\noindent Pairs (consisting of patterns and their
translators and sometimes referred to as SIATEC-
output) in a projected dataset are supplied as
arguments to this function. The output is a list of
lists, each of which contains the compactness,
coverage and compression ratio of the corresponding
pattern. |#

(defun threeCs-pattern-translators-pairs
       (SIATEC-output dataset)
  (if (null SIATEC-output) ()
    (cons
     (threeCs-pattern-translators-pair
      (car (first SIATEC-output))
      (cdr (first SIATEC-output)) dataset)
     (threeCs-pattern-translators-pairs
      (rest SIATEC-output) dataset))))
