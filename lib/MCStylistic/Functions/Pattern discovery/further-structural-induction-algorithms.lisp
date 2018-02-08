#| Copyright 2008-2013 Tom Collins
   Monday 25 January 2010

The functions below implement SIATEC (Structural
Induction Algorithm for Transational Equivalence
Classes) as described by \citet{meredith2002}, and
COSIATEC (COvering Strucutral Induction Algorithm for
Translational Equivalence Classes) as described by 
\citet{forth2009,meredith2003}.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "evaluation-for-SIA+"
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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(COSIATEC
 '((0 61) (0 65) (1 64) (4 62) (4 66) (5 65) (8 60)
   (8 64) (9 63) (12 56) (13 69) (15 65) (16 57)
   (16 59) (17 64) (19 63))
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/COSIATEC output"))
--> 1 2 T
\end{verbatim}

\noindent Implementation of the COSIATEC algorithm. It
can be verified (by checking the files created in the
specified location) that the output (pattern-
translators pairs) constitutes a cover of the input
dataset. |#

(defun COSIATEC
       (dataset prefix &optional (counter 1)
	(growing-list nil)
	(SIA-path&name
	 (concatenate
	  'string
	  prefix " SIA " (write-to-string counter)
	  ".txt"))
	(SIATEC-path&name
	 (concatenate
	  'string
	  prefix " SIATEC " (write-to-string counter)
	  ".txt"))
	(SIATEC-output
	 (if dataset
	   (progn
	     (SIA-reflected-for-COSIATEC
	      dataset SIA-path&name)
	     (SIATEC
	      (read-from-file SIA-path&name) dataset
	      SIATEC-path&name)
	     (read-from-file SIATEC-path&name))))
	(pattern&translators
	 (if SIATEC-output
	   (nth
	    (argmax-of-threeCs
	     (threeCs-pattern-translators-pairs
	      SIATEC-output dataset))
	    SIATEC-output))))
  (if (null pattern&translators)
    (write-to-file
     growing-list
     (concatenate 'string prefix " COSIATEC.txt"))
    (progn
      (print counter)
      (COSIATEC
       (remove-pattern-occurrences-from-dataset
	pattern&translators dataset)
       prefix (+ counter 1)
       (append
	growing-list (list pattern&translators))))))

#|
\noindent Example:
\begin{verbatim}
(COSIATEC-mod-2nd-n
 '((0 1) (0 5) (1 4) (4 2) (4 6) (5 5) (8 0)
   (8 4) (9 3) (12 8) (13 9) (15 5) (16 9)
   (16 11) (17 4) (19 3))
 12
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/COSIATEC mod 2nd output"))
--> 1 2 T
\end{verbatim}

\noindent Implementation of the COSIATEC algorithm,
where translations in the second dimension are
performed modulo the second argument. It can be
verified (by checking the files created in the
specified location) that the output above
(pattern-translators pairs) constitutes a cover of the
input dataset. |#

(defun COSIATEC-mod-2nd-n
       (dataset n prefix &optional (counter 1)
	(growing-list nil)
	(SIA-path&name
	 (concatenate
	  'string
	  prefix " SIA " (write-to-string counter)
	  ".txt"))
	(SIATEC-path&name
	 (concatenate
	  'string
	  prefix " SIATEC " (write-to-string counter)
	  ".txt"))
	(SIATEC-output
	 (if dataset
	   (progn
	     (SIA-reflected-for-COSIATEC-mod-2nd-n
	      dataset n SIA-path&name)
	     (SIATEC-mod-2nd-n
	      (read-from-file SIA-path&name) dataset n
	      SIATEC-path&name)
	     (read-from-file SIATEC-path&name))))
	(pattern&translators
	 (if SIATEC-output
	   (nth
	    (argmax-of-threeCs
	     (threeCs-pattern-trans-pairs-mod-2nd-n
	      SIATEC-output dataset n))
	    SIATEC-output))))
  (if (null pattern&translators)
    (write-to-file
     growing-list
     (concatenate 'string prefix " COSIATEC.txt"))
    (progn
      (print counter)
      (COSIATEC-mod-2nd-n
       (remove-pattern-occs-from-dataset-mod-2nd-n
	pattern&translators dataset n)
       n prefix (+ counter 1)
       (append
	growing-list (list pattern&translators))))))

#|
\noindent Example:
\begin{verbatim}
(remove-pattern-occurrences-from-dataset
 '(((0 60) (1 61)) (0 0) (1 1) (4 -1))
 '((0 60) (1 61) (2 62) (3 60) (4 59) (5 60) (6 57)))
--> ((3 60) (6 57))
\end{verbatim}

\noindent All of the datapoints that are members of
occurrences of a given pattern are calculated. These
datapoints are then removed from the dataset (calling
the function set-difference-multidimensional-sorted-
asc), and the new dataset returned. |#

(defun remove-pattern-occurrences-from-dataset
       (pattern&translators dataset &optional
	(to-be-removed
	 (unions-multidimensional-sorted-asc
	  (translations
	   (car pattern&translators)
	   (cdr pattern&translators)))))
  (set-difference-multidimensional-sorted-asc
   dataset to-be-removed))

#|
\noindent Example:
\begin{verbatim}
(remove-pattern-occs-from-dataset-mod-2nd-n
 '(((0 0) (1 1)) (0 0) (1 1) (4 11))
 '((0 0) (1 1) (2 2) (3 0) (4 11) (5 0) (6 9))
 12)
--> ((3 0) (6 9))
\end{verbatim}

\noindent All of the datapoints that are members of
occurrences of a given pattern are calculated, where
translations in the second dimension are carried out
modulo the third argument. These datapoints are then
removed from the dataset (calling the function set-
difference-multidimensional-sorted-asc), and the new
dataset returned. |#

(defun remove-pattern-occs-from-dataset-mod-2nd-n
       (pattern&translators dataset n &optional
	(to-be-removed
	 (unions-multidimensional-sorted-asc
	  (translations-mod-2nd-n
	   (car pattern&translators)
	   (cdr pattern&translators) n))))
  (set-difference-multidimensional-sorted-asc
   dataset to-be-removed))

#|
\noindent Example:
\begin{verbatim}
(SIA-reflected-for-COSIATEC
 '((0 61) (0 65) (1 64) (4 62) (4 66) (5 65) (8 60)
   (8 64) (9 63) (12 56) (13 69) (15 65) (16 57)
   (16 59) (17 64) (19 63))
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/SIA4COSIATEC output.txt"))
--> T
\end{verbatim}

\noindent This function is a version of the SIA
algorithm. It is called `SIA-reflected-for-COSIATEC'
because it is a slight variant on SIA-reflected. In
particular it does not allow a partition size to be
set. |#

(defun SIA-reflected-for-COSIATEC
       (dataset path&name &optional
        (growing-list nil)
	(first-dataset (first dataset))
	(rest-dataset (rest dataset))
	(probe
	 (if (null rest-dataset) ()
	   (subtract-two-lists
	    (first rest-dataset) (first dataset))))
	(result-recent
	 (assoc probe growing-list :test #'equalp)))
  (if (null dataset)
    (write-to-file growing-list path&name)
    (if (null probe)
      (SIA-reflected-for-COSIATEC
       (rest dataset) path&name growing-list)
      (if result-recent
	(SIA-reflected-for-COSIATEC
	 dataset path&name
	 (progn
	   (rplacd
	    (assoc probe
		   growing-list
		   :test #'equalp)
	    (append
	     (cdr result-recent)
	     (list first-dataset)))
	   (identity growing-list))
	 first-dataset (rest rest-dataset))
	(SIA-reflected-for-COSIATEC
	 dataset path&name
	 (append
	  (list
	   (cons probe
		 (list first-dataset)))
	  growing-list)
	 first-dataset
	 (rest rest-dataset))))))

#|
\noindent Example:
\begin{verbatim}
(SIA-reflected-for-COSIATEC-mod-2nd-n
 '((0 1) (0 5) (1 4) (4 2) (4 6) (5 5) (8 0)
   (8 4) (9 3) (12 8) (13 9) (15 5) (16 9)
   (16 11) (17 4) (19 3))
 12
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/SIA4COSIATEC mod 2nd output.txt"))
--> T
\end{verbatim}

\noindent This function is a version of the SIA
algorithm. It is called `SIA-reflected-for-COSIATEC'
because it is a slight variant on SIA-reflected. In
particular it does not allow a partition size to be
set. Also in the mod-2nd-n version, translations in
the second dimension are made modulo the second
argument. |#

(defun SIA-reflected-for-COSIATEC-mod-2nd-n
       (dataset n path&name &optional
        (growing-list nil)
	(first-dataset (first dataset))
	(rest-dataset (rest dataset))
	(probe
	 (if (null rest-dataset) ()
	   (subtract-two-lists-mod-2nd-n
	    (first rest-dataset) (first dataset) n)))
	(result-recent
	 (assoc probe growing-list :test #'equalp)))
  (if (null dataset)
    (write-to-file growing-list path&name)
    (if (null probe)
      (SIA-reflected-for-COSIATEC-mod-2nd-n
       (rest dataset) n path&name growing-list)
      (if result-recent
	(SIA-reflected-for-COSIATEC-mod-2nd-n
	 dataset n path&name
	 (progn
	   (rplacd
	    (assoc probe
		   growing-list
		   :test #'equalp)
	    (append
	     (cdr result-recent)
	     (list first-dataset)))
	   (identity growing-list))
	 first-dataset (rest rest-dataset))
	(SIA-reflected-for-COSIATEC-mod-2nd-n
	 dataset n path&name
	 (append
	  (list
	   (cons probe
		 (list first-dataset)))
	  growing-list)
	 first-dataset
	 (rest rest-dataset))))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   SIA-output
   '(((1/2 60) (1 62) (3/2 64) (2 67) (5/2 69) (3 71)
      (7/2 74) (4 71) (9/2 69) (5 67) (11/2 64)
      (6 60))
     ((49/4 71) (25/2 72) (51/4 73) (13 69) (13 74)
      (27/2 68) (27/2 73) (14 69) (14 74) (29/2 68)
      (29/2 73) (15 69) (15 74))))
  (setq
   dataset
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/scarlatti-L10-bars1-19.txt")))
  (setq
   projected-dataset
   (orthogonal-projection-unique-equalp
    dataset '(1 0 1 0 0)))
  (setq
   path&name
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/SIATEC output.txt"))
  (SIATEC SIA-output projected-dataset path&name))
--> T
\end{verbatim}

\noindent This function applies the SIATEC algorithm
to the output of the function SIA-reflected. The
example causes a file to be created in the specified
location. |#

(defun SIATEC
       (SIA-output dataset path&name &optional
	(growing-list nil)
	(probe
	 (cdr (first SIA-output)))
	(result
	 (if probe
	   (assoc
	    probe growing-list
	    :test #'test-translation))))
  (if (null SIA-output)
    (write-to-file growing-list path&name)
    (if result
      (SIATEC
       (rest SIA-output) dataset path&name
       growing-list)
      (SIATEC
       (rest SIA-output) dataset path&name
       (cons
	(cons
	 probe
	 (translators-of-pattern-in-dataset
	  probe dataset))
	growing-list)))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   SIA-mod-2nd-n-output
   '(((1/2 4) (1 6) (3/2 1) (2 4) (5/2 6) (3 1)
      (7/2 4) (4 1) (9/2 6) (5 4) (11/2 1) (6 4))
     ((49/4 1) (25/2 2) (51/4 3) (13 6) (13 4)
      (27/2 5) (27/2 3) (14 6) (14 4) (29/2 5)
      (29/2 3) (15 6) (15 4))))
  (setq
   dataset
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/scarlatti-L10-bars1-19.txt")))
  (setq
   dataset-1-0-1-0-0
   (orthogonal-projection-unique-equalp
    dataset '(1 0 1 0 0)))
  (setq
   dataset-1-0-1*-1-0
   (sort-dataset-asc
    (mod-column
     dataset-1-0-1-0-0 7 1)))
  (setq
   path&name
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/SIATEC mod 2nd output.txt"))
  (SIATEC-mod-2nd-n
   SIA-mod-2nd-n-output dataset-1-0-1*-1-0 7
   path&name))
--> T
\end{verbatim}

\noindent This function applies the SIATEC algorithm
to the output of the function SIA-reflected, where
translations in the second dimension are carried out
modulo the third argument. The example causes a file
to be created in the specified location. |#

(defun SIATEC-mod-2nd-n
       (SIA-output dataset n path&name &optional
	(growing-list nil)
	(probe
	 (cdr (first SIA-output)))
	(result
	 (if probe
	   (assoc
	    probe growing-list
	    :test #'test-translation-mod-2nd-n))))
  (if (null SIA-output)
    (write-to-file growing-list path&name)
    (if result
      (SIATEC-mod-2nd-n
       (rest SIA-output) dataset n path&name
       growing-list)
      (SIATEC-mod-2nd-n
       (rest SIA-output) dataset n path&name
       (cons
	(cons
	 probe
	 (translators-of-pattern-in-dataset-mod-2nd-n
	  probe dataset n))
	growing-list)))))
