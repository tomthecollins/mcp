#| Copyright 2008-2013 Tom Collins
   Friday 15 January 2010

The functions below include two early implementations
of SIA (Structure induction algorithm,
\citeauthor{meredith2002}, \citeyear{meredith2002}),
one version working modulo $n$.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
   (merge-pathnames
    (make-pathname
   :directory '(:relative "Third party" "cl-fad")
   :name "fad2"
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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

; (defvar *pitch-mod* 12)

#|
\noindent Example:
\begin{verbatim}
(assoc-files
 '(2 (2 6)) nil 1 1
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Racchman-Oct2010 example")
   :name "initial-states" :type "txt")
  *MCStylistic-MonthYear-example-files-results-path*))
--> (1
     ((2 (2 6))
      (NIL NIL "C-17-4"
       ((1 57 58 1 1 2 0) (1 59 59 1 1 2 1)
        (1 65 63 1 1 2 2)))))
\end{verbatim}

\noindent The arguments to this function are a probe,
a path\&name, and a positive integer. The integer
indicates how many files there are with the specified
path\&name. Each one, assumed to contain an assoc-
list, is read in turn and probed for the presence of
the argument in probe. If it is present the relevant
row is returned. |#

(defun assoc-files
       (probe path&name number-of-files &optional
        (filename-counter 1)
        (filename
         (if (<= filename-counter number-of-files)
           (merge-pathnames
            (make-pathname
             :name
             (concatenate
              'string
              (pathname-name path&name) " "
              (write-to-string filename-counter))
             :type (pathname-type path&name))
            (pathname-directory-pathname
             path&name))))
        (list-to-probe
         (read-from-file filename))
	(result
	 (assoc probe list-to-probe :test #'equalp)))
  (if (null filename) ()
    (if result
      (append (list filename-counter) (list result))
      (assoc-files
       probe path&name number-of-files
       (+ filename-counter 1)))))

#|
\noindent Example:
\begin{verbatim}
(add-two-lists-mod-2nd-n '(4 2 -3) '(8 60 -3) 12)
--> (12 2 -6)
\end{verbatim}

\noindent Adds two lists element-by-element, treating
the second elements of each list modulo $n$. |#

(defun add-two-lists-mod-2nd-n (a-list b-list n)
  (if (null a-list) ()
    (append
     (list (+ (first a-list) (first b-list)))
     (list (mod
            (+ (second a-list) (second b-list)) n))
     (add-two-lists
      (rest (rest a-list)) (rest (rest b-list))))))

#|
\noindent Example:
\begin{verbatim}
(check-potential-translators-mod-2nd-n
 '(3 4) '((0 0) (1 2) (1 5) (2 9))
 '((0 0) (3 4) (4 9) (5 1)) 12)
--> ((0 0) (1 5) (2 9))
\end{verbatim}

\noindent This function is very similar to the
function check-potential-translators. The difference
is that the translation of the 2nd element is being
carried out modulo n. |#

(defun check-potential-translators-mod-2nd-n
       (patternpoint potential-translators dataset n)
  (if (null potential-translators) ()
    (append
     (test-equal<potential-translator-mod-2nd-n
      dataset patternpoint
      (first potential-translators) n)
     (check-potential-translators-mod-2nd-n
      patternpoint (rest potential-translators)
      dataset n))))

#|
\noindent Example:
\begin{verbatim}
(dataset-restricted-to-m-in-nth
 '((12 41 49 1 1) (12 81 72 2 0) (13 53 56 1 1)
   (14 55 57 1 1) (14 74 68 2 0) (15 43 50 1 1)
   (16 36 46 2 1) (16 72 67 1/2 0)) 1 4) 
--> ((12 41 49 1 1) (13 53 56 1 1) (14 55 57 1 1)
     (15 43 50 1 1) (16 36 46 2 1))
\end{verbatim}

\noindent This function acts on a list of sublists.
The nth item of each sublist is tested for equality
(equalp) with the second argument. If it is equal it
is retained, otherwise it is not included in the
output. |#

(defun dataset-restricted-to-m-in-nth
       (dataset m n)
  (if (null dataset) ()
    (append
     (if (equalp (nth n (first dataset)) m)
       (list (first dataset)))
     (dataset-restricted-to-m-in-nth
      (rest dataset) m n))))

#|
\noindent Example:
\begin{verbatim}
(indices-of-matrix-passing-tests
 '((2 4 -1 6 9) (0 0 4 2 -7) (-3 -2 -1 4 1)
   (2 4 -1 3.3 9) (0 0 4 6.8 -7)) (list #'>= #'<) 
   '(3.9 6.8))
--> ((0 1) (0 3) (1 2) (2 3) (3 1) (4 2))
\end{verbatim}

\noindent The first argument to this function is a
matrix in list representation (a list of sublists,
where each sublist correspond to a row, and the $j$th
item of each sublist to the $j$th column). The tests
specified in the second argument are applied for
constants specified in the third argument. If an
element of the matrix passes all tests, its index in
$(i, j)$ form is appended to the output list. |#

(defun indices-of-matrix-passing-tests
       (a-matrix-list fn x &optional
        (m (length a-matrix-list))
        (n (length (first a-matrix-list)))
        (i 0) (j 0) (rel-idx nil))
  (if (>= i m) (identity rel-idx)
    (if (>= j n)
      (indices-of-matrix-passing-tests
       a-matrix-list fn x m n (+ i 1) 0 rel-idx)
      (if (test-all-true
           (mapcar 
            #'(lambda (y z)
                (funcall
                 y
                 (nth j (nth i a-matrix-list))
                 z)) fn x))
        (indices-of-matrix-passing-tests
         a-matrix-list fn x m n i (+ j 1)
         (append rel-idx (list (list i j))))
        (indices-of-matrix-passing-tests
         a-matrix-list fn x m n i (+ j 1) rel-idx)))))

#|
\noindent Example:
\begin{verbatim}
(maximal-translatable-pattern-mod-2nd-n
 '(2 0) '((0 0) (1 1) (1 2) (2 0) (2 5) (3 1)) 12)
--> ((0 0) (1 1))
\end{verbatim}

\noindent This function computes the maximal
translatable pattern of an arbitrary vector
$\mathbf{u}$, searching in some dataset $D$, and
treating the second element of each datapoint modulo
$n$. |#

(defun maximal-translatable-pattern-mod-2nd-n
       (vector dataset n &optional
        (first-datapoint
         (first dataset))
        (vector+datapoint
         (add-two-lists-mod-2nd-n
          first-datapoint vector n)))
  (if (null dataset) ()
    (append
     (if (test-equal<list-elements
          dataset vector+datapoint)
       (list first-datapoint))
     (maximal-translatable-pattern-mod-2nd-n
      vector (rest dataset) n))))

#|
\noindent Example:
\begin{verbatim}
(mod-column
 '((1 2 12 4) (1 2 16 -1) (2 4 32 6) (5 2 50 6)) 7 2)
--> ((1 2 5 4) (1 2 2 -1) (2 4 4 6) (5 2 1 6))
\end{verbatim}

\noindent The first argument to this function is a
list, assumed to contain sublists of equal length. The
second argument specifies what modulo will be
calculated for the $n$th item of each sublist, where
$n$ is given by the third argument. |#

(defun mod-column
       (a-list modulo index-modulo &optional
        (length-row (length (first a-list))))
  (if (null a-list) ()
    (cons
     (append
      (firstn index-modulo (first a-list))
      (list
       (mod
        (nth index-modulo (first a-list)) modulo))
      (lastn (- (- length-row index-modulo) 1)
             (first a-list)))
     (mod-column
      (rest a-list) modulo
      index-modulo length-row))))

#|
\noindent Example:
\begin{verbatim}
(mod-list '(1 2 3 4 5 7) 3)
--> (1 2 0 1 2 1)
\end{verbatim}

\noindent This function gives the value of each item
of a list, modulo $b$. |#

(defun mod-list (a-list b)
  (mapcar
   #'(lambda (x) (mod x b)) a-list))

#|
\noindent Example:
\begin{verbatim}
(restrict-dataset-in-nth-to-xs
 '((2 4 -1 6 9) (0 0 4 2 -7) (-3 -2 -1 -1 1)
   (12 0 -7 5 3) (1 2 3 4 3) (1 2 5 4 5)
   (12 0 -6 5 4) (-3 -2 1 -1 0) (12 0 -7 5 4))
 3 '(4 5 6))
--> ((2 4 -1 6 9) (12 0 -7 5 3) (1 2 3 4 3)
     (1 2 5 4 5) (12 0 -6 5 4) (12 0 -7 5 4))
\end{verbatim}

\noindent The first argument to this function is a
dataset. We are interested in the $n$th dimension of
each vector, where $n$ is the second argument.
A datapoint is retained in the output if its $n$th
value is a member of the list specified by the third
argument. Note it will not recognise 1.0 as 1. |#

(defun restrict-dataset-in-nth-to-xs
       (dataset n probes)
  (if (null dataset) ()
    (append
     (if (test-equalp-nth-to-xs
	  (first dataset) n probes)
       (list (first dataset)))
     (restrict-dataset-in-nth-to-xs
      (rest dataset) n probes))))

#|
\noindent Example:
\begin{verbatim}
(restrict-dataset-in-nth-to-tests
 '((2 4 -1 6 9) (0 0 4 2 -7) (-3 -2 -1 4 1)
   (2 4 -1 3.3 9) (0 0 4 6.8 -7))
 3 (list #'>= #'<) '(3.9 6.8))
--> ((2 4 -1 6 9) (12 0 -7 5 3) (1 2 3 4 3)
     (1 2 5 4 5) (12 0 -6 5 4) (12 0 -7 5 4))
\end{verbatim}

\noindent The first argument to this function is a
dataset. We are interested in the $n$th dimension of
each vector, where $n$ is the second argument. A
datapoint is retained in the output if its $n$th value
is true compared with the each element of the third
argument using the test supplied as each element of
the fourth argument. |#

(defun restrict-dataset-in-nth-to-tests
       (dataset n fn x)
  (if (null dataset) ()
    (append
     (if (test-all-true
          (mapcar 
           #'(lambda (y z)
               (funcall
                y (nth n (first dataset)) z)) fn x))
       (list (first dataset)))
     (restrict-dataset-in-nth-to-tests
      (rest dataset) n fn x))))

#|
\noindent Example:
\begin{verbatim}
(SIA-reflected
 '((0 61) (0 65) (1 64) (4 62) (4 66) (5 65) (8 60)
   (8 64) (9 63) (12 56) (13 69) (15 65) (16 57)
   (16 59) (17 64) (19 63))
 (merge-pathnames
  (make-pathname
   :name "SIA output" :type "txt")
  *MCStylistic-MonthYear-example-files-results-path*)
 50)
--> 2
\end{verbatim}

\noindent This function is a version of the SIA
algorithm. It is called `SIA-reflected' because the
results (pairs of vectors and the corresponding MTPs)
are the other way round to the algorithm specified by
\citet{meredith2002}. The example causes two files to
be created in the specified location. |#

(defun SIA-reflected
       (dataset path&name partition-size &optional
        (filename-counter 1) (growing-list nil) (j 0)
	(first-dataset (first dataset))
	(rest-dataset (rest dataset))
	(probe
	 (if (null rest-dataset) ()
	   (subtract-two-lists
	    (first rest-dataset) (first dataset))))
	(result-recent
	 (assoc probe growing-list :test #'equalp))
	(result
	 (if (and (> filename-counter 1)
		  (null result-recent))
	   (assoc-files
	    probe path&name (- filename-counter 1))
	   (identity result-recent))))
  (if (null dataset)
    (progn
      (write-to-file
       growing-list
       (merge-pathnames
        (make-pathname
         :name
         (concatenate
          'string
          (pathname-name path&name) " "
          (write-to-string filename-counter))
         :type (pathname-type path&name))
        (pathname-directory-pathname
         path&name)))
      (identity filename-counter))
    (if (null probe)
      (SIA-reflected
       (rest dataset) path&name partition-size
       filename-counter growing-list j)
      (if (equal j partition-size)
	(progn
          (write-to-file
           growing-list
           (merge-pathnames
            (make-pathname
             :name
             (concatenate
              'string
              (pathname-name path&name) " "
              (write-to-string filename-counter))
             :type (pathname-type path&name))
            (pathname-directory-pathname
             path&name)))
	  (SIA-reflected
	   dataset path&name partition-size
           (+ filename-counter 1) nil
	   0 first-dataset rest-dataset
	   probe nil))
        (if result-recent
	  (SIA-reflected
	   dataset path&name partition-size
           filename-counter
           (progn
             (rplacd
              (assoc
               probe growing-list :test #'equalp)
              (append
               (cdr result-recent)
               (list first-dataset)))
             (identity growing-list))
	   j first-dataset (rest rest-dataset))
	  (if result
	    (progn
	      (update-written-file
	       path&name (first result) first-dataset
               (second result))
	      (SIA-reflected
	       dataset path&name partition-size
               filename-counter growing-list
	       j first-dataset (rest rest-dataset)))
	    (SIA-reflected
	     dataset path&name partition-size
             filename-counter
	     (append
              (list
	       (cons probe (list first-dataset)))
	      growing-list)
	     (+ j 1) first-dataset
	     (rest rest-dataset))))))))

#|
\noindent Example:
\begin{verbatim}
(SIA-reflected-mod-2nd-n
 '((0 61) (0 65) (1 64) (4 62) (4 66) (5 65) (8 60)
   (8 64) (9 63) (12 56) (13 69) (15 65) (16 57)
   (16 59) (17 64) (19 63))
 12
 (merge-pathnames
  (make-pathname
   :name "SIA mod 2nd n output" :type "txt")
  *MCStylistic-MonthYear-example-files-results-path*)
 50)
--> 2
\end{verbatim}

\noindent This function is a version of the SIA
algorithm that works with a pitch representation
modulo $n$. The example causes two files to be created
in the specified location. |#

(defun SIA-reflected-mod-2nd-n
       (dataset n path&name partition-size &optional
        (filename-counter 1) (growing-list nil) (j 0)
	(first-dataset (first dataset))
	(rest-dataset (rest dataset))
	(probe
	 (if (null rest-dataset) ()
	   (subtract-two-lists-mod-2nd-n
	    (first rest-dataset) (first dataset) n)))
	(result-recent
	 (assoc probe growing-list :test #'equalp))
	(result
	 (if (and (> filename-counter 1)
		  (null result-recent))
	   (assoc-files
	    probe path&name (- filename-counter 1))
	   (identity result-recent))))
  (if (null dataset)
    (progn
      (write-to-file
       growing-list
       (merge-pathnames
        (make-pathname
         :name
         (concatenate
          'string
          (pathname-name path&name) " "
          (write-to-string filename-counter))
         :type (pathname-type path&name))
        (pathname-directory-pathname
         path&name)))
      (identity filename-counter))
    (if (null probe)
      (SIA-reflected-mod-2nd-n
       (rest dataset) n path&name partition-size
       filename-counter growing-list j)
      (if (equal j partition-size)
	(progn
	  (write-to-file
	   growing-list
	   (merge-pathnames
            (make-pathname
             :name
             (concatenate
              'string
              (pathname-name path&name) " "
              (write-to-string filename-counter))
             :type (pathname-type path&name))
            (pathname-directory-pathname
             path&name)))
	  (SIA-reflected-mod-2nd-n
	   dataset n path&name partition-size
           (+ filename-counter 1) nil
	   0 first-dataset rest-dataset
	   probe nil))
	(if result-recent
	  (SIA-reflected-mod-2nd-n
	   dataset n path&name partition-size
           filename-counter
           (progn
             (rplacd
              (assoc
               probe growing-list :test #'equalp)
              (append
               (cdr result-recent)
               (list first-dataset)))
             (identity growing-list))
	   j first-dataset (rest rest-dataset))
	  (if result
	    (progn
	      (update-written-file
	       path&name (first result) first-dataset
               (second result))
	      (SIA-reflected-mod-2nd-n
	       dataset n path&name partition-size
               filename-counter growing-list
	       j first-dataset (rest rest-dataset)))
	    (SIA-reflected-mod-2nd-n
	     dataset n path&name partition-size
             filename-counter
	     (append
              (list
	       (cons probe (list first-dataset)))
	      growing-list) (+ j 1) first-dataset
	     (rest rest-dataset))))))))

#|
\noindent Example:
\begin{verbatim}
(split-point-set-by-staff
 '((13 55 3 1) (13 60 2 0) (13 64 1 0) (14 55 2 0)
   (15 55 1/2 1) (15 59 1/2 1) (15 65 1/2 0)
   (15 55 1/2 0)) 3)
--> (((13 60 2 0) (13 64 1 0) (14 55 2 0)
     (15 65 1/2 0) (15 55 1/2 0))
    ((13 55 3 1) (15 55 1/2 1) (15 59 1/2 1)))
\end{verbatim}

\noindent This function splits the input point set
into different point sets depending on the value in
the staff index.

\emph{Unlike} the function \nameref{fun:monophonise},
this function does not create monophonic lines within
staves, or translate (or 'unfold') point sets
belonging to successive staves. |#

(defun split-point-set-by-staff
       (point-set &optional
        (staff-index 4)
        (unique-staves
         (remove-duplicates
          (sort
           (nth-list-of-lists
            staff-index point-set) #'<)
          :test #'equalp)))
  (mapcar
   #'(lambda (x)
       (dataset-restricted-to-m-in-nth
        point-set x staff-index)) unique-staves))

#|
\noindent Example:
\begin{verbatim}
(subtract-list-from-each-list-mod-2nd-n
 '((8 -2 -3) (4 6 6) (0 0 0) (4 7 -3)) '(4 7 -3) 12)
--> ((4 3 0) (0 11 9) (-4 5 3) (0 0 0))
\end{verbatim}

\noindent The function subtract-two-lists-mod-2nd-n is
applied recursively to each sublist in the first list
argument, and the second argument. |#

(defun subtract-list-from-each-list-mod-2nd-n
       (a-list b-list n)
  (if (null a-list) ()
    (cons
     (subtract-two-lists-mod-2nd-n
      (first a-list) b-list n)
     (subtract-list-from-each-list-mod-2nd-n
      (rest a-list) b-list n))))

#|
\noindent Example:
\begin{verbatim}
(subtract-two-lists-mod-2nd-n '(8 60 1) '(4 67 2) 12)
--> (4 5 -1)
\end{verbatim}

\noindent Subtracts the second list from the first,
element-by-element. The subtraction of the second
elements is performed modulo $n$, where $n$ is the
third argument to the function. It is assumed that the
list is at least of length 2. |#

(defun subtract-two-lists-mod-2nd-n (a-list b-list n)
  (if (null a-list) ()
    (append
     (list (- (first a-list) (first b-list)))
     (list
      (mod (- (second a-list) (second b-list)) n))
     (subtract-two-lists
      (rest (rest a-list)) (rest (rest b-list))))))

#|
\noindent Example:
\begin{verbatim}
(test-equal<potential-translator-mod-2nd-n
 '((0 0) (3 4) (4 9) (5 1)) '(2 9) '(3 4) 12)
--> ((3 4))
\end{verbatim}

\noindent This function is very similar to the
function test-equal<potential-translator. The
difference is the call to the function add-two-lists-
mod-2nd-n (as oppposed to calling add-two-lists), and
this requires the inclusion of an extra argument. |#

(defun test-equal<potential-translator-mod-2nd-n
       (dataset patternpoint potential-translator n
        &optional
        (sum
         (add-two-lists-mod-2nd-n
          patternpoint potential-translator n))
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
	(test-equal<potential-translator-mod-2nd-n
	 dataset patternpoint potential-translator n
         sum (+ i 1) v1)))))

#|
\noindent Example:
\begin{verbatim}
(test-translation-mod-2nd-n
 '((2 2) (4 5)) '((11 9) (13 0)) 12)
--> T
\end{verbatim}

\noindent This function is very similar to the
function test-translation, except that here the
translation in the second dimension is performed
modulo the third argument. |#

(defun test-translation-mod-2nd-n
       (a-list b-list &optional (n *pitch-mod*))
  (if (equal (length a-list) (length b-list))
    (test-translation-mod-2nd-n-no-length-check
     a-list b-list n)))

#|
\noindent Example:
\begin{verbatim}
(test-translation-mod-2nd-n-no-length-check
 '((40 0) (40 10) (43 7)) '((44 7) (44 9) (47 4)) 12)
--> T
\end{verbatim}

\noindent This function ought to be very similar to
the function test-translation-no-length-check.
However simply altering the translation in the second
dimension to modulo $n$ (the third argument) can be
problematic: In the above example, the pitch classes
B$\flat$, C, G are a translation of G, A, E, but
when these are ordered modulo 12, the C and the
B$\flat$ swap positions. The function below accounts
for this but will generally take longer to return an
answer than test-translation-no-length-check. |#

(defun test-translation-mod-2nd-n-no-length-check
    (a-list b-list n &optional (i 0)
     (j (length a-list))
     (b-list-mod
      (sort-dataset-asc
       (mod-column b-list n 1)))
     (probe-difference
      (if (< i j)
        (subtract-two-lists-mod-2nd-n
         (first b-list) (nth i a-list) n)))
     (probe-translation
      (sort-dataset-asc
       (translation-mod-2nd-n
        a-list probe-difference n))))
  (if (null probe-difference) ()
    (if (equalp probe-translation b-list-mod)
      (identity T)
      (test-translation-mod-2nd-n-no-length-check
       a-list b-list n (+ i 1) j b-list-mod))))

#|
\noindent Example:
\begin{verbatim}
(translation-mod-2nd-n
 '((8 0 3) (9 11 1) (9 4 2)) '(3 3 0) 12)
--> ((11 3 3) (12 2 1) (12 7 2))
\end{verbatim}

\noindent The first argument is a list of sublists,
but we imagine it as a set of vectors (all members of
the same n-dimensional vector space). The second
argument---another list---is also an $n$-dimensional
vector, and this is added to each of the members of
the first argument. `Added' means vector addition,
that is element-wise, and addition in the second
dimension is performed modulo the third argument. The
resulting set is a translation of the first argument
by the second. |#

(defun translation-mod-2nd-n
       (a-list b-list n &optional (growing-list nil))
  (if (or (null a-list) (null b-list))
    (sort-dataset-asc growing-list)
    (translation-mod-2nd-n
     (rest a-list) b-list n
     (append
      growing-list
      (list
       (add-two-lists-mod-2nd-n
        (first a-list) b-list n))))))

#|
\noindent Example:
\begin{verbatim}
(translations-mod-2nd-n
 '((8 0 3) (9 11 1) (9 4 2)) '((0 0 0) (3 3 0)) 12)
--> (((8 0 3) (9 4 1) (9 11 2))
     ((11 3 3) (12 2 1) (12 7 2)))
\end{verbatim}

\noindent There are three arguments to this function,
a pattern, some translators and a modulo argument. The
pattern is translated by each translator, modulo $n$
in the second dimension, and the results returned. |#

(defun translations-mod-2nd-n
       (pattern translators n)
  (if (null translators) ()
    (cons
     (translation-mod-2nd-n
      pattern (first translators) n)
     (translations-mod-2nd-n
      pattern (rest translators) n))))

#|
\noindent Example:
\begin{verbatim}
(translators-of-pattern-in-dataset-mod-2nd-n
 '((8 3) (8 7))
 '((4 7) (8 3) (8 4) (8 7) (9 3) (10 7)
   (11 3) (13 0) (13 4)) 12)
--> ((0 0) (5 9))
\end{verbatim}

\noindent A pattern and dataset are provided. The
transaltors of the pattern in the dataset are
returned. |#

(defun translators-of-pattern-in-dataset-mod-2nd-n
       (pattern dataset n &optional
        (translators
         (subtract-list-from-each-list-mod-2nd-n
          dataset (first pattern) n))
        (next-translators
         (if (null (second pattern))
           (identity translators)
           (check-potential-translators-mod-2nd-n
            (second pattern) translators dataset n))))
  (if (or
       (equal (length translators) 1) 
       (null (first pattern)))
    (identity translators)
    (translators-of-pattern-in-dataset-mod-2nd-n
     (rest pattern) dataset n next-translators)))
