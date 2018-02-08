#| Copyright 2008-2013 Tom Collins
   Wednesday 6 October 2010
   Incomplete

\noindent The functions below are for finding summary
statistics, and for taking random samples from
data.

; REQUIRED PACKAGES:
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
   :name "vector-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

;(defvar *rs* (make-random-state t))

#|
\noindent Example:
\begin{verbatim}
(choose-one '(1 2 4))
--> 4.
\end{verbatim}

\noindent A random, equiprobable choice is made
between elements of a list. |#

(defun choose-one
       (a-list &optional (n (length a-list)))
  (if (null a-list) ()
    (nth (random n *rs*) a-list)))

#|
\noindent Example:
\begin{verbatim}
(cor '(6 7 4) '(6 7 4))
--> 1.0
(cor '(6 7 4) '(-6 -7 -4))
--> -1.0
(cor '(6 7 4) '(0 2 1.5))
--> 0.05
\end{verbatim}

\noindent The sample Pearson correlation coefficient
is returned for two input lists, which are assumed to
be of equal length. |#

(defun cor
       (x y &optional (n (length x)) (x-bar (mean x))
	(y-bar (mean y)) (x-sd (sd x x-bar))
	(y-sd (sd y y-bar))
	(numerator
	 (my-last
	  (fibonacci-list
	   (mapcar
	    #'(lambda (z w)
		(* (- z x-bar) (- w y-bar))) x y)))))
  (/ numerator (* x-sd y-sd n)))

#|
\noindent Example:
\begin{verbatim}
(frequency-count
 '((5 4) (3 2) (3 2.000001) (0 1)) t)
--> (((0 1) 1) ((3 2.000001) 2) ((5 4) 1))
\end{verbatim}

\noindent The frequency of occurrence of a list member
in a list of lists is returned. It is possible to
specify use of equality up to an error tolerance
(given by the variable *equality-tolerance*). |#

(defun frequency-count
       (a-list &optional (tolp nil)
        (a-sorted (sort-dataset-asc a-list))
        (a-index
         (if tolp
           (remove-duplicates
            a-sorted
            :test #'equal-up-to-tol)
           (remove-duplicates
            a-sorted
            :test #'equal))))
  (mapcar
   #'(lambda (x)
       (list
        x
        (if tolp
          (count x a-sorted :test #'equal-up-to-tol)
          (count x a-sorted :test #'equal))))
   a-index))

#|
\noindent Example:
\begin{verbatim}
(setq a-list '(2 4 -1 6 9 12 0 -7 5 3 1 2 3 8 3 1 -5))
(setq edges '(-7.5 -3.5 0.5 4.5 8.5 12.5))
(histogram a-list edges)
--> (2 2 8 3 2)
\end{verbatim}

\noindent A list of scalar data is the input to this
function, along with a list of edges, assumed to be in
ascending order. The output is a list of length one
less than the number of edges. Item $i$ of the output
list gives the number of data $d$ that satisfy
$e(i-1) < d \leq e(i)$. |#

(defun histogram
       (a-list edges &optional
        (bins
         (constant-vector 0 (- (length edges) 1)))
        (curr-bin
         (if a-list
           (index-1st-sublist-item>=
            (first a-list) edges))))
  (if (null a-list) bins
    (histogram
     (rest a-list) edges
     (if (and curr-bin (> curr-bin 0))
       (add-to-nth 1 curr-bin bins) bins))))

#|
\noindent Example:
\begin{verbatim}
(mean '(6 7 4))
--> 17/3.
\end{verbatim}

\noindent The mean of a list of number is returned. |#

(defun mean (a-list)
  (/
   (my-last (fibonacci-list a-list)) (length a-list)))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(0 9 0 4 0 29 82 21 28 4 17 78 33 8 8 8 17 20 4 12))
(median a-list)
--> 21/2
\end{verbatim}

\noindent The median of a list of numbers is
returned. |#

(defun median
       (a-list &optional (b-list (copy-list a-list))
	(c-list (sort b-list #'<))
	(n (length c-list)))
  (if (oddp n) (nth (/ (- n 1) 2) c-list)
    (mean
     (list
      (nth (- (/ n 2) 1) c-list)
      (nth (/ n 2) c-list)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(0 9 0 4 0 29 82 21 28 4 17 78 33 8 8 8 17 20 4))
(quartiles a-list)
--> (4 9 28)
\end{verbatim}

\noindent The lower, median, and upper quartiles of a
list of numbers are returned. |#

(defun quartiles
       (a-list &optional (b-list (copy-list a-list))
	(c-list (sort b-list #'<))
	(n (length c-list))
	(a-med (median nil nil c-list n))
	(a-lower
	 (if (evenp n)
	   (median (subseq c-list 0 (/ n 2)))
	   (median
            (subseq c-list 0 (floor (/ n 2))))))
	(a-upper
	 (if (evenp n)
	   (median (subseq c-list (/ n 2)))
	   (median
            (subseq c-list (ceiling (/ n 2)))))))
  (list a-lower a-med a-upper))

#|
\noindent Example:
\begin{verbatim}
(random-permutation '("A" "B" "C" "D" "E"))
--> ("C" "A" "E" "D" "B").
\end{verbatim}

\noindent The output of this function is a random
permutation of an input list. |#

(defun random-permutation
       (a-list &optional (n (length a-list))
        (indices
         (sample-integers-no-replacement n n)))
  (nth-list indices a-list))

#|
\noindent Example:
\begin{verbatim}
(range '(60 61 62))
--> 2

\end{verbatim}

\noindent Range is the maximum member of a list, minus
the minimum member. |#

(defun range (data)
  (- (max-item data) (min-item data)))

#|
\noindent Example:
\begin{verbatim}
(sample-integers-no-replacement 10 7)
--> (5 4 8 2 0 3 9).
\end{verbatim}

\noindent The first argument to this function, n, is
an integer, as is the second m <= n. The output is a
random sample (without replacement) from the integers
0,..., n-1 of size m. If m > n, we set m = n. |#

(defun sample-integers-no-replacement
       (n m &optional
        (m (if (> m n) n m))
        (integer-subset
         (add-to-list
          -1 (reverse (first-n-naturals n))))
        (sample nil)
        (choice
         (choose-one integer-subset n)))
  (if (zerop m) sample
    (sample-integers-no-replacement
     (- n 1) m (- m 1)
     (remove choice integer-subset :test #'equalp)
     (cons choice sample))))

#|
\noindent Example:
\begin{verbatim}
(sd '(64 55 65 55 72 55 55 55 60 59 67))
--> 5.7178855
\end{verbatim}

\noindent The standard deviation of the sample (using
a denominator of $n$, where $n$ is the sample
size). |#

(defun sd
       (data &optional (x-bar (mean data))
        (square-deviations
         (mapcar
          #'(lambda (x) (expt (- x x-bar) 2))
          data)))
  (sqrt
   (/
    (my-last (fibonacci-list square-deviations))
    (length data))))
