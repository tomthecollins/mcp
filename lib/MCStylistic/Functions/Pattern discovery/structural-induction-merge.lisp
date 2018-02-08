#| Copyright 2008-2013 Tom Collins
   Monday 6 September 2010

These functions implement SIA (Structural Induction
Algorithm, \citeauthor{meredith2002},
\citeyear{meredith2002}) using a merge sort.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "musical-properties"
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
(collect-by-car
 '(((1 -14) 7/2 60) ((1 -14) 2 74) ((1 -2) 5/2 64)))
--> ((2 74))
\end{verbatim}

\noindent A list is the only argument to this
function. The car of the first element is compared
with the cars of proceeding elements, and these
proceeding elements are returned so long as there is
equality. |#

(defun collect-by-car
       (a-list &optional
	(vector1 (car (first a-list)))
        (vector2 (car (second a-list))))
  (if (or (null vector2)
          (not (equalp vector2 vector1))) ()
    (cons
     (cdr (second a-list))
     (collect-by-car
      (rest a-list) vector1))))

#|
\noindent Example:
\begin{verbatim}
(collect-by-cars
 '(((1/2 -14) 7/2 60) ((1/2 -10) 2 74)
   ((1/2 -2) 5/2 64) ((1/2 -2) 3 62) ((1/2 2) 1/2 67)
   ((1/2 2) 1 69) ((1/2 3) 3/2 71) ((1/2 14) 0 53)
   ((1 21) 2 74) ((1 21) 3 62) ((1 21) 4 46)
   ((1 -7) 3/2 71) ((1 -4) 5/2 64) ((1 4) 1/2 67)))
--> (((1/2 -14) (7/2 60)) ((1/2 -10) (2 74))
     ((1/2 -2) (5/2 64) (3 62))
     ((1/2 2) (1/2 67) (1 69))
     ((1/2 3) (3/2 71)) ((1/2 14) (0 53))
     ((1 21) (2 74) (3 62) (4 46))
     ((1 -7) (3/2 71)) ((1 -4) (5/2 64))
     ((1 4) (1/2 67)))
\end{verbatim}

\noindent A list is the only argument to this
function. The function collect-by-car is applied to
each new vector appearing as the car of each element
of the list. |#

(defun collect-by-cars
       (a-list &optional
        (vector1 (car (first a-list)))
        (result
         (collect-by-car a-list vector1)))
  (if (null a-list) ()
    (cons
     (cons
       vector1
       (cons (cdr (first a-list)) result))
     (collect-by-cars
      (restn a-list (+ (length result) 1))))))

#|
\noindent Example:
\begin{verbatim}
(collect-by-cars-partition
 '(((1/2 -14) 7/2 60) ((1/2 -10) 2 74)
   ((1/2 -2) 5/2 64) ((1/2 -2) 3 62) ((1/2 2) 1/2 67)
   ((1/2 2) 1 69) ((1/2 3) 3/2 71) ((1/2 14) 0 53)
   ((1 21) 2 74) ((1 21) 3 62) ((1 21) 4 46)
   ((1 -7) 3/2 71) ((1 -4) 5/2 64) ((1 4) 1/2 67))
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/collected-by-cars.txt") 5)
--> NIL
\end{verbatim}

\noindent The function collect-by-cars can cause a
stack overflow for moderately sized lists. This
function writes the output of collect-by-cars to a
text file (using write-to-file-append) every so often
to prevent stack overflow. The example causes a file
to be created in the specified location. |#

(defun collect-by-cars-partition
       (a-list path&name partition-size &optional
        (actual-partition-size
         (if (<= (length a-list) partition-size)
           (length a-list)
           (+
            partition-size
            (length
             (collect-by-car
              (subseq
               a-list (- partition-size 1)))))))
        (b-list
         (collect-by-cars
          (subseq a-list 0 actual-partition-size))))
  (if b-list
    (progn
      (write-to-file-append b-list path&name)
      (collect-by-cars-partition
       (subseq a-list actual-partition-size)
       path&name partition-size))))

#|
\noindent Example: see Discovering and rating musical
patterns (Sec. \ref{sec:disc&rate-musical-patterns}), 
especially lines 83-88).
\vspace{0.5cm}

\noindent This function is a faster version of the
function SIA-reflected. The improved runtime is due to
the use of merge-sort. |#

(defun SIA-reflected-merge-sort
       (dataset path&name &optional
        (partition-size 10000)
	(result
	 (if (> (length dataset) 1)
	   (mapcar
	    #'(lambda (x)
		(cons
		 (subtract-two-lists
		  x (first dataset))
		 (first dataset)))
	    (rest dataset))))
	(next-row
	 (if (> (length dataset) 2)
	   (mapcar
	    #'(lambda (x)
		(cons
		 (subtract-two-lists
		  x (second dataset))
		 (second dataset)))
	    (rest (rest dataset))))))
  (if (null next-row)
    (collect-by-cars-partition
     result path&name partition-size)
    (SIA-reflected-merge-sort
     (rest dataset) path&name partition-size
     (merge
      'list result next-row #'vector<vector-car))))

#|
\noindent Example:
\begin{verbatim}
(vector<vector-car '((1 1) . (1 3)) '((2 2) . (1 3)))
--> T
\end{verbatim}

\noindent Applies the function
vector$<$vector-t-or-nil to the car of each list (the
two arguments). |#

(defun vector<vector-car (a-list b-list)
  (vector<vector-t-or-nil (car a-list) (car b-list)))

#|
\noindent Example:
\begin{verbatim}
(vector<vector-t-or-nil '(4 6 7) '(4 6 7.1))
--> T
\end{verbatim}

\noindent The function vector$<$vector returns "equal"
if the arguments were equal. This function returns nil
in such a scenario. |#

(defun vector<vector-t-or-nil (a-vector b-vector)
  (if (null a-vector) ()
    (if (< (first a-vector) (first b-vector))
      (identity t)
      (if (equal (first a-vector) (first b-vector))
	(vector<vector-t-or-nil
	 (rest a-vector) (rest b-vector))))))
