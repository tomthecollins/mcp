#| Copyright 2008-2013 Tom Collins
   Tuesday 20 October 2009

These functions make it possible to form empirical
$n$-dimensional distributions. One of the applications
of these empirical distributions is to adapt pattern
interest \citep*{conklin2008a} for polyphonic
music.

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
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(accumulate-to-mass
 '(6 72) '((6 72) 1/4)
 '(((6 72) 1/4) ((4 0.1) 1/2)) 1/4)
--> (((6 72) 1/2) ((4 0.1) 1/2))
\end{verbatim}

\noindent This function takes four arguments: a
datapoint $\mathbf{d}$; an element (to be updated) of
the emerging empirical probability mass function $L$;
$L$ itself is the third argument; and the fourth
argument is $\mu$, the reciprocal of the number of
datapoints that have been observed. This function has
been called because $\mathbf{d}$ is new to the
empirical mass---it is added with mass $\mu$. |#

(defun accumulate-to-mass
       (datapoint relevant-element mass
        reciprocal-length)
  (cons
   (list
    datapoint
    (+
     (second relevant-element) reciprocal-length))
   (remove relevant-element mass :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(add-to-mass '(6 72) '(((4 0.1) 2/3)) 1/3)
--> (((6 72) 1/3) ((4 0.1) 2/3))
\end{verbatim}

\noindent This function takes three arguments: a
datapoint $\mathbf{d}$; an emerging empirical
probability mass function $L$; and the third argument
is $\mu$, the reciprocal of the number of datapoints
that have been observed. This function has been called
because $\mathbf{d}$ already forms part $\lambda$ of
the mass. This element is increased to $\lambda +
\mu$. |#

(defun add-to-mass
       (datapoint mass reciprocal-length)
  (cons (list datapoint reciprocal-length)
        mass))

#|
\noindent Example:
\begin{verbatim}
(direct-product-of-n-sets
 '((1 2) ((59) (60)) (-4 -2)))
--> ((1 59 -4) (1 59 -2) (1 60 -4) (1 60 -2) (2 59 -4)
     (2 59 -2) (2 60 -4) (2 60 -2)).
\end{verbatim}

\noindent This function takes a single argument
(assumed to be a list of sets of numbers or sets of
sets), and returns the direct product of these
sets. |#

(defun direct-product-of-n-sets
       (n-sets &optional
        (first-set (first n-sets))
        (rest-sets (rest n-sets))       
        (result
         (if (numberp (first first-set))
           (mapcar #'(lambda (x) (list x))
                   first-set)
           (identity first-set)))
        (b-listed
         (if rest-sets
           (if (numberp (first (first rest-sets)))
             (mapcar #'(lambda (x) (list x))
                     (first rest-sets))
             (identity (first rest-sets))))))
  (if (null rest-sets) (identity result)
    (direct-product-of-n-sets
     n-sets first-set (rest rest-sets)
     (direct-product-of-two-sets
      result b-listed))))

#|
\noindent Example:
\begin{verbatim}
(direct-product-of-two-sets '(1/3 1 2) '(59 60))
--> ((1/3 59) (1/3 60) (1 59) (1 60) (2 59) (2 60))
\end{verbatim}

\noindent This function takes two arguments (assumed
to be sets of numbers or sets of sets), and returns
the direct product of these sets. |#

(defun direct-product-of-two-sets
       (a-set b-set &optional
        (a-listed
         (if (numberp (first a-set))
           (mapcar #'(lambda (x) (list x)) a-set)
           (identity a-set)))
        (b-listed
         (if (numberp (first b-set))
           (mapcar #'(lambda (x) (list x)) b-set)
           (identity b-set)))
        (m (length a-set)) (n (length b-set))
        (i 0) (j 0) (result nil))
  (if (>= i m) (identity result)
    (direct-product-of-two-sets
     a-set b-set a-listed b-listed m n
     (if (>= j n) (+ i 1) (identity i))
     (if (>= j n) (identity 0) (+ j 1))
     (append result
             (if (< j n)
               (list
                (append
                 (nth i a-listed)
                 (nth j b-listed))))))))

#|
\noindent Example:
\begin{verbatim}
(empirical-mass '((4 0) (4 0) (0 4)) '())
--> (((0 4) 1/3) ((4 0) 2/3))
\end{verbatim}

\noindent This function returns the empirical
probability mass function $L$ for a dataset listed
$\mathbf{d}_1^\ast, \mathbf{d}_2^\ast,\ldots,
\mathbf{d}_n^\ast$. |#

(defun empirical-mass
       (dataset &optional (mass nil)
        (reciprocal-length (/ 1 (length dataset))))
  (if (null dataset)
    (identity mass)
    (empirical-mass
     (rest dataset)
     (present-to-mass
      (first dataset) mass reciprocal-length)
     reciprocal-length)))

#|
\noindent Example:
\begin{verbatim}
(events-with-these-ontime-others
 '((6 63) (7 96) (9 112))
 '((23/4 86 1/4 2 46) (6 55 1/2 1 37)
   (6 63 1/3 1 37) (6 63 1/2 2 34) (7 91 1 1 56)
   (7 96 1/2 1 73) (7 96 1 1 95) (7 108 3/2 2 50)
   (17/2 109 1/2 2 49) (9 95 1 1 71)
   (9 98 1 1 71) (9 102 1 1 71) (9 112 3/4 2 73)) 1 2)
--> ((6 1/3) (6 1/2) (7 1/2) (7 1) (9 3/4))
\end{verbatim}

\noindent The first argument to this function is a
pattern, under the projection of ontime and MIDI note
number (in which case the variable other-index is 1)
or morphetic pitch (in which case other-index is 2).
The corresponding members of the full dataset are
sought out and returned as ontime-other pairs. |#

(defun events-with-these-ontime-others
       (projected-datapoints unprojected-dataset
        &optional (other-index 1)
	(return-index 3) (ontime-index 0)
        (ontimes (nth-list-of-lists
                  ontime-index
                  unprojected-dataset))
        (relevant-start
         (index-1st-sublist-item>=
          (first (first projected-datapoints))
          ontimes))
        (relevant-finish
         (index-1st-sublist-item>=
          (+ (first (my-last projected-datapoints))
	     0.01)
          ontimes))
        (relevant-dataset
         (subseq
          unprojected-dataset
          (if (null relevant-start)
             (identity 0)
             (identity relevant-start))
          (if (null relevant-finish)
             (length ontimes)
             (identity relevant-finish)))))
  (if (null projected-datapoints) ()
    (append
     (events-with-this-ontime-other
      (first projected-datapoints)
      relevant-dataset
      other-index return-index ontime-index)
     (events-with-these-ontime-others
      (rest projected-datapoints)
      unprojected-dataset
      other-index return-index ontime-index
      ontimes
      relevant-start relevant-finish
      relevant-dataset))))

#|
\noindent Example:
\begin{verbatim}
(events-with-this-ontime-other
 '(7 96)
 '((23/4 86 1/4 2 46) (6 55 1/2 1 37)
   (6 63 1/3 1 37) (6 63 1/2 2 34) (7 91 1 1 56)
   (7 96 1/2 1 73) (7 96 1 1 95) (7 108 3/2 2 50)
   (17/2 109 1/2 2 49) (9 95 1 1 71)
   (9 98 1 1 71) (9 102 1 1 71) (9 112 3/4 2 73)) 1 2)
--> ((7 1/2) (7 1))
\end{verbatim}

\noindent The first argument to this function is a
datapoint, under the projection of ontime and MIDI
note number (in which case the variable other-index is
1) or morphetic pitch (in which case other-index is
2). The corresponding members of the full dataset are
sought out and returned as ontime-other pairs. |#

(defun events-with-this-ontime-other
       (projected-datapoint relevant-dataset
        &optional (other-index 1)
	(return-index 3) (ontime-index 0))
  (if (null projected-datapoint) ()
    (if (null relevant-dataset) ()
      (append
       (if (equalp
            (list (nth
                   ontime-index
                   (first relevant-dataset))
                  (nth
                   other-index
                   (first relevant-dataset)))
            projected-datapoint)
         (list
	  (list
	   (nth 0 (first relevant-dataset))
	   (nth return-index
		(first relevant-dataset)))))
       (events-with-this-ontime-other
        projected-datapoint
        (rest relevant-dataset)
        other-index return-index ontime-index)))))

#|
\noindent Example:
\begin{verbatim}
(likelihood-of-pattern-or-translation
 '((0 60 60 1) (1 62 61 1/2) (2 64 62 1/3)
   (3 60 60 1))
 '((0 60 60 1) (1 62 61 1/2) (2 64 62 1/3) (3 60 60 1)
   (4 62 61 1) (5 64 62 1/2) (6 66 63 1/3) (7 62 61 1)
   (8 69 65 3) (11 59 59 1) (12 60 60 1)))
--> 9/14641 + 4/14641 = 13/14641

(likelihood-of-pattern-or-translation
 '((0 60 1) (1 61 1) (2 62 1) (3 60 1))
 '((0 60 1) (1 61 1) (1 66 1/2) (3/2 67 1/2) (2 62 1) 
   (2 68 1) (5/2 66 1/2) (3 60 1)))
--> 1/4*1/8*1/8*1/4 = 1/1024

(likelihood-of-pattern-or-translation
 '((0 60) (1 61) (2 62) (3 60))
 '((0 60) (1 61) (1 66) (3/2 67) (2 62) 
   (2 68) (5/2 66) (3 60)))
--> 1/4*1/8*1/8*1/4 + 1/4*1/8*1/8*1/4 = 1/512

(likelihood-of-pattern-or-translation
 '((0 1) (1 1) (2 1) (3 1))
 '((0 1) (1 1) (1 1/2) (3/2 1/2) (2 1) 
   (2 1/2) (5/2 1/2) (3 1)))
--> 1/16 + 1/16 = 1/8
\end{verbatim}

\noindent This function takes a pattern and the
dataset in which the pattern occurs. It calculates the
potential translations of the pattern in the dataset
and returns the sum of their likelihoods. |#

(defun likelihood-of-pattern-or-translation
       (pattern dataset &optional
        (pattern-palette
         (orthogonal-projection-not-unique-equalp
          pattern
          (append
           (list 0)
           (constant-vector
            1
            (- (length (first pattern)) 1)))))
        (dataset-palette
         (orthogonal-projection-not-unique-equalp
          dataset
          (append
           (list 0)
           (constant-vector
            1
            (- (length (first pattern)) 1)))))
        (empirical-mass
         (empirical-mass dataset-palette))
        (potential-translations
         (direct-product-of-n-sets
          (potential-n-dim-translations
           (rest (first pattern)) dataset-palette)))
        (result 0))
  (if (null potential-translations) (identity result)
    (likelihood-of-pattern-or-translation
     pattern dataset pattern-palette dataset-palette
     empirical-mass (rest potential-translations)
     (+ result
        (likelihood-of-subset
         (translation pattern-palette
                      (first potential-translations))
         empirical-mass)))))

#|
\noindent Example:
\begin{verbatim}
(likelihood-of-subset
 '((60 60 1) (62 61 1/2) (64 62 1/3) (60 60 1))
 '(((60 60 1) 3/11) ((62 61 1/2) 1/11)
   ((64 62 1/3) 1/11) ((62 61 1) 2/11)
   ((64 62 1/2) 1/11) ((66 63 1/3) 1/11)
   ((69 65 3) 1/11) ((59 59 1) 1/11)))
--> 9/14641
\end{verbatim}

\noindent This function takes a pattern-palette and
the empirical mass for the dataset-palette in which
the pattern occurs. The product of the individual
masses is returned, and reverts to zero if any pattern
points do not occur in the empirical mass. |#

(defun likelihood-of-subset
       (subset empirical-mass &optional
        (result 1)
        (increment
         (second
          (assoc (first subset)
                 empirical-mass :test #'equalp))))
  (if (null subset) (identity result)
    (if increment
      (likelihood-of-subset
       (rest subset) empirical-mass
       (* result increment))
      (identity 0))))

#|
\noindent Example:
\begin{verbatim}
(likelihood-of-subset-geometric-mean
 '((60 60 1) (62 61 1/2) (64 62 1/3) (60 60 1)) 1/4
 '(((60 60 1) 3/11) ((62 61 1/2) 1/11)
   ((64 62 1/3) 1/11) ((62 61 1) 2/11)
   ((64 62 1/2) 1/11) ((66 63 1/3) 1/11)
   ((69 65 3) 1/11) ((59 59 1) 1/11)))
--> 0.1574592
\end{verbatim}

\noindent This function takes a pattern-palette, the
reciprocal length of that pattern, and the empirical
mass for the dataset-palette in which the pattern
occurs. The geometric mean of the individual masses is
returned, and reverts to zero if any pattern points do
not occur in the empirical mass. |#

(defun likelihood-of-subset-geometric-mean
       (subset nth-root empirical-mass
        &optional (result 1)
        (mass
         (second
          (assoc
           (first subset) empirical-mass
           :test #'equalp)))
        (increment
         (if mass
           (expt mass nth-root))))
  (if (null subset) (identity result)
    (if increment
      (likelihood-of-subset-geometric-mean
       (rest subset) nth-root empirical-mass
       (* result increment))
      (identity 0))))

#|
\noindent Example:
\begin{verbatim}
(likelihood-of-translations-geometric-mean
 '((0 60 60 1) (1 62 61 1/2) (2 64 62 1/3)
   (3 60 60 1))
 '((0 60 60 1) (1 62 61 1/2) (2 64 62 1/3) (3 60 60 1)
   (4 62 61 1) (5 64 62 1/2) (6 66 63 1/3) (7 62 61 1)
   (8 69 65 3) (11 59 59 1) (12 60 60 1)))
--> (9/14641)^(1/4) + (4/14641)^(1/4) = 0.2860241

(likelihood-of-translations-geometric-mean
 '((0 60 1) (1 61 1) (2 62 1) (3 60 1))
 '((0 60 1) (1 61 1) (1 66 1/2) (3/2 67 1/2) (2 62 1) 
   (2 68 1) (5/2 66 1/2) (3 60 1)))
--> (1/4*1/8*1/8*1/4)^(1/4) = 0.17677668

(likelihood-of-translations-geometric-mean
 '((0 60) (1 61) (2 62) (3 60))
 '((0 60) (1 61) (1 66) (3/2 67) (2 62) 
   (2 68) (5/2 66) (3 60)))
--> (1/4*1/8*1/8*1/4)^(1/4) + (1/4*1/8*1/8*1/4)^(1/4)
 = 0.35355335

(likelihood-of-translations-geometric-mean
 '((0 1) (1 1) (2 1) (3 1))
 '((0 1) (1 1) (1 1/2) (3/2 1/2) (2 1) 
   (2 1/2) (5/2 1/2) (3 1)))
--> (1/16)^(1/4) + (1/16)^(1/4) = 1.
\end{verbatim}

\noindent This function takes a pattern and the
dataset in which the pattern occurs. It calculates the
potential translations of the pattern in the dataset
and returns the sum of the geometric means of their
likelihoods.

Note that this is not really a likelihood, as it is
possible for probabilities to be greater than 1. |#

(defun likelihood-of-translations-geometric-mean
       (pattern dataset &optional
        (pattern-palette
         (orthogonal-projection-not-unique-equalp
          pattern
          (append
           (list 0)
           (constant-vector
            1
            (- (length (first pattern)) 1)))))
        (nth-root (/ 1 (length pattern-palette)))
        (dataset-palette
         (orthogonal-projection-not-unique-equalp
          dataset
          (append
           (list 0)
           (constant-vector
            1
            (- (length (first pattern)) 1)))))
        (empirical-mass
         (empirical-mass dataset-palette))
        (potential-translations
         (direct-product-of-n-sets
          (potential-n-dim-translations
           (rest (first pattern)) dataset-palette)))
        (result 0))
  (if (null potential-translations) (identity result)
    (likelihood-of-translations-geometric-mean
     pattern dataset pattern-palette nth-root
     dataset-palette empirical-mass
     (rest potential-translations)
     (+ result
        (likelihood-of-subset-geometric-mean
         (translation pattern-palette
                      (first potential-translations))
         nth-root
         empirical-mass)))))

#|
\noindent Example:
\begin{verbatim}
(likelihood-of-translations-reordered
 '((0 60 60 1) (1 62 61 1/2) (2 64 62 1/3)
   (3 60 60 1))
 '((60 60 1) (62 61 1/2) (64 62 1/3) (60 60 1)
   (62 61 1) (64 62 1/2) (66 63 1/3) (62 61 1)
   (69 65 3) (59 59 1) (60 60 1))
 '(((60 60 1) 3/11) ((59 59 1) 1/11) ((69 65 3) 1/11)
   ((62 61 1) 2/11) ((66 63 1/3) 1/11)
   ((64 62 1/2) 1/11) ((64 62 1/3) 1/11)
   ((62 61 1/2) 1/11)))
--> 9/14641 + 4/14641 = 13/14641
\end{verbatim}

\noindent This function takes a pattern and the
dataset in which the pattern occurs. It calculates the
potential translations of the pattern in the dataset
and returns the sum of their likelihoods. Note the
order (and mandate) of the arguments is different to
the original version of this function, which is called
likelihood-of-pattern-or-translation. |#

(defun likelihood-of-translations-reordered
       (pattern dataset-palette empirical-mass
	&optional
        (pattern-palette
         (orthogonal-projection-not-unique-equalp
          pattern
          (append
           (list 0)
           (constant-vector
            1
            (- (length (first pattern)) 1)))))
        (potential-translations
         (direct-product-of-n-sets
          (potential-n-dim-translations
           (rest (first pattern)) dataset-palette)))
        (result 0))
  (if (null potential-translations) (identity result)
    (likelihood-of-translations-reordered
     pattern dataset-palette empirical-mass
     pattern-palette (rest potential-translations)
     (+ result
        (likelihood-of-subset
         (translation pattern-palette
                      (first potential-translations))
         empirical-mass)))))

#|
\noindent Example:
\begin{verbatim}
(potential-1-dim-translations
 '(60 60 1)
 '((60 60 1) (62 61 1/2) (64 62 1/3) (60 60 1)
   (62 61 1) (64 62 1/2) (66 63 1/3) (62 61 1)
   (69 65 3) (59 59 1) (60 60 1)) 0)
--> (-1 0 2 4 6 9)
\end{verbatim}

\noindent This function takes three arguments, the
first member of a pattern palette, the dataset palette
and an index $i$. First of all, the dataset is
projected uniquely along the dimension of index,
creating a vector $\mathbf{u}$. Then the $i$th member
of the first-pattern-palette is subtracted from each
member of $\mathbf{u}$, giving a list of potential
translations along this dimension. |#

(defun potential-1-dim-translations
       (first-pattern-palette dataset-palette index
        &optional
        (uniques
         (nth-list-of-lists
          0
          (orthogonal-projection-unique-equalp
           dataset-palette
           (append
            (constant-vector 0 index)
            (list 1)
            (constant-vector
             0
             (- (- (length
                    first-pattern-palette)
                   index) 1))))))
        (p1 (nth index first-pattern-palette)))
  (mapcar #'(lambda (x) (- x p1)) uniques))

#|
\noindent Example:
\begin{verbatim}
(potential-n-dim-translations
 '(60 60 1)
 '((60 60 1) (62 61 1/2) (64 62 1/3) (60 60 1)
   (62 61 1) (64 62 1/2) (66 63 1/3) (62 61 1)
   (69 65 3) (59 59 1) (60 60 1)))
--> ((-1 0 2 4 6 9) (-1 0 1 2 3 5) (-2/3 -1/2 0 2))
\end{verbatim}

\noindent This function takes two arguments, the first
member of a pattern palette, the dataset palette and
an index. The function potential-n-dim-translations is
applied recursively to an increment. |#

(defun potential-n-dim-translations
       (first-pattern-palette dataset-palette
        &optional
        (indices
         (add-to-list
          -1
          (reverse
           (first-n-naturals
            (length first-pattern-palette))))))
  (if (null indices) ()
    (append
     (list
      (potential-1-dim-translations
       first-pattern-palette dataset-palette
       (first indices)))
     (potential-n-dim-translations
      first-pattern-palette dataset-palette
      (rest indices)))))

#|
\noindent Example:
\begin{verbatim}
(present-to-mass '(0 4) '(((4 0) 2/3)) 1/3)
--> (((0 4) 1/3) ((4 0) 2/3))
\end{verbatim}

\noindent This function takes three arguments: a
datapoint $\mathbf{d}$, an empirical probability mass
function $L$ which is in the process of being
calculated, and $\mu$, the reciprocal of the number of
datapoints that have been observed. If $\mathbf{d}$ is
new to the empirical mass, it is added with mass
$\mu$, and if it already forms part $\lambda$ of the
mass, then this component is increased to $\lambda +
\mu$. |#

(defun present-to-mass
       (datapoint mass reciprocal-length)
  (let ((relevant-element
         (assoc
          datapoint mass :test #'equalp)))
    (if (identity relevant-element)
      (accumulate-to-mass
       datapoint relevant-element mass
       reciprocal-length)
      (add-to-mass
       datapoint mass reciprocal-length))))
