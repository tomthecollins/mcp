#| Copyright 2008-2013 Tom Collins
   Tuesday 26 April 2011
   Incomplete

\noindent An implementation of the calculation of
keyscapes as described by \citet*{sapp2005}, based on
key profiles described by \citet*{krumhansl1982} and
\citet*{aarden2003}. Not currently able to depict the
calculated keyscapes. The function
\ref{fun:fifth-steps-mode} is an implementation of the
Krumhansl-Schmuckler key-finding algorithm
\citep*{krumhansl1990}.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
#|
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Pattern matching"
  "/pattern-importance-preliminaries.lisp"))
|#
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "segmentation"
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
   :directory '(:relative "Maths foundation")
   :name "stats-sampling"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
#|
(load
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Maths foundation"
  "/vector-operations.lisp"))
|#
|#

(defvar
    *Aarden-key-profiles*
  (mapcar
   #'(lambda (x y)
       (cons x y))
   '("C major" "Db major" "D major" "Eb major"
     "E major" "F major" "Gb major" "G major"
     "Ab major" "A major" "Bb major" "B major"
     "C minor" "C# minor" "D minor" "Eb minor"
     "E minor" "F minor" "F# minor" "G minor"
     "G# minor" "A minor" "Bb minor" "B minor")
   (append
    (mapcar
     #'(lambda (x)
         (cyclically-permute-list-by
          '(17.77 0.15 14.93 0.16 19.8 11.36 0.29
            22.06 0.15 8.15 0.23 4.95) x))
     (cons 0 (first-n-naturals 11)))
    (mapcar
     #'(lambda (x)
         (cyclically-permute-list-by
          '(18.26 0.74 14.05 16.86 0.7 14.44 0.7
            18.62 4.57 1.93 7.38 1.76) x))
     (cons 0 (first-n-naturals 11))))))

(defvar
    *Krumhansl&Kessler-key-profiles*
  (mapcar
   #'(lambda (x y)
       (cons x y))
   '("C major" "Db major" "D major" "Eb major"
     "E major" "F major" "Gb major" "G major"
     "Ab major" "A major" "Bb major" "B major"
     "C minor" "C# minor" "D minor" "Eb minor"
     "E minor" "F minor" "F# minor" "G minor"
     "G# minor" "A minor" "Bb minor" "B minor")
   (append
    (mapcar
     #'(lambda (x)
         (cyclically-permute-list-by
          '(6.35 2.23 3.48 2.33 4.38 4.09 2.52 5.19
            2.39 3.66 2.29 2.88) x))
     (cons 0 (first-n-naturals 11)))
    (mapcar
     #'(lambda (x)
         (cyclically-permute-list-by
          '(6.33 2.68 3.52 5.38 2.6 3.53 2.54 4.75
            3.98 2.69 3.34 3.17) x))
     (cons 0 (first-n-naturals 11))))))


#|
\noindent Example:
\begin{verbatim}
(accumulate-to-weighted-mass
 '(4 0 1) '((4 0) 7) '(((0 4) 3) ((4 0) 7)))
--> (((4 0) 8) ((0 4) 3))
\end{verbatim}

\noindent This function takes three arguments: a
datapoint d (the last dimension of which is a
weighting); an element (to be updated) of the
emerging empirical probability mass function p;
p itself. The weighting of the observation is added to
the existing mass. |#

(defun accumulate-to-weighted-mass
       (datapoint relevant-element mass)
  (cons
   (list
    (butlast datapoint)
    (+ (second relevant-element)
       (my-last datapoint)))
   (remove relevant-element mass :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(add-to-weighted-mass '(3 1 6) '(((0 4) 3) ((4 0) 7)))
--> (((6 72) 1/3) ((4 0.1) 2/3))
\end{verbatim}

\noindent This function takes two arguments: a
datapoint d (the last dimension of which is a
weighting), and an emerging empirical probability mass
function p. The observation and its weighting is
is added to the existing mass. |#

(defun add-to-weighted-mass
       (datapoint mass)
  (cons
   (list (butlast datapoint) (my-last datapoint))
   mass))

#|
\noindent Example:
\begin{verbatim}
(datapoints-sounding-between
 '((5/2 72 66 1/2 0 3) (3 42 49 3 1 6)
   (3 74 68 1/3 0 10/3) (10/3 76 69 1/3 0 11/3)
   (11/3 74 68 1/3 0 4) (4 57 58 1 1 5)
   (4 61 60 1 1 5)) 3 4)
--> ((3 42 49 3 1 4 1) (3 74 68 1/3 0 10/3 1/3)
     (10/3 76 69 1/3 0 11/3 1/3)
     (11/3 74 68 1/3 0 4 1/3))
\end{verbatim}

\noindent A list of datapoints with offtimes
appended is the first argument to this function. The
second argument is the ontime of a window, a, and the
third argument is the offtime of the same window, b. A
datapoint appears in the output of the function if it
sounds during the window [a b). The amount of time for
which it sounds in [a b) is appended. |#

(defun datapoints-sounding-between
       (dataset-with-offtimes a b &optional
        (x (first (first dataset-with-offtimes)))
        (y (my-last (first dataset-with-offtimes))))
  (if (null dataset-with-offtimes) ()
    (if (and (< x b) (> y a))
      (cons
       (append
        (first dataset-with-offtimes)
        (list (- (min y b) (max x a))))
       (datapoints-sounding-between
        (rest dataset-with-offtimes) a b))
      (datapoints-sounding-between
       (rest dataset-with-offtimes) a b))))

#|
\noindent Example:
\begin{verbatim}
(dataset2pcs-norm-tonic
 '((3 42 49 3 1) (3 74 68 1/3 0) (10/3 76 69 1/3 0)
   (11/3 74 68 1/3 0)))
--> (7 3 5 3)
\end{verbatim}

\noindent This function estimates the key of the input
dataset. It subtracts the tonic pitch class from each
input MIDI note number, and outputs the answer modulo
twelve. |#

(defun dataset2pcs-norm-tonic
       (dataset &optional (MNN-idx 1) (dur-index 3)
	(dataset-durs-appended
	 (mapcar
	  #'(lambda (x)
	      (append
	       x (list (nth dur-index x)))) dataset))
	(key-profiles *Aarden-key-profiles*)
	(key-corr
	 (key-correlations
	  dataset-durs-appended key-profiles))
	(tonic-pc
	 (mod
	  (second
	   (max-argmax
	    (nth-list-of-lists 1 key-corr))) 12)))
  (mapcar
   #'(lambda (x) (mod (- (nth MNN-idx x) tonic-pc) 12))
   dataset))

#|
\noindent Example:
\begin{verbatim}
(setq
 relevant-datapoints
 '((-1 72 67 7/4 0) (0 55 57 1 1) (0 61 61 1 1)
   (0 64 63 1 1) (3/4 70 66 1/4 0) (1 56 58 1 1)
   (1 60 60 2 1) (1 63 62 2 1) (1 68 65 1/2 0)
   (3/2 70 66 1/2 0) (2 51 55 1 1) (2 72 67 7/4 0)
   (3 55 57 1 1) (3 61 61 1 1) (3 64 63 1 1)
   (15/4 70 66 1/4 0) (4 56 58 1 1) (4 60 60 2 1)
   (4 63 62 2 1) (4 68 65 1/2 0) (9/2 77 70 1/2 0)
   (5 51 55 1 1) (5 75 69 1 0) (6 55 57 1 1)
   (6 61 61 1 1) (6 64 63 1 1) (6 72 67 3/4 0)
   (27/4 70 66 1/4 0) (7 56 58 1 1) (7 60 60 2 1)
   (7 63 62 2 1) (7 68 65 1/2 0) (15/2 70 66 1/2 0)
   (8 51 55 1 1) (8 72 67 1 0) (9 56 58 3/4 1)
   (9 61 61 3 1) (9 63 62 3 0) (39/4 55 57 1/4 1)
   (10 53 56 1/2 1) (21/2 55 57 1/2 1) (11 51 55 1 1)
   (12 55 57 1 1) (12 61 61 1 1) (12 64 63 1 1)
   (12 72 67 3/4 0)))
(fifth-steps-mode relevant-datapoints)
--> (-4 0)
\end{verbatim}

\noindent This function returns the key of input
datapoints, in the format of steps on the cycle of
fifths (e.g., -1 for F major) and mode (e.g., 5 for
Aeolian). |#

(defun fifth-steps-mode
       (relevant-datapoints &optional
        (key-profiles *Aarden-key-profiles*)
        (ontime-index 0) (MNN-index 1)
        (duration-index 3)
        (key-corr
         (key-correlations
          (mapcar
           #'(lambda (x)
               (list
                (nth ontime-index x) (nth MNN-index x)
                (nth duration-index x)))
           relevant-datapoints) key-profiles))
        (quasi-key-MNN
         (second
          (max-argmax
           (nth-list-of-lists 1 key-corr)))))
  (list
   (nth
    (mod quasi-key-MNN 12)
    (list 0 -5 2 -3 4 -1 6 1 -4 3 -2 5))
   (if (< quasi-key-MNN 12) 0 5)))
  
#|
\noindent Example:
\begin{verbatim}
(setq
 relevant-datapoints
 '((3 42 49 3 1 4 1) (3 74 68 1/3 0 10/3 1/3)
   (10/3 76 69 1/3 0 11/3 1/3)
   (11/3 74 68 1/3 0 4 1/3)))
(key-correlations relevant-datapoints)
--> (("C major" 0.0056350203) ("Db major" -0.16437216)
     ("D major" 0.6365436) ("Eb major" -0.41964883)
     ("E major" 0.1384299) ("F major" -0.30088827)
     ("Gb major" 0.040827263) ("G major" 0.18202147)
     ("Ab major" -0.51650715) ("A major" 0.2044552)
     ("Bb major" -0.10270603) ("B major" 0.29621017)
     ("C minor" -0.23124762) ("C# minor" 0.14107251)
     ("D minor" 0.03225725) ("Eb minor" 0.058523033)
     ("E minor" 0.30459806) ("F minor" -0.51036686)
     ("F# minor" 0.25775552) ("G minor" -0.067237906)
     ("G# minor" -0.2650179) ("A minor" 0.039761756)
     ("Bb minor" -0.41389754) ("B minor" 0.65379965))
\end{verbatim}

\noindent This function takes datapoints as its only
default argument. These datapoints have already had
offtimes appended, and have passed a test for
membership of the time window [a b). Their duration
within [a b) is given as the final dimension. The
weighted empirical mass of these datapoints is
calculated, using the projection on to MIDI note
number mod 12, weighted by duration within [a b). This
mass is converted to a key profile (vector), and the
pairwise correlations between this probe profile and
various other profiles contained in the optional
argument key-profiles are returned. |#

(defun key-correlations
       (relevant-datapoints &optional
        (key-profiles *Aarden-key-profiles*)
        (probe-profile
         (weighted-mass2key-profile
          (weighted-empirical-mass
           (mapcar
            #'(lambda (x)
                (list
                 (mod (second x) 12) (my-last x)))
            relevant-datapoints)))))
  (mapcar
   #'(lambda (x)
       (list
        (car x) (cor (cdr x) probe-profile)))
   key-profiles))

#|
\noindent Example:
\begin{verbatim}
(setq
 relevant-datapoints
 '((3 42 49 3 1 4 1) (3 74 68 1/3 0 10/3 1/3)
   (10/3 76 69 1/3 0 11/3 1/3)
   (11/3 74 68 1/3 0 4 1/3)))
(normalised-key-correlations relevant-datapoints)
--> (("C major" 0.04212125) ("Db major" 0.028406756)
     ("D major" 0.09301668) ("Eb major" 0.007813568)
     ("E major" 0.052833818) ("F major" 0.017393991)
     ("Gb major" 0.04496021) ("G major" 0.056350354)
     ("Ab major" 0.0) ("A major" 0.058160085)
     ("Bb major" 0.033381365) ("B major" 0.065561965)
     ("C minor" 0.02301191) ("C# minor" 0.053046998)
     ("D minor" 0.044268865) ("Eb minor" 0.04638773)
     ("E minor" 0.06623862) ("F minor" 4.9533776E-4)
     ("F# minor" 0.06245983) ("G minor" 0.036242582)
     ("G# minor" 0.020287657) ("A minor" 0.044874255)
     ("Bb minor" 0.008277524)
     ("B minor" 0.094408736))
\end{verbatim}

\noindent The output of the function key-correlations
is converted to a probability vector. |#

(defun normalised-key-correlations
       (relevant-datapoints &optional
        (key-profiles *Aarden-key-profiles*)
        (keys&correlations
         (key-correlations
          relevant-datapoints key-profiles))
        (transformed-correlations
         (multiply-list-by-constant
          (add-to-list
           1
           (nth-list-of-lists 1 keys&correlations))
          2))
        (probability-vector
         (multiply-list-by-constant
          transformed-correlations
          (/
           1
           (my-last
            (fibonacci-list
             transformed-correlations))))))
  (mapcar
   #'(lambda (x y)
       (list (first x) y))
   keys&correlations probability-vector))

#|
\noindent Example:
\begin{verbatim}
(setq
 dataset
 '((-1 61 60 1 0) (0 30 42 1 1) (0 66 63 3/2 0)
   (1 49 53 1 1) (1 57 58 1 1) (1 61 60 1 1)
   (3/2 68 64 1/2 0) (2 49 53 1 1) (2 57 58 1 1)
   (2 61 60 1 1) (2 69 65 1/2 0) (5/2 72 66 1/2 0)
   (3 42 49 1 1) (3 74 68 1/3 0) (10/3 76 69 1/3 0)
   (11/3 74 68 1/3 0) (4 57 58 1 1) (4 61 60 1 1)
   (4 66 63 1 1) (4 73 67 1 0) (5 57 58 1 1)
   (5 61 60 1 1) (5 66 63 1 1) (5 78 70 1 0)
   (6 54 56 1 1) (6 73 67 1/3 0) (19/3 74 68 1/3 0)
   (20/3 73 67 1/3 0) (7 59 59 1 1) (7 62 61 1 1)
   (7 66 63 1 1) (7 71 66 1 0) (8 59 59 1 1)
   (8 62 61 1 1) (8 66 63 1 1) (8 78 70 1 0)))
(keyscape-list dataset *Aarden-key-profiles* 3 1 3)
--> ((-3 3
      ((-1 61 60 1 0 0 1))
      (("C major" 0.0024341724) ...) (0.10747497 6))
     (0 3
      ((0 30 42 1 1 1 1) (0 66 63 3/2 0 3/2 3/2)
       (1 49 53 1 1 2 1) (1 57 58 1 1 2 1)
       (1 61 60 1 1 2 1) (3/2 68 64 1/2 0 2 1/2)
       (2 49 53 1 1 3 1) (2 57 58 1 1 3 1)
       (2 61 60 1 1 3 1) (2 69 65 1/2 0 5/2 1/2)
       (5/2 72 66 1/2 0 3 1/2))
      (("C major" 0.010985555) ...) (0.09890273 18))
     (3 3
      ((3 42 49 1 1 4 1) (3 74 68 1/3 0 10/3 1/3)
       (10/3 76 69 1/3 0 11/3 1/3)
       (11/3 74 68 1/3 0 4 1/3) (4 57 58 1 1 5 1)
       (4 61 60 1 1 5 1) (4 66 63 1 1 5 1)
       (4 73 67 1 0 5 1) (5 57 58 1 1 6 1)
       (5 61 60 1 1 6 1) (5 66 63 1 1 6 1)
       (5 78 70 1 0 6 1))
      (("C major" 0.015585414) ...) (0.09317962 18))
     (6 3
      ((6 54 56 1 1 7 1) (6 73 67 1/3 0 19/3 1/3)
       (19/3 74 68 1/3 0 20/3 1/3)
       (20/3 73 67 1/3 0 7 1/3) (7 59 59 1 1 8 1)
       (7 62 61 1 1 8 1) (7 66 63 1 1 8 1)
       (7 71 66 1 0 8 1) (8 59 59 1 1 9 1)
       (8 62 61 1 1 9 1) (8 66 63 1 1 9 1)
       (8 78 70 1 0 9 1))
      (("C major" 0.024908373) ...) (0.09740843 23))
     (-3 4
      ((-1 61 60 1 0 0 1) (0 30 42 1 1 1 1)
       (0 66 63 3/2 0 3/2 1))
      (("C major" 0.002813767) ...) (0.09659196 18))
     (0 4 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.015838815) ...) (0.09398684 18))
     (3 4 ((3 42 49 1 1 4 1) ...)
      (("C major" 0.014643886) ...) (0.09295517 18))
     (-3 5 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.005957871) ...) (0.09737477 18))
     (0 5 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.014239526) ...) (0.09530763 18))
     (3 5 ((3 42 49 1 1 4 1) ...) 
      (("C major" 0.018060159) ...) (0.088710114 23))
     (-3 6 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.009841155) ...) (0.099177815 18))
     (0 6 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.013386026) ...) (0.096429521 18))
     (3 6 ((3 42 49 1 1 4 1) ...)
      (("C major" 0.019494385) ...) (0.09097412 23))
     (-3 7 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.014419735) ...) (0.09467592 18))
     (0 7 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.013051211) ...) (0.09554448 18))
     (-3 8 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.013291403) ...) (0.09573281 18))
     (0 8 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.015427576) ...) (0.0922521 18))
     (-3 9 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.012677101) ...) (0.09622239 18))
     (0 9 ((0 30 42 1 1 1 1) ...)
      (("C major" 0.01631804) ...) (0.09062016 18))
     (-3 10 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.012418479) ...) (0.09584213 18))
     (-3 11 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.014786912) ...) (0.09264141 18))
     (-3 12 ((-1 61 60 1 0 0 1) ...)
      (("C major" 0.015724108) ...) (0.09101982 18)))
\end{verbatim}

\noindent Implementation of keyscapes as described by
\citet{sapp2005}. |#

(defun keyscape-list
       (dataset &optional
        (key-profiles *Aarden-key-profiles*)
        (window-length-min 4)
        (window-length-increment 1) (step-size 4)
        (duration-index 3) (offtime-index 5)
        (dataset-with-offtimes
         (append-offtimes dataset duration-index))
        (terminus
         (max-item
          (nth-list-of-lists
           offtime-index dataset-with-offtimes)))
        (first-window-ontime
         (*
          (floor
           (/
            (first (first dataset))
            window-length-min)) window-length-min))
        (growing-list nil)
        (window-ontime first-window-ontime)
        (window-length window-length-min)
        (window-offtime
         (+ window-ontime window-length))
        (relevant-datapoints
         (if (<= window-offtime terminus)
           (datapoints-sounding-between
            dataset-with-offtimes window-ontime
            window-offtime)))
        (norm-key-correlations
         (if relevant-datapoints
           (normalised-key-correlations
            relevant-datapoints key-profiles))))
  (if (> window-offtime terminus)
    (if (equalp window-ontime first-window-ontime)
      growing-list
      (keyscape-list
       dataset key-profiles window-length-min
       window-length-increment step-size
       duration-index offtime-index
       dataset-with-offtimes terminus
       first-window-ontime
       growing-list first-window-ontime
       (+ window-length window-length-increment)))
    (keyscape-list
     dataset key-profiles window-length-min
     window-length-increment step-size duration-index
     offtime-index dataset-with-offtimes terminus
     first-window-ontime
     (append
      growing-list
      (list
       (list
        window-ontime window-length
        relevant-datapoints norm-key-correlations
        (max-argmax
         (nth-list-of-lists
          1 norm-key-correlations)))))
     (+ window-ontime step-size) window-length)))

#|
\noindent Example:
\begin{verbatim}
(points-sounding-at
 '((5/2 42 49 3 1 11/2) (5/2 72 66 1/2 0 3)
   (3 74 68 1/3 0 10/3) (10/3 76 69 1/3 0 11/3)
   (11/3 74 68 1/3 0 4) (4 57 58 1 1 5)
   (4 61 60 1 1 5)) 3)
--> ((5/2 42 49 3 1 11/2) (3 74 68 1/3 0 10/3)).
\end{verbatim}

\noindent A list of points with offtimes appended is
the first argument to this function. The second
argument is a time, $t$. A point appears in the output
of the function if it sounds at $t$, meaning its ontime
$x$ and offtime $y$ satisfy $x \leq t$ and $y > t$. |#

(defun points-sounding-at
       (point-set-with-offs-append time &optional
        (ontime-idx 0)
        (offtime-idx
         (if point-set-with-offs-append
           (-
            (length (first point-set-with-offs-append))
            1))))
  (loop for point in point-set-with-offs-append
    when (and (<= (nth ontime-idx point) time)
              (> (nth offtime-idx point) time))
    collect point))

#|
\noindent Example:
\begin{verbatim}
(present-to-weighted-mass '(0 4 3) '(((4 0) 7)))
--> (((0 4) 3) ((4 0) 7))
\end{verbatim}

\noindent This function takes two arguments: a
datapoint d (the last dimension of which contains a
weighting), and an unnormalised empirical probability
mass function p which is in the process of being
calculated. If d is new to the empirical mass, it is
added with mass given by its weight, and if it already
forms part of the mass, then this component is
increased by its weight. |#

(defun present-to-weighted-mass
       (datapoint mass)
  (let ((relevant-element
         (assoc
          (butlast datapoint) mass :test #'equalp)))
    (if relevant-element
      (accumulate-to-weighted-mass
       datapoint relevant-element mass)
      (add-to-weighted-mass datapoint mass))))

#|
\noindent Example:
\begin{verbatim}
(weighted-empirical-mass
 '((4 0 5) (4 0 2) (0 4 3)) '())
--> (((0 4) 3) ((4 0) 7))
\end{verbatim}

\noindent This function returns the weighted empirical
probability mass function p for a dataset listed
d1*, d2*,..., dN*, where the last dimension of each
datapoint is the weighting. |#

(defun weighted-empirical-mass
       (dataset &optional (mass nil))
  (if (null dataset) mass
    (weighted-empirical-mass
     (rest dataset)
     (present-to-weighted-mass
      (first dataset) mass))))

#|
\noindent Example:
\begin{verbatim}
(weighted-mass2key-profile '(((0) 3) ((7) 4)))
--> (3 0 0 0 0 0 0 4 0 0 0 0)
\end{verbatim}

\noindent This function converts an unnormalised
probability mass function to a twelve-point vector,
with the weight from the mass function corresponding
to the ith MIDI note number mod 12 appearing as the
ith element of the vector. |#

(defun weighted-mass2key-profile
       (weighted-mass &optional (i 0)
        (relevant-mass
         (assoc
          (list i) weighted-mass :test #'equalp)))
  (if (>= i 12) ()
    (cons
     (if relevant-mass (second relevant-mass) 0)
     (weighted-mass2key-profile
      weighted-mass (+ i 1)))))
