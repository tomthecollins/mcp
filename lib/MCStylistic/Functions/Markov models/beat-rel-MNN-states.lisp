#| Tom Collins
   Thursday 2 January 2013
   Incomplete

\noindent The aim of these functions is to convert a
dataset representing a melody or polyphonic piece to
beat-MNN states, where MNN (MIDI note number) is
relative to the tonic note closest to the mean MNN of
the melody. The key is either required as an
argument, or estimated using the Krumhansl-Schmuckler
key-finding algorithm \citep{krumhansl1990}. The
function \ref{fun:beat-rel-MNN-states} is the most
robust here. The functions \ref{fun:beat-MNN-states}
and \ref{fun:beat-MNNs-states} contribute towards
\cite{collins2012}. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Harmony and metre")
   :name "keyscape"
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

#|
\noindent Example:
\begin{verbatim}
(beat-MNN-states 4 '(-3 0)
 '((0 63 62 3/4 0) (3/4 63 62 1/4 0) (1 65 63 1/2 0)
   (2 66 64 1 0) (3 65 63 3/4 0) (15/4 63 62 1/4 0)
   (4 66 64 1 0) (5 65 63 3/4 0) (23/4 63 62 1/4 0)
   (6 66 64 1 0) (7 65 63 1 0) (35/4 63 62 1/4 0)
   (9 65 63 3/4 0) (39/4 63 62 1/4 0)
   (10 66 64 3/4 0) (43/4 65 63 1/4 0)
   (11 63 62 1 0)) "gersh06")
--> (((1 0)
      ("gersh06" (0 63 62 3/4 0) (63 62) (-3 0)))
     ((7/4 0)
      ("gersh06" (3/4 63 62 1/4 0) (63 62) (-3 0)))
     ((2 2)
      ("gersh06" (1 65 63 1/2 0) (63 62) (-3 0)))
     ((5/2 NIL)
      ("gersh06" NIL (63 62) (-3 0)))
     ((3 3)
      ("gersh06" (2 66 64 1 0) (63 62) (-3 0)))
     ((4 2)
      ("gersh06" (3 65 63 3/4 0) (63 62) (-3 0)))
     ((19/4 0)
      ("gersh06" (15/4 63 62 1/4 0) (63 62) (-3 0)))
     ((1 3)
      ("gersh06" (4 66 64 1 0) (63 62) (-3 0)))
     ((2 2)
      ("gersh06" (5 65 63 3/4 0) (63 62) (-3 0)))
     ((11/4 0)
      ("gersh06" (23/4 63 62 1/4 0) (63 62) (-3 0)))
     ((3 3)
      ("gersh06" (6 66 64 1 0) (63 62) (-3 0)))
     ((4 2)
      ("gersh06" (7 65 63 1 0) (63 62) (-3 0)))
     ((1 NIL)
      ("gersh06" NIL (63 62) (-3 0)))
     ((7/4 0)
      ("gersh06" (35/4 63 62 1/4 0) (63 62) (-3 0)))
     ((2 2)
      ("gersh06" (9 65 63 3/4 0) (63 62) (-3 0)))
     ((11/4 0)
      ("gersh06" (39/4 63 62 1/4 0) (63 62) (-3 0)))
     ((3 3)
      ("gersh06" (10 66 64 3/4 0) (63 62) (-3 0)))
     ((15/4 2)
      ("gersh06" (43/4 65 63 1/4 0) (63 62) (-3 0)))
     ((4 0)
      ("gersh06" (11 63 62 1 0) (63 62) (-3 0)))).
\end{verbatim}

\noindent The function contributes towards
\cite{collins2012}. It converts the dataset into
beat-MNN states. The dataset is assumed to represent
a melody. MNN is relative to the tonic note closest
to the mean MNN, which can be worked out from the
second argument. |#

(defun beat-MNN-states
       (beats-in-bar fifth-steps-mode dataset
        string-ID &optional (MNN-index 1)
        (MPN-index 2) (dur-index 3)
        (MNN-MPN-pair
         (fifth-steps-mode2MNN-MPN
          fifth-steps-mode))
        (trans-pair&c-dataset
         (centre-dataset
          fifth-steps-mode dataset MNN-index
          MPN-index))
        (trans-pair (first trans-pair&c-dataset))
        (c-dataset (second trans-pair&c-dataset))
        (c-segments
         (butlast
          (segments-strict
           c-dataset MNN-index dur-index)))
        (c-segment (first c-segments)))
  (if (null c-segment) ()
    (cons
     (list
      (list
       (+ (mod (first c-segment) beats-in-bar) 1)
       (nth MNN-index (first (second c-segment))))
      (list
       string-ID
       (if (null (second c-segment)) ()
         (first dataset)) trans-pair
         fifth-steps-mode))
     (beat-MNN-states
      beats-in-bar fifth-steps-mode
      (if (null (second c-segment))
        dataset (rest dataset))
      string-ID MNN-index MPN-index dur-index
      MNN-MPN-pair trans-pair&c-dataset trans-pair
      c-dataset (rest c-segments)))))

#|
\noindent Example:
\begin{verbatim}
(beat-MNNs-states 4 '(1 0)
 '((-1 55 57 1 3) (-1 59 59 1 2) (-1 62 61 1 1)
   (-1 67 64 1 0) (0 54 56 1 3) (0 57 58 1 2)
   (0 62 61 1/2 1) (0 74 68 1 0) (1/2 64 62 1/2 1)
   (1 50 54 1 3) (1 62 61 1/2 2) (1 66 63 1 1)
   (1 74 68 1 0) (3/2 60 60 1/2 2) (2 55 57 1/2 3)
   (2 59 59 1/2 2) (2 67 64 1 1) (2 74 68 1 0)
   (5/2 57 58 1/2 3) (5/2 60 60 1/2 2) (3 59 59 1 3)
   (3 62 61 1 2) (3 67 64 1 1) (3 74 68 1 0))
 "chorale-bwv-151-ed")
--> (((4 (-12 -8 -5 0))
      ("chorale-bwv-151-ed"
       ((-1 55 57 1 3) (-1 59 59 1 2) (-1 62 61 1 1)
        (-1 67 64 1 0)) (67 64) (1 0)))
     ((1 (-13 -10 -5 7))
      ("chorale-bwv-151-ed"
       ((0 54 56 1 3) (0 57 58 1 2) (0 62 61 1/2 1)
        (0 74 68 1 0)) (67 64) (1 0)))
     ((3/2 (-13 -10 -3 7))
      ("chorale-bwv-151-ed"
       ((0 54 56 1 3) (0 57 58 1 2)
        (1/2 64 62 1/2 1) (0 74 68 1 0)) (67 64)
       (1 0)))
     ((2 (-17 -5 -1 7))
      ("chorale-bwv-151-ed"
       ((1 50 54 1 3) (1 62 61 1/2 2) (1 66 63 1 1)
        (1 74 68 1 0)) (67 64) (1 0)))
     ((5/2 (-17 -7 -1 7))
      ("chorale-bwv-151-ed"
       ((1 50 54 1 3) (3/2 60 60 1/2 2)
        (1 66 63 1 1) (1 74 68 1 0)) (67 64) (1 0)))
     ((3 (-12 -8 0 7))
      ("chorale-bwv-151-ed"
       ((2 55 57 1/2 3) (2 59 59 1/2 2)
        (2 67 64 1 1) (2 74 68 1 0)) (67 64) (1 0)))
     ((7/2 (-10 -7 0 7))
      ("chorale-bwv-151-ed"
       ((5/2 57 58 1/2 3) (5/2 60 60 1/2 2)
        (2 67 64 1 1) (2 74 68 1 0)) (67 64) (1 0)))
     ((4 (-8 -5 0 7))
      ("chorale-bwv-151-ed"
       ((3 59 59 1 3) (3 62 61 1 2) (3 67 64 1 1)
        (3 74 68 1 0)) (67 64) (1 0)))).
\end{verbatim}

\noindent The function contributes towards
\cite{collins2012}. It converts the dataset into
beat-MNNs states. The dataset can represent
polyphonic or melodic material. MNN is relative to
the tonic note closest to the mean MNN, which can be
worked out from the second argument. |#

(defun beat-MNNs-states
       (beats-in-bar fifth-steps-mode dataset
        string-ID &optional (MNN-index 1)
        (MPN-index 2) (dur-index 3)
        (MNN-MPN-pair
         (fifth-steps-mode2MNN-MPN fifth-steps-mode))
        (trans-pair&c-dataset
         (centre-dataset
          fifth-steps-mode dataset MNN-index
          MPN-index))
        (trans-pair (first trans-pair&c-dataset))
        (c-dataset (second trans-pair&c-dataset))
        (c-segments
         (butlast
          (segments-strict
           c-dataset MNN-index dur-index)))
        (segments
         (butlast
          (segments-strict
           dataset MNN-index dur-index)))
        (c-segment (first c-segments))
        (segment (first segments)))
  (if (null c-segment) ()
    (cons
     (list
      (list
       (+ (mod (first c-segment) beats-in-bar) 1)
       (nth-list-of-lists
        MNN-index (second c-segment)))
      (list
       string-ID
       (mapcar
        #'(lambda (x) (subseq x 0 5))
        (second segment))
       trans-pair fifth-steps-mode))
     (beat-MNNs-states
      beats-in-bar fifth-steps-mode dataset
      string-ID MNN-index MPN-index dur-index
      MNN-MPN-pair trans-pair&c-dataset trans-pair
      c-dataset (rest c-segments) (rest segments)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 dataset
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
(beat-rel-MNN-states
 dataset "C-17-4-mini" 3 1 2 3)
--> (((3 (4))
      ("C-17-4-mini" ((-1 72 67 7/4 0)) (68 65)
       (-4 0)))
     ((1 (-13 -7 -4 4))
      ("C-17-4-mini"
       ((0 55 57 1 1) (0 61 61 1 1) (0 64 63 1 1)
        (-1 72 67 7/4 0)) (68 65) (-4 0)))
     ((7/4 (-13 -7 -4 2))
      ("C-17-4-mini"
       ((0 55 57 1 1) (0 61 61 1 1) (0 64 63 1 1)
        (3/4 70 66 1/4 0)) (68 65) (-4 0)))
     ((2 (-12 -8 -5 0))
      ("C-17-4-mini"
       ((1 56 58 1 1) (1 60 60 2 1) (1 63 62 2 1)
        (1 68 65 1/2 0)) (68 65) (-4 0)))
     ((5/2 (-12 -8 -5 2))
      ("C-17-4-mini"
       ((1 56 58 1 1) (1 60 60 2 1) (1 63 62 2 1)
        (3/2 70 66 1/2 0)) (68 65) (-4 0)))
     ((3 (-17 -8 -5 4))
      ("C-17-4-mini"
       ((2 51 55 1 1) (1 60 60 2 1) (1 63 62 2 1)
        (2 72 67 7/4 0)) (68 65) (-4 0)))
     ((1 (-13 -7 -4 4))
      ("C-17-4-mini"
       ((3 55 57 1 1) (3 61 61 1 1) (3 64 63 1 1)
        (2 72 67 7/4 0)) (68 65) (-4 0)))
     ((7/4 (-13 -7 -4 2))
      ("C-17-4-mini"
       ((3 55 57 1 1) (3 61 61 1 1) (3 64 63 1 1)
        (15/4 70 66 1/4 0)) (68 65) (-4 0)))
     ((2 (-12 -8 -5 0))
      ("C-17-4-mini"
       ((4 56 58 1 1) (4 60 60 2 1) (4 63 62 2 1)
        (4 68 65 1/2 0)) (68 65) (-4 0)))
     ((5/2 (-12 -8 -5 9))
      ("C-17-4-mini"
       ((4 56 58 1 1) (4 60 60 2 1) (4 63 62 2 1)
        (9/2 77 70 1/2 0)) (68 65) (-4 0)))
     ((3 (-17 -8 -5 7))
      ("C-17-4-mini"
       ((5 51 55 1 1) (4 60 60 2 1) (4 63 62 2 1)
        (5 75 69 1 0)) (68 65) (-4 0)))
     ((1 (-13 -7 -4 4))
      ("C-17-4-mini"
       ((6 55 57 1 1) (6 61 61 1 1) (6 64 63 1 1)
        (6 72 67 3/4 0)) (68 65) (-4 0)))
     ((7/4 (-13 -7 -4 2))
      ("C-17-4-mini"
       ((6 55 57 1 1) (6 61 61 1 1) (6 64 63 1 1)
        (27/4 70 66 1/4 0)) (68 65) (-4 0)))
     ((2 (-12 -8 -5 0))
      ("C-17-4-mini"
       ((7 56 58 1 1) (7 60 60 2 1) (7 63 62 2 1)
        (7 68 65 1/2 0)) (68 65) (-4 0)))
     ((5/2 (-12 -8 -5 2))
      ("C-17-4-mini"
       ((7 56 58 1 1) (7 60 60 2 1) (7 63 62 2 1)
        (15/2 70 66 1/2 0)) (68 65) (-4 0)))
     ((3 (-17 -8 -5 4))
      ("C-17-4-mini"
       ((8 51 55 1 1) (7 60 60 2 1) (7 63 62 2 1)
        (8 72 67 1 0)) (68 65) (-4 0)))
     ((1 (-12 -7 -5))
      ("C-17-4-mini"
       ((9 56 58 3/4 1) (9 61 61 3 1) (9 63 62 3 0))
       (68 65) (-4 0)))
     ((7/4 (-13 -7 -5))
      ("C-17-4-mini"
       ((39/4 55 57 1/4 1) (9 61 61 3 1)
        (9 63 62 3 0)) (68 65) (-4 0)))
     ((2 (-15 -7 -5))
      ("C-17-4-mini"
       ((10 53 56 1/2 1) (9 61 61 3 1)
        (9 63 62 3 0)) (68 65) (-4 0)))
     ((5/2 (-13 -7 -5))
      ("C-17-4-mini"
       ((21/2 55 57 1/2 1) (9 61 61 3 1)
        (9 63 62 3 0)) (68 65) (-4 0)))
     ((3 (-17 -7 -5))
      ("C-17-4-mini"
       ((11 51 55 1 1) (9 61 61 3 1) (9 63 62 3 0))
       (68 65) (-4 0)))
     ((1 (-13 -7 -4 4))
      ("C-17-4-mini"
       ((12 55 57 1 1) (12 61 61 1 1) (12 64 63 1 1)
        (12 72 67 3/4 0)) (68 65) (-4 0)))
     ((7/4 (-13 -7 -4))
      ("C-17-4-mini"
       ((12 55 57 1 1) (12 61 61 1 1)
        (12 64 63 1 1)) (68 65) (-4 0))))
\end{verbatim}

\noindent Suppose you have three states $X_{n-1},
X_n, X_{n+1}$. The function beat-rel-MNN-states
looks at the beat and MIDI note numbers of $X_n$,
the latter being centred relative to an estimated
tonic.

14/1/2015. It was noticed that the first attempt at
this function did not sort the relative MIDI note
numbers ascending, nor did it remove duplicates. This
is likely to exacerbate problems with dead ends, so
the function was altered to do both sorting and
removing of duplicates, and comparative generation
tests were performed. |#

(defun beat-rel-MNN-states
       (dataset &optional
        (catalogue-information "no information")
	(beats-in-bar 4) (MNN-index 1) (MPN-index 2)
	(dur-index 3)
        (fifth-steps-mode
         (fifth-steps-mode
          dataset *Aarden-key-profiles* 0 1
          dur-index))
        (trans-pair&c-dataset
         (centre-dataset
          fifth-steps-mode dataset MNN-index
          MPN-index))
        (trans-pair (first trans-pair&c-dataset))
        (c-dataset (second trans-pair&c-dataset))
        (c-segments
         (butlast
          (segments-strict
           c-dataset MNN-index dur-index)))
        (segments
         (butlast
          (segments-strict
           dataset MNN-index dur-index)))
        (c-segment (first c-segments))
        (segment (first segments)))
  (if (null c-segment) ()
    (cons
     (list
      (list
       (+ (mod (first c-segment) beats-in-bar) 1)
       (remove-duplicates
        (sort
         (nth-list-of-lists
          MNN-index (second c-segment)) #'<)
        :test #'equalp))
      (list
       catalogue-information
       (mapcar
        #'(lambda (x) (subseq x 0 5))
        (second segment))
       trans-pair fifth-steps-mode))
     (beat-rel-MNN-states
      dataset catalogue-information beats-in-bar
      MNN-index MPN-index dur-index fifth-steps-mode
      trans-pair&c-dataset trans-pair c-dataset
      (rest c-segments) (rest segments)))))

#| 1st attempt. Did not sort MIDI note numbers.
(defun beat-rel-MNN-states
       (dataset &optional
        (catalogue-information "no information")
	(beats-in-bar 4) (MNN-index 1) (MPN-index 2)
	(dur-index 3)
        (fifth-steps-mode
         (fifth-steps-mode
          dataset *Aarden-key-profiles* 0 1
          dur-index))
        (trans-pair&c-dataset
         (centre-dataset
          fifth-steps-mode dataset MNN-index
          MPN-index))
        (trans-pair (first trans-pair&c-dataset))
        (c-dataset (second trans-pair&c-dataset))
        (c-segments
         (butlast
          (segments-strict
           c-dataset MNN-index dur-index)))
        (segments
         (butlast
          (segments-strict
           dataset MNN-index dur-index)))
        (c-segment (first c-segments))
        (segment (first segments)))
  (if (null c-segment) ()
    (cons
     (list
      (list
       (+ (mod (first c-segment) beats-in-bar) 1)
       (nth-list-of-lists
        MNN-index (second c-segment)))
      (list
       catalogue-information
       (mapcar
        #'(lambda (x) (subseq x 0 5))
        (second segment))
       trans-pair fifth-steps-mode))
     (beat-rel-MNN-states
      dataset catalogue-information beats-in-bar
      MNN-index MPN-index dur-index fifth-steps-mode
      trans-pair&c-dataset trans-pair c-dataset
      (rest c-segments) (rest segments)))))
|#

#|
\noindent Example:
\begin{verbatim}
(centre-dataset '(-3 0)
 '((0 63 62 3/4 0) (3/4 63 62 1/4 0) (1 65 63 3/4 0)
   (7/4 63 62 1/4 0) (2 66 64 1 0) (3 65 63 3/4 0)
   (15/4 63 62 1/4 0) (4 66 64 1 0) (5 65 63 3/4 0)
   (23/4 63 62 1/4 0) (6 66 64 1 0) (7 65 63 1 0)
   (35/4 63 62 1/4 0) (9 65 63 3/4 0)
   (39/4 63 62 1/4 0) (10 66 64 3/4 0)
   (43/4 65 63 1/4 0) (11 63 62 1 0)))
--> ((63 62)
     ((0 0 0 3/4 0) (3/4 0 0 1/4 0) (1 2 1 3/4 0)
      (7/4 0 0 1/4 0) (2 3 2 1 0) (3 2 1 3/4 0)
      (15/4 0 0 1/4 0) (4 3 2 1 0) (5 2 1 3/4 0)
      (23/4 0 0 1/4 0) (6 3 2 1 0) (7 2 1 1 0)
      (35/4 0 0 1/4 0) (9 2 1 3/4 0) (39/4 0 0 1/4 0)
      (10 3 2 3/4 0) (43/4 2 1 1/4 0) (11 0 0 1 0)))
\end{verbatim}

\noindent Translates the dataset so that the tonic
note closest to the mean MNN is represented by the
pair (0 0). |#

(defun centre-dataset
       (fifth-steps-mode dataset &optional
        (MNN-index 1) (MPN-index 2)
        (MNN-MPN-pair
         (fifth-steps-mode2MNN-MPN fifth-steps-mode))
        (mean-MNN
         (mean
          (mapcar
           #'(lambda (x)
               (nth MNN-index x)) dataset)))
        (candidate-tonic-notes
         (add-to-list
          (first MNN-MPN-pair)
          (list
           -60 -48 -36 -24 -12 0 12 24 36 48 60 72)))
        (min&index
         (min-argmin
          (mapcar
           #'(lambda (x) (abs (- mean-MNN x)))
           candidate-tonic-notes)))
        (trans-pair
         (list
          (+ (first MNN-MPN-pair)
             (* 12 (- (second min&index) 5)))
          (+ (second MNN-MPN-pair)
             (* 7 (- (second min&index) 5))))))
  (list
   trans-pair
   (mapcar
    #'(lambda (x)
        (progn
          (setf
           (nth MNN-index x)
           (- (nth MNN-index x) (first trans-pair)))
          (setf
           (nth MPN-index x)
           (- (nth MPN-index x) (second trans-pair)))
          x)) (copy-tree dataset))))

#|
\noindent Example:
\begin{verbatim}
(fifth-steps-mode2MNN-MPN '(-5 0))
--> (61 61)
\end{verbatim}

\noindent A pair consisting of position on the cycle
of fifths and mode (0 for Ionian, 1 for Dorian, etc.)
is converted to a pair consisting of a MIDI note
number and morphetic pitch number for the tonic. This
was called by an older version of
\ref{fun:beat-rel-MNN-states} but now it may be
obsolete. |#

(defun fifth-steps-mode2MNN-MPN
       (fifth-steps-mode &optional
        (conversion
         '(((0 0) (60 60)) ((1 0) (67 64))
           ((2 0) (62 61)) ((3 0) (69 65))
           ((4 0) (64 62)) ((5 0) (71 66))
           ((6 0) (66 63)) ((7 0) (61 60))
           ((8 0) (68 64)) ((9 0) (63 61))
           ((10 0) (70 65)) ((-1 0) (65 63))
           ((-2 0) (70 66)) ((-3 0) (63 62))
           ((-4 0) (68 65)) ((-5 0) (61 61))
           ((-6 0) (66 64)) ((-7 0) (71 67))
           ((-8 0) (64 63))
           ((0 5) (63 62)) ((1 5) (70 66))
           ((2 5) (65 63)) ((3 5) (60 60))
           ((4 5) (67 64)) ((5 5) (62 61))
           ((6 5) (69 65)) ((7 5) (64 62))
           ((8 5) (71 66)) ((9 5) (66 63))
           ((-1 5) (68 65)) ((-2 5) (61 61))
           ((-3 5) (66 64)) ((-4 5) (71 66))
           ((-5 5) (64 62)) ((-6 5) (69 65))
           )))
  (second
   (assoc
    fifth-steps-mode conversion :test #'equalp)))

#| Old version. I feel like I have changed this
function before...
(defun fifth-steps-mode2MNN-MPN
       (fifth-steps-mode &optional
        (conversion
         '(((0 0) (60 60)) ((1 0) (67 64))
           ((2 0) (62 61)) ((3 0) (69 65))
           ((4 0) (64 62)) ((5 0) (71 66))
           ((6 0) (66 63)) ((7 0) (61 60))
           ((8 0) (68 64)) ((9 0) (63 61))
           ((10 0) (70 65)) ((-1 0) (65 63))
           ((-2 0) (70 66)) ((-3 0) (63 62))
           ((-4 0) (68 65)) ((-5 0) (61 61))
           ((-6 0) (66 64)) ((-7 0) (71 67))
           ((-8 0) (64 63))
           ((0 5) (69 65)) ((1 5) (64 62))
           ((2 5) (71 66)) ((3 5) (66 63))
           ((4 5) (61 60)) ((5 5) (68 64))
           ((6 5) (63 61)) ((7 5) (70 65))
           ((8 5) (65 62)) ((9 5) (72 66))
           ((-1 5) (62 61)) ((-2 5) (67 74))
           ((-3 5) (60 60)) ((-4 5) (65 63))
           ((-5 5) (70 66)) ((-6 5) (63 62))
           ((-7 5) (68 65)) ((-8 5) (61 61)))))
  (second
   (assoc
    fifth-steps-mode conversion :test #'equalp)))
|#
