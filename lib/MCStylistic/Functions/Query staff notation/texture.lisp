#| Copyright 2008-2014 Tom Collins
   Monday 9 June 2014
   Incomplete

\noindent The function
\nameref{fun:texture-from-kern} will parse a kern
file, convert it to a point-set representation in
which notes appear as points in pitch-time space, and
identify different types of musical texture in the
point set (e.g., monophonic, homophonic, melody with
accompaniment, polyphonic, or contrapuntal). These
textures are output as a list of quads: the first
element is the beginning of a time window; the second
is the end of a time window; the third is a texture
string (from the options above) or nil, and the fourth
is a value in $[0, 1]$ expressing the confidence with
which the texture label has been assigned.

The functions were coded hastily and require further
testing. For examples of different textures, see
below.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "kern-articulation"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "kern-by-col"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Query staff notation")
   :name "kern-to-staff-features"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
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
   :directory '(:relative "Pattern rating")
   :name "musical-properties"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Harmony and metre")
   :name "ontimes-signatures"
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
   :directory '(:relative "Markov models")
   :name "segmentation"
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
|#

#| Example of a monophonic point set, by taking the
top line of the opening 8 bars of Chopin's Mazurka in
C major op.24 no.2.
(setq
 point-set
 '((0 64 62 1 0) (1 67 64 1 0) (2 64 62 1 0)
   (3 67 64 1 0) (4 64 62 1 0) (5 67 64 1 0)
   (6 64 62 1 0) (7 67 64 1 0) (8 60 60 1 0)
   (8 64 62 1 0) (9 67 64 1 0) (10 64 62 1 0)
   (11 67 64 1 0) (12 64 62 1 0) (13 67 64 1 0)
   (14 64 62 1 0) (15 67 64 1 0) (16 64 62 1 0)
   (17 67 64 1 0) (18 64 62 1 0) (19 67 64 1 0)
   (20 64 62 1 0) (21 67 64 1 0) (22 64 62 1 0)
   (23 67 64 1 0)))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .9)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq ontimes-signatures '((1 3 4 0)))
(setq lyrics-tf nil)
(texture-from-kern
 "path&name usually required here" win-size hop-size
 mono-thresh homo-thresh macc-thresh
 ontimes-signatures point-set lyrics-tf)
--> ((0 24 "monophonic" 0.96703297))
|#

#| Example of a homophonic point set, by taking the
first four bars of Chopin's Mazurka in C major op.24
no.2.
(setq
 point-set
 '((0 48 53 1 1) (0 55 57 1 1) (0 60 60 1 0)
   (0 64 62 1 0) (1 43 50 1 1) (1 50 54 1 1)
   (1 59 59 1 0) (1 67 64 1 0) (2 48 53 1 1)
   (2 55 57 1 1) (2 60 60 1 0) (2 64 62 1 0)
   (3 43 50 1 1) (3 50 54 1 1) (3 59 59 1 0)
   (3 67 64 1 0) (4 48 53 1 1) (4 55 57 1 1)
   (4 60 60 1 0) (4 64 62 1 0) (5 43 50 1 1)
   (5 50 54 1 1) (5 59 59 1 0) (5 67 64 1 0)
   (6 48 53 1 1) (6 55 57 1 1) (6 60 60 1 0)
   (6 64 62 1 0) (7 43 50 1 1) (7 50 54 1 1)
   (7 59 59 1 0) (7 67 64 1 0) (8 48 53 1 1)
   (8 55 57 1 1) (8 60 60 1 0) (8 64 62 1 0)
   (9 43 50 1 1) (9 50 54 1 1) (9 59 59 1 0)
   (9 67 64 1 0) (10 48 53 1 1) (10 55 57 1 1)
   (10 60 60 1 0) (10 64 62 1 0) (11 43 50 1 1)
   (11 50 54 1 1) (11 59 59 1 0) (11 67 64 1 0)
   (12 57 58 1 1) (12 60 60 1 1) (12 64 62 1 1)
   (12 72 67 1/2 0) (25/2 84 74 1/2 0)))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .95)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq ontimes-signatures '((1 3 4 0)))
(setq lyrics-tf nil)
(texture-from-kern
 "path&name usually required here" win-size hop-size
 mono-thresh homo-thresh macc-thresh
 ontimes-signatures point-set lyrics-tf)
--> ((0 13 "homophonic" 0.9280303))
|#

#| Example of melody with accompaniment, by taking
bars 4-8 of Chopin's Mazurka in C major op.24 no.2.
(setq
 point-set
 '((12 57 58 1 1) (12 60 60 1 1) (12 64 62 1 1)
   (12 72 67 1/2 0) (25/2 84 74 1/2 0) (13 57 58 1 1)
   (13 60 60 1 1) (13 65 63 1 1) (13 83 73 1/3 0)
   (40/3 84 74 1/3 0) (41/3 83 73 1/3 0)
   (14 57 58 1 1) (14 60 60 1 1) (14 65 63 1 1)
   (14 81 72 1/2 0) (29/2 77 70 1/2 0) (15 59 59 1 1)
   (15 65 63 1 1) (15 74 68 1/2 0) (31/2 74 68 1/2 0)
   (16 59 59 1 1) (16 65 63 1 1) (16 74 68 1 0)
   (17 60 60 1 1) (17 64 62 1 1) (17 72 67 1 0)
   (18 62 61 1 1) (18 65 63 1 1) (18 69 65 1/2 0)
   (37/2 81 72 1/2 0) (19 53 56 1 1) (19 57 58 1 1)
   (19 62 61 1 1) (19 79 71 1/3 0) (58/3 81 72 1/3 0)
   (59/3 79 71 1/3 0) (20 53 56 1 1) (20 57 58 1 1)
   (20 62 61 1 1) (20 77 70 1/2 0) (41/2 74 68 1/2 0)
   (21 52 55 1 1) (21 62 61 1 1) (21 71 66 1/2 0)
   (43/2 71 66 1/2 0) (22 52 55 1 1) (22 62 61 1 1)
   (22 71 66 1 0) (23 57 58 1 1) (23 60 60 1 1)
   (23 69 65 1 0)))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .95)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq ontimes-signatures '((1 3 4 0)))
(setq lyrics-tf nil)
(texture-from-kern
 "path&name usually required here" win-size hop-size
 mono-thresh homo-thresh macc-thresh
 ontimes-signatures point-set lyrics-tf)
--> ((0 15 NIL NIL)
     (15 24 "melody with accompaniment" 1.0))
|#

#| Example of a fugue, by taking the first five bars
of Bach's Fugue in D major BWV850.
(setq
 point-set
 '((1 50 54 1/8 3) (9/8 52 55 1/8 3) (5/4 54 56 1/8 3)
   (11/8 55 57 1/8 3) (3/2 54 56 1/8 3)
   (13/8 52 55 1/8 3) (7/4 54 56 1/8 3)
   (15/8 50 54 1/8 3) (2 59 59 3/4 3)
   (11/4 59 59 1/4 3) (3 57 58 3/4 3)
   (15/4 55 57 1/4 3) (4 54 56 3/4 3)
   (19/4 55 57 1/4 3) (5 54 56 3/4 3) (5 57 58 1/8 2)
   (41/8 59 59 1/8 2) (21/4 61 60 1/8 2)
   (43/8 62 61 1/8 2) (11/2 61 60 1/8 2)
   (45/8 59 59 1/8 2) (23/4 52 55 1/4 3)
   (23/4 61 60 1/8 2) (47/8 57 58 1/8 2)
   (6 50 54 1 3) (6 66 63 3/4 2) (27/4 66 63 1/4 2)
   (7 52 55 1 3) (7 64 62 3/4 2) (31/4 62 61 1/4 2)
   (8 45 51 5/8 3) (8 61 60 1 2) (69/8 52 55 1/8 3)
   (35/4 54 56 1/8 3) (71/8 55 57 1/8 3) (9 57 58 2 3)
   (9 66 63 3/4 2) (39/4 66 63 1/4 2) (10 59 59 5/8 2)
   (85/8 59 59 1/8 2) (43/4 61 60 1/8 2)
   (87/8 62 61 1/8 2) (11 55 57 7/4 3)
   (11 64 62 1/4 2) (45/4 62 61 1/4 2)
   (23/2 64 62 1/4 2) (47/4 61 60 1/4 2)
   (12 57 58 1 2) (51/4 54 56 1/8 3)
   (103/8 52 55 1/8 3) (13 54 56 3/4 3)
   (13 62 61 1/8 1) (13 62 61 2 2) (105/8 64 62 1/8 1)
   (53/4 66 63 1/8 1) (107/8 67 64 1/8 1)
   (27/2 66 63 1/8 1) (109/8 64 62 1/8 1)
   (55/4 50 54 1/4 3) (55/4 66 63 1/8 1)
   (111/8 62 61 1/8 1) (14 55 57 3/4 3)
   (14 71 66 3/4 1) (59/4 52 55 1/4 3)
   (59/4 71 66 1/4 1) (15 57 58 3/4 3) (15 61 60 1 2)
   (15 69 65 3/4 1) (63/4 45 51 1/4 3)
   (63/4 67 64 1/4 1) (16 50 54 3/4 3) (16 62 61 1 2)
   (16 66 63 3/4 1) (67/4 52 55 1/4 3)
   (67/4 68 64 1/4 1) (17 54 56 3/4 3) (17 61 60 1 2)
   (17 69 65 1/8 0) (17 69 65 2 1) (137/8 71 66 1/8 0)
   (69/4 73 67 1/8 0) (139/8 74 68 1/8 0)
   (35/2 73 67 1/8 0) (141/8 71 66 1/8 0)
   (71/4 52 55 1/4 3) (71/4 73 67 1/8 0)
   (143/8 69 65 1/8 0) (18 50 54 3/4 3) (18 59 59 2 2)
   (18 78 70 3/4 0) (75/4 47 52 1/4 3)
   (75/4 78 70 1/4 0) (19 52 55 1 3) (19 68 64 1 1)
   (19 76 69 3/4 0) (79/4 74 68 1/4 0) (20 45 51 1 3)
   (20 57 58 1 2) (20 69 65 1 1) (20 73 67 5/8 0)))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .95)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq ontimes-signatures '((1 4 4 0)))
(setq lyrics-tf nil)
(texture-from-kern
 "path&name usually required here" win-size hop-size
 mono-thresh homo-thresh macc-thresh
 ontimes-signatures point-set lyrics-tf)
--> ((0 21 "contrapuntal" 1.0))
|#

#| Example of a homophonic point set, by taking bars
15-23 of Morley's `April is in my mistress' face'
(1594).
(setq
 point-set
 '((0 46 52 4 3) (0 58 59 2 2) (0 65 63 2 1)
   (0 70 66 2 0) (3 58 59 1 2) (3 70 66 1 1)
   (3 74 68 1 0) (4 65 63 1 2) (4 69 65 1 1)
   (4 72 67 1 0) (5 62 61 1 2) (5 70 66 2 1)
   (5 74 68 1 0) (6 60 60 2 2) (6 75 69 2 0)
   (7 69 65 1 1) (8 58 59 2 2) (8 70 66 2 1)
   (8 74 68 2 0) (10 46 52 2 3) (10 58 59 2 2)
   (10 65 63 2 1) (11 74 68 1 0) (12 51 55 1 3)
   (12 55 57 1 2) (12 67 64 1 1) (12 70 66 1 0)
   (13 46 52 1 3) (13 58 59 2 2) (13 65 63 1 1)
   (13 74 68 1 0) (14 53 56 2 3) (14 65 63 2 1)
   (14 72 67 2 0) (15 57 58 1 2) (16 46 52 4 3)
   (16 58 59 2 2) (16 65 63 2 1) (16 70 66 2 0)
   (18 65 63 4 2) (18 70 66 2 1) (18 74 68 2 0)
   (20 53 56 2 3) (20 69 65 2 1) (20 72 67 2 0)
   (22 50 54 2 3) (22 65 63 2 2) (22 70 66 2 1)
   (22 74 68 2 0) (24 55 57 8 3) (24 62 61 8 2)
   (24 70 66 1 1) (24 74 68 1 0) (25 69 65 1 1)
   (25 72 67 1 0) (26 67 64 1 1) (26 70 66 1 0)
   (27 65 63 1 1) (27 69 65 1 0) (28 67 64 4 1)
   (28 70 66 4 0) (32 50 54 4 3) (32 62 61 2 2)
   (32 66 63 4 1) (32 69 65 1 0)))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .95)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq ontimes-signatures '((1 4 4 0)))
(setq lyrics-tf t)
(texture-from-kern
 "path&name usually required here" win-size hop-size
 mono-thresh homo-thresh macc-thresh
 ontimes-signatures point-set lyrics-tf)
--> ((0 36 "homophonic" 0.9215278))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "C@merata2014" "misc")
   :name "dowland_denmark_galliard" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(setq win-size 4)
(setq hop-size 1)
(setq mono-thresh .95)
(setq homo-thresh .8)
(setq macc-thresh .8)
(setq
 ontimes-signatures
 (append-ontimes-to-time-signatures
  (kern-file2ontimes-signatures path&name)))
(setq
 point-set
 (kern-file2dataset-by-col path&name))
--> ((0 38 47 1 1) (0 57 58 1 0) (0 62 61 1 0)
     (0 69 65 1 0) (1 50 54 2 1) (1 57 58 2 0)
     (1 62 61 2 0) (1 69 65 1 0) (2 69 65 1 0)
     (3 38 47 3 1) (3 57 58 3 0) (3 69 65 1 0)
     (4 67 64 1/2 0) (9/2 66 63 1/2 0)
     (5 64 62 1/2 0) (11/2 67 64 1/2 0) (6 38 47 2 1)
     (6 57 58 2 0) (6 66 63 1 0) (7 64 62 1/2 0)
     (15/2 62 61 1/2 0) (8 45 51 1 1) (8 64 62 1 0)
     (9 50 54 1 1) (9 57 58 3 0) (9 62 61 3 0)
     (10 38 47 2 1))
(texture-from-kern
 path&name win-size hop-size mono-thresh homo-thresh
 macc-thresh ontimes-signatures point-set)
--> ((0 12 "melody with accompaniment" 0.8333333))

(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "C@merata2014" "training_v1")
   :name "f1" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(texture-from-kern path&name)
--> ((0 36 "homophonic" 0.89666665)
     (36 64 "polyphonic" 1.0))
\end{verbatim}

\noindent This function parses a kern file, converts
it to a point-set representation in which notes appear
as points in pitch-time space, and identifies
different types of musical texture in the point set
(e.g., monophonic, homophonic, melody with
accompaniment, polyphonic, or contrapuntal). These
textures are output as a list of quads: the first
element is the beginning of a time window; the second
is the end of a time window; the third is a texture
string (from the options above) or nil, and the fourth
is a value in $[0, 1]$ expressing the confidence with
which the texture label has been assigned.

The function works by passing windows of specifiable
size (four bars) and hop (one size) to the function
\nameref{fun:texture-whole-point-set}. Windows with
contiguous labels are elided. The thresholds for
textural labels can be set also.

The point set in the second example comes from bars
23-30 of Morley's `April is in my mistress' face'
(1594). Please see above for more example point
sets. |#

(defun texture-from-kern
       (path&name &optional
        (win-size 4) (hop-size 1)
        #| Thresholds for different texture types. |#
        (mono-thresh .95) (homo-thresh .8)
        (macc-thresh .8)
        (ontimes-signatures
         (append-ontimes-to-time-signatures
          (kern-file2ontimes-signatures path&name)))
        (point-set
         (kern-file2dataset-by-col path&name))
        ; Lyrics determine polyphonic/contrapuntal.
        (lyrics-tf (kern-file2lyrics-tf path&name))
        (ontime-idx 0) (MNN-idx 1) ;(MPN-idx 2)
        (dur-idx 3) (staff-idx 4)
        #| Get first ontime and last offtime to define
        appropriate windows. |#
        (on-first
         (nth ontime-idx (first point-set)))
        (off-last
         (max-item
          (mapcar
           #'(lambda (x)
               (+
                (nth ontime-idx x) (nth dur-idx x)))
           point-set)))
        (bar-last
         (first
          (bar&beat-number-of-ontime
           off-last ontimes-signatures)))
        ; Define the windows in terms of bars.
        (bar-win
         (mapcar
          #'(lambda (x)
              (list x (+ x win-size)))
          (increment-by-x-n-times
           hop-size
           (- (ceiling (/ bar-last hop-size)) 1) 1)))
        ; Define the windows in terms of ontimes.
        (on-win
         (cons
          (list
           on-first
           (my-last
            (ontime-of-bar&beat-number
             (second
              (first bar-win)) 1 ontimes-signatures)))
          (mapcar
           #'(lambda (x)
               (list
                (my-last
                 (ontime-of-bar&beat-number
                  (first x) 1 ontimes-signatures))
                (my-last
                 (ontime-of-bar&beat-number
                  (second x) 1 ontimes-signatures))))
           (rest bar-win))))
        ; Points (notes) in each window.
        (points-win
         (mapcar
          #'(lambda (x)
              (datapoints-sounding-between
               (append-offtimes point-set dur-idx)
               (first x) (second x)))
          on-win))
        #| Pass each non-empty point set to the
        function texture-whole-point-set to get raw
        texture estimates. |#
        (texture-raw
         (butlast
          (mapcar
           #'(lambda (x)
               (if (not (null x))
                 (texture-whole-point-set
                  x lyrics-tf mono-thresh
                  homo-thresh macc-thresh ontime-idx
                  MNN-idx ;MPN-idx
                  dur-idx staff-idx)))
           points-win)))
        #| If one wanted to "smooth" the texture
        estimates, so they did not flip for alternate
        time windows for instance, this could be done
        here. |#
        
        #| Collect the contiguous textures
        estimates. |#
        (texture-change-idxs
         (append
          (loop for i from 1
            to (- (length texture-raw) 1) append
            (if (not
                 (string=
                  (first (nth i texture-raw))
                  (first (nth (- i 1) texture-raw))))
              (list i)))
          (list (- (length texture-raw) 1))))
        #| Output the texture summaries. |#
        (texture-summaries
         (loop for i from 0
           to (- (length texture-change-idxs) 1)
           collect
           (list
            (if (equalp i 0)
              0
              (second
               (nth
                (nth
                 (- i 1)
                 texture-change-idxs) on-win)))
            (min-item
             (list
              (second
               (nth
                (nth i texture-change-idxs) on-win))
              off-last))
            (if (equalp i 0)
              (first (first texture-raw))
              (first
               (nth
                (nth (- i 1) texture-change-idxs)
                texture-raw)))
            (if (not
                 (null
                  (if (equalp i 0)
                    (first (first texture-raw))
                    (first
                     (nth
                      (nth
                       (- i 1) texture-change-idxs)
                      texture-raw)))))
              (float
               (mean
                (if (equalp i 0)
                  (subseq
                   (nth-list-of-lists 1 texture-raw)
                   0 (nth i texture-change-idxs))
                  (if (<
                       i
                       (-
                        (length texture-change-idxs)
                        1))
                    (subseq
                     (nth-list-of-lists 1 texture-raw)
                     (nth (- i 1) texture-change-idxs)
                     (nth i texture-change-idxs))
                    (subseq
                     (nth-list-of-lists 1 texture-raw)
                     (nth
                      (- i 1)
                      texture-change-idxs)))))))))))
  (loop for i from 0
    to (- (length texture-summaries) 1) append
    (if (<
         (first (nth i texture-summaries))
         (second (nth i texture-summaries)))
      (list
       (list
        (first (nth i texture-summaries))
        (if (<=
             (second (nth i texture-summaries))
             off-last)
          (second (nth i texture-summaries)) off-last)
        (third (nth i texture-summaries))
        (fourth (nth i texture-summaries)))))))

#|
\noindent Example:
\begin{verbatim}
(setq question-string "polyphony")
(setq
 texture-point-set
 '((0 36 "homophonic" 0.89666665)
   (36 64 "polyphonic" 1.0)))
(texture-time-intervals
 question-string texture-point-set)
--> ((36 64))
\end{verbatim}

\noindent This function identifies the time intervals
in some texture point set that correspond to the
contents of a question string, and returns those time
intervals. |#

(defun texture-time-intervals
       (question-string texture-point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        #| Another way to express homophony is to say
        "simultaneous rhythm in all voices", so this
        chunk of code checks for that, and replaces
        the question string with "homophonic" in this
        instance. |#
        (n-voices
         (nth
          (length staff&clef-names)
          (list
           nil nil "two" "three" "four" "five" "six"
           "seven" "eight" "nine" "ten" "eleven"
           "twelve" "thirteen" "fourteen" "fifteen"
           "sixteen" "seventeen" "eighteen" "nineteen"
           "twenty")))
        (question-string
         (if (or
              (string=
               question-string
               "simultaneous rhythm in all voices")
              (and
               n-voices
               (search
                (concatenate
                 'string "simultaneous rhythm in "
                 n-voices " voices")
                question-string)))
           "homophonic" question-string))
        (texture-assoc
         '(("monophonic" "monophonic")
           ("monophonic passage" "monophonic")
           ("monophony" "monophonic")
           ("homophonic" "homophonic")
           ("homophonic passage" "homophonic")
           ("homophony" "homophonic")
           ("melody with accompaniment"
            "melody with accompaniment")
           ("polyphonic" "polyphonic")
           ("polyphonic passage" "polyphonic")
           ("polyphony" "polyphonic")
           ("contrapuntal" "contrapuntal")))
        #| Determine if a texture is mentioned in the
        question string. |#
        (probe-texture
         (second
          (assoc
           question-string texture-assoc
           :test #'string=))))
  (if probe-texture
    (loop for i from 0 to
      (- (length texture-point-set) 1) when
      (and
       (stringp (third (nth i texture-point-set)))
       (string=
        (third (nth i texture-point-set))
        probe-texture))
      collect
      (list
       (first (nth i texture-point-set))
       (second (nth i texture-point-set))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 point-set
 '((32 50 54 4 3) (32 62 61 2 2) (32 66 63 4 1)
   (32 69 65 1 0) (33 74 68 1 0) (34 74 68 1 0)
   (35 62 61 1 2) (35 74 68 1 0) (36 62 61 1 2)
   (36 77 70 4 0) (37 50 54 1 3) (37 62 61 1 2)
   (38 50 54 1 3) (38 65 63 4 2) (39 50 54 1 3)
   (40 53 56 4 3) (41 69 65 1 1) (41 72 67 1 0)
   (42 65 63 2 2) (42 69 65 1 1) (42 72 67 1 0)
   (43 69 65 1 1) (43 72 67 1 0) (44 60 60 4 2)
   (44 72 67 2 1) (44 75 69 2 0) (45 48 53 1 3)
   (46 48 53 1 3) (46 67 64 2 1) (46 75 69 2 0)
   (47 48 53 1 3) (48 51 55 3 3) (48 67 64 2 1)
   (48 70 66 1 0) (49 67 64 1 0) (50 55 57 2 2)
   (50 67 64 2 1) (50 70 66 1 0) (51 51 55 1 3)
   (51 72 67 1 0) (52 46 52 3 3) (52 58 59 2 2)
   (52 65 63 1 1) (52 74 68 8 0) (53 62 61 1 1)
   (54 58 59 2 2) (54 67 64 3 1) (55 48 53 1 3)
   (56 50 54 4 3) (56 57 58 4 2) (57 66 63 1/2 1)
   (115/2 64 62 1/2 1) (58 66 63 2 1) (60 55 57 2 2)
   (60 67 64 1 1) (60 71 66 4 0) (61 67 64 1 1)
   (62 67 64 1 1) (63 55 57 1 2) (63 67 64 1 1)))
(texture-whole-point-set point-set)
--> ("polyphonic" 1)
\end{verbatim}

\noindent This function takes a point set as its only
mandatory argument. Optional arguments include
thresholds between 0 and 1 for the proportion of
conformant notes that must be surpassed in order to
declare a texture monophonic, homophonic, melody with
accompaniment, etc. The other possible textures are
polyphonic (with words), contrapuntal (without words),
or no texture defined (nil).

The point set in the example comes from bars 23-30 of
Morley's `April is in my mistress' face' (1594).
Please see above for more example point
sets. |#

(defun texture-whole-point-set
       (point-set &optional (lyrics-tf T)
        #| Thresholds for declaring textures. |#
        (mono-thresh .95) (homo-thresh .8)
        (macc-thresh .8)
        #| Typical point set columns. |#
        (ontime-idx 0) (MNN-idx 1) ;(MPN-idx 2)
        (dur-idx 3) (staff-idx 4)
        (proj-vec
         (add-to-nth
           1 (+ ontime-idx 1) (constant-vector 0 5)))
        (proj-vec2
         (add-to-nth
          1 (+ ontime-idx 1)
          (add-to-nth
           1 (+ MNN-idx 1) (constant-vector 0 5))))
        #| Extract the first sky line, use it to
        define an accompaniment, and then extract the
        skyline of that as well and define a second
        accompaniment. This is most useful for testing
        whether a texture is melody with
        accompaniment, but variables like sky are used
        to test other textures as well. |#
        (sky
         (sky-line-clipped
          point-set ontime-idx MNN-idx dur-idx))
        (accomp
         (set-difference-multidimensional-sorted-asc
          point-set sky))
        (sky2
         (if accomp
           (sky-line-clipped
            accomp ontime-idx MNN-idx dur-idx)))
        (accomp2
         (set-difference-multidimensional-sorted-asc
          accomp sky2))
        #| Required to test monophony. |#
        (unq-on-MNN
         (orthogonal-projection-unique-equalp
          point-set proj-vec2))
        (sky-on-MNN
         (orthogonal-projection-unique-equalp
          sky proj-vec2))
        (mono-obs
         (if (not (null unq-on-MNN))
           (/
            (length
             (intersection-multidimensional
              unq-on-MNN sky-on-MNN))
            (length unq-on-MNN))))
        #| Required to test homophony. |#
        (accomp-on-MNN
         (set-difference-multidimensional-sorted-asc
          unq-on-MNN sky-on-MNN))
        (accomp-on
         (orthogonal-projection-unique-equalp
          accomp-on-MNN proj-vec))
        (sky-on
         (orthogonal-projection-unique-equalp
          sky proj-vec))
        (homo-obs
         (if (not (null sky-on)) 
           (/
            (length
             (intersection-multidimensional
              accomp-on sky-on))
            (max-item
             (list
              (length accomp-on) (length sky-on))))))
        #| Required to test melody + accompaniment. |#
        (sky2-on
         (orthogonal-projection-unique-equalp
          sky2 proj-vec))
        (accomp2-on
         (orthogonal-projection-unique-equalp
          accomp2 proj-vec))
        (macc-obs
         (if (not (null sky2-on))
           (/
            (length
             (intersection-multidimensional
              accomp2-on sky2-on))
            (max-item
             (list
              (length accomp2-on)
              (length sky2-on))))))
        #| This macc-obs measure needs more thought.
        I'm including something about rhythmic
        density as well. In short, if the voice
        labelled 0 is about three times more active
        or dense than the voice labelled 1, then it is
        probably melody with accompaniment. |#
        (point-set-top
         (dataset-restricted-to-m-in-nth
          point-set 0 staff-idx))
        (point-set-bottom
         (dataset-restricted-to-m-in-nth
          point-set 1 staff-idx))
        (rhythmic-density-top
         (if point-set-top
           (rhythmic-density
            (remove-duplicates
             (mapcar
              #'(lambda (x)
                  (list (first x))) point-set-top)
             :test #'equalp))))
        (rhythmic-density-bottom
         (if point-set-bottom
           (rhythmic-density
            (remove-duplicates
             (mapcar
              #'(lambda (x)
                  (list (first x))) point-set-bottom)
             :test #'equalp))))
        (normalised-density-ratio
         (if (and
              (numberp rhythmic-density-top)
              (numberp rhythmic-density-bottom)
              (> rhythmic-density-top 0)
              (> rhythmic-density-bottom 0))
         (float
          (/
           (/
            rhythmic-density-top
            rhythmic-density-bottom) 3)) 0))
        (macc-obs
         (if macc-obs
           (max-item
            (list macc-obs normalised-density-ratio))
           normalised-density-ratio))
        
        #| Required to test polyphony. Polyphony is
        tested by extracting point sets according to
        staff numbers and testing whether each within-
        staff set is monophonic. If it is, then the
        result "polyphonic" is returned. |#
        (unique-staves
         (remove-duplicates
          (sort
           (nth-list-of-lists staff-idx point-set)
           #'<) :test #'equalp))
        (datasets-restricted-to-m-in-nth
         (mapcar
          #'(lambda (x)
              (dataset-restricted-to-m-in-nth
               point-set x staff-idx))
          unique-staves))
        (monos-obs
         (mapcar
          #'(lambda (x)
              (/
               (length
                (intersection-multidimensional
                 (orthogonal-projection-unique-equalp
                  x proj-vec2)
                 (orthogonal-projection-unique-equalp
                  (sky-line-clipped
                   x ontime-idx MNN-idx dur-idx)
                  proj-vec2)))
               (length
                (orthogonal-projection-unique-equalp
                 x proj-vec2))))
          datasets-restricted-to-m-in-nth)))
  (if (>= mono-obs mono-thresh)
    (list "monophonic" mono-obs)
    (if (>= homo-obs homo-thresh)
      (list "homophonic" homo-obs)
      (if (>= macc-obs macc-thresh)
        (list
         "melody with accompaniment" macc-obs)
        (if (test-all-true
             (mapcar
              #'(lambda (x)
                  (>= x mono-thresh)) monos-obs))
          (if lyrics-tf
            (list
             "polyphonic" (mean monos-obs))
            (list
             "contrapuntal" (mean monos-obs))))))))






