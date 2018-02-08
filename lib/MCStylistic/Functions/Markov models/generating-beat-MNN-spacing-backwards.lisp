#| Copyright 2008-2013 Tom Collins
   Tuesday 12 October 2010
   Incomplete

\noindent The main function here is called generate-
beat-MNN-spacing<-. Given final states, a state-
transition matrix, a lower limit for the ontime of
the first state, and a template dataset, it generates
datapoints (among other output) that conform to
various criteria, which can be specified using the
optional arguments. The criteria are things like: not
too many consecutive states from the same source, the
range is comparable with that of the template, and the
likelihood of the states is comparable with that of
the template.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "empirical-preliminaries"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-beat-MNN-spacing-forwards"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "interpolation"
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
   :directory '(:relative "File conversion")
   :name "midi-export"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "realising-states-backwards"
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
   :directory '(:relative "Maths foundation")
   :name "stats-sampling"
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

;(defvar *rs* (make-random-state t))

#|
\noindent Example:
\begin{verbatim}
(setq
 states<-
 '(((2 (7 5 4))
    (NIL NIL "C-24-2"
     ((358 48 53 2 1 360 6) (358 55 57 2 1 360 7)
      (358 60 60 2 0 360 8) (358 64 62 2 0 360 9))))
   ((3/2 (19 7))
    (12 7 "C-30-2"
     ((153 45 51 1 1 154 599) (153 64 62 1 0 154 600)
      (307/2 71 66 1/2 0 154 602))))))
(setq
 datapoints
 '((67/2 32 44 1/2 1) (67/2 51 55 5/2 0)
   (67/2 58 59 1/2 0) (34 44 51 2 1) (34 56 58 2 0)
   (34 60 60 2 0)))
(setq
 template-segments
 '((27
    ((27 39 48 1 1 28 91) (27 63 62 1/2 0 55/2 92)))
   (55/2 ((27 39 48 1 1 28 91)))
   (111/4
    ((27 39 48 1 1 28 91) (111/4 72 67 1/4 0 28 93)))
   (28
    ((28 51 55 1 1 29 94) (28 60 60 1 1 29 95)
     (28 68 65 1 1 29 96) (28 84 74 1 0 29 97)))
   (29 ((29 82 73 1/3 0 88/3 98)))
   (88/3 ((88/3 80 72 1/3 0 89/3 99)))
   (89/3 ((89/3 77 70 1/3 0 30 100)))
   (30
    ((30 39 48 1 1 31 101) (30 79 71 1/2 0 61/2 102)))
   (61/2
    ((30 39 48 1 1 31 101) (61/2 77 70 1/2 0 31 103)))
   (31
    ((31 51 55 1 1 32 104) (31 55 57 1 1 32 105)
     (31 61 61 1 1 32 106) (31 75 69 1/2 0 63/2 107)))
   (63/2
    ((31 51 55 1 1 32 104) (31 55 57 1 1 32 105)
     (31 61 61 1 1 32 106) (63/2 73 68 1/2 0 32 108)))
   (32
    ((32 51 55 1 1 33 109) (32 55 57 1 1 33 110)
     (32 61 61 1 1 33 111) (32 70 66 1/2 0 65/2 112)))
   (65/2
    ((32 51 55 1 1 33 109) (32 55 57 1 1 33 110)
     (32 61 61 1 1 33 111) (65/2 65 63 1/2 0 33 113)))
   (33
    ((33 44 51 2 1 35 114) (33 63 62 1/2 0 67/2 115)))
   (67/2 ((33 44 51 2 1 35 114)))
   (135/4
    ((33 44 51 2 1 35 114)
     (135/4 72 67 1/4 0 34 116)))
   (34
    ((33 44 51 2 1 35 114) (34 51 55 1 1 35 117)
     (34 60 60 1 1 35 118) (34 68 65 1 0 35 119)))))
(setq
 template-likelihood-profile
 '((27 0.11785113) (55/2 1/6) (111/4 0.10878566)
   (28 0.08494337) (29 1/15) (88/3 1/14) (89/3 1/7)
   (30 0.06666667) (61/2 0.06666667) (31 0.11632561)
   (63/2 0.11632561) (32 0.108109735)
   (65/2 0.108109735) (33 0.16666667) (67/2 1/6)
   (135/4 0.16666667) (34 0.16666667)))
(setq
 checklist '("originalp" "range&meanp" "likelihoodp"))
(setq c-sources 3)
(setq c-bar 12)
(setq c-min 7)
(setq c-max 7)
(setq c-beats 3)
(setq c-prob 0.1)
(checklist<-p
 states<- datapoints template-segments
 template-likelihood-profile checklist c-sources c-bar
 c-min c-max c-beats c-prob)
--> T.
\end{verbatim}

\noindent Checks are made of sources, of the range
and mean of the notes supplied in the last element of
the backwards-generated states, and their
likelihoods. |#

(defun checklist<-p
       (states<- datapoints template-segments
        template-likelihood-profile checklist
        c-sources c-bar c-min c-max c-beats c-prob
        &optional
        (lastn-sources
         (mapcar
          #'(lambda (x)
              (third (second x)))
          (lastn c-sources states<-)))
        (originaledp
         (or
          (< (length lastn-sources) c-sources)
          (index-item-1st-doesnt-occur
           (first lastn-sources)
           (rest lastn-sources))))
        (datapoints-segments
         (if (or
              (find
               "mean&rangep" checklist
               :test #'string=)
              (find
               "likelihoodp" checklist
               :test #'string=))
           (segments-strict datapoints 1 3)))
        (first-segment
         (first datapoints-segments))
        (mean&rangedp
         (if (find
              "mean&rangep" checklist :test #'string=)
           (mean&rangep
            first-segment template-segments c-bar
            c-min c-max) t))
        (likelihoodp
         (if (find
              "likelihoodp" checklist :test #'string=)
           (and
            (pitch&octave-spellingp
             (second first-segment))
            (comparable-likelihood-profile<-p
             first-segment datapoints c-beats
             template-likelihood-profile c-prob)) t)))
  (and originaledp mean&rangedp likelihoodp))

#| \noindent Example:
\begin{verbatim}
(setq
 ontime-state-points-pair
 '(111/4
   ((27 39 48 1 1) (111/4 72 67 1/4 0))))
(setq
 datapoints
 '((27 39 48 1 1) (27 63 62 1/2 0) (111/4 72 67 1/4 0)
   (28 51 55 1 1) (28 60 60 1 1) (28 68 65 1 1)
   (28 84 74 1 0) (29 82 73 1/3 0) (88/3 80 72 1/3 0)
   (89/3 77 70 1/3 0) (30 39 48 1 1) (30 79 71 1/2 0)
   (61/2 77 70 1/2 0) (31 51 55 1 1) (31 55 57 1 1)
   (31 61 61 1 1) (31 75 69 1/2 0) (63/2 73 68 1/2 0)
   (32 51 55 1 1) (32 55 57 1 1) (32 61 61 1 1)
   (32 70 66 1/2 0) (65/2 65 63 1/2 0) (33 44 51 2 1)
   (33 63 62 1/2 0) (135/4 72 67 1/4 0) (34 51 55 1 1)
   (34 60 60 1 1) (34 68 65 1 0)))
(setq c-beats 3)
(setq
 template-likelihood-profile
 '((27 0.07142857) (55/2 1/14) (111/4 0.062499996)
   (28 0.10958345) (29 0.09672784) (30 0.11785113)
   (91/3 0.083333336) (92/3 0.11785113) (31 0.1514267)
   (32 0.13999122) (33 0.16666667) (67/2 1/6)
   (135/4 0.16666667) (34 0.3333333)))
(setq c-prob 0.1)
(comparable-likelihood-profile<-p
 ontime-state-points-pair datapoints c-beats
 template-likelihood-profile c-prob)
--> T.
\end{verbatim}

\noindent This function is similar to the function
comparable-likelihood-profilep, the difference being
it calls the function geom-mean-likelihood-of-
subset<- (forwards looking for the empirical
distribution) rather than geom-mean-likelihood-of-
subset. It takes a pair consisting of an ontime and
points that sound at the ontime. Based on an empirical
distribution over the argument named datapoints (with
a context governed by c-beats), it calculates the
geometric mean of the likelihood of these points. This
likelihood is then compared with that at the same
ontime in the template. If the absolute difference
between the likelihoods is less than the threshold
c-prob, T is returned, and NIL otherwise. T will also
be returned if there are no points. |#

(defun comparable-likelihood-profile<-p
       (ontime-state-points-pair datapoints c-beats
        template-likelihood-profile c-prob &optional
        (subset (second ontime-state-points-pair))
        (subset-ontimes (nth-list-of-lists 0 subset))
        (state-likelihood
         (if subset
           (geom-mean-likelihood-of-subset<-
            subset
            (orthogonal-projection-not-unique-equalp
             subset (list 0 1))
            (min-item subset-ontimes)
            (max-item subset-ontimes)
            (orthogonal-projection-not-unique-equalp
             datapoints (list 0 1))
            (nth-list-of-lists 0 datapoints)
            c-beats))))
  (if state-likelihood
    (<
     (abs
      (-
       state-likelihood
       (linearly-interpolate
        (first ontime-state-points-pair)
        template-likelihood-profile)))
     c-prob) t))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   final-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-final-states.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix<-.txt")))
  (setq no-ontimes< 111/4)
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-24-3-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 0 120))
  "Yes!")

(generate-beat-MNN-spacing<-
 final-states stm<- no-ontimes<
 dataset-template-shifted)
--> '.

\end{verbatim}

\noindent This function is similar to the function
generate-beat-MNN-spacing->, the difference is that
the former generates a passage backwards one state at
a time. The checking process is analogous. Given final
states, a state-transition matrix, a lower limit for
ontime, and a template dataset, this function
generates datapoints (among other output) that conform
to various criteria, which can be specified using the
optional arguments. The criteria are things like: not
too many consecutive states from the same source (c-
sources), the range is comparable with that of the
template (c-bar, c-min, and c-max), and the likelihood
of the states is comparable with that of the template
(c-beats and c-prob). A record, named index-failures,
is kept of how many times a generated state fails to
meet the criteria. When a threshold c-failures is
exceeded at any state index, the penultimate state is
removed and generation continues. If the value of c-
failures is exceeded for the first state, a message is
returned.

This function returns the states as well as the
datapoints, and also index-failures and i, the total
number of iterations. |#

(defun generate-beat-MNN-spacing<-
       (final-states stm<- no-ontimes<
        dataset-template &optional
        (checklist (list "originalp"))
        (beats-in-bar 4)
        (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (template-likelihood-profile
         (geom-mean-likelihood-of-states<-
          template-segments dataset-template c-beats))
        (final-MNN
         (if template-segments
           (second
            (first
             (second (my-last template-segments))))
           60))
        (final-MPN
         (if template-segments
           (third
            (first
             (second (my-last template-segments))))
           60))
        (i 1) (index-failures (list 0))
        (states<-
         (list
          (if template-segments
            (choose-one-with-beat
             (+
              (mod
               (first (my-last template-segments))
               beats-in-bar) 1)
             final-states)
            (choose-one final-states))))
        (datapoints
         (translate-datapoints-to-last-ontime
          (first (my-last template-segments)) 0
          (sort-dataset-asc
           (states2datapoints-by-lookup<-
            states<- beats-in-bar final-MNN
            final-MPN))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states<-)) stm<-
            :test #'equalp))))
        (checklistedp
         (checklist<-p
          states<- datapoints template-segments
          template-likelihood-profile checklist
          c-sources c-bar c-min c-max c-beats c-prob))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    #| 12/10/2010 For testing purposes.
    (write-to-file-append
     (list datapoints)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/datapoints.txt"))
    (write-to-file-append
     (list states<-)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/states<-.txt"))
    (write-to-file-append
     (list next-state)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/next-state.txt"))
    (write-to-file-append
     (list index-failures)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/index-failures.txt"))
    |#
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states<- datapoints i
         index-failures)
        (generate-beat-MNN-spacing<-
         final-states stm<- no-ontimes<
         dataset-template checklist beats-in-bar
         c-failures c-sources c-bar c-min c-max
         c-beats c-prob template-segments
         template-likelihood-profile final-MNN
         final-MPN (+ i 1)
         (append
          (subseq index-failures 0 (- failurep 1))
          (list
           (+ (nth (- failurep 1) index-failures) 1)))
         ; 12/10/2010 Special case avoids null states
         (if (equalp failurep 1)
           (list
            (if template-segments
              (choose-one-with-beat
               (+
                (first (my-last template-segments)) 1)
               final-states)
              (choose-one final-states)))
           (subseq states<- 0 (- failurep 1)))))
      (if checklistedp
        (if (<=
             (first (first datapoints)) no-ontimes<)
          (list states<- datapoints i index-failures)
          (if next-state
            (generate-beat-MNN-spacing<-
             final-states stm<- no-ontimes<
             dataset-template checklist beats-in-bar
             c-failures c-sources c-bar c-min c-max
             c-beats c-prob template-segments
             template-likelihood-profile final-MNN
             final-MPN (+ i 1)
             (if (< (length states<-)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states<- (list next-state)))
            (generate-beat-MNN-spacing<-
             final-states stm<- no-ontimes<
             dataset-template checklist beats-in-bar
             c-failures c-sources c-bar c-min c-max
             c-beats c-prob template-segments
             template-likelihood-profile final-MNN
             final-MPN (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states<- datapoints)))
        (generate-beat-MNN-spacing<-
         final-states stm<- no-ontimes<
         dataset-template checklist beats-in-bar
         c-failures c-sources c-bar c-min c-max
         c-beats c-prob template-segments
         template-likelihood-profile final-MNN
         final-MPN i ;Only instance not incremented
         (append
          (butlast index-failures)
          (list (+ (my-last index-failures) 1)))
         (if (equalp (length index-failures) 1)
           (list
            (if template-segments
              (choose-one-with-beat
               (+ (first (my-last template-segments)) 1)
               final-states)
              (choose-one final-states)))
           (butlast states<-)))))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   final-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/internal-final-states.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/transition-matrix<-.txt")))
  (setq no-ontimes< 2)
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-24-3-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 0 120))
  (setq
   *rs* #.(CCL::INITIALIZE-RANDOM-STATE 48164 60796))
  (setq
   datapoints-from-next-interval
   '((5/2 63 62 1/2 0) (5/2 72 67 1/2 0)
     (3 34 45 1 1) (3 46 52 1 1) (3 65 63 1/2 0)
     (3 74 68 1/2 0) (15/4 65 63 1/4 0)))
  (setq
   next-state-context-pair
   (first
    (beat-spacing-states
     datapoints-from-next-interval "No information"
     3 1 3)))
  (setq generation-interval '(1 3))
  (setq
   pattern-region
   '((1 51 55) (1 55 57) (1 61 61) (1 63 62) (2 51 55)
     (2 55 57) (2 61 61) (5/2 73 68)))
  "Data loaded and variables set.")
(generate-beat-spacing-forcing<-
 final-states stm<- no-ontimes< dataset-template
 generation-interval pattern-region
 next-state-context-pair (list "originalp")
 3 10 4 19 12 12 12 .15)
--> ((((7/2 (9))
       (NIL NIL "No information"
        ((5/2 63 62 1/2 0 3 0)
         (5/2 72 67 1/2 0 3 1))))
      ((3 (9))
       (-2 -1 "C-6-3"
        ((101 71 66 1/2 0 203/2 424)
         (101 80 71 1/2 0 203/2 425)))))
     ((2 65 63 1/2 0) (2 74 68 1/2 0)
      (5/2 63 62 1/2 0) (5/2 72 67 1/2 0))
     2 (0 0))
\end{verbatim}

\noindent This function appears to be very simiar to
the function
\nameref{fun:generate-beat-MNN-spacing<-}. The
difference is that there are some extra arguments
here, which allow for using either external or
internal initial/final states, and for using
information from a discovered pattern or previous/next
state to further guide the generation, hence
`forcing'. |#

(defun generate-beat-spacing-forcing<-
       (final-states stm<- no-ontimes<
        dataset-template generation-interval
        pattern-region next-state-context-pair
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (template-likelihood-profile
         (geom-mean-likelihood-of-states<-
          template-segments dataset-template c-beats))
        (final-MNN
         (if next-state-context-pair
           (second
            (first
             (fourth
              (second next-state-context-pair))))
           (if pattern-region
             (second
              (first
               (second
                (nth
                 (position
                  (first (my-last pattern-region))
                  (nth-list-of-lists
                   0 template-segments)
                  :test #'equalp)
                 template-segments)))) 60)))
        (final-MPN
         (if next-state-context-pair
           (third
            (first
             (fourth
              (second next-state-context-pair))))
           (if pattern-region
             (third
              (first
               (second
                (nth
                 (position
                  (first (my-last pattern-region))
                  (nth-list-of-lists
                   0 template-segments)
                  :test #'equalp)
                 template-segments)))) 60)))
        (i 1) (index-failures (list 0))
        (states<-
         (list
          (if next-state-context-pair
            next-state-context-pair
            (choose-one-with-beat
             (+ (mod
                 (ceiling
                  (first (my-last pattern-region)))
                 beats-in-bar) 1)
             final-states))))
        (datapoints
         ; 25/10/10 This could go wrong!
         (translate-datapoints-to-last-ontime
          (if next-state-context-pair
            (first
             (my-last
              (sort-dataset-asc
               (fourth
                (second
                 next-state-context-pair)))))
            (second generation-interval))
          0
          (sort-dataset-asc
           (states2datapoints-by-lookup<-
            (if next-state-context-pair
              states<-
              (append
               (list
                (list
                 (first (first states<-))
                 (list
                  nil nil
                  (third (second (first states<-)))
                  (fourth
                   (second (first states<-))))))
               (rest states<-)))
            beats-in-bar final-MNN final-MPN))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states<-)) stm<-
            :test #'equalp))))
        (checklistedp
         (checklist<-p
          states<- datapoints template-segments
          template-likelihood-profile checklist
          c-sources c-bar c-min c-max c-beats c-prob))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    #| 12/10/2010 For testing purposes.
    (write-to-file-append
     (list datapoints)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/datapoints.txt"))
    (write-to-file-append
     (list states<-)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/states<-.txt"))
    (write-to-file-append
     (list next-state)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/next-state.txt"))
    (write-to-file-append
     (list index-failures)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/index-failures.txt"))
    |#
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states<- datapoints i
         index-failures)
        (generate-beat-spacing-forcing<-
         final-states stm<- no-ontimes<
         dataset-template generation-interval
         pattern-region next-state-context-pair
         checklist beats-in-bar c-failures c-sources
         c-bar c-min c-max c-beats c-prob
         template-segments template-likelihood-profile
         final-MNN final-MPN (+ i 1)
         (append
          (subseq index-failures 0 (- failurep 1))
          (list
           (+ (nth (- failurep 1) index-failures) 1)))
         ; 12/10/2010 Special case avoids null states
         (if (equalp failurep 1)
           (list
            (if next-state-context-pair
              next-state-context-pair
              (choose-one-with-beat
               (+ (mod
                   (ceiling
                    (first (my-last pattern-region)))
                   beats-in-bar) 1)
               final-states)))
           (subseq states<- 0 (- failurep 1)))))
      (if checklistedp
        (if (<=
             (first (first datapoints)) no-ontimes<)
          (list states<- datapoints i index-failures)
          (if next-state
            (generate-beat-spacing-forcing<-
             final-states stm<- no-ontimes<
             dataset-template generation-interval
             pattern-region next-state-context-pair
             checklist beats-in-bar c-failures
             c-sources c-bar c-min c-max c-beats
             c-prob template-segments
             template-likelihood-profile final-MNN
             final-MPN (+ i 1)
             (if (< (length states<-)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states<- (list next-state)))
            (generate-beat-spacing-forcing<-
             final-states stm<- no-ontimes<
             dataset-template generation-interval
             pattern-region next-state-context-pair
             checklist beats-in-bar c-failures
             c-sources c-bar c-min c-max c-beats
             c-prob template-segments
             template-likelihood-profile final-MNN
             final-MPN (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states<- datapoints)))
        (generate-beat-spacing-forcing<-
         final-states stm<- no-ontimes<
         dataset-template generation-interval
         pattern-region next-state-context-pair
         checklist beats-in-bar c-failures c-sources
         c-bar c-min c-max c-beats c-prob
         template-segments template-likelihood-profile
         final-MNN final-MPN
         i ;Only instance not incremented
         (append
          (butlast index-failures)
          (list (+ (my-last index-failures) 1)))
         (if (equalp (length index-failures) 1)
           (list
            (if next-state-context-pair
              next-state-context-pair
              (choose-one-with-beat
               (+ (mod
                   (ceiling
                    (first (my-last pattern-region)))
                   beats-in-bar) 1)
               final-states)))
           (butlast states<-)))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 ontime-state-points-pairs
 '((27
    ((27 42 49 1 1 28 0) (27 70 65 1/2 0 55/2 1)))
   (55/2 ((27 42 49 1 1 28 0)))
   (111/4
    ((27 42 49 1 1 28 0) (111/4 67 64 1/4 0 28 2)
     (111/4 79 71 1/4 0 28 3)))
   (28
    ((28 54 56 1 1 29 4) (28 58 58 1 1 29 5)
     (28 64 62 1 1 29 6) (28 66 63 2 0 30 7)
     (28 78 70 2 0 30 8)))
   (29
    ((29 54 56 1 1 30 9) (29 58 58 1 1 30 10)
     (29 64 62 1 1 30 11) (28 66 63 2 0 30 7)
     (28 78 70 2 0 30 8)))
   (30
    ((30 47 52 1 1 31 12) (30 74 68 1/3 0 91/3 13)))
   (91/3
    ((30 47 52 1 1 31 12) (91/3 76 69 1/3 0 92/3 14)))
   (92/3
    ((30 47 52 1 1 31 12) (92/3 74 68 1/3 0 31 15)))
   (31
    ((31 54 56 1 1 32 16) (31 62 61 1 1 32 17)
     (31 73 67 1 0 32 18)))
   (32
    ((32 54 56 1 1 33 19) (32 62 61 1 1 33 20)
     (32 71 66 1 0 33 21)))
   (33
    ((33 42 49 1 1 34 22) (33 69 65 1/2 0 67/2 23)))
   (67/2 ((33 42 49 1 1 34 22)))
   (135/4
    ((33 42 49 1 1 34 22) (135/4 67 64 1/4 0 34 24)))
   (34
    ((34 54 56 1 1 35 25) (34 57 58 1 1 35 26)
     (34 61 60 1 1 35 27))) (35 NIL)))
(setq
 dataset
 '((27 42 49 1 1) (27 70 65 1/2 0) (111/4 67 64 1/4 0)
   (111/4 79 71 1/4 0) (28 54 56 1 1) (28 58 58 1 1)
   (28 64 62 1 1) (28 66 63 2 0) (28 78 70 2 0)
   (29 54 56 1 1) (29 58 58 1 1) (29 64 62 1 1)
   (30 47 52 1 1) (30 74 68 1/3 0) (91/3 76 69 1/3 0)
   (92/3 74 68 1/3 0) (31 54 56 1 1) (31 62 61 1 1)
   (31 73 67 1 0) (32 54 56 1 1) (32 62 61 1 1)
   (32 71 66 1 0) (33 42 49 1 1) (33 69 65 1/2 0)
   (135/4 67 64 1/4 0) (34 54 56 1 1) (34 57 58 1 1)
   (34 61 60 1 1)))
(geom-mean-likelihood-of-states<-
 ontime-state-points-pairs dataset 3)
--> ((27 0.07142857) (55/2 1/14) (111/4 0.062499996)
     (28 0.10958345) (29 0.09672784) (30 0.11785113)
     (91/3 0.083333336) (92/3 0.11785113)
     (31 0.1514267) (32 0.13999122) (33 0.16666667)
     (67/2 1/6) (135/4 0.16666667) (34 0.3333333)).
\end{verbatim}

\noindent Applies the function
\nameref{fun:geom-mean-likelihood-of-subset<-}
recursively to the first argument. |#

(defun geom-mean-likelihood-of-states<-
       (ontime-state-points-pairs dataset c-beats
        &optional
        (dataset-palette
         (orthogonal-projection-not-unique-equalp
          dataset (list 0 1)))
        (ontimes-list
         (nth-list-of-lists 0 dataset))
        (subset
         (second
          (first ontime-state-points-pairs)))
        (subset-palette
         (orthogonal-projection-not-unique-equalp
          subset (list 0 1)))
        (subset-ontimes
         (nth-list-of-lists 0 subset))
        (first-subset-ontime
         (if subset-ontimes
           (min-item subset-ontimes)))
        (last-subset-ontime
         (if subset-ontimes
           (max-item subset-ontimes))))
  (if (null ontime-state-points-pairs) ()
    (if subset
      (cons
       (list
        (first
         (first ontime-state-points-pairs))
        (geom-mean-likelihood-of-subset<-
         subset subset-palette first-subset-ontime
         last-subset-ontime dataset-palette
         ontimes-list c-beats))
       (geom-mean-likelihood-of-states<-
        (rest ontime-state-points-pairs) dataset
        c-beats dataset-palette ontimes-list))
      (geom-mean-likelihood-of-states<-
        (rest ontime-state-points-pairs) dataset
        c-beats dataset-palette ontimes-list))))

#|
\noindent Example:
\begin{verbatim}
(setq
 subset
 '((27 42 49 1 1 28 0) (111/4 67 64 1/4 0 28 2)
   (111/4 79 71 1/4 0 28 3)))
(setq
 subset-palette
 (orthogonal-projection-not-unique-equalp
  subset '(0 1)))
(setq first-subset-ontime 27)
(setq last-subset-ontime 111/4)
(setq
 dataset
 '((27 42 49 1 1) (27 70 65 1/2 0) (111/4 67 64 1/4 0)
   (111/4 79 71 1/4 0) (28 54 56 1 1) (28 58 58 1 1)
   (28 64 62 1 1) (28 66 63 2 0) (28 78 70 2 0)
   (29 54 56 1 1) (29 58 58 1 1) (29 64 62 1 1)
   (30 47 52 1 1) (30 74 68 1/3 0) (91/3 76 69 1/3 0)
   (92/3 74 68 1/3 0) (31 54 56 1 1) (31 62 61 1 1)
   (31 73 67 1 0) (32 54 56 1 1) (32 62 61 1 1)
   (32 71 66 1 0) (33 42 49 1 1) (33 69 65 1/2 0)
   (135/4 67 64 1/4 0) (34 54 56 1 1) (34 57 58 1 1)
   (34 61 60 1 1)))
(setq
 dataset-palette
 (orthogonal-projection-not-unique-equalp
  dataset '(0 1)))
(setq
 ontimes-list (nth-list-of-lists 0 dataset))
(setq c-beats 4)
(geom-mean-likelihood-of-subset<-
 subset subset-palette first-subset-ontime
 last-subset-ontime dataset-palette ontimes-list
 c-beats)
--> 0.052631576.
\end{verbatim}

\noindent This function is similar to the function
geom-mean-likelihood-of-subset, the difference being
we look forward to form the empirical distribution,
rather than backward. The first argument to
this function, called subset, is a point set. Both in
the scenario of likelihood calculation for an original
excerpt and for a generated passage, the point set is
a segment of the music. The argument subset-palette
consists of a (listed) list of MIDI note numbers from
the subset. BE CAREFUL: first-subset-ontime is not
necessarily the ontime of the first datapoint, as they
will have been sorted by MIDI note number. The
variable dataset-palette is analogous, ontimes-list is
a list of ontimes from the dataset. The threshold
c-beats determines how far forward we look to form the
empirical distribution. The output of this function is
the geometric mean of the likelihood of the subset
(that is, a product of the individual empirical
probabilities of the constituent MIDI note numbers. |#

(defun geom-mean-likelihood-of-subset<-
       (subset subset-palette first-subset-ontime
        last-subset-ontime dataset-palette
        ontimes-list c-beats &optional
        (empirical-massed
         (empirical-mass
          (subseq
           dataset-palette
           (index-1st-sublist-item>=
            first-subset-ontime ontimes-list)
           (index-1st-sublist-item>
            (+ last-subset-ontime c-beats)
            ontimes-list)))))
  (if subset
    (expt
     (likelihood-of-subset
      subset-palette empirical-massed)
     (/ 1 (length subset-palette)))))

#|
\noindent Example:
\begin{verbatim}
(translate-datapoints-to-last-offtime
 30
 '((0 44 51 1 1) (0 48 53 1 1) (0 56 58 1 0)
   (2 44 51 1 1) (3 48 53 5 1) (6 56 58 1 0)))
--> ((22 44 51 1 1) (22 48 53 1 1) (22 56 58 1 0)
     (24 44 51 1 1) (25 48 53 5 1) (28 56 58 1 0))
\end{verbatim}

\noindent This function takes two arguments: a time
$t$ and a list of datapoints. It calculates the
maximum offtime of the datapoints, and translates the
datapoints, such that the maximum offtime is now
$t$. |#

(defun translate-datapoints-to-last-offtime
       (offtime datapoints &optional
        (offtimes
         (mapcar
          #'(lambda (x)
              (+ (first x) (fourth x)))
          datapoints))
        (translation-vector
         (cons
          (- offtime (max-item offtimes))
          (constant-vector
           0 (- (length (first datapoints)) 1)))))
  (translation datapoints translation-vector))

#|
\noindent Example:
\begin{verbatim}
(translate-datapoints-to-last-ontime
 34 0
 '((0 44 51 1 1) (0 48 53 1 1) (0 56 58 1 0)
   (2 44 51 1 1) (3 48 53 1 1) (6 56 58 1 0)))
--> ((28 44 51 1 1) (28 48 53 1 1) (28 56 58 1 0)
     (30 44 51 1 1) (31 48 53 1 1) (34 56 58 1 0)).
\end{verbatim}

This function takes three arguments: an ontime, an
ontime index and a list of datapoints (assumed to
be sorted in lexicographical order). It translates
these datapoints such that the ontime of the last
datapoint equals the first argument. |#

(defun translate-datapoints-to-last-ontime
       (ontime ontime-index datapoints &optional
        (translation-vector
         (append
          (constant-vector 0 ontime-index)
          (list
           (-
            ontime
            (nth
             ontime-index (my-last datapoints))))
          (constant-vector
           0
           (-
            (-
             (length
              (first datapoints))
             ontime-index) 1)))))
  (translation datapoints translation-vector))
