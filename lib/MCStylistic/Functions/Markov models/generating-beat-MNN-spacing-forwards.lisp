#| Copyright 2008-2013 Tom Collins
   Tuesday 28 September 2010
   Incomplete

\noindent The main function here is called generate-
beat-MNN-spacing->. Given initial states, a state-
transition matrix, an upper limit for ontime, and
a template dataset, it generates datapoints (among
other output) that conform to various criteria, which
can be specified using the optional arguments. The
criteria are things like: not too many consecutive
states from the same source, the range is comparable
with that of the template, and the likelihood of the
states is comparable with that of the template.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "director-musices"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
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
   :directory '(:relative "Markov models")
   :name "markov-compose"
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
   :name "realising-states"
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
   :directory '(:relative "Markov models")
   :name "spacing-states"
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
 states
 '(((1 (60 72 79 84 88) (60 67 71 74 76))
      (NIL NIL "C-63-1"
           ((0 35 45 1/2 1 1/2 0)
            (0 47 52 1/2 1 1/2 1)
            (0 54 56 1/2 0 1/2 2)
            (0 59 59 1/2 0 1/2 3)
            (0 63 61 1/2 0 1/2 4))))
   ((3/2 NIL NIL)
      (NIL NIL "C-63-1" NIL))
   ((7/4 (54) (57))
      (-6 -3 "C-63-1"
          ((831/4 56 57 1/4 0 208 701))))
   ((2 (28 40 54 60 63) (42 49 57 60 62))
      (-26 -15 "C-63-1"
           ((208 30 42 1 1 209 702)
            (208 42 49 1 1 209 703)
            (208 56 57 1 0 209 704)
            (208 62 60 1 0 209 705)
            (208 65 62 1 0 209 706))))))
(setq datapoints nil)
(setq checklist '("originalp"))
(setq c-sources 3)
(setq c-bar 12)
(setq c-min 7)
(setq c-max 7)
(setq c-beats 12)
(setq c-prob 0.1)
(checklistp
 states datapoints dataset-template
 template-segments checklist c-sources c-bar c-min
 c-max c-beats c-prob)
--> NIL.
\end{verbatim}

\noindent A simple version of this function. Just
checks whether the sources are all the same. |#

(defun checklistp
       (states datapoints template-segments
        template-likelihood-profile checklist
        c-sources c-bar c-min c-max c-beats c-prob
        &optional
        (lastn-sources
         (mapcar
          #'(lambda (x)
              (third (second x)))
          (lastn c-sources states)))
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
        (last-segment
         (my-last (butlast datapoints-segments)))
        (mean&rangedp
         (if (find
              "mean&rangep" checklist :test #'string=)
           (mean&rangep
            last-segment template-segments c-bar c-min
            c-max) t))
        (likelihoodp
         (if (find
              "likelihoodp" checklist :test #'string=)
           (and
            (pitch&octave-spellingp
             (second last-segment))
            (comparable-likelihood-profilep
             last-segment datapoints c-beats
             template-likelihood-profile c-prob)) t)))
  (and originaledp mean&rangedp likelihoodp))

#|
\noindent Example:
\begin{verbatim}
(setq
 mini-initial-states
 '(((3 NIL)
    (NIL NIL "C-6-1" ((-1 66 63 4/3 0 1/3 0))))
   ((1 (7 9 8))
    (NIL NIL "C-6-2"
     ((0 44 50 1 1 1 0) (0 51 54 1 1 1 1)
      (0 60 59 1 1 1 2) (0 68 64 1 0 1 3))))
   ((1 (7))
    (NIL NIL "C-6-3"
     ((0 52 55 1 1 1 0) (0 59 59 1 1 1 1))))
   ((3 NIL)
    (NIL NIL "C-6-4" ((-1 70 66 3/2 0 1/2 0))))
   ((1 (24))
    (NIL NIL "C-7-1"
     ((0 41 49 1 1 1 0) (0 65 63 1/2 0 1/2 1))))))
(choose-one-with-beat 1 mini-initial-states)
--> ((1 (7))
     (NIL NIL "C-6-3"
      ((0 52 55 1 1 1 0) (0 59 59 1 1 1 1)))).
\end{verbatim}

\noindent This function takes a beat of a bar and a
list of initial states as its arguments. These states
may be genuinely initial, or constructed from
appropriate beginnings. A search of the states is
made beginning at a random index, and the first state
whose beat matches that of the first argument is
returned. In the event that all the states are
searched, the output reverts to the original random
index, so something is always output. |#

(defun choose-one-with-beat
       (beat initial-states &optional
        (l (length initial-states))
        (i (random l *rs*)) (j i) (initialp t)
        (state&context (nth i initial-states)))
  (if (and (equalp i j) (not initialp))
    state&context
    (if (equalp (first (first state&context)) beat)
      state&context
      (choose-one-with-beat
       beat initial-states l
       (if (>= i l) 0 (+ i 1)) j nil))))


#| \noindent Example:
\begin{verbatim}
(setq
 ontime-state-points-pair
 '(7/2
   ((3 45 51 1 0) (7/2 71 66 1/2 0))))
(setq
 datapoints
 '((0 52 55 1 1) (0 59 59 1 1) (0 64 62 1 0)
   (0 68 64 1 0) (1 52 55 2 1) (1 59 59 1 1)
   (1 64 62 1 0) (1 68 64 1/2 0) (3/2 69 65 1/2 0)
   (2 62 61 1 0) (2 71 66 1/2 0) (5/2 66 63 1/2 0)
   (3 45 51 1 0) (3 64 62 1/2 0) (7/2 71 66 1/2 0)
   (4 52 55 2 1) (4 57 58 2 1) (4 61 60 2 1)
   (4 69 65 1/2 0) (9/2 73 67 1/2 0) (5 76 69 1 0)
   (6 38 47 1 1) (6 71 66 1/2 0) (13/2 69 65 1/2 0)
   (7 50 54 1 1) (7 54 56 1 1) (7 57 58 2 1)
   (7 66 63 1/2 0) (15/2 67 64 1/2 0) (8 49 53 1 1)
   (8 52 55 1 1) (8 69 65 1 0) (9 33 44 1 1)
   (9 64 62 1/2 0) (19/2 61 60 1/2 0) (10 45 51 1 1)
   (10 52 55 1 1) (10 57 58 1 0) (11 45 51 1 1)
   (11 52 55 1 1) (11 64 62 2 0) (12 45 51 1 1)
   (12 57 58 1 1) (12 60 60 1 0)))
(setq c-beats 12)
(setq
 template-likelihood-profile
 '((0 0.19999999) (1 0.19999999) (2 0.16054831)
   (11/4 0.1875) (3 0.10939984) (4 0.07914095)
   (19/4 0.09988681) (5 0.08333333) (6 0.03448276)))
(setq c-prob 0.1)
(comparable-likelihood-profilep
 ontime-state-points-pair datapoints c-beats
 template-likelihood-profile c-prob)
--> T.
\end{verbatim}

\noindent This function takes a pair consisting of an
ontime and points that sound at the ontime. Based on
an empirical distribution over the argument named
datapoints (with a context governed by c-beats), it
calculates the geometric mean of the likelihood of
these points. This likelihood is then compared with
that at the same ontime in the template. If the
absolute difference between the likelihoods is less
than the threshold c-prob, T is returned, and NIL
otherwise. T will also be returned if there are no
points. |#

(defun comparable-likelihood-profilep
       (ontime-state-points-pair datapoints c-beats
        template-likelihood-profile c-prob &optional
        (subset (second ontime-state-points-pair))
        (subset-ontimes (nth-list-of-lists 0 subset))
        (state-likelihood
         (if subset
           (geom-mean-likelihood-of-subset
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
(setq
 mini-template-segments
 '((0
    ((0 52 55 1 1 1 0) (0 62 61 1 0 1 1)
     (0 64 62 1 0 1 2) (0 68 64 1 0 1 3)
     (0 71 66 1 0 1 4)))
   (1
    ((1 52 55 1 1 2 5) (1 62 61 1 0 2 6)
     (1 64 62 1 0 2 7) (1 68 64 1 0 2 8)
     (1 71 66 1 0 2 9)))
   (2
    ((2 52 55 1 1 3 10) (2 62 61 1 0 3 11)
     (2 64 62 1 0 3 12) (2 68 64 1 0 3 13)
     (2 72 67 3/4 0 11/4 14)))
   (11/4
    ((2 52 55 1 1 3 10) (2 62 61 1 0 3 11)
     (2 64 62 1 0 3 12) (2 68 64 1 0 3 13)
     (11/4 71 66 1/4 0 3 15)))
   (3
    ((3 45 51 3 1 6 16) (3 52 55 3 1 6 17)
     (3 60 60 3 0 6 18) (3 64 62 3 0 6 19)
     (3 71 66 1 0 4 20)))))
(full-segment-nearest<ontime 1 mini-template-segments)
--> (1
     ((1 52 55 1 1 2 5) (1 62 61 1 0 2 6)
      (1 64 62 1 0 2 7) (1 68 64 1 0 2 8)
      (1 71 66 1 0 2 9))).
\end{verbatim}

\noindent This function takes an ontime and a list of
segments as its arguments. It returns the full (that
is, non-null) segment whose ontime is closest to and
less than the first argument. There should always be
such a segment, but if there is not, NIL is
returned. |#

(defun full-segment-nearest<ontime
       (ontime template-segments &optional
        (ontimes
         (nth-list-of-lists 0 template-segments))
        (i (index-1st-sublist-item> ontime ontimes))
        (j
         (if i (- i 1)
           (- (length template-segments) 1))))
  (if (< j 0) ()
    (if (second (nth j template-segments))
      (nth j template-segments)
      (full-segment-nearest<ontime
       ontime template-segments ontimes i (- j 1)))))


#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   initial-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-initial-states.txt")))
  (setq
   stm
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix.txt")))
  (setq no-ontimes> 6)
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-41-2-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 0 184))
  "Yes!")

(generate-beat-MNN-spacing->
 initial-states stm no-ontimes>
 dataset-template)
--> '.

\end{verbatim}

\noindent Given initial states, a state-transition
matrix, an upper limit for ontime, and a template
dataset, this function generates datapoints (among
other output) that conform to various criteria, which
can be specified using the optional arguments. The
criteria are things like: not too many consecutive
states from the same source (c-sources), the range is
comparable with that of the template (c-bar, c-min,
and c-max), and the likelihood of the states is
comparable with that of the template (c-beats and
c-prob). A record, named index-failures, is kept of
how many times a generated state fails to meet the
criteria. When a threshold c-failures is exceeded at
any state index, the penultimate state is removed and
generation continues. If the value of c-failures is
exceeded for the first state, a message is
returned.

This function returns the states as well as the
datapoints, and also index-failures and i, the total
number of iterations. |#

(defun generate-beat-MNN-spacing->
       (#| Basic ingredients for generating from a
        Markov model: intial states and state
        transition matrix. |#
        initial-states stm
        #| If no-ontimes> = 24, function will
        generate material from 0 to 24 then spit it
        out. |#
        no-ontimes>
        #| This is a point-set representation of some
        piece that is used for quite abstract
        information, like the MIDI note number of the
        lowest opening note, etc. |#
        dataset-template &optional
        #| Checklist is a list of strings. The strings
        specify what aspects of the output material to
        check as it's generated. "originalp" means that
        a later function will check that no more than
        three consecutive states come from the same
        piece, say. "mean&rangep" means that a later
        function will check that the range of the
        output material falls roughly within that of
        the template piece. This is to prevent the
        output wandering off high or low in the piano's
        range (but it can still happen if the
        parameters are too lax). I don't think this is
        going to be such an issue for Racchman-Jun2014.
        "likelihoodp" compares the empirical likelihood
        of events in the generated output with events
        in the template piece. This is to try to
        identify when really crazy chords appear
        (because output likelihood and template
        likelihood will be really different here), and
        go back and replace them. Again, I don't think
        this is going to be such an issue for
        Racchman-Jun2014. These checklist tests are
        performed after each new state is generated. |#
        (checklist (list "originalp"))
        (beats-in-bar 4) ; Number of beats in bar.
        #| How many times the algorithm tries to
        replace a failing state at step n before
        going back and revising the choice for step
        n - 1. A failing state means that the checklist
        tests fail for the generated output up to and
        including this state. |#
        (c-failures 10)
        #| Number of consecutive sources permitted to
        come from the same database piece. |#
        (c-sources 3)
        #| Parameters for controlling range of
        generated output. c-bar controls mean MIDI
        note number (MNN) of generated output: must be
        within c-bar of the mean MNN of the template
        piece. c-min and c-max control the lowest- and
        highest-sounding MNNs in the same fashion. |#
        (c-bar 12) (c-min 7) (c-max 7)
        #| Parameters for controlling likelihood of
        generated output. Likelihood is calculated by
        forming a zero-order distribution of MNNs over
        some time window [t0, t1], where t1 is the
        current point in time, and t0 is some amount
        less. c-beats is this "some amount less". That
        is, the time window in this case is 12 beats
        long. The likelihood of the generated output
        and template piece are compared. If they are
        within c-prob of each other, the likehood test
        is passed. |#
        (c-beats 12)
        (c-prob 0.1)
        #| Segmentation of the template piece, useful
        for other functions below. |#
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        #| Calculate this template likelihood profile
        once at the start, so that it is ready for
        comparison at each step. |#
        (template-likelihood-profile
         (geom-mean-likelihood-of-states
          template-segments dataset-template c-beats))
        #| Grab the initial MNN from the template
        piece. |#
        (initial-MNN
         (if template-segments
           (second
            (first
             (second (first template-segments))))
           60))
        #| Grab the initial morphetic pitch number from
        the template piece. |#
        (initial-MPN
         (if template-segments
           (third
            (first
             (second (first template-segments))))
           60))
        #| Index to record the total number of times a
        selection has been made from the transition
        matrix. |#
        (i 1)
        #| When the algorithm exceeds no-ontimes> and
        spits out its generated material, this
        variable will be a list of numbers. The nth
        number will tell us, for the finally successful
        generated material how many times there was a
        checklist failure (and reselection) at state
        n. |#
        (index-failures (list 0))
        #| This variable contains the currently
        selected beat-MNN-spacing states. To begin
        with, a state is selected from the initial
        distribution. If the template provided is non-
        empty, the initial state will have the same
        starting beat as the first note(s) of the
        template piece. |#
        (states
         (list
          (if template-segments
            (choose-one-with-beat
             (+ (first (first template-segments)) 1)
             initial-states)
            (choose-one initial-states))))
        #| As suggested in the AIEDAM paper, there is
        a subtle but important difference between beat-
        MNN-spacing states, and actual notes with
        ontimes, pitches, and durations. The variable
        datapoints contains this information, and its
        calculated by passing the variable states to
        the function states2datapoints-by-lookup. We
        could look at this function in more detail if
        you like, but I suggest we leave it at that for
        now. |#
        (datapoints
         (translate-datapoints-to-first-ontime
          (first (first template-segments)) 0
          (sort-dataset-asc
           (states2datapoints-by-lookup
            states beats-in-bar initial-MNN
            initial-MPN))))
        #| This is the choice for the next state. It
        will get appended to the current states
        variable if there are no problems with the
        current states variable, according to the tests
        performed by the function checklistp. |#
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states)) stm
            :test #'equalp))))
        #| Send the current generated output to the
        function checklistp, which will run all the
        checks mentioned above about not too many
        consecutive states from the same piece
        (source), sensible range, sensible
        likelihood. |#
        (checklistedp
         (checklistp
          states datapoints template-segments
          template-likelihood-profile checklist
          c-sources c-bar c-min c-max c-beats c-prob))
        #| As mentioned above, index-failures lists the
        number of times that checks have failed when
        trying to generate the nth state. The parameter
        c-failures specifies the maximum number of
        times the algorithm is permitted to fail checks
        at any state. This function
        index-1st-sublist-item>= checks if c-failures
        has been exceeded by any member of
        index-failures. If so it will return that
        index; else it will be nil. |#
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures))
        ; End of arguments!
        )
  (progn
    #| 29/9/2010 For testing purposes.
    (write-to-file-append
     (list datapoints)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/datapoints.txt"))
    (write-to-file-append
     (list states)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/states.txt"))
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
    (if failurep ; Has c-failures been exceeded?
      (if (zerop failurep) ; Exceeded in state 1?
        #| If so, the whole generation process is said
        to have failed. Of course, if we want it to
        keep going, we can just call it again with a
        new seed. |#
        (list
         "Failure!" states datapoints i index-failures)
        #| In this situation, c-failures has been
        exceeded at state n > 1. Let's say c-failures
        = 3 and index-failures = (0 2 1 3). What
        happens below is that index-failures gets set
        to (0 2 2), and states gets reset from having
        n = 4 elements to just the first n - 2 = 2
        elements. (If states only had n = 2 elements,
        then it is reset to a new initial state.
        Subsequent arguments (like datapoints, next-
        state, etc.) will be set appropriately
        because of the use of &optional. |#
        (generate-beat-MNN-spacing->
         initial-states stm no-ontimes>
         dataset-template checklist beats-in-bar
         c-failures c-sources c-bar c-min c-max
         c-beats c-prob template-segments
         template-likelihood-profile initial-MNN
         initial-MPN (+ i 1)
         ; Change to index-failures here.
         (append
          (subseq index-failures 0 (- failurep 1))
          (list
           (+ (nth (- failurep 1) index-failures) 1)))
         ; 29/9/2010 Special case to avoid null states
         (if (equalp failurep 1)
           (list
            (if template-segments
              (choose-one-with-beat
               (+ (first (first template-segments)) 1)
               initial-states)
              (choose-one initial-states)))
           (subseq states 0 (- failurep 1)))))
      #| Here failurep has not occurred, but the checks
      might have failed. |#
      (if checklistedp ; Have checks passed?
        (if (>= ; Yes. Have we got enough material?
             (first (my-last datapoints)) no-ontimes>)
          ; Yes! Then spit it out and we're done.
          (list states datapoints i index-failures)
          #| Checks passed but we do not have enough
          material yet. |#
          (if next-state ; Is next-state non-empty?
            #| The variable next-state is non-empty.
            It's going to be appended to the first
            n states to give a states variable that
            now has n + 1 states in it, and
            index-failures is going to have a zero
            appended to the end. |#
            (generate-beat-MNN-spacing->
             initial-states stm no-ontimes>
             dataset-template checklist beats-in-bar
             c-failures c-sources c-bar c-min c-max
             c-beats c-prob template-segments
             template-likelihood-profile initial-MNN
             initial-MPN (+ i 1)
             ; index-failures updated here.
             (if (< (length states)
                    (length index-failures))
               #| Don't recall how we'd get to this,
               but maybe it's to do with failures
               in the first or second states, in which
               case states might be shorter than
               index-failures. |#
               (identity index-failures)
               ; This is the usual case.
               ; states updated here.
               (append index-failures (list 0)))
             (append states (list next-state))
             #| Subsequent arguments (like
             datapoints, next-state, etc.) will be
             set appropriately because of the use of
             &optional. |#
             )
            #| The variable next-state is empty. This
            is a rare eventuality, where the current
            state x ended one of the pieces in the
            database, so one of its continuations in
            the state transition matrix is empty. In
            this case we increase the last element
            of index-failures by one, leave the
            states and datapoints unchanged, and
            use the &optional arguments to select
            another (hopefully different) next
            state. |#
            (generate-beat-MNN-spacing->
             initial-states stm no-ontimes>
             dataset-template checklist beats-in-bar
             c-failures c-sources c-bar c-min c-max
             c-beats c-prob template-segments
             template-likelihood-profile initial-MNN
             initial-MPN (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states datapoints)))
        #| If we get here, checks did not pass on the
        current states variable. This means we never
        use the information in the next-state
        variable, so variable i below is not
        incremented. We increase the last element of
        index-failures by one, and remove the last
        element of states (the &optional arguments
        will then cause this to be replaced by a new
        alternative continuation). If this removing of
        the last element would empty the states
        variable, then a new initial state is
        selected. |#
        (generate-beat-MNN-spacing->
         initial-states stm no-ontimes>
         dataset-template checklist beats-in-bar
         c-failures c-sources c-bar c-min c-max
         c-beats c-prob template-segments
         template-likelihood-profile initial-MNN
         initial-MPN i ;Only instance not incremented
         (append
          (butlast index-failures)
          (list (+ (my-last index-failures) 1)))
         (if (equalp (length index-failures) 1)
           (list
            (if template-segments
              (choose-one-with-beat
               (+ (first (first template-segments)) 1)
               initial-states)
              (choose-one initial-states)))
           (butlast states)))))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   initial-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/initial-states.txt")))
  (setq
   stm
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/transition-matrix.txt")))
  (setq no-ontimes> 14)
  (setq
   dataset-all
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-41-2-ed.txt")))
  (setq
   dataset-template
   (subseq dataset-all 0 184))
  (setq
   *rs* #.(CCL::INITIALIZE-RANDOM-STATE 477 10894))
  (setq
   datapoints-from-previous-interval
   '((17/2 63 62 1/2 0) (17/2 72 67 1/2 0)
     (9 34 45 1 1) (9 46 52 1 1) (9 65 63 1/2 0)
     (9 74 68 1/2 0) (39/4 65 63 1/4 0)))
  (setq
   previous-state-context-pair
   (my-last
    (beat-spacing-states
     datapoints-from-previous-interval
     "No information" 3 1 3)))
  (setq generation-interval '(13 15))
  (setq
   pattern-region
   '((27/2 66 63) (27/2 69 65) (14 55 57) (14 62 61)
     (14 67 64) (14 71 66) (29/2 69 65) (29/2 72 67)))
  "Data loaded and variables set.")
(generate-beat-spacing-forcing->
 initial-states stm no-ontimes> dataset-template
 generation-interval pattern-region
 previous-state-context-pair (list "originalp")
 3 10 4 19 12 12 12 .15)
--> ((((7/4 (12 19))
       (0 0 "No information"
        ((9 34 45 1 1 10 2) (9 46 52 1 1 10 3)
         (39/4 65 63 1/4 0 10 6))))
      ((2 (7 5 4 3 5))
       (12 7 "C-17-1"
        ((202 46 52 1 1 203 908)
         (202 53 56 1 1 203 909)
         (202 58 59 1 1 203 910)
         (202 62 61 1 0 203 911)
         (202 65 63 1 0 203 912)
         (202 70 66 1 0 203 913))))
      ((3 (7 5 4 3 9))
       (0 0 "C-17-1"
        ((203 46 52 1 1 204 914)
         (203 53 56 1 1 204 915)
         (203 58 59 1 1 204 916)
         (203 62 61 1 0 204 917)
         (203 65 63 1 0 204 918)
         (203 74 68 1 0 204 919))))
      ((15/4 (7 5 4 3 9))
       (0 0 "C-41-2"
        ((137 47 52 1 1 138 539)
         (137 54 56 1 1 138 540)
         (137 59 59 1 1 138 541)
         (137 63 61 1 0 138 542)
         (137 66 63 1 0 138 543)
         (551/4 75 68 1/4 0 138 544))))
      ((1 NIL)
       (28 16 "C-41-2" ((66 75 68 2 0 68 261))))
      ((3/2 NIL)
       (1 1 "C-30-1" ((61/2 75 69 1/2 0 31 110))))
      ((2 NIL)
       (-5 -3 "C-56-1"
        ((409 68 65 1/2 0 819/2 1334))))
      ((3 NIL)
       (0 0 "C-56-3" ((143 62 61 1 0 144 423)))))
     ((9 34 45 1 1) (9 46 52 2 1) (39/4 65 63 1/4 0)
      (10 53 56 1 1) (10 58 59 1 1) (10 62 61 1 0)
      (10 65 63 1 0) (10 70 66 1 0) (11 46 52 1 1)
      (11 53 56 1 1) (11 58 59 1 1) (11 62 61 1 0)
      (11 65 63 1 0) (11 74 68 1 0) (12 74 68 1/2 0)
      (25/2 75 69 1/2 0) (13 70 66 1 0)
      (14 70 66 1 0))
     20 (0 0 0 1 0 0 0 0))
\end{verbatim}

\noindent This function appears to be very simiar to
the function
\nameref{fun:generate-beat-MNN-spacing->}. The
difference is that there are some extra arguments
here, which allow for using either external or
internal initial/final states, and for using
information from a discovered pattern or previous/next
state to further guide the generation, hence
`forcing'. |#

(defun generate-beat-spacing-forcing->
       (initial-states stm no-ontimes>
        dataset-template generation-interval
        pattern-region previous-state-context-pair
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (template-likelihood-profile
         (geom-mean-likelihood-of-states
          template-segments dataset-template c-beats))
        (initial-MNN
         (if previous-state-context-pair
           (second
            (first
             (fourth
              (second previous-state-context-pair))))
           (if pattern-region
             (second (first pattern-region)) 60)))
        (initial-MPN
         (if previous-state-context-pair
           (third
            (first
             (fourth
              (second previous-state-context-pair))))
           (if pattern-region
             (third (first pattern-region)) 60)))
        (i 1) (index-failures (list 0))
        (states
         (list
          (if previous-state-context-pair
            previous-state-context-pair
            (choose-one-with-beat
             (+ (mod
                 (floor
                  (first (first pattern-region)))
                 beats-in-bar) 1)
             initial-states))))
        (datapoints
         (translate-datapoints-to-first-ontime
          (if previous-state-context-pair
            (first
             (first
              (sort-dataset-asc
               (fourth
                (second
                 previous-state-context-pair)))))
            (first generation-interval))
          0
          (sort-dataset-asc
           (states2datapoints-by-lookup
            (if previous-state-context-pair
              (append
               (butlast
                (beat-spacing-states
                 (fourth
                  (second
                   previous-state-context-pair))
                 "No information" 3 1 3))
               states)
              (append
               (list
                (list
                 (first (first states))
                 (list
                  nil nil
                  (third (second (first states)))
                  (fourth (second (first states))))))
               (rest states)))
            beats-in-bar initial-MNN initial-MPN))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states)) stm
            :test #'equalp))))
        (checklistedp
         (checklistp
          states datapoints template-segments
          template-likelihood-profile checklist
          c-sources c-bar c-min c-max c-beats c-prob))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    #| 29/9/2010 For testing purposes.
    (write-to-file-append
     (list datapoints)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/datapoints.txt"))
    (write-to-file-append
     (list states)
     (concatenate
      'string
      "/Applications/CCL/Lisp code/Markov models"
      "/Testing/states.txt"))
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
         "Failure!" states datapoints i
         index-failures)
        (generate-beat-spacing-forcing->
         initial-states stm no-ontimes>
         dataset-template generation-interval
         pattern-region previous-state-context-pair
         checklist beats-in-bar c-failures c-sources
         c-bar c-min c-max c-beats c-prob
         template-segments template-likelihood-profile
         initial-MNN initial-MPN (+ i 1)
         (append
          (subseq index-failures 0 (- failurep 1))
          (list
           (+ (nth (- failurep 1) index-failures) 1)))
         ; 29/9/2010 Special case to avoid null states
         (if (equalp failurep 1)
           (list
            (if previous-state-context-pair
              previous-state-context-pair
              (choose-one-with-beat
               (+ (mod
                   (floor
                    (first (first pattern-region)))
                   beats-in-bar) 1)
               initial-states)))
           (subseq states 0 (- failurep 1)))))
      (if checklistedp
        (if (>= (first (my-last datapoints))
                no-ontimes>)
          (list states datapoints i index-failures)
          (if next-state
            (generate-beat-spacing-forcing->
             initial-states stm no-ontimes>
             dataset-template generation-interval
             pattern-region
             previous-state-context-pair checklist
             beats-in-bar c-failures c-sources c-bar
             c-min c-max c-beats c-prob
             template-segments
             template-likelihood-profile initial-MNN
             initial-MPN (+ i 1)
             (if (< (length states)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states (list next-state)))
            (generate-beat-spacing-forcing->
             initial-states stm no-ontimes>
             dataset-template generation-interval
             pattern-region
             previous-state-context-pair checklist
             beats-in-bar c-failures c-sources c-bar
             c-min c-max c-beats c-prob
             template-segments
             template-likelihood-profile initial-MNN
             initial-MPN (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states datapoints)))
        (generate-beat-spacing-forcing->
         initial-states stm no-ontimes>
         dataset-template generation-interval
         pattern-region previous-state-context-pair
         checklist beats-in-bar c-failures c-sources
         c-bar c-min c-max c-beats c-prob
         template-segments template-likelihood-profile
         initial-MNN initial-MPN
         i ;Only instance not incremented
         (append
          (butlast index-failures)
          (list (+ (my-last index-failures) 1)))
         (if (equalp (length index-failures) 1)
           (list
            (if previous-state-context-pair
              previous-state-context-pair
              (choose-one-with-beat
               (+ (mod
                   (floor
                    (first (first pattern-region)))
                   beats-in-bar) 1)
               initial-states)))
           (butlast states)))))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   ontime-state-points-pairs
   '((0 ((0 38 47 1/2 1 1/2 0) (0 62 61 1/2 0 1/2 1)))
     (1/2 NIL)
     (1
      ((1 50 54 2 1 3 2) (1 57 58 1/2 1 3/2 3)
       (1 60 60 3 0 4 4) (1 66 63 1/2 0 3/2 5)))
     (3/2
      ((1 50 54 2 1 3 2) (3/2 55 57 1/2 1 2 6)
       (1 60 60 3 0 4 4) (3/2 64 62 1/2 0 2 7)))
     (2
      ((1 50 54 2 1 3 2) (2 54 56 1/2 1 5/2 8)
       (1 60 60 3 0 4 4) (2 62 61 1/2 0 5/2 9)))
     (5/2
      ((1 50 54 2 1 3 2) (5/2 57 58 1/2 1 3 10)
       (1 60 60 3 0 4 4) (5/2 66 63 1/2 0 3 11)))
     (3
      ((3 50 54 15/4 1 27/4 12) (1 60 60 3 0 4 4)
       (3 69 65 1/2 0 7/2 13)))
     (7/2
      ((3 50 54 15/4 1 27/4 12) (1 60 60 3 0 4 4)))
     (15/4
      ((3 50 54 15/4 1 27/4 12) (1 60 60 3 0 4 4)
       (15/4 71 66 1/4 0 4 14)))
     (4
      ((3 50 54 15/4 1 27/4 12) (4 60 60 2 0 6 15)
       (4 62 61 2 1 6 16) (4 66 63 2 0 6 17)
       (4 72 67 2 0 6 18)))
     (6
      ((6 43 50 2 1 8 19) (3 50 54 15/4 1 27/4 12)
       (6 62 61 1/2 0 13/2 20) (6 67 64 1/2 0 13/2 21)
       (6 71 66 1/2 0 13/2 22)))
     (13/2
      ((6 43 50 2 1 8 19) (3 50 54 15/4 1 27/4 12)))
     (27/4
      ((6 43 50 2 1 8 19) (27/4 50 54 1/4 1 7 23)
       (27/4 60 60 1/4 0 7 24)
       (27/4 69 65 1/4 0 7 25)))
     (7
      ((6 43 50 2 1 8 19) (7 55 57 2 1 9 26)
       (7 59 59 1 0 8 27) (7 67 64 1 0 8 28)))
     (8
      ((8 43 50 2 1 10 29) (7 55 57 2 1 9 26)
       (8 59 59 1 0 9 30) (8 62 61 1 0 9 31)
       (8 71 66 1 0 9 32)))
     (9
      ((8 43 50 2 1 10 29) (9 49 53 1 1 10 33)
       (9 58 58 1 1 10 34) (9 64 62 1 0 10 35)
       (9 67 64 1 0 10 36) (9 76 69 1 0 10 37)))
     (10 NIL)))
  (setq
   dataset
   '((0 38 47 1/2 1) (0 62 61 1/2 0) (1 50 54 2 1)
     (1 57 58 1/2 1) (1 60 60 3 0) (1 66 63 1/2 0)
     (3/2 55 57 1/2 1) (3/2 64 62 1/2 0)
     (2 54 56 1/2 1) (2 62 61 1/2 0) (5/2 57 58 1/2 1)
     (5/2 66 63 1/2 0) (3 50 54 15/4 1)
     (3 69 65 1/2 0) (15/4 71 66 1/4 0) (4 60 60 2 0)
     (4 62 61 2 1) (4 66 63 2 0) (4 72 67 2 0)
     (6 43 50 2 1) (6 62 61 1/2 0) (6 67 64 1/2 0)
     (6 71 66 1/2 0) (27/4 50 54 1/4 1)
     (27/4 60 60 1/4 0) (27/4 69 65 1/4 0)
     (7 55 57 2 1) (7 59 59 1 0) (7 67 64 1 0)
     (8 43 50 2 1) (8 59 59 1 0) (8 62 61 1 0)
     (8 71 66 1 0) (9 49 53 1 1) (9 58 58 1 1)
     (9 64 62 1 0) (9 67 64 1 0) (9 76 69 1 0)))
  (setq c-beats 4)
  "Variables set.")
(geom-mean-likelihood-of-states
 ontime-state-points-pairs dataset c-beats)
--> ((0 0.5) (1 0.16666667) (3/2 0.125) (2 0.11892071)
     (5/2 0.11785113) (3 0.089994356)
     (7/2 0.101015255) (15/4 0.08399473)
     (4 0.10777223) (6 0.075700045) (13/2 0.061487548)
     (27/4 0.09343293) (7 0.05662891) (8 0.09750821)
     (9 0.058608957))
\end{verbatim}

\noindent This function applies the function
geom-mean-likelihood-of-subset recursively, having
extracted a subset from each ontime-state-points
pair. |#

(defun geom-mean-likelihood-of-states
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
        (geom-mean-likelihood-of-subset
         subset subset-palette first-subset-ontime
         last-subset-ontime dataset-palette
         ontimes-list c-beats))
       (geom-mean-likelihood-of-states
        (rest ontime-state-points-pairs) dataset
        c-beats dataset-palette ontimes-list))
      (geom-mean-likelihood-of-states
        (rest ontime-state-points-pairs) dataset
        c-beats dataset-palette ontimes-list))))

#|
\noindent Example:
\begin{verbatim}
(setq
 subset
 '((8 43 50 2 1 10 29) (7 55 57 2 1 9 26)
   (8 59 59 1 0 9 30) (8 62 61 1 0 9 31)
   (8 71 66 1 0 9 32)))
(setq
 subset-palette
 (orthogonal-projection-not-unique-equalp
  subset '(0 1)))
(setq first-subset-ontime 7)
(setq
 dataset
 '((0 38 47 1/2 1) (0 62 61 1/2 0) (1 50 54 2 1)
   (1 57 58 1/2 1) (1 60 60 3 0) (1 66 63 1/2 0)
   (3/2 55 57 1/2 1) (3/2 64 62 1/2 0) (2 54 56 1/2 1)
   (2 62 61 1/2 0) (5/2 57 58 1/2 1) (5/2 66 63 1/2 0)
   (3 50 54 15/4 1) (3 69 65 1/2 0) (15/4 71 66 1/4 0)
   (4 60 60 2 0) (4 62 61 2 1) (4 66 63 2 0)
   (4 72 67 2 0) (6 43 50 2 1) (6 62 61 1/2 0)
   (6 67 64 1/2 0) (6 71 66 1/2 0) (27/4 50 54 1/4 1)
   (27/4 60 60 1/4 0) (27/4 69 65 1/4 0) (7 55 57 2 1)
   (7 59 59 1 0) (7 67 64 1 0) (8 43 50 2 1)
   (8 59 59 1 0) (8 62 61 1 0) (8 71 66 1 0)
   (9 49 53 1 1) (9 58 58 1 1) (9 64 62 1 0)
   (9 67 64 1 0) (9 76 69 1 0)))
(setq
 dataset-palette
 (orthogonal-projection-not-unique-equalp
  dataset '(0 1)))
(setq
 ontimes-list (nth-list-of-lists 0 dataset))
(setq c-beats 4)
(geom-mean-likelihood-of-subset
 subset subset-palette first-subset-ontime dataset
 dataset-palette ontimes-list c-beats)
--> 0.09750821.
\end{verbatim}

\noindent The first argument to this function, called
subset, is a point set. Both in the scenario of
likelihood calculation for an original excerpt and
for a generated passage, the point set is a segment
of the music. The argument subset-palette consists of
a (listed) list of MIDI note numbers from the subset.
BE CAREFUL: first-subset-ontime is not necessarily
the ontime of the first datapoint, as they will have
been sorted by MIDI note number. The variables
dataset and dataset-palette are analogous, ontimes-
list is a list of ontimes from the dataset. The
threshold c-beats determines how far back we look to
form the empirical distribution. The output of this
function is the geometric mean of the likelihood of
the subset (that is, a product of the individual
empirical probabilities of the constituent MIDI note
numbers. |#

(defun geom-mean-likelihood-of-subset
       (subset subset-palette first-subset-ontime
        last-subset-ontime dataset-palette
        ontimes-list c-beats &optional
        (empirical-massed
         (empirical-mass
          (subseq
           dataset-palette
           (index-1st-sublist-item>=
            (- first-subset-ontime c-beats)
            ontimes-list)
           (index-1st-sublist-item>
            last-subset-ontime ontimes-list)))))
  (if subset
    (expt
     (likelihood-of-subset
      subset-palette empirical-massed)
     (/ 1 (length subset-palette)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 mini-template-segments
 '((0
    ((0 52 55 1 1 1 0) (0 62 61 1 0 1 1)
     (0 64 62 1 0 1 2) (0 68 64 1 0 1 3)
     (0 71 66 1 0 1 4)))
   (1
    ((1 52 55 1 1 2 5) (1 62 61 1 0 2 6)
     (1 64 62 1 0 2 7) (1 68 64 1 0 2 8)
     (1 71 66 1 0 2 9)))
   (2
    ((2 52 55 1 1 3 10) (2 62 61 1 0 3 11)
     (2 64 62 1 0 3 12) (2 68 64 1 0 3 13)
     (2 72 67 3/4 0 11/4 14)))
   (11/4
    ((2 52 55 1 1 3 10) (2 62 61 1 0 3 11)
     (2 64 62 1 0 3 12) (2 68 64 1 0 3 13)
     (11/4 71 66 1/4 0 3 15)))
   (3
    ((3 45 51 3 1 6 16) (3 52 55 3 1 6 17)
     (3 60 60 3 0 6 18) (3 64 62 3 0 6 19)
     (3 71 66 1 0 4 20)))))
(setq
 datapoints-segment
 '(3/2 ((1 64 62 1 0 2 7) (1 68 64 1 0 2 8))))
(mean&rangep
 datapoints-segment mini-template-segments 4 3 3)
--> NIL.
\end{verbatim}

\noindent This function takes five arguments: a pair
consisting of an ontime and a list of datapoints, a
list of pairs as above, and three thresholds. It uses
the ontime in the first argument to determine which
list is relevant in the second argument. Then the
two sets of MIDI note numbers are compared, in terms
of their mean, min, and max values. If, for each of
these summary statistics, the absolute difference
between each set of MNNs is smaller than the
threshold, then T is returned, and NIL otherwise. |#

(defun mean&rangep
       (datapoints-segment template-segments c-bar
        c-min c-max &optional 
        (template-segment
         (full-segment-nearest<ontime
          (first datapoints-segment)
          template-segments))
        (MNNs1
         (if template-segment
           (nth-list-of-lists
            1 (second template-segment))))
        (MNNs2
         (if template-segment
           (nth-list-of-lists
            1 (second datapoints-segment)))))
  (and
   template-segment
   (< (abs (- (mean MNNs2) (mean MNNs1))) c-bar)
   (< (abs
       (- (min-item MNNs2) (min-item MNNs1))) c-min)
   (< (abs
       (- (max-item MNNs2) (max-item MNNs1))) c-max)))

#|
\noindent Example:
\begin{verbatim}
(pitch&octave-spellingp
 '((12 50 54 1 1 13 42) (12 62 61 1 0 13 43)
   (12 65 63 1 0 13 44) (12 69 65 1 0 13 45)))
--> T.
\end{verbatim}

\noindent This function converts each MIDI-morphetic
pair (assumed to be second and third entries of each
list) to pitch and octave number. If the spelling
requires more than two flats or sharps, then nil will
be returned, and t otherwise. |#

(defun pitch&octave-spellingp
       (a-list &optional (MIDI-index 1)
        (morphetic-index 2))
  (if (null a-list) t
    (not
     (position
      nil
      (mapcar
       #'(lambda (x)
           (MIDI-morphetic-pair2pitch&octave
            (list
             (nth MIDI-index x)
             (nth morphetic-index x))))
       a-list)))))

#|
\noindent Example:
\begin{verbatim}
(translate-datapoints-to-first-ontime
 4 0
 '((28 44 51 1 1) (28 48 53 1 1) (28 56 58 1 0)
   (30 44 51 1 1) (31 48 53 1 1) (34 56 58 1 0)))
--> .
\end{verbatim}

This function takes three arguments: an ontime, an
ontime index and a list of datapoints (assumed to
be sorted in lexicographical order). It translates
these datapoints such that the ontime of the first
datapoint equals the first argument. |#

(defun translate-datapoints-to-first-ontime
       (ontime ontime-index datapoints &optional
        (translation-vector
         (append
          (constant-vector 0 ontime-index)
          (list
           (-
            ontime
            (nth
             ontime-index (first datapoints))))
          (constant-vector
           0
           (-
            (-
             (length
              (first datapoints))
             ontime-index) 1)))))
  (translation datapoints translation-vector))
