#| Copyright 2008-2014 Tom Collins
   Tuesday 12 August 2014
   Incomplete

The main function here is called
\nameref{fun:generate-beat-rel-MNN->}. It is embedded
in Racchman-Jun2015, and is similar to
\nameref{fun:generate-beat-MNN-spacing->} from
Racchman-Oct2010. The difference is that the MIDI note
numbers in \nameref{fun:generate-beat-rel-MNN->} (and
Racchman-Jun2015) are assumed to be relative to a
global tonic. Given initial states, a state transition
matrix, an upper limit for ontime, a template point
set, and the tonic pitch closest (tpc) to its mean
MIDI note number, this function generates points
(notes, among other output) that conform to various
criteria, which can be specified using the optional
arguments. The main criterion that has been tested for
\nameref{fun:generate-beat-rel-MNN->} so far is not
too many consecutive states coming from the same
source. It is not clear whether having to control for
range or expectancy (as in Racchman-Oct2010) is
necessary here. Some unusual pauses have been noted in
the output already, however, so this will need
checking.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
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
   :name "list-processing"
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
   :directory '(:relative "Maths foundation")
   :name "stats-sampling"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "text-files"
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

#|
\noindent Example:
\begin{verbatim}
(setq
 states
 '(((1 (-15 4))
    ("Complicated"
     ((0 50 54 3/2 1) (0 69 65 1 0)) (65 63) (-1 0)))
   ((5/4 (-15 2))
    ("Complicated"
     ((72 50 54 1 1) (289/4 67 64 1/4 0))
     (65 63) (-1 0)))
   ((3/2 (-8 0))
    ("Am_I_wrong"
     ((449/2 55 57 1/2 1) (449/2 63 62 1/2 0))
     (63 62) (-3 0)))
   ((2 (-3))
    ("Am_I_wrong" ((185 60 60 1/2 1))
     (63 62) (-3 0)))))
(setq point-set nil)
(setq template-segments nil)
(setq checklist '("originalp"))
(setq c-sources 3)
(checklistp-rel
 states point-set template-segments checklist
 c-sources)
--> T.
\end{verbatim}

\noindent A simpler version of the function
\nameref{fun:checklistp} that just checks whether the
sources are all the same. Note the location of the
sources within the context have changed for
Racchman-Jun2014 compared to Racchman-Oct2010, from
third to first. |#

(defun checklistp-rel
       (states &optional
        (point-set nil) (template-segments nil)
        (checklist "originalp") (c-sources 3)
        ; This argument no longer required.
        ; template-likelihood-profile
        ; These arguments no longer required.
        ; c-bar c-min c-max c-beats c-prob
        (lastn-sources
         (mapcar
          #'(lambda (x)
              (; Next line is the index change.
               first
               (second x)))
          (lastn c-sources states)))
        (originaledp
         (if (find
              "originalp" checklist :test #'string=)
           (if (or
                (< (length lastn-sources) c-sources)
                (index-item-1st-doesnt-occur
                 (first lastn-sources)
                 (rest lastn-sources))) t) t))
        #| These extra checks on range/likelihood
        removed.
        (point-set-segments
         (if (or
              (find
               "mean&rangep" checklist
               :test #'string=)
              (find
               "likelihoodp" checklist
               :test #'string=))
           (segments-strict point-set 1 3)))
        (last-segment
         (my-last (butlast point-set-segments)))
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
             last-segment point-set c-beats
             template-likelihood-profile c-prob)) t))
        |#
        )
  ; Again, this is the checklistp version.
  ; (and originaledp mean&rangedp likelihoodp)
  (progn
    ; Next 2 lines avoid err msg while dev continues.
    (length point-set)
    (length template-segments)
    originaledp))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   temp-path
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "Racchman-Jun2014 example"))
  *MCStylistic-MonthYear-example-files-results-path*))
  (setq
   initial-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2014 example")
      :name "initial-states" :type "txt")
 *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   stm
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2014 example")
      :name "transition-matrix" :type "txt")
 *MCStylistic-MonthYear-example-files-results-path*)))
  (setq no-ontimes> 12)
  (setq
   dataset-all
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "softEmotivePop" "lisp")
      :name "Young_and_beautiful" :type "txt")
     *MCStylistic-MonthYear-data-path*)))
  (setq
   template-fsm (fifth-steps-mode dataset-all))
  (setq
   trans-pair&c-dataset
   (centre-dataset template-fsm dataset-all))
  (setq template-tpc (first trans-pair&c-dataset))
  (setq beats-in-bar 4)
  (setq checklist (list "originalp"))
  (setq scale 1000)
  "Yes!")

(setq
 *rs*
 #.(CCL::INITIALIZE-MRG31K3P-STATE 119640237
    1896132409 1283053466 2078949444 1948704030
    110577318))
(setq time-a (get-internal-real-time))
(setq
 output
 (generate-beat-rel-MNN->
  initial-states stm no-ontimes> dataset-all
  template-tpc checklist beats-in-bar))
(setq time-b (get-internal-real-time))
(setq
 time-taken
 (float
  (/
   (- time-b time-a)
   internal-time-units-per-second)))
--> 81.822464
(write-to-file
 (cons time-taken output)
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2014-sample-output" :type "txt")
  temp-path))
(saveit
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2014-sample-output" :type "mid")
  temp-path)
 (modify-to-check-dataset (second output) scale))
(firstn 11 (second output))
--> ((0 50 54 3/2 1) (0 69 65 3/2 0) (0 74 68 3/2 0)
     (0 78 70 3/2 0) (3/2 50 54 1/2 1) (3/2 69 65 2 0)
     (3/2 74 68 2 0) (3/2 78 70 2 0) (2 50 54 3/2 1)
     (7/2 50 54 1/2 1) (7/2 69 65 1/2 0))
\end{verbatim}

\noindent This function is embedded in
Racchman-Jun2015, and is similar to
\nameref{fun:generate-beat-MNN-spacing->} from
Racchman-Oct2010. The difference is that the MIDI note
numbers in \nameref{fun:generate-beat-rel-MNN->} (and
Racchman-Jun2014) are assumed to be relative to a
global tonic. Given initial states, a state-transition
matrix, an upper limit for ontime, a template point
set, and the tonic pitch closest (tpc) to its mean
MIDI note number, this function generates points
(notes, among output) that conform to various
criteria, which can be specified using the optional
arguments. At present, the criteria are things like:
number of failures tolerated at any point before
process terminated (c-failures); not too many
consecutive states from the same source (c-sources).
A record, named index-failures, is kept of how many
times a generated state fails to meet the criteria.
When a threshold c-failures is exceeded at any state
index, the penultimate state is removed and generation
continues. If the value of c-failures is exceeded for
the first state, a message is returned. This function
returns the states as well as the points, and also
index-failures and i, the total number of
iterations. |#

(defun generate-beat-rel-MNN->
       (initial-states stm no-ontimes>
        dataset-template template-tpc &optional
        (checklist (list "originalp"))
        (beats-in-bar 4)
        (c-failures 10) (c-sources 3)
        #| These arguments not necessary at present.
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        |#
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        #| This argument not necessary at present.
        (template-likelihood-profile
         (geom-mean-likelihood-of-states
          template-segments dataset-template c-beats))
        |#
        #| Probably not necessary either.
        (initial-MNN
         (if template-segments
           (second
            (first
             (second (first template-segments)))) 60))
        (initial-MPN
         (if template-segments
           (third
            (first
             (second (first template-segments)))) 60))
        |#
        (i 1) (index-failures (list 0))
        (states
         (list
          (if template-segments
            (choose-one-with-beat
             (+
              ; I feel there should be mod beats here...
              (first (first template-segments)) 1)
             initial-states)
            (choose-one initial-states))))
        (point-set
         (translate-datapoints-to-first-ontime
          (first (first template-segments)) 0
          (sort-dataset-asc
           (states2datapoints-by-rel
            states beats-in-bar (first template-tpc)
            (second template-tpc)
            point-set-idx state-tonic-idx MNN-idx
            MPN-idx))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states)) stm
            :test #'equalp))))
        (checklistedp
         (checklistp-rel
          states point-set template-segments
          #| Extra argument
          template-likelihood-profile
          could go here. |#
          checklist c-sources
          #| Extra arguments like
          c-bar c-min c-max c-beats c-prob
          could go here. |#
          ))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    (if (zerop (mod i 1000))
      (format
       t "i=~s for generate-beat-rel-MNN->~%" i))
    #| Begin debugging.
    (if ; (equalp i 5493)
        (>
         (length
          (restrict-dataset-in-nth-to-tests
           point-set
           0 (list #'equalp) (list 0))) 4)
      (progn
        (write-to-file
         (list states point-set i index-failures)
         (make-pathname
          :directory
          '(:absolute "Users" "tomthecollins" "Shizz"
            "JKU" "JournalPapers" "ComputerMusicJournal"
            "2014b" "20141123" "output")
          :name "debug" :type "txt"))
        ; Throw an error.
        (/ 1 0)))
    End debugging. |#
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states point-set i index-failures)
        (generate-beat-rel-MNN->
         initial-states stm no-ontimes>
         dataset-template template-tpc checklist
         beats-in-bar c-failures c-sources
         #| Extra arguments like
         c-bar c-min c-max c-beats c-prob
         could go here. |#
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments
         #| An extra argument like
         template-likelihood-profile
         could go here. |#
         (+ i 1)
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
      (if checklistedp
        (if (>= (first (my-last point-set))
                no-ontimes>)
          (list states point-set i index-failures)
          (if next-state
            (generate-beat-rel-MNN->
             initial-states stm no-ontimes>
             dataset-template template-tpc checklist
             beats-in-bar c-failures c-sources
             #| Extra arguments like
             c-bar c-min c-max c-beats c-prob
             could go here. |#
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments
             #| An extra argument like
             template-likelihood-profile
             could go here. |#
             (+ i 1)
             (if (< (length states)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states (list next-state)))
            (generate-beat-rel-MNN->
             initial-states stm no-ontimes>
             dataset-template template-tpc checklist
             beats-in-bar c-failures c-sources
             #| Extra arguments like
             c-bar c-min c-max c-beats c-prob
             could go here. |#
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments
             #| An extra argument like
             template-likelihood-profile
             could go here. |#
             (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states point-set)))
        (generate-beat-rel-MNN->
         initial-states stm no-ontimes>
         dataset-template template-tpc checklist
         beats-in-bar c-failures c-sources
         #| Extra arguments like
         c-bar c-min c-max c-beats c-prob
         could go here |#
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments
         #| An extra argument like
         template-likelihood-profile
         could go here. |#
         i ;Only instance not incremented
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
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "initial-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   stm
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq no-ontimes> 9)
  (setq
   point-set
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "bachChorales" "bachChoraleBWV411R246" "lisp")
      :name "bachChoraleBWV411R246" :type "txt")
    *MCStylistic-MonthYear-data-path*)))
  (setq
   point-set-template (subseq point-set 0))
  (setq
   *rs*
   #.(CCL::INITIALIZE-MRG31K3P-STATE 401231135
      2091957934 562386227 1913959126 1052216893
      619759959))
  (setq
   points-from-previous-interval
   '((5 42 49 1/2 3) (5 62 61 1 2) (5 69 65 1/2 1)
     (5 69 65 1 0) (11/2 38 47 1/2 3)
     (11/2 66 63 1/2 1)))
  (setq beats-in-bar 4)
  (setq MNN-index 1)
  (setq MPN-index 2)
  (setq duration-index 3)
  (setq fifth-steps-mode '(4 5))
  (setq
   previous-state-context-pair
   (my-last
    (beat-rel-MNN-states
     points-from-previous-interval "No information"
     beats-in-bar MNN-index MPN-index duration-index
     fifth-steps-mode)))
  (setq generation-interval '(6 12))
  (setq
   pattern-region
   '((27/2 66 63) (27/2 69 65) (14 55 57) (14 62 61)
     (14 67 64) (14 71 66) (29/2 69 65) (29/2 72 67)))
  "Data loaded and variables set.")
(generate-beat-rel-MNN-forcing->
 initial-states stm no-ontimes> point-set-template
 '(55 57) generation-interval pattern-region
 previous-state-context-pair (list "originalp")
 beats-in-bar 10 3)
--> ((((5/2 (-17 7 11 14))
       ("No information"
        ((5 62 61 1 2) ... (11/2 66 63 1/2 1))
        (55 57) (4 5)))
      ((3 (-12 7 12 16))
       ("bachChoraleBWV259R39"
        ((6 43 50 1 3) ... (6 71 66 1 0))
        (55 57) (4 5)))
      ((1 (-12 7 12 16))
       ("bachChoraleBWV379R151"
        ((28 43 50 1/2 3) ... (28 71 66 1 0))
        (55 57) (1 0)))
      ((2 (-10 5 11 17))
       ("bachChoraleBWV39R67"
        ((33 45 51 1 3) ... (33 72 67 1 0))
        (55 57) (1 0)))
      ((5/2 (-10 5 9 17))
       ("bachChoraleBWV350R360"
        ((25 48 53 1 3) ... (51/2 67 64 1/2 1))
        (58 59) (-2 0))))
     ((5 38 47 1/2 3) (5 62 61 5/2 2) (5 66 63 1/2 1)
      (5 69 65 1/2 0) ... (9 64 62 1/2 1))
     5 (0 0 0 0 0))
\end{verbatim}

\noindent This function is simiar to the function
\nameref{fun:generate-beat-spacing-forcing->}. The
difference is that this one is embedded in
Racchmaninof-Jun2015 rather than Racchmaninof-Oct2010,
and MIDI note numbers in
\nameref{fun:generate-beat-rel-MNN-forcing->} (and
Racchman-Jun2015) are assumed to be relative to a global
tonic. The function is also similar to
\nameref{fun:generate-beat-rel-MNN->}. The difference is
that there are some extra arguments here, which allow
for using external/internal initial/final states, and
for using information from a discovered pattern or
previous/next state to further guide the generation,
hence `forcing'. |#

(defun generate-beat-rel-MNN-forcing->
       (initial-states stm no-ontimes>
        point-set-template template-tpc
        generation-interval pattern-region
        previous-state-context-pair
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict point-set-template 1 3)))
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
        (point-set
         (progn
           ; (format t "point-set says hi!~%")
           (translate-datapoints-to-first-ontime
            (if previous-state-context-pair
              #| 21/1/2016. Don't know why the
              commented out option here would ever be
              correct. How are ontimes in the context
              reliable representations of the current
              location in the generated passage?

              (first
               (first
                (sort-dataset-asc
                 (second
                  (second
                   previous-state-context-pair)))))
              |#
              
              (first generation-interval)
              (first generation-interval))
            0
            (sort-dataset-asc
             (states2datapoints-by-rel
              states beats-in-bar (first template-tpc)
              (second template-tpc) point-set-idx
              state-tonic-idx MNN-idx MPN-idx)))))
        (next-state
         (progn
           ; (format t "next-state says hi!~%")
           (choose-one
            (second
             (assoc
              (first (my-last states)) stm
              :test #'equalp)))))
        (checklistedp
         (progn
           ; (format t "checklistedp says hi!~%")
           (checklistp-rel
            states point-set template-segments
            checklist c-sources)))
        (failurep
         (progn
           ; (format t "failurep says hi!~%")
           (index-1st-sublist-item>=
            c-failures index-failures))))
  (progn
    (if (zerop (mod i 1000))
      (format
       t "i=~s for generate-beat-rel-MNN-forcing->~%" i))
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states point-set i index-failures)
        (progn
          ; (format t "1. i=~s~%" i)
          (generate-beat-rel-MNN-forcing->
           initial-states stm no-ontimes>
           point-set-template template-tpc
           generation-interval pattern-region
           previous-state-context-pair checklist
           beats-in-bar c-failures c-sources
           point-set-idx state-tonic-idx MNN-idx MPN-idx
           template-segments (+ i 1)
           (append
            (subseq index-failures 0 (- failurep 1))
            (list
             (+ (nth (- failurep 1) index-failures) 1)))
           ; 29/9/2010 Special case to avoid null states
           (if (equalp failurep 1)
             (list
              (if previous-state-context-pair
                previous-state-context-pair
                (progn
                  (format t "This got used!~%")
                  (choose-one-with-beat
                   (+ (mod
                       (floor
                        (first (first pattern-region)))
                       beats-in-bar) 1)
                   initial-states))))
             (subseq states 0 (- failurep 1))))
          ))
      (if checklistedp
        (if (>= (first (my-last point-set))
                no-ontimes>)
          (list states point-set i index-failures)
          (if next-state
            (progn
              ; (format t "2. i=~s~%" i)
              (generate-beat-rel-MNN-forcing->
               initial-states stm no-ontimes>
               point-set-template template-tpc
               generation-interval pattern-region
               previous-state-context-pair checklist
               beats-in-bar c-failures c-sources
               point-set-idx state-tonic-idx MNN-idx
               MPN-idx template-segments (+ i 1)
               (if (< (length states)
                      (length index-failures))
                 (identity index-failures)
                 (append index-failures (list 0)))
               (append states (list next-state))))
            (progn
              ; (format t "3. i=~s~%" i)
              (generate-beat-rel-MNN-forcing->
               initial-states stm no-ontimes>
               point-set-template template-tpc
               generation-interval pattern-region
               previous-state-context-pair checklist
               beats-in-bar c-failures c-sources
               point-set-idx state-tonic-idx MNN-idx
               MPN-idx template-segments (+ i 1)
               (append
                (butlast index-failures)
                (list (+ (my-last index-failures) 1)))
               states point-set))))
        (progn
          ; (format t "4. i=~s~%" i)
          (generate-beat-rel-MNN-forcing->
           initial-states stm no-ontimes>
           point-set-template template-tpc
           generation-interval pattern-region
           previous-state-context-pair checklist
           beats-in-bar c-failures c-sources point-set-idx
           state-tonic-idx MNN-idx MPN-idx
           template-segments i ;1 instance not incremented
           (append
            (butlast index-failures)
            (list (+ (my-last index-failures) 1)))
           (if (equalp (length index-failures) 1)
             (list
              (if previous-state-context-pair
                previous-state-context-pair
                (progn
                  (format t "This got used!~%")
                  (choose-one-with-beat
                   (+ (mod
                       (floor
                        (first (first pattern-region)))
                       beats-in-bar) 1)
                   initial-states))))
             (butlast states))))))))

#| Old version. Don't think it works.
(defun generate-beat-rel-MNN-forcing->
       (initial-states stm no-ontimes>
        dataset-template template-tpc
        generation-interval pattern-region
        previous-state-context-pair
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
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
        (point-set
         (translate-datapoints-to-first-ontime
          (if previous-state-context-pair
            (first
             (first
              (sort-dataset-asc
               (second
                (second
                 previous-state-context-pair)))))
            (first generation-interval))
          0
          (sort-dataset-asc
           (states2datapoints-by-rel
            (if previous-state-context-pair
              (append
               (first previous-state-context-pair)
               states)
              states)
            beats-in-bar (first template-tpc)
            (second template-tpc) point-set-idx
            state-tonic-idx MNN-idx MPN-idx))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states)) stm
            :test #'equalp))))
        (checklistedp
         (checklistp-rel
          states point-set template-segments
          checklist c-sources))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states point-set i index-failures)
        (generate-beat-rel-MNN-forcing->
         initial-states stm no-ontimes>
         dataset-template template-tpc
         generation-interval pattern-region
         previous-state-context-pair checklist
         beats-in-bar c-failures c-sources
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments (+ i 1)
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
        (if (>= (first (my-last point-set))
                no-ontimes>)
          (list states point-set i index-failures)
          (if next-state
            (generate-beat-rel-MNN-forcing->
             initial-states stm no-ontimes>
             dataset-template template-tpc
             generation-interval pattern-region
             previous-state-context-pair checklist
             beats-in-bar c-failures c-sources             template-segments
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments (+ i 1)
             (if (< (length states)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states (list next-state)))
            (generate-beat-rel-MNN-forcing->
             initial-states stm no-ontimes>
             dataset-template template-tpc
             generation-interval pattern-region
             previous-state-context-pair checklist
             beats-in-bar c-failures c-sources
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states point-set)))
        (generate-beat-rel-MNN-forcing->
         initial-states stm no-ontimes>
         point-set generation-interval
         pattern-region previous-state-context-pair
         checklist beats-in-bar c-failures c-sources
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments
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
|#

#|
\noindent Example:
\begin{verbatim}
(setq 
 states
 '(((1 (-12 -5))
    ("C-6-3" ((0 52 55 1 1) (0 59 59 1 1))
     (64 62) (4 0)))
   ((2 (0))
    ("C-7-2" ((4 62 61 3/2 0)) (62 61) (2 0)))
   ((7/2 (-1))
    ("C-6-4" ((125/2 59 59 1/2 0)) (60 60) (0 0)))
   ((1 (-12 -5 4))
    ("C-6-3"
     ((0 52 55 1 1) (0 59 59 1 1) (0 66 63 2 0))
     (64 62) (4 0)))
   ((2 (4))
    ("C-6-4" ((7 64 62 1 0)) (60 60) (0 0)))
   ((3 (2))
    ("C-6-4" ((8 64 62 1 0)) (60 60) (0 0)))
   ((1 "rest")
    ("C-6-3" nil (64 62) (4 0)))
   ((3/2 (2))
    ("C-7-2" ((13/2 64 62 1/2 0)) (62 61) (2 0)))
   ((2 (4))
    ("C-7-2" ((7 66 63 1/2 0)) (62 61) (2 0)))
   ((3 (5))
    ("C-7-2" ((8 67 64 1 0)) (62 61) (2 0)))
   ((1 (-8 -1 7))
    ("C-6-3"
     ((72 56 57 3 1) (72 63 61 3 1) (72 71 66 3 1))
     (64 62) (4 0)))))
(states2datapoints-by-rel states 3 60 60)
--> ((0 48 53 1 1) (0 55 57 1 1) (1 60 60 3/2 0)
     (5/2 59 59 1/2 0) (3 48 53 1 1) (3 55 57 1 1)
     (3 62 61 1 0) (4 64 62 1 0) (5 64 62 1 0)
     (13/2 62 61 1/2 0) (7 64 62 1 0) (8 65 63 1 0)
     (9 52 55 3 1) (9 59 59 3 1) (9 67 64 3 1))
\end{verbatim}

This function applies the function
\nameref{fun:half-states2datapoint-by-lookup}
recursively to a list of states. It is very similar
to the function
\nameref{fun:states2datapoints-by-lookup}. |#

(defun states2datapoints-by-rel
       (states &optional (beats-in-bar 4)
        (closest-tonic-MNN 60)
        (closest-tonic-MPN 60)
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        ; Unpack states into MNNs and MPNs
        (half-states
         (mapcar
          #'(lambda (x)
              (list
               (list
                (first (first x))
                (add-to-list
                 closest-tonic-MNN
                 (add-to-list
                  (*
                   -1
                   (first
                    (nth state-tonic-idx (second x))))
                  (nth-list-of-lists
                   MNN-idx
                   (nth point-set-idx (second x)))))
                (add-to-list
                 closest-tonic-MPN
                 (add-to-list
                  (*
                   -1
                   (second
                    (nth state-tonic-idx (second x))))
                  (nth-list-of-lists
                   MPN-idx
                   (nth point-set-idx (second x))))))
               (second x)))
          states))
        (j 0) (n (length half-states))
        (state-durs
         (state-durations-by-beat
          states beats-in-bar point-set-idx))
        (unique-times
         (cons
          0 (fibonacci-list state-durs))))
  (if (equal j n) ()
    (append
     (half-state2datapoints-by-lookup
      j half-states state-durs unique-times
      point-set-idx)
     (states2datapoints-by-rel
      states beats-in-bar closest-tonic-MNN
      closest-tonic-MPN point-set-idx state-tonic-idx
      MNN-idx MPN-idx half-states (+ j 1) n state-durs
      unique-times))))
