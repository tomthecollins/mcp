#| Copyright 2008-2014 Tom Collins
   Tuesday 12 August 2014
   Incomplete

The main function here is called
\nameref{fun:generate-beat-rel-MNN<-}. It is embedded
in Racchman-Jun2014, and is similar to
\nameref{fun:generate-beat-MNN-spacing<-} from
Racchman-Oct2010. The difference is that the MIDI note
numbers in \nameref{fun:generate-beat-rel-MNN<-} (and
Racchman-Jun2014) are assumed to be relative to a
global tonic. Given initial states, a state transition
matrix, an upper limit for ontime, a template point
set, and the tonic pitch closest (tpc) to its mean
MIDI note number, this function generates points
(notes, among other output) that conform to various
criteria, which can be specified using the optional
arguments. The main criterion that has been tested for
\nameref{fun:generate-beat-rel-MNN<-} so far is not
too many consecutive states coming from the same
source. It is not clear whether having to control for
range or expectancy (as in Racchman-Oct2010) is
necessary here.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-beat-MNN-spacing-backwards"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-beat-relative-MNN"
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
(progn
  (setq
   temp-path
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "Racchman-Jun2015 example"))
  *MCStylistic-MonthYear-example-files-results-path*))
  (setq
   final-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "final-states" :type "txt")
 *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   stm<-
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix<-" :type "txt")
 *MCStylistic-MonthYear-example-files-results-path*)))
  (setq no-ontimes< 388)
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
 (generate-beat-rel-MNN<-
  final-states stm<- no-ontimes< dataset-all
  template-tpc checklist beats-in-bar))
(setq time-b (get-internal-real-time))
(setq
 time-taken
 (float
  (/
   (- time-b time-a)
   internal-time-units-per-second)))
--> 0.254569
(write-to-file
 (cons time-taken output)
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2015-sample-output<-" :type "txt")
  temp-path))
(saveit
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2014-sample-output<-" :type "mid")
  temp-path)
 (modify-to-check-dataset
  (mapcar
   #'(lambda (x)
       (cons
        (- (first x) (first (first (second output))))
        (rest x)))
   (second output))
 scale))
(firstn 11 (second output))
--> ((388 59 59 1 3) (388 62 61 1 2) (388 71 66 1 1)
     (388 78 70 1 0) (389 57 58 1/2 3)
     (389 64 62 3/2 2) (389 71 66 1 1) (389 79 71 1 0)
     (779/2 55 57 1/2 3) (390 54 56 1 3)
     (390 69 65 1 1))
\end{verbatim}

\noindent This function is embedded in
Racchman-Jun2014, and is similar to
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

(defun generate-beat-rel-MNN<-
       (final-states stm<- no-ontimes<
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
        (point-set
         (translate-datapoints-to-last-ontime
          (first (my-last template-segments)) 0
          (sort-dataset-asc
           (states2datapoints-by-rel<-
            states<- beats-in-bar (first template-tpc)
            (second template-tpc)
            point-set-idx state-tonic-idx MNN-idx
            MPN-idx))))
        (next-state
         (choose-one
          (second
           (assoc
            (first (my-last states<-)) stm<-
            :test #'equalp))))
        (checklistedp
         (checklistp-rel
          states<- point-set template-segments
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
       t "i=~s for generate-beat-rel-MNN<-~%" i))
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states<- point-set i
         index-failures)
        (generate-beat-rel-MNN<-
         final-states stm<- no-ontimes<
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
               (+ (first (my-last template-segments)) 1)
               final-states)
              (choose-one final-states)))
           (subseq states<- 0 (- failurep 1)))))
      (if checklistedp
        (if (<= (first (first point-set)) no-ontimes<)
          (list states<- point-set i index-failures)
          (if next-state
            (generate-beat-rel-MNN<-
             final-states stm<- no-ontimes<
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
             (if (< (length states<-)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states<- (list next-state)))
            (generate-beat-rel-MNN<-
             final-states stm<- no-ontimes<
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
             states<- point-set)))
        (generate-beat-rel-MNN<-
         final-states stm<- no-ontimes<
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
               (+ (first (my-last template-segments)) 1)
               final-states)
              (choose-one final-states)))
           (butlast states<-)))))))


(defun states2datapoints-by-rel<-
       (states<- &optional (beats-in-bar 4)
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
          (reverse states<-)))
        (j 0) (n (length half-states))
        (state-durs
         (state-durations-by-beat
          (reverse states<-) beats-in-bar
          point-set-idx))
        (unique-times
         (cons
          0 (fibonacci-list state-durs))))
  (if (equal j n) ()
    (append
     (half-state2datapoints-by-lookup
      j half-states state-durs unique-times
      point-set-idx)
     (states2datapoints-by-rel<-
      states<- beats-in-bar closest-tonic-MNN
      closest-tonic-MPN point-set-idx state-tonic-idx
      MNN-idx MPN-idx half-states (+ j 1) n state-durs
      unique-times))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   final-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   stm<-
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix<-" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq no-ontimes< 12)
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
   points-from-next-interval
   '((18 40 48 2 3) (18 55 57 2 2) (18 59 59 2 1)
     (18 64 62 2 0)))
  (setq beats-in-bar 4)
  (setq MNN-index 1)
  (setq MPN-index 2)
  (setq duration-index 3)
  (setq fifth-steps-mode '(4 5))
  (setq
   next-state-context-pair
   (my-last
    (beat-rel-MNN-states
     points-from-next-interval "No information"
     beats-in-bar MNN-index MPN-index duration-index
     fifth-steps-mode)))
  (setq generation-interval '(6 18))
  (setq
   pattern-region
   '((27/2 66 63) (27/2 69 65) (14 55 57) (14 62 61)
     (14 67 64) (14 71 66) (29/2 69 65) (29/2 72 67)))
  "Data loaded and variables set.")
(generate-beat-rel-MNN-forcing<-
 final-states stm<- no-ontimes< point-set-template
 '(55 57) generation-interval pattern-region
 next-state-context-pair (list "originalp")
 beats-in-bar 10 3)
--> ((((3 (-15 0 4 9))
       ("No information"
        ((18 40 48 2 3) ... (18 64 62 2 0))
        (55 57) (4 5)))
      ((5/2 (-8 2 8 11))
       ("bachChoraleBWV4p8R184"
        ((13 45 51 1 3) ... (27/2 55 57 1/2 2))
        (53 56) (2 5)))
      ...
      ((1 (-3 0 9 16))
       ("bachChoraleBWV353R269"
        ((20 55 57 1/2 3) ... (20 74 68 1 0))
        (58 59) (1 5))))
     ((12 52 55 1/2 3) (12 55 57 1/2 2)
      ... (18 64 62 2 0))
     14 (0 0 0 0 0 0 0 0 0 0 0 1 0))
\end{verbatim}

\noindent This function is simiar to the function
\nameref{fun:generate-beat-spacing-forcing<-}. The
difference is that this one is embedded in
Racchmaninof-Jun2015 rather than Racchmaninof-Oct2010,
and MIDI note numbers in
\nameref{fun:generate-beat-rel-MNN-forcing<-} (and
Racchman-Jun2015) are assumed to be relative to a global
tonic. The function is also similar to
\nameref{fun:generate-beat-rel-MNN<-}. The difference is
that there are some extra arguments here, which allow
for using external/internal initial/final states, and
for using information from a discovered pattern or
previous/next state to further guide the generation,
hence `forcing'. |#

(defun generate-beat-rel-MNN-forcing<-
       (final-states stm<- no-ontimes<
        point-set-template template-tpc
        generation-interval pattern-region
        next-state-context-pair &optional
        (checklist (list "originalp")) (beats-in-bar 4)
        (c-failures 10) (c-sources 3) (point-set-idx 1)
        (state-tonic-idx 2) (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict point-set-template 1 3)))
        (i 1) (index-failures (list 0))
        (states<-
         (progn
           ; (format t "states<- says hi!~%")
           (list
            (if next-state-context-pair
              next-state-context-pair
              (choose-one-with-beat
               (+ (mod
                   (ceiling
                    (first (my-last pattern-region)))
                   beats-in-bar) 1)
               final-states)))))
        (point-set
         (progn
           ; (format t "point-set says hi! nscp is:~%")
           ; (format t "~s~%" next-state-context-pair)
           ; (format t "generation-interval is:~%")
           ; (format t "~s~%" generation-interval)
           ; (format t "states<- is:~%")
           ; (format t "~s~%" states<-)
           ; (format t "beats-in-bar is:~%")
           ; (format t "~s~%" beats-in-bar)
           ; (format t "template-tpc is:~%")
           ; (format t "~s~%" template-tpc)
           ; (format t "point-set-idx is:~%")
           ; (format t "~s~%" point-set-idx)
           ; (format t "state-tonic-idx is:~%")
           ; (format t "~s~%" state-tonic-idx)
           ; (format t "MNN-idx is:~%")
           ; (format t "~s~%" MNN-idx)
           ; (format t "MPN-idx is:~%")
           ; (format t "~s~%" MPN-idx)
           ; 25/10/10 This could go wrong!
           ; 24/1/15 Could this go wrong?!
           (translate-datapoints-to-last-ontime
            (if next-state-context-pair
              #| 21/1/2016. Don't know why the
              commented out option here would ever be
              correct. How are ontimes in the context
              reliable representations of the current
              location in the generated passage?
              
              (first
               (my-last
                (sort-dataset-asc
                 (second
                  (second
                   next-state-context-pair)))))
              |#
              
              (second generation-interval)
              (second generation-interval))
            0
            (sort-dataset-asc
             (states2datapoints-by-rel<-
              states<- beats-in-bar (first template-tpc)
              (second template-tpc) point-set-idx
              state-tonic-idx MNN-idx MPN-idx)))))
        (next-state
         (progn
           ; (format t "next-state says hi!~%")
           (choose-one
            (second
             (assoc
              (first (my-last states<-)) stm<-
              :test #'equalp)))))
        (checklistedp
         (checklistp-rel
          states<- point-set template-segments
          checklist c-sources))
        (failurep
         (index-1st-sublist-item>=
          c-failures index-failures)))
  (progn
    (if (zerop (mod i 1000))
      (format
       t "i=~s for generate-beat-rel-MNN-forcing<-~%" i))
    (if failurep
      (if (zerop failurep)
        (list
         "Failure!" states<- point-set i
         index-failures)
        (generate-beat-rel-MNN-forcing<-
         final-states stm<- no-ontimes<
         point-set-template template-tpc
         generation-interval pattern-region
         next-state-context-pair checklist
         beats-in-bar c-failures c-sources
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments (+ i 1)
         (append
          (subseq index-failures 0 (- failurep 1))
          (list
           (+ (nth (- failurep 1) index-failures) 1)))
         ; 12/10/2010 Special case avoids null states
         (if (equalp failurep 1)
           (list
            (if next-state-context-pair
              next-state-context-pair
              (progn
                (format t "This got used!~%")
                (choose-one-with-beat
                 (+ (mod
                     (ceiling
                      (first (my-last pattern-region)))
                     beats-in-bar) 1)
                 final-states)
                )))
           (subseq states<- 0 (- failurep 1)))))
      (if checklistedp
        (if (<=
             (first (first point-set)) no-ontimes<)
          (list states<- point-set i index-failures)
          (if next-state
            (generate-beat-rel-MNN-forcing<-
             final-states stm<- no-ontimes<
             point-set-template template-tpc
             generation-interval pattern-region
             next-state-context-pair checklist
             beats-in-bar c-failures c-sources
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments (+ i 1)
             (if (< (length states<-)
                    (length index-failures))
               (identity index-failures)
               (append index-failures (list 0)))
             (append states<- (list next-state)))
            (generate-beat-rel-MNN-forcing<-
             final-states stm<- no-ontimes<
             point-set-template template-tpc
             generation-interval pattern-region
             next-state-context-pair checklist
             beats-in-bar c-failures c-sources
             point-set-idx state-tonic-idx MNN-idx
             MPN-idx template-segments (+ i 1)
             (append
              (butlast index-failures)
              (list (+ (my-last index-failures) 1)))
             states<- point-set)))
        (generate-beat-rel-MNN-forcing<-
         final-states stm<- no-ontimes<
         point-set-template template-tpc
         generation-interval pattern-region
         next-state-context-pair checklist
         beats-in-bar c-failures c-sources
         point-set-idx state-tonic-idx MNN-idx MPN-idx
         template-segments
         i ;Only instance not incremented
         (append
          (butlast index-failures)
          (list (+ (my-last index-failures) 1)))
         (if (equalp (length index-failures) 1)
           (list
            (if next-state-context-pair
              next-state-context-pair
              (progn
                (format t "This got used!~%")
                (choose-one-with-beat
                 (+ (mod
                     (ceiling
                      (first (my-last pattern-region)))
                     beats-in-bar) 1)
                 final-states))))
           (butlast states<-)))))))





