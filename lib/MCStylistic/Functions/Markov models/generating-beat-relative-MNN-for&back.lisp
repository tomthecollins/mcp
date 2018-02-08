#| Copyright 2008-2014 Tom Collins
   Monday 24 November 2014
   Incomplete

The main function here,
\ref{fun:generate-rel-for-or-back-no-fail}, is an
MNN-relative version of the function
\ref{fun:generate-forward-or-back-no-fail}.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
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
   :directory '(:relative "Markov models")
   :name "generating-beat-relative-MNN-backwards"
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
   :directory '(:relative "Markov models")
   :name "segmentation"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 *rs*
 #.(CCL::INITIALIZE-MRG31K3P-STATE 660880148 974840345
    1231458079 334849240 1409987611 1407274776))
(progn
  (setq
   initial-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "initial-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   stm->
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   final-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   stm<-
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix<-" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   dataset-all
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "bachChorales"
        "bachChoraleBWV411R246" "lisp")
      :name "bachChoraleBWV411R246" :type "txt")
    *MCStylistic-MonthYear-data-path*)))
  (setq dataset-template (firstn 216 dataset-all))
  (setq
   template-fsm
   (fifth-steps-mode dataset-template))
  (setq
   trans-pair&c-dataset
   (centre-dataset template-fsm dataset-template))
  (setq template-tpc (first trans-pair&c-dataset))
  "Yes!")
(setq checklist (list "originalp"))
(setq beats-in-bar 4)
(setq c-failures 10)
(setq c-sources 4)
(setq c-forwards 2)
(setq c-backwards 2)
(progn
  (setq time-a (get-internal-real-time))
  (setq
   output
   (generate-beat-rel-MNN<->
    initial-states stm-> final-states stm<-
    dataset-template template-tpc checklist
    beats-in-bar c-failures c-sources c-forwards
    c-backwards))
  (setq time-b (get-internal-real-time))
  (float
   (/
    (- time-b time-a)
    internal-time-units-per-second)))
--> .
(setq
 output-datapoints
 (gethash '"united,1,2,superimpose" (third output)))
(saveit
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/Testing/united,1,2,superimpose.mid")
 (modify-to-check-dataset
  (translation
   output-datapoints
   (list
    (- 0 (first (first output-datapoints)))
    0 0 0 0)) 1500))
\end{verbatim}


DOCS OK FROM HERE!!
\noindent This function unites several forwards- and
backwards running realisations of Markov models built
on the arguments initial-states, stm->, final-states,
and stm<-. It is constrained by a template (the
argument dataset-template) and various parameters:
like not too many consecutive states from the same
source (c-sources). The difference between this function
and \ref{fun:generate-beat-MNN-spacing<->} is that the
current function operates on beat of the bar and MIDI
note number relative to a global tonal centre, whereas
\ref{fun:generate-beat-MNN-spacing<->} operates on beat
of the bar and MIDI note spacing.

The numbers of forwards- and backwards- realisations
generated are determined by the arguments c-forwards
and c-backwards respectively. The output is a list of
three hash tables (one containing the forwards
candidates, one the backwards candidates, and one the
united candidates). Another difference between this
function and \ref{fun:generate-beat-MNN-spacing<->} is
the current function also searches for states that would
enable forwards- and backwards-running processes to be
joined without superposing or removing notes (see
\ref{fun:most-probable-join2joined-points} for details).
The most probable join function will not always result in
output, but if it does, then with $\text{c-forwards} =
m$ and $\text{c-backwards} = n$, the number of united
candidates is $6mn$. |#

(defun generate-beat-rel-MNN<->
       (initial-states stm-> final-states stm<-
        dataset-template template-tpc ; <- a new one.
        &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        #| Parameters not currently in use.
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        |#
        (c-forwards 3) (c-backwards 3)
        (ontime-index 0) ; <- new (for completeness).
        (MNN-index 1)
        (MPN-index 2) ; <- another newbie.
        (duration-index 3)
        ; These two are new arguments.
        (point-set-idx 1) (state-tonic-idx 2)
        (template-segments
         (butlast
          (segments-strict
           dataset-template MNN-index duration-index)))
        (bisection
         (ceiling
          (/
           (+
            (first (my-last template-segments))
            (first (first template-segments))) 2)))
        (forwards-candidates
         (make-hash-table :test #'equal))
        (backwards-candidates
         (make-hash-table :test #'equal))
        (united-candidates
         (make-hash-table :test #'equal)))
  (progn
    (loop for i from 1 to c-forwards do
      (format t "Generating forwards excerpt ~s...~%" i)
      (setf
       (gethash
        (concatenate
         'string "forwards," (write-to-string i))
        forwards-candidates)
       (generate-rel-for-or-back-no-fail
        "forwards" initial-states stm-> bisection
        dataset-template template-tpc checklist
        beats-in-bar c-failures c-sources point-set-idx
        state-tonic-idx MNN-index MPN-index
        template-segments)))
    (loop for i from 1 to c-backwards do
      (format t "Generating backwards excerpt ~s...~%" i)
      (setf
       (gethash
        (concatenate
         'string "backwards," (write-to-string i))
        backwards-candidates)
       (generate-rel-for-or-back-no-fail
        "backwards" final-states stm<- bisection
        dataset-template template-tpc checklist
        beats-in-bar c-failures c-sources point-set-idx
        state-tonic-idx MNN-index MPN-index
        template-segments)))
    (loop for i from 1 to c-forwards do
      (loop for j from 1 to c-backwards do
        (format
         t
         (concatenate
          'string "Joining forwards excerpt ~s"
          " and backwards excerpt ~s...~%") i j)
        ; 1. Call most-probable-join for neutral.
        ; 2. Now the same for join-forwards.
        ; 3. And join-backwards.
        (loop for s in
          (list
           "neutral" "forwards-dominant"
           "backwards-dominant") do
          (let
              ((new-states&points
                (most-probable-join2joined-states&points
                 ; states->
                 (fourth
                  (gethash
                   (concatenate
                    'string "forwards,"
                    (write-to-string i))
                   forwards-candidates))
                 ; states<-
                 (fourth
                  (gethash
                   (concatenate
                    'string "backwards,"
                    (write-to-string j))
                   backwards-candidates))
                 ; points<-
                 (fifth
                  (gethash
                   (concatenate
                    'string "backwards,"
                    (write-to-string j))
                   backwards-candidates))
                 stm-> stm<- beats-in-bar template-tpc
                 s ontime-index)))
            (if new-states&points
              (progn
                (format
                 t
                 (concatenate
                  'string "Successful join: ~a~%") s)
                (setf
                 (gethash
                  (concatenate
                   'string
                   "united," (write-to-string i) ","
                   (write-to-string j) ",join-" s)
                  united-candidates)
                 new-states&points))
              (format
               t
               (concatenate
                'string "Unsuccessful join: ~a~%") s))))
        
        ; 4. Old-fashioned superimposing of points.
        ; 5. Old-fashioned forwards-dominant imposing.
        ; 6. Old-fashioned backwards-dominant imposing.
        (loop for s in
          (list
           "superimpose" "forwards-dominant"
           "backwards-dominant") do
          (setf
           (gethash
            (concatenate
             'string
             "united," (write-to-string i) ","
             (write-to-string j) "," s)
            united-candidates)
           (unite-states&points
            ; states->
            (fourth
             (gethash
              (concatenate
               'string "forwards,"
               (write-to-string i))
              forwards-candidates))
            ; states<-
            (fourth
             (gethash
              (concatenate
               'string "backwards,"
               (write-to-string j))
              backwards-candidates))
            ; points->
            (fifth
             (gethash
              (concatenate
               'string "forwards,"
               (write-to-string i))
              forwards-candidates))
            ; points<-
            (fifth
             (gethash
              (concatenate
               'string "backwards,"
               (write-to-string j))
              backwards-candidates))
            bisection beats-in-bar s)))))
    (list
     forwards-candidates backwards-candidates
     united-candidates)))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   *rs*
   #.(CCL::INITIALIZE-MRG31K3P-STATE 1224151479
      949095849 1645874974 1124867833 937056442
      1111346568))
  (setq generation-interval '(12 24))
  (setq terminal->p nil)
  (setq terminal<-p nil)
  (setq external-initial-states nil)
  (setq
   internal-initial-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "internal-initial-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   stm->
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq external-final-states nil)
  (setq
   internal-final-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "internal-final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   stm<-
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix-back" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*
    )))
  (setq
   point-set-template
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "bachChorales"
        "bachChoraleBWV259R39" "lisp")
      :name "bachChoraleBWV259R39" :type "txt")
    *MCStylistic-MonthYear-data-path*)))
  (setq template-tpc '(55 57))
  (setq
   pattern-region
   '((24 52 55 1/2 3) (24 59 59 1/2 2) (24 67 64 1/2 1)
     (24 72 67 1 0) (49/2 54 56 1/2 3)
     (49/2 57 58 1/2 2) (49/2 69 65 1 1) (25 55 57 1 3)
     (25 59 59 1/2 2) (25 71 66 1 0) (51/2 60 60 1/2 2)
     (51/2 67 64 1/2 1)))
  (setq state-context-pair-> nil)
  (setq
   state-context-pair<-
   '((1 (-8 -4 4 11))
     ("bachChoraleBWV364R174"
      ((24 50 54 2 3) (24 54 56 2 2) (24 62 61 2 1)
       (24 69 65 2 0)) (58 59) (1 5))))
  (setq checklist '("originalp"))
  (setq beats-in-bar 4)
  (setq c-failures 10)
  (setq c-sources 3))
(setq
 output
 (generate-beat-rel-MNN-forced<->
  generation-interval terminal->p terminal<-p
  external-initial-states internal-initial-states stm->
  external-final-states internal-final-states stm<-
  point-set-template template-tpc pattern-region
  state-context-pair-> state-context-pair<- checklist
  beats-in-bar c-failures c-sources))
(setq
 some-point-set
 (second
  (gethash
   "united,3,2,forwards-dominant" (third output))))
--> ((12 43 50 1 3) (12 47 52 1 2) (12 55 57 1 1)
     (12 62 61 1 0) (13 45 51 1 3) (13 48 53 1 2)
     ...
     (23 59 59 3 1) (23 64 62 1 0) (47/2 52 55 1/2 2)
     (24 47 52 2 3) (24 51 54 2 2) (24 66 63 2 0)).
(saveit
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "Racchman-Jun2015 example")
   :name "Racchman-Jun2015-sample-output" :type "mid")
  *MCStylistic-MonthYear-example-files-results-path*)
 (modify-to-check-dataset
  (translation
   some-point-set
   (list
    (- 0 (first (first some-point-set))) 0 0 0 0))
  1500))
--> Writes the generated point set to a MIDI file.
\end{verbatim}

\noindent The meaning of `forced' in these functions is
that the generation process is partway through, and for
the current generation time interval $(a, b)$ there is
some existing material either up to $a$ or from $b$
onwards. `Forced' means this existing material imposes
extra constraints in terms of initial/final states, and
these are handled by this function.

The difference between this function and
\ref{fun:generate-beat-spacing-forced<->} is that the
current function operates on beat of the bar and MIDI
note number relative to a global tonal centre, whereas
\ref{fun:generate-beat-spacing-forced<->} operates on
beat of the bar and MIDI note spacing. |#

(defun generate-beat-rel-MNN-forced<->
       (generation-interval terminal->p terminal<-p
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- point-set-template template-tpc
        pattern-region state-context-pair->
        state-context-pair<- &optional
        (checklist (list "originalp")) (beats-in-bar 4)
        (c-failures 10) (c-sources 3) (c-forwards 3)
        (c-backwards 3)
        (ontime-index 0) ; <- new (for completeness).
        (MNN-index 1)
        (MPN-index 2) ; <- another newbie.
        (duration-index 3)
        ; These two are new arguments.
        (point-set-idx 1) (state-tonic-idx 2)
        (template-segments
         (butlast
          (segments-strict
           point-set-template MNN-index
           duration-index)))
        (bisection
         (ceiling
          (/
           (+
            (second generation-interval)
            (first generation-interval)) 2)))
        (forwards-candidates
         (make-hash-table :test #'equal))
        (backwards-candidates
         (make-hash-table :test #'equal))
        (united-candidates
         (make-hash-table :test #'equal)))
  (progn
    (loop for i from 1 to c-forwards do
      (format
       t "Generating forwards excerpt ~s...~%" i)
      (setf
       (gethash
        (concatenate
         'string "forwards," (write-to-string i))
        forwards-candidates)
       (if state-context-pair-> ; pattern-region
         ; 21/1/2016. I think above this should be state-context-pair->
         #| `Forced' means there is existing material
         on one side of the generation time interval,
         and this is passed to the function as an
         extra beginning/ending constraint. |#
         (generate-forced-rel<->no-failure
          "forwards" terminal->p
          external-initial-states
          internal-initial-states stm-> bisection
          point-set-template template-tpc
          generation-interval pattern-region
          state-context-pair-> checklist beats-in-bar
          c-failures c-sources point-set-idx
          state-tonic-idx MNN-index MPN-index
          template-segments)
         #| No existing material on either side of the
         generation time interval, so we can use the
         regular version of the function without extra
         constraints. |#
         (generate-rel-for-or-back-no-fail
          "forwards"
          (if terminal->p
            external-initial-states
            internal-initial-states)
          stm-> bisection point-set-template
          template-tpc checklist beats-in-bar
          c-failures c-sources point-set-idx
          state-tonic-idx MNN-index MPN-index
          template-segments))))
    ; (format t "Got to here!~%")
    (loop for i from 1 to c-backwards do
      (format
       t "Generating backwards excerpt ~s...~%" i)
      (setf
       (gethash
        (concatenate
         'string "backwards," (write-to-string i))
        backwards-candidates)
       (if state-context-pair<- ; pattern-region
         ; 21/1/2016. I think above this should be state-context-pair<-
         #| `Forced' means there is existing material
         on one side of the generation time interval,
         and this is passed to the function as an
         extra beginning/ending constraint. |#
         (generate-forced-rel<->no-failure
          "backwards" terminal<-p
          external-final-states internal-final-states
          stm<- bisection point-set-template
          template-tpc generation-interval
          pattern-region state-context-pair<- checklist
          beats-in-bar c-failures c-sources
          point-set-idx state-tonic-idx MNN-index
          MPN-index template-segments)
         #| No existing material on either side of the
         generation time interval, so we can use the
         regular version of the function without extra
         constrains. |#
         (generate-rel-for-or-back-no-fail
          "backwards"
          (if terminal<-p
            external-final-states
            internal-final-states)
          stm<- bisection point-set-template
          template-tpc checklist beats-in-bar
          c-failures c-sources point-set-idx
          state-tonic-idx MNN-index MPN-index
          template-segments))))
    (loop for i from 1 to c-forwards do
      (loop for j from 1 to c-backwards do
        (format
         t
         (concatenate
          'string "Joining forwards excerpt ~s"
          " and backwards excerpt ~s...~%") i j)
        ; 1. Call most-probable-join for neutral.
        ; 2. Now the same for join-forwards.
        ; 3. And join-backwards.
        (loop for s in
          (list
           "neutral" "forwards-dominant"
           "backwards-dominant") do
          (let
              ((new-states&points
                (most-probable-join2joined-states&points
                 ; states->
                 (fourth
                  (gethash
                   (concatenate
                    'string "forwards,"
                    (write-to-string i))
                   forwards-candidates))
                 ; states<-
                 (fourth
                  (gethash
                   (concatenate
                    'string "backwards,"
                    (write-to-string j))
                   backwards-candidates))
                 ; points<-
                 (fifth
                  (gethash
                   (concatenate
                    'string "backwards,"
                    (write-to-string j))
                   backwards-candidates))
                 stm-> stm<- beats-in-bar template-tpc
                 s ontime-index)))
            (if new-states&points
              (progn
                (format
                 t
                 (concatenate
                  'string "Successful join: ~a~%") s)
                (setf
                 (gethash
                  (concatenate
                   'string
                   "united," (write-to-string i) ","
                   (write-to-string j) ",join-" s)
                  united-candidates)
                 new-states&points))
              (format
               t
               (concatenate
                'string "Unsuccessful join: ~a~%") s))))
        
        ; 4. Old-fashioned superimposing of points.
        ; 5. Old-fashioned forwards-dominant imposing.
        ; 6. Old-fashioned backwards-dominant imposing.
        (loop for s in
          (list
           "superimpose" "forwards-dominant"
           "backwards-dominant") do
          (setf
           (gethash
            (concatenate
             'string
             "united," (write-to-string i) ","
             (write-to-string j) "," s)
            united-candidates)
           (unite-states&points
            ; states->
            (fourth
             (gethash
              (concatenate
               'string "forwards,"
               (write-to-string i))
              forwards-candidates))
            ; states<-
            (fourth
             (gethash
              (concatenate
               'string "backwards,"
               (write-to-string j))
              backwards-candidates))
            ; points->
            (fifth
             (gethash
              (concatenate
               'string "forwards,"
               (write-to-string i))
              forwards-candidates))
            ; points<-
            (fifth
             (gethash
              (concatenate
               'string "backwards,"
               (write-to-string j))
              backwards-candidates))
            bisection beats-in-bar s)))))
    (list
     forwards-candidates backwards-candidates
     united-candidates)))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq direction "backwards")
  (setq terminalp nil)
  (setq
   external-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   internal-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "internal-final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq
   stm
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Racchman-Jun2015 example")
      :name "transition-matrix<-" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)))
  (setq no-ontimes-beyond 12)
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
   state-context-pair
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
(generate-forced-rel<->no-failure
 direction terminalp  external-states internal-states
 stm no-ontimes-beyond point-set-template '(55 57)
 generation-interval pattern-region state-context-pair
 (list "originalp") beats-in-bar 10 3)
--> (0.003563 0 14
     (((3 (-15 0 4 9))
       ("No information"
        ((18 40 48 2 3) ... (18 64 62 2 0))
        (55 57) (4 5)))
      ((5/2 (-8 2 8 11))
       ("bachChoraleBWV4p8R184"
        ((13 45 51 1 3) ... (27/2 55 57 1/2 2))
        (53 56) (2 5))) ...
      ((1 (-3 0 9 16))
       ("bachChoraleBWV353R269"
        ((20 55 57 1/2 3) ... (20 74 68 1 0))
        (58 59) (1 5))))
     ((12 52 55 1/2 3) (12 55 57 1/2 2)
      (12 64 62 1/2 1) ... (18 64 62 2 0))
     14 (0 0 0 0 0 0 0 0 0 0 0 1 0))
\end{verbatim}

\noindent This function is simiar to the function
\nameref{fun:generate-beat-spacing-forced<->}. The
difference is that this one is embedded in
Racchmaninof-Jun2015 rather than Racchmaninof-Oct2010,
and MIDI note numbers in
\nameref{fun:generate-forced-rel<->no-failure} (and
Racchman-Jun2015) are assumed to be relative to a
global tonic. The function is also similar to
\nameref{fun:generate-beat-rel-MNN<->}. The difference
is that there are some extra arguments here, which
allow for using external/internal initial/final states,
and for using information from a discovered pattern or
previous/next state to further guide the generation,
hence `forcing'. |#

(defun generate-forced-rel<->no-failure
       (direction terminalp external-states
        internal-states stm no-ontimes-beyond
        point-set-template template-tpc
        generation-interval pattern-region
        state-context-pair &optional
        (checklist (list "originalp")) (beats-in-bar 4)
        (c-failures 10) (c-sources 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict point-set-template 1 3)))
        (total-failures 0) (total-iterations 0)
        (time-a (get-internal-real-time))
        (output
         (if (string= direction "forwards")
           (generate-beat-rel-MNN-forcing->
            (if terminalp
              external-states internal-states)
            stm no-ontimes-beyond point-set-template
            template-tpc generation-interval
            pattern-region state-context-pair checklist
            beats-in-bar c-failures c-sources
            point-set-idx state-tonic-idx MNN-idx
            MPN-idx template-segments)
           (generate-beat-rel-MNN-forcing<-
            (if terminalp
              external-states internal-states)
            stm no-ontimes-beyond point-set-template
            template-tpc generation-interval
            pattern-region state-context-pair checklist
            beats-in-bar c-failures c-sources
            point-set-idx state-tonic-idx MNN-idx
            MPN-idx template-segments)))
        (time-b (get-internal-real-time)))
  (if (stringp (first output))
    (generate-forced-rel<->no-failure
     direction terminalp external-states
     internal-states stm no-ontimes-beyond
     point-set-template template-tpc
     generation-interval pattern-region
     state-context-pair checklist beats-in-bar
     c-failures c-sources point-set-idx state-tonic-idx
     MNN-idx MPN-idx template-segments
     (+ total-failures 1)
     (+ total-iterations (fourth output)) time-a)
    (append
     (list
      (float
       (/ (- time-b time-a)
          internal-time-units-per-second))
      total-failures
      (+ total-iterations (third output))) output)))

#|
\noindent Example:
\begin{verbatim}
(setq
 *rs*
 #.(CCL::INITIALIZE-MRG31K3P-STATE 1629903369
    1220234573 1692867889 1912670146 1571590464
    1100632021))
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
  (setq no-ontimes-beyond 24)
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
  (setq c-sources 4)
  (setq c-failures 10)
  (setq scale 1000)
  "Yes!")
(setq
 output
 (generate-rel-for-or-back-no-fail
  "forwards" initial-states stm no-ontimes-beyond
  dataset-all template-tpc checklist beats-in-bar
  c-failures c-sources))
(setq time-taken (first output))
--> 7.701541.
(write-to-file
 output
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2014-sample-output2" :type "txt")
  temp-path))
(saveit
 (merge-pathnames
  (make-pathname
   :name "Racchman-Jun2014-sample-output2" :type "mid")
  temp-path)
 (modify-to-check-dataset
  (mapcar
   #'(lambda (x)
       (cons
        (- (first x) (first (first (fifth output))))
        (rest x)))
   (fifth output))
 scale))
(firstn 11 (fifth output))
--> ((0 50 54 1/2 1) (0 62 61 1 0) (1/2 57 58 1/2 1)
     (1 50 54 1/2 1) (3/2 54 56 1/2 1) (2 54 56 1/2 3)
     (5/2 57 58 1/2 1) (5/2 61 60 1/2 1)
     (3 54 56 1/2 1) (7/2 55 57 1/2 1) (4 59 59 1/2 1))
\end{verbatim}

This function generates states (and realisations of
those states), taking initial (or final) states and
a forwards- (or backwards-) running stm as arguments.
The direction must be specified as the first
argument, so that the appropriate generating function
is called. The difference between this function and
\ref{fun:generate-forwards-or-backwards-no-failure} is
that the current function operates on beat of the bar
and MIDI note number relative to a global tonal centre,
whereas
\ref{fun:generate-forwards-or-backwards-no-failure}
operates on beat of the bar and MIDI note spacing.

Depending on the values of the parameters, a call to a
generating function can fail to produce a generated
passage, in which case this function runs again, until
a passage has been generated, hence `no failure'.

The output is structued as follows: the first item in
the list is the total time in seconds taken to produce
output. The second item is the number of times a call
to the generating function has failed (and so the
function has been run again). The third item is the
total number of iterations by the generating function
(e.g., total number of times the stm has been queried
to append states to a random generation Markov chain.
The fourth item is a list of the output state-context
pairs. The fifth item is the corresponding realised
point set. The sixth item is the total number of
iterations by the generating function on the most
recent (successful) run. The seventh item shows, for
the most recent successful chain, how many failures
(forced to generate alternative continuations) there
were at each state index. |#

(defun generate-rel-for-or-back-no-fail
       (direction initial-or-final-states stm
        no-ontimes-beyond dataset-template template-tpc
        &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (MNN-idx 1) (MPN-idx 2)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (total-failures 0) (total-iterations 0)
        (time-a (get-internal-real-time))
        (output
         (if (string= direction "forwards")
           (generate-beat-rel-MNN->
            initial-or-final-states stm
            no-ontimes-beyond dataset-template
            template-tpc checklist beats-in-bar
            c-failures c-sources point-set-idx
            state-tonic-idx MNN-idx MPN-idx
            template-segments)
           (generate-beat-rel-MNN<-
            initial-or-final-states stm
            no-ontimes-beyond dataset-template
            template-tpc checklist beats-in-bar
            c-failures c-sources point-set-idx
            state-tonic-idx MNN-idx MPN-idx
            template-segments)))
        (time-b (get-internal-real-time)))
  (if (stringp (first output))
    (generate-rel-for-or-back-no-fail
     direction initial-or-final-states stm
     no-ontimes-beyond dataset-template template-tpc
     checklist beats-in-bar c-failures c-sources
     point-set-idx state-tonic-idx MNN-idx MPN-idx
     template-segments
     (+ total-failures 1)
     (+ total-iterations (fourth output)) time-a)
    (append
     (list
      (float
       (/ (- time-b time-a)
          internal-time-units-per-second))
      total-failures
      (+ total-iterations (third output))) output)))

#|
\noindent Example:
\begin{verbatim}
variabless output and stm-> as in
generate-beat-rel-MNN<->.
(setq
 *rs*
 #.(CCL::INITIALIZE-MRG31K3P-STATE 660880148 974840345
    1231458079 334849240 1409987611 1407274776))
(most-plausible-join-rel (third output) 20 stm->)
--> "united,2,1,forwards-dominant".
\end{verbatim}

\noindent Docs to be finished. |#

(defun most-plausible-join-rel
       (ht-states&points join-at stm &optional
        (beats-in-bar 4) (ontime-index 0) (MNN-index 1)
        (MPN-index 2) (duration-index 3)
        (sp-keys
         (disp-ht-key ht-states&points))
        #| If there are any states-points pairs that
        have been created by joining, then locate
        these. |#
        (join-keys
         (loop for k in sp-keys when
           (search "join-" k) collect k))
        #| If not we will have to recreate states for
        each of the point sets, and test their
        membership in the stm at the join-at ontime. |#
        (unite-keys
         (if (null join-keys)
           (loop for k in sp-keys when
             (not (search "join-" k)) collect k)))
        (indices
         (if (null join-keys)
           (mapcar
            #'(lambda (x)
                (position
                 join-at
                 (nth-list-of-lists
                  ontime-index
                  (segments-strict
                   (second
                    (gethash x ht-states&points))
                   MNN-index duration-index))))
            unite-keys)))
        (states
         (if (null join-keys)
           (mapcar
            #'(lambda (x y)
                (if x
                  (nth
                   x
                   (beat-rel-MNN-states
                    (second
                     (gethash y ht-states&points))
                    "no information"
                    beats-in-bar MNN-index MPN-index
                    duration-index))))
            indices unite-keys)))
        (in-stm
         (if (null join-keys)
           (loop for i from 0
             to (- (length unite-keys) 1) when
             (and
              (nth i states)
              (assoc
               (first (nth i states))
               stm :test #'equalp))
             collect (nth i unite-keys)))))
  (if join-keys
    (choose-one join-keys)
    (if in-stm
      (choose-one in-stm)
      (choose-one unite-keys))))

#|
\noindent Example:
\begin{verbatim}
(setq
 states->
 '(((3 (-4 9 12 16))
    ("piece1"
     ((6 50 54 1 3) (6 63 62 1 2) (6 66 63 1 1)
      (6 70 66 1 0)) (53 56) (2 5)))
   ((4 (-3 9 12 16))
    ("piece2"
     ((3 51 55 1 3) (3 63 62 1 2) (3 66 63 1 1)
      (3 70 66 1 0)) (53 56) (2 5)))))
(setq
 states<-
 '(((2 (5 5 5 5))
    ("piece3"
     ((1 60 60 1 3) (1 65 63 1 2) (1 70 66 1 1)
      (1 75 69 1 0)) (53 56) (2 5)))
   ((1 (-8 4 11 19))
    ("piece3"
     ((0 48 53 1 3) (0 60 60 1 2) (0 67 64 1 1)
      (0 75 69 1 0)) (53 56) (2 5)))))
(setq points<- '((20 "MNN" "MPN" "etc")))
(setq stm-> nil) ; Simulate with s->.
(setq stm<- nil) ; Simulate with s<-.
(setq beats-in-bar 4)
(setq template-tpc '(67 64))
(setq direction "neutral")
(setq ontime-index 0)
(setq final-ontime 3)
(setq
 s->
 '((4 (-3 9 12 16))
   (((9/2 (-3 12 16 7))
     ("bachChoraleBWV4p8R184"
      ((15 50 54 1 3) (15 65 63 1 1) (15 69 65 1 0)
       (31/2 60 60 1/2 2)) (53 56) (2 5)))
    ((9/2 (-3 12 16 7))
     ("bachChoraleBWV4p8R184"
      ((-1 50 54 1 3) (-1 65 63 1 1) (-1 69 65 1 0)
       (-1/2 60 60 1/2 2)) (53 56) (2 5))))))
(setq
 s<-
 '((1 (-8 4 11 19))
   (((4 (-9 6 11 21))
     ("bachChoraleBWV4p8R184"
      ((19 44 50 1 3) (19 59 59 1 2) (19 64 62 1 1)
       (19 74 68 1 0)) (53 56) (2 5)))
    ((9/2 (-3 12 16 7))
     ("bachChoraleBWV4p8R184"
      ((15 50 54 1 3) (15 65 63 1 1) (15 69 65 1 0)
       (31/2 60 60 1/2 2)) (53 56) (2 5)))
    ((4 (-9 6 11 21))
     ("bachChoraleBWV4p8R184"
      ((3 44 50 1 3) (3 59 59 1 2) (3 64 62 1 1)
       (3 74 68 1 0)) (53 56) (2 5))))))
(most-probable-join2joined-states&points
 states-> states<- points<- stm-> stm<- beats-in-bar
 template-tpc direction ontime-index final-ontime
 (list (first states->)) (list (first states<-))
 s-> s<-)
--> ((((3 (-4 9 12 16))
       ("piece1"
        ((6 50 54 1 3) (6 63 62 1 2) (6 66 63 1 1)
         (6 70 66 1 0)) (53 56) (2 5)))
      ((9/2 (-3 12 16 7))
       ("bachChoraleBWV4p8R184"
        ((-1 50 54 1 3) (-1 65 63 1 1) (-1 69 65 1 0)
         (-1/2 60 60 1/2 2)) (53 56) (2 5)))
      ((2 (5 5 5 5))
       ("piece3"
        ((1 60 60 1 3) (1 65 63 1 2) (1 70 66 1 1)
         (1 75 69 1 0)) (53 56) (2 5))))
     ((0 64 62 3/2 3) (0 77 70 3/2 2) (0 80 71 3/2 1)
      (0 84 74 3/2 0) (3/2 64 62 3/2 3)
      (3/2 79 71 3/2 1) (3/2 83 73 3/2 0)
      (3/2 74 68 3/2 2) (3 74 68 1 3) (3 79 71 1 2)
      (3 84 74 1 1) (3 89 77 1 0))).
\end{verbatim}

This function searches for a state that is a valid
continuation for both forwards- and
backwards-generated states. If there are multiple such
states, one is chosen at random.

This function is more complicated than
\ref{fun:unite-states&points} and is more likely to
produce plausible-sounding output, but has the
disadvantage that it is not always possible to find
such a joining state. |#

(defun most-probable-join2joined-states&points
       (states-> states<- points<- stm-> stm<-
        beats-in-bar template-tpc
        &optional
        (direction "neutral")
        (ontime-index 0)
        #| It's unlikely but not impossible that this
        method of joining creates an extra measure of
        music. (Maybe it's even possible it creates one
        measure less of music -- I can't imagine so.)
        To be on the safe side, we record the final
        ontime of the incoming data here, to check
        whether an extra measure is created. If it is,
        the output will be nullified. |#
        (final-ontime
         (nth ontime-index (my-last points<-)))
        (states->
         (if (and
              (string= direction "neutral")
              (> (length states->) 1))
           (butlast states->)
           (if (and
                (string= direction "backwards-dominant")
                (> (length states->) 2))
             (butlast (butlast states->)) states->)))
        (states<-
         (if (and
              (string= direction "neutral")
              (> (length states<-) 1))
           #| Recall the first element of states<- is
           the last to sound, which is why butlast is
           called again here. |#
           (butlast states<-)
           (if (and
                (string= direction "forwards-dominant")
                (> (length states<-) 2))
             (butlast (butlast states<-)) states<-)))
        (s->
         (assoc
          (first
           (my-last states->)) stm-> :test #'equalp))
        (s<-
         (assoc
          (first
           (my-last states<-)) stm<- :test #'equalp))
        #| See if there are any instances of the same
        state in the continuations of forwards- and
        backwards-running processes. |#
        (intersecting-states
         (intersection
          (nth-list-of-lists 0 (second s->))
          (nth-list-of-lists 0 (second s<-))
          :test #'equalp))
        #| If there are, select one at random. |#
        (join-state (choose-one intersecting-states))
        #| Get the state-context pair from whichever
        forwards- or backwards-running process is
        specified. |#
        (state-context-pair
         (if join-state
           (if (or
                (string= direction "neutral")
                (string= direction "forwards-dominant"))
             (choose-one
              (loop for i from 0
                to (- (length (second s->)) 1) when
                (equalp
                 join-state
                 (first (nth i (second s->))))
                collect (nth i (second s->))))
             (if (string= direction "backwards-dominant")
               (choose-one
                (loop for i from 0
                  to (- (length (second s<-)) 1) when
                  (equalp
                   join-state
                   (first (nth i (second s<-))))
                  collect (nth i (second s<-))))
               "No direction specified."))
           "No join state detected."))
        (new-states
         (if join-state
           (append
            states-> (list state-context-pair)
            (reverse states<-))))
        (new-points
         (if new-states
           (states2datapoints-by-rel
            new-states beats-in-bar (first template-tpc)
            (second template-tpc))))
        (final-ontime-OK
         (if new-points
           (equalp
            (nth ontime-index (my-last new-points))
            final-ontime))))
  (if final-ontime-OK (list new-states new-points)))
    
#| First attempt.
(defun most-probable-join2joined-states
       (states-> states<- stm-> stm<- &optional
        (direction "forwards-dominant")
        (states->
         (if (string= direction "backwards-dominant")
           (butlast (butlast states->)) states->))
        (states<-
         (if (string= direction "forwards-dominant")
           #| Recall the first element of states<- is
           the last to sound, which is why butlast is
           called again here. |#
           (butlast (butlast states<-)) states<-))
        (s->
         (assoc
          (first
           (my-last states->)) stm-> :test #'equalp))
        (s<-
         (assoc
          (first
           (my-last states<-)) stm<- :test #'equalp))
        #| See if there are any instances of the same
        state in the continuations of forwards- and
        backwards-running processes. |#
        (intersecting-states
         (intersection
          (nth-list-of-lists 0 (second s->))
          (nth-list-of-lists 0 (second s<-))
          :test #'equalp))
        #| If there are, select one at random. |#
        (join-state (choose-one intersecting-states))
        #| Get the state-context pair from whichever
        forwards- or backwards-running process is
        specified. |#
        (state-context-pair
         (if join-state
           (if (string= direction "forwards-dominant")
             (choose-one
              (loop for i from 0
                to (- (length (second s->)) 1) when
                (equalp
                 join-state
                 (first (nth i (second s->))))
                collect (nth i (second s->))))
             (if (string= direction "backwards-dominant")
               (choose-one
                (loop for i from 0
                  to (- (length (second s<-)) 1) when
                  (equalp
                   join-state
                   (first (nth i (second s<-))))
                  collect (nth i (second s<-))))
               "No direction specified."))
           "No join state detected.")))
  (if join-state
    (append
     states-> (list state-context-pair)
     (reverse states<-))))
|#



#|
\noindent Example:
\begin{verbatim}
(setq
 states->
 '(((2 (-5 7 11 14))
    ("bachChoraleBWV268R124" "ps here" (55 57) (4 5)))
   ((5/2 (7 14 -1 14))
    ("bachChoraleBWV115p6R38" "ps here" (51 55) (-3 0)))
   ((3 (0 7 12 16))
    ("bachChoraleBWV115p6R38" "ps here" (51 55) (-3 0)))
   ((7/2 (7 12 16 -1))
    ("bachChoraleBWV226R69" "ps here" (55 57) (1 0)))
   ((4 (-3 0 12 18))
    ("bachChoraleBWV226R69" "ps here" (55 57) (1 0)))
   ((1 (-5 2 11 19))
    ("bachChoraleBWV226R69" "ps here" (55 57) (1 0)))))
(setq
 points->
 '((17 62 61 1/2 3) (17 74 68 1/2 2) (17 78 70 1/2 1)
   (17 81 72 1 0) (35/2 66 63 1/2 3) (35/2 74 68 3/2 2)
   (18 67 64 1/2 3) (18 79 71 2 1) (18 83 73 1 0)
   (37/2 66 63 1/2 3) (19 64 62 1 3) (19 67 64 1 2)
   (19 85 74 1 0) (20 62 61 2 3) (20 69 65 2 2)
   (20 78 70 2 1) (20 86 75 2 0)))
(setq
 states<-
 '(((1 (0 7 12 16))
    ("bachChoraleBWV226R69" "ps here" (55 57) (1 0)))
   ((9/2 (7 14 -1 14))
    ("bachChoraleBWV115p6R38" "ps here" (51 55) (-3 0)))
   ((4 (-5 7 11 14))
    ("bachChoraleBWV115p6R38" "ps here" (51 55) (-3 0)))
   ((7/2 (14 -3 6 12))
    ("bachChoraleBWV268R124" "ps here" (55 57) (4 5)))
   ((3 (-1 7 11 14))
    ("bachChoraleBWV268R124" "ps here" (55 57) (4 5)))
   ((1 (-12 4 7 12))
    ("bachChoraleBWV259R39" "ps here" (55 57) (4 5)))))
(setq
 points<-
 '((20 55 57 2 3) (20 71 66 2 2) (20 74 68 2 1)
   (20 79 71 2 0) (22 66 63 1/2 3) (22 74 68 1/2 2)
   (22 78 70 1/2 1) (22 81 72 2 0) (45/2 64 62 1/2 3)
   (45/2 73 67 1/2 2) (45/2 79 71 1/2 1)
   (23 62 61 1/2 3) (23 74 68 2 2) (23 78 70 1/2 1)
   (47/2 66 63 1/2 3) (24 67 64 1/2 3) (24 79 71 2 1)
   (24 83 73 1 0)))
(setq join-at 20)
(setq beats-in-bar 4)
(unite-states&points
 states-> states<- points-> points<- join-at
 beats-in-bar "superimpose")
--> ((((2 (-5 7 11 14))
       ("bachChoraleBWV268R124" "ps here" (55 57) (4 5)))
      ...
      ((1 (-5 2 11 19))
       ("bachChoraleBWV226R69" "ps here" (55 57) (1 0)))
      ((1 (55 62 69 71 74 78 79 86))
       ("bachChoraleBWV226R69+bachChoraleBWV259R39"))
      ((1 (-12 4 7 12))
       ("bachChoraleBWV259R39" "ps here" (55 57) (4 5)))
      ...
      ((9/2 (7 14 -1 14))
       ("bachChoraleBWV115p6R38" "ps here" (51 55) (-3 0)))
      ((1 (0 7 12 16))
       ("bachChoraleBWV226R69" "ps here" (55 57) (1 0))))
     ((17 62 61 1/2 3) (17 74 68 1/2 2) (17 78 70 1/2 1)
      ... (20 55 57 2 3) (20 62 61 2 3) (20 69 65 2 2)
      (20 71 66 2 2) (20 74 68 2 1) (20 78 70 2 1)
      (20 79 71 2 0) (20 86 75 2 0) (22 66 63 1/2 3)
      (22 74 68 1/2 2) (22 78 70 1/2 1) (22 81 72 2 0)
      ... (47/2 66 63 1/2 3) (24 67 64 1/2 3)
      (24 79 71 2 1) (24 83 73 1 0))).
\end{verbatim}

\noindent This function unites forwards- and
backwards-generated sets of states and corresponding
points sets. Its output is a list of two items: the
united states and point set. The first optional
argument join-by controls how the two sets are united.
If join-by is set to `superimpose', a new state at the
joining ontime will be formed by combining points from
each set (creating some interesting chords!). If states
do exist at this joining ontime, they will appear
either side of this new state in the output states. If
join-by is set to `forwards-dominant' (or
`backwards-dominant'), the backwards- (or forwards-)
generated points are truncated so that a new state is
not formed.

This function is more straightforward than
\ref{fun:most-probable-join2joined-states&points} and
always produces output, but is less likely to produce
plausible-sounding output. |#
 
(defun unite-states&points
       (states-> states<- points-> points<- join-at
        beats-in-bar &optional
        (join-by "superimpose")
        (MNN-index 1) (duration-index 3)
        (trimmed-points->
         (remove-datapoints-with-nth-item>
          points-> join-at 0))
        (trimmed-points<-
         (remove-datapoints-with-nth-item<
          points<- join-at 0))
        (trimmed-points->=
         (remove-datapoints-with-nth-item>=
          points-> join-at 0))
        (trimmed-points<-=
         (remove-datapoints-with-nth-item<=
          points<- join-at 0))
        #| Using segments-strict, we need to work out
        whether trimmed-points-> or trimmed-points<- are
        any shorter than points-> and points<-
        respectively. If so, states-> and states<-
        need to be adjusted accordingly. |#
        (diff->
         (-
          (length
           (segments-strict
            points-> MNN-index duration-index))
          (length
           (segments-strict
            trimmed-points-> MNN-index
            duration-index))))
        (diff<-
         (-
          (length
           (segments-strict
            points<- MNN-index duration-index))
          (length
           (segments-strict
            trimmed-points<- MNN-index
            duration-index))))
        (diff->=
         (-
          (length
           (segments-strict
            points-> MNN-index duration-index))
          (length
           (segments-strict
            trimmed-points->= MNN-index
            duration-index))))
        (diff<-=
         (-
          (length
           (segments-strict
            points<- MNN-index duration-index))
          (length
           (segments-strict
            trimmed-points<-= MNN-index
            duration-index))))
        (join-state&context
         (list
          (list
           (+ (mod join-at beats-in-bar) 1)
           (remove-duplicates
            (nth-list-of-lists
             MNN-index
             (points-sounding-at
              (sort-dataset-asc
               (append-offtimes
                (append
                 trimmed-points-> trimmed-points<-)
                duration-index)) join-at))
            :test #'equalp))
          (list
           (concatenate
            'string
            (first
             (second
              (nth
               (-
                (- (length states->) diff->) 1)
               states->)))
            "+"
            (first
             (second
              (nth
               (-
                (- (length states<-) diff<-) 1)
               states<-))))))))
  (if (string= join-by "superimpose")
    (list
     (append
      (subseq states-> 0 (- (length states->) diff->))
      (list join-state&context)
      (reverse
       (subseq 
        states<- 0 (- (length states<-) diff<-))))
     (sort-dataset-asc
      (append
       trimmed-points->
       (remove-coincident-datapoints
        trimmed-points-> trimmed-points<- MNN-index
        duration-index))))
    (if (string= join-by "forwards-dominant")
      (list
       (append
        (subseq states-> 0 (- (length states->) diff->))
        (reverse
         (subseq 
          states<- 0 (- (length states<-) diff<-=))))
       (sort-dataset-asc
        (append
         trimmed-points->
         (remove-coincident-datapoints
          trimmed-points-> trimmed-points<-=
          MNN-index duration-index))))
      (list
       (append
        (subseq states-> 0 (- (length states->) diff->=))
        (reverse
         (subseq 
          states<- 0 (- (length states<-) diff<-))))
       (sort-dataset-asc
        (append
         trimmed-points<-
         (remove-coincident-datapoints
          trimmed-points<- trimmed-points->=
          MNN-index duration-index)))))))
