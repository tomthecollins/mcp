#| Copyright 2008-2013 Tom Collins
   Wednesday 13 October 2010
   Incomplete

\noindent The main function here is called generate-
beat-MNN-spacing<->. Given initial and final states,
forwards- and backwards-running state-transition
matrices, and a template dataset, it generates
datapoints (among other output) that conform to
various criteria, which can be specified using the
optional arguments. The idea is to join forwards- and
backwards-generated phrases, choosing whichever pair
leads to a phrase whose mean deviation from the
template likelihood profile is minimal.

The criteria are things like: not too many consecutive
states from the same source, the range is comparable
with that of the template, and the likelihood of the
states is comparable with that of the template.

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
   :name "generating-beat-MNN-spacing-forwards"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "hash-tables"
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
|#

#|
\noindent Example:
\begin{verbatim}
(defvar *rs* (make-random-state t))
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
   stm->
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix.txt")))
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
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-59-3-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 48 184))
  "Yes!")
(setq
 checklist
 (list "originalp" "mean&rangep" "likelihoodp"))
;(setq checklist (list "originalp" "mean&rangep"))
;(setq checklist (list "originalp"))
(setq beats-in-bar 3)
(setq c-failures 10)
(setq c-sources 3)
(setq c-bar 19)
(setq c-min 12)
(setq c-max 12)
(setq c-beats 12)
(setq c-prob 0.15)
(setq c-forwards 3)
(setq c-backwards 3)
(progn
  (setq time-a (get-internal-real-time))
  (setq
   output
   (generate-beat-MNN-spacing<->
    initial-states stm-> final-states stm<-
    dataset-template checklist beats-in-bar c-failures
    c-sources c-min c-max c-bar c-beats c-prob
    c-forwards c-backwards))
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

\noindent This function unites several forwards- and
backwards running realisations of Markov models built
on the arguments initial-states, stm->, final-states,
and stm<-. It is constrained by a template (the
argument dataset-template) and various parameters:
like not too many consecutive states from the same
source (c-sources), the range is comparable with that
of the template (c-bar, c-min, and c-max), and the
likelihood of the states is comparable with that of
the template (c-beats and c-prob).

The numbers of forwards- and backwards- realisations
generated are determined by the arguments c-forwards
and c-backwards respectively. The output is a list of
three hash tables (one containing the forwards
candidates, one the backwards candidates, and one the
united candidates). If c-forwards = m and c-backwards
= n, then the number of united candidates is 3mn. |#

(defun generate-beat-MNN-spacing<->
       (initial-states stm-> final-states stm<-
        dataset-template &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (pitch-index 1) (duration-index 3)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
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
      (setf
       (gethash
        (concatenate
         'string "forwards," (write-to-string i))
        forwards-candidates)
       (generate-forwards-or-backwards-no-failure
        "forwards" initial-states stm-> bisection
        dataset-template checklist beats-in-bar
        c-failures c-sources c-bar c-min c-max c-beats
        c-prob template-segments)))
    (loop for i from 1 to c-backwards do
      (setf
       (gethash
        (concatenate
         'string "backwards," (write-to-string i))
        backwards-candidates)
       (generate-forwards-or-backwards-no-failure
        "backwards" final-states stm<- bisection
        dataset-template checklist beats-in-bar
        c-failures c-sources c-bar c-min c-max c-beats
        c-prob template-segments)))
    (loop for i from 1 to c-forwards do
      (loop for j from 1 to c-backwards do
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",superimpose")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "superimpose" pitch-index
          duration-index))
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",forwards-dominant")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "forwards-dominant" pitch-index
          duration-index))
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",backwards-dominant")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "backwards-dominant" pitch-index
          duration-index))))
    (list
     forwards-candidates backwards-candidates
     united-candidates)))

#|
\noindent Example:
\begin{verbatim}
(defvar *rs* (make-random-state t))
(setq
 *rs* #.(CCL::INITIALIZE-RANDOM-STATE 17947 1744))
(progn
  (setq generation-interval '(12 24))
  (setq terminal->p nil)
  (setq terminal<-p nil)
  (setq
   external-initial-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-initial-states.txt")))
  (setq
   internal-initial-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-internal-initial-states-all.txt")))
  (setq
   stm->
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix.txt")))
  (setq
   external-final-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-final-states.txt")))
  (setq
   internal-final-states
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-internal-final-states-all.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix<-.txt")))
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-56-1-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 0 132))
  "Yes!")
(setq
 pattern-region
 '((12 60 60) (12 64 62) (25/2 57 58) (51/4 64 62)
   (13 52 55) (13 64 62) (14 54 56) (29/2 62 61)
   (15 55 57) (15 59 59) (63/4 64 62) (16 43 50)
   (16 50 54) (16 66 63) (17 67 64) (18 57 58)
   (18 60 60) (18 64 62) (75/4 66 63) (19 43 50)
   (19 50 54) (19 67 64) (20 66 63) (20 69 65)
   (21 59 59) (21 67 64) (21 71 66) (87/4 74 68)
   (22 43 50) (22 50 54) (22 74 68) (23 62 61)
   (24 60 60) (24 64 62)))
(setq state-context-pair-> nil)
(setq state-context-pair<- nil)
(setq
 checklist
 (list "originalp" "mean&rangep" "likelihoodp"))
;(setq checklist (list "originalp" "mean&rangep"))
;(setq checklist (list "originalp"))
(setq beats-in-bar 3)
(setq c-failures 10)
(setq c-sources 4)
(setq c-bar 19)
(setq c-min 12)
(setq c-max 12)
(setq c-beats 12)
(setq c-prob 0.15)
(setq c-forwards 3)
(setq c-backwards 3)
(progn
  (setq time-a (get-internal-real-time))
  (setq
   output
   (generate-beat-spacing-forced<->
    generation-interval terminal->p terminal<-p
    external-initial-states internal-initial-states
    stm-> external-final-states internal-final-states
    stm<- dataset-template pattern-region
    state-context-pair-> state-context-pair<-
    checklist beats-in-bar c-failures
    c-sources c-min c-max c-bar c-beats c-prob
    c-forwards c-backwards))
  (setq time-b (get-internal-real-time))
  (float
   (/
    (- time-b time-a)
    internal-time-units-per-second)))
--> .
(setq
 output-datapoints
 (gethash '"united,2,3,superimpose" (third output)))
(saveit
 (concatenate
  'string
  "/Applications/CCL/Lisp code/Markov models"
  "/Testing/united,2,3,superimpose.mid")
 (modify-to-check-dataset
  (translation
   output-datapoints
   (list
    (- 0 (first (first output-datapoints)))
    0 0 0 0)) 1500))
\end{verbatim}

\noindent This function is similar to the function
generate-beat-MNN-spacing<->. The difference is that
there are some extra arguments here, which allow for
using either external or internal initial/final
states, and for using information from a discovered
pattern or previous/next state to further guide the
generation.

The function unites several forwards- and backwards
running realisations of Markov models built on the
arguments initial-states, stm->, final-states, and
stm<-. It is constrained by a template (the argument
dataset-template) and various parameters: like not too
many consecutive states from the same source
(c-sources), the range is comparable with that of the
template (c-bar, c-min, and c-max), and the likelihood
of the states is comparable with that of the template
(c-beats and c-prob).

The numbers of forwards- and backwards- realisations
generated are determined by the arguments c-forwards
and c-backwards respectively. The output is a list of
three hash tables (one containing the forwards
candidates, one the backwards candidates, and one the
united candidates). If c-forwards = m and c-backwards
= n, then the number of united candidates is 3mn. |#

(defun generate-beat-spacing-forced<->
       (generation-interval terminal->p terminal<-p
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        state-context-pair-> state-context-pair<-
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (pitch-index 1) (duration-index 3)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
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
      (setf
       (gethash
        (concatenate
         'string "forwards," (write-to-string i))
        forwards-candidates)
       (if pattern-region
         (generate-forced<->no-failure
          "forwards" terminal->p
          external-initial-states
          internal-initial-states stm-> bisection
          dataset-template generation-interval
          pattern-region state-context-pair->
          checklist beats-in-bar c-failures c-sources
          c-bar c-min c-max c-beats c-prob
          template-segments)
         (generate-forwards-or-backwards-no-failure
          "forwards"
          (if terminal->p
            external-initial-states
            internal-initial-states)
          stm-> bisection dataset-template checklist
          beats-in-bar c-failures c-sources c-bar
          c-min c-max c-beats c-prob
          template-segments))))
    (loop for i from 1 to c-backwards do
      (setf
       (gethash
        (concatenate
         'string "backwards," (write-to-string i))
        backwards-candidates)
       (if pattern-region
         (generate-forced<->no-failure
          "backwards" terminal<-p
          external-final-states internal-final-states
          stm<- bisection dataset-template
          generation-interval pattern-region
          state-context-pair<- checklist beats-in-bar
          c-failures c-sources c-bar c-min c-max
          c-beats c-prob template-segments)
         (generate-forwards-or-backwards-no-failure
          "backwards"
          (if terminal->p
            external-final-states
            internal-final-states)
          stm<- bisection dataset-template checklist
          beats-in-bar c-failures c-sources c-bar
          c-min c-max c-beats c-prob
          template-segments))))
    (loop for i from 1 to c-forwards do
      (loop for j from 1 to c-backwards do
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",superimpose")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "superimpose" pitch-index
          duration-index))
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",forwards-dominant")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "forwards-dominant" pitch-index
          duration-index))
        (setf
         (gethash
          (concatenate
           'string
           "united," (write-to-string i) ","
           (write-to-string j) ",backwards-dominant")
          united-candidates)
         (unite-datapoints
          (fifth
           (gethash
            (concatenate
             'string "forwards," (write-to-string i))
            forwards-candidates))
          (fifth
           (gethash
            (concatenate
             'string "backwards," (write-to-string j))
            backwards-candidates))
          bisection "backwards-dominant" pitch-index
          duration-index))))
    (list
     forwards-candidates backwards-candidates
     united-candidates)))

#|
\noindent Example:
\begin{verbatim}
(defvar *rs* (make-random-state t))
(progn
  (setq
   internal-states->
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-internal-initial-states-all.txt")))
  (setq
   internal-states<-
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-internal-final-states-all.txt")))
  (setq
   stm->
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     "/Applications/CCL/Lisp code/Markov models"
     "/Chopin mazurka model (kern scores)"
     "/chopin-transition-matrix<-.txt")))
  (setq no-ontimes> 29)
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-17-1-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 0 250))
  "Yes!")
(setq
 pattern-region
 '((25 60 60) (25 67 64) (25 70 66) (25 76 69)
   (25 82 73) (53/2 60 60) (53/2 67 64) (53/2 70 66)
   (53/2 76 69) (53/2 81 72) (27 60 60) (27 67 64)
   (27 70 66) (27 76 69) (27 78 70) (111/4 60 60)
   (111/4 67 64) (111/4 70 66) (111/4 76 69)
   (111/4 79 71) (28 60 60) (28 67 64) (28 70 66)
   (28 76 69) (28 81 72) (29 60 60) (29 67 64)
   (29 70 66) (29 76 69) (29 79 71)))
(setq
 checklist
 (list "originalp" "mean&rangep" "likelihoodp"))
;(setq checklist (list "originalp" "mean&rangep"))
;(setq checklist (list "originalp"))
(setq beats-in-bar 3)
(setq c-failures 10)
(setq c-sources 4)
(setq c-bar 19)
(setq c-min 12)
(setq c-max 12)
(setq c-beats 12)
(setq c-prob 0.2)
(progn
  (setq
   output
   (generate-forced<->no-failure
    "forwards" nil nil internal-states-> stm->
    no-ontimes> dataset-template pattern-region nil
    checklist beats-in-bar c-failures
    c-sources c-bar c-min c-max c-beats c-prob))
  (first output))
--> .
(if (listp (fifth output))
  (saveit
   (concatenate
    'string
    "/Applications/CCL/Lisp code/Markov models"
    "/Testing/test.mid")
   (modify-to-check-dataset
    (translation
     (fifth output)
     (list
      (- 0 (first (first (fifth output))))
      0 0 0 0)) 1500)))
\end{verbatim}

\noindent This function is similar to the function
generate-forwards-or-backwards-no-failure. The
difference is that this can take a pattern or a 
previous or next state as extra constraints. This is
necessary when generating a passage according to a
template. It generates states (and realisations of
those states), taking initial (or final) states and
a forwards- (or backwards-) running stm as arguments.
The direction must be specified as the first
argument, so that the appropriate generating function
is called. Depending on the values of the
parameters, a call to a generating function can fail
to produce a generated passage, in which case this
function runs again, until a passage has been
generated, hence `no failure'. |#

(defun generate-forced<->no-failure
       (direction terminalp external-states
        internal-states stm no-ontimes-beyond
        dataset-template generation-interval
        pattern-region state-context-pair &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (total-failures 0) (total-iterations 0)
        (time-a (get-internal-real-time))
        (output
         (if (string= direction "forwards")
           (generate-beat-spacing-forcing->
            (if terminalp
              external-states internal-states)
            stm no-ontimes-beyond dataset-template
            generation-interval pattern-region
            state-context-pair checklist beats-in-bar
            c-failures c-sources c-bar c-min c-max
            c-beats c-prob template-segments)
           (generate-beat-spacing-forcing<-
            (if terminalp
              external-states internal-states)
            stm no-ontimes-beyond dataset-template
            generation-interval pattern-region
            state-context-pair checklist beats-in-bar
            c-failures c-sources c-bar c-min c-max
            c-beats c-prob template-segments)))
        (time-b (get-internal-real-time)))
  (if (stringp (first output))
    (generate-forced<->no-failure
     direction terminalp external-states
     internal-states stm no-ontimes-beyond
     dataset-template generation-interval
     pattern-region state-context-pair
     checklist beats-in-bar c-failures c-sources c-bar
     c-min c-max c-beats c-prob template-segments
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
(defvar *rs* (make-random-state t))
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
  (setq no-ontimes> 24)
  (setq
   dataset-all
   (read-from-file
    "/Users/tec69/Open/Music/Datasets/C-59-3-ed.txt"))
  (setq
   dataset-template
   (subseq dataset-all 48 184))
  "Yes!")
(setq
 checklist
 (list "originalp" "mean&rangep" "likelihoodp"))
;(setq checklist (list "originalp" "mean&rangep"))
;(setq checklist (list "originalp"))
(setq beats-in-bar 3)
(setq c-failures 10)
(setq c-sources 3)
(setq c-bar 15)
(setq c-min 10)
(setq c-max 10)
(setq c-beats 12)
(setq c-prob 0.1)
(progn
  (setq
   output
   (generate-forwards-or-backwards-no-failure
    "forwards" initial-states stm no-ontimes>
    dataset-template checklist beats-in-bar c-failures
    c-sources c-bar c-min c-max c-beats c-prob))
  (first output))
--> .
(if (listp (fifth output))
  (saveit
   (concatenate
    'string
    "/Applications/CCL/Lisp code/Markov models"
    "/Testing/test.mid")
   (modify-to-check-dataset
    (translation
     (fifth output)
     (list
      (- 0 (first (first (fifth output))))
      0 0 0 0)) 1500)))
\end{verbatim}

This function generates states (and realisations of
those states), taking initial (or final) states and
a forwards- (or backwards-) running stm as arguments.
The direction must be specified as the first
argument, so that the appropriate generating function
is called. Depending on the values of the
parameters, a call to a generating function can fail
to produce a generated passage, in which case this
function runs again, until a passage has been
generated, hence `no failure'. |#

(defun generate-forwards-or-backwards-no-failure
       (direction initial-or-final-states stm
        no-ontimes-beyond dataset-template &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1)
        (template-segments
         (butlast
          (segments-strict dataset-template 1 3)))
        (total-failures 0) (total-iterations 0)
        (time-a (get-internal-real-time))
        (output
         (if (string= direction "forwards")
           (generate-beat-MNN-spacing->
            initial-or-final-states stm
            no-ontimes-beyond dataset-template
            checklist beats-in-bar c-failures
            c-sources c-bar c-min c-max c-beats
            c-prob template-segments)
           (generate-beat-MNN-spacing<-
            initial-or-final-states stm
            no-ontimes-beyond dataset-template
            checklist beats-in-bar c-failures
            c-sources c-bar c-min c-max c-beats
            c-prob template-segments)))
        (time-b (get-internal-real-time)))
  (if (stringp (first output))
    (generate-forwards-or-backwards-no-failure
     direction initial-or-final-states stm
     no-ontimes-beyond dataset-template checklist
     beats-in-bar c-failures c-sources c-bar c-min
     c-max c-beats c-prob template-segments
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
(setq A (make-hash-table :test #'equal))
(setf
 (gethash '"cand,1,1,superimpose" A)
 '((3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 67 64 1 0)))
(setf
 (gethash '"cand,1,2,superimpose" A)
 '((0 60 60 1 0) (3/4 64 62 1 0) (2 67 64 1 0)))
(setf
 (gethash '"cand,2,1,superimpose" A)
 '((3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 69 65 1 0)))
(setf
 (gethash '"cand,2,2,superimpose" A)
 '((3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 72 67 1 0)))
(setq
 dataset-keys
 '("cand,1,1,superimpose" "cand,1,2,superimpose"
   "cand,2,1,superimpose" "cand,2,2,superimpose"))
(setq ontime 3/4)
(setq
 stm
 '(((7/4 (4 5)) "etc") ((7/4 (4 3)) "etc")
   ((7/4 (4 3 17)) "etc")))
(keys-of-states-in-transition-matrix
 A dataset-keys ontime stm "beat-spacing-states" 3 3
 1)
--> ("cand,1,1,superimpose" "cand,2,1,superimpose").
\end{verbatim}

\noindent This function takes a hash table consisting
of datasets and a list of keys for that hash table as
its first two arguments. The function state-in-
transition-matrixp is applied to the dataset
associated with each key, and if it is in the state-
transition matrix, this key is included in the
returned list. This function can be used to check
that the composer actually wrote the chords that are
created at the bisection. |#

(defun keys-of-states-in-transition-matrix
       (hash-table-of-datasets dataset-keys ontime
        stm state-fn &optional (duration-index 3)
        (beats-in-bar 4) (sort-index 1)
        (dataset
         (if dataset-keys
           (gethash
            (first dataset-keys)
            hash-table-of-datasets))))
  (if (null dataset-keys) ()
    (append
     (if (state-in-transition-matrixp
          dataset ontime stm state-fn duration-index
          beats-in-bar sort-index)
       (list (first dataset-keys)))
     (keys-of-states-in-transition-matrix
      hash-table-of-datasets (rest dataset-keys)
      ontime stm state-fn duration-index beats-in-bar
      sort-index))))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-hash-table :test #'equal))
(setf
 (gethash '"cand,1,1,superimpose" A)
 '((0 60 60 3/4 0) (0 64 62 3/4 0) (0 67 64 3/4 0)
   (3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 67 64 1 0)
   (7/4 60 60 1 0) (7/4 64 62 1 0) (7/4 67 64 1 0)))
(setf
 (gethash '"cand,1,2,superimpose" A)
 '((0 60 60 1 0) (3/4 64 62 1 0) (2 67 64 1 0)))
(setf
 (gethash '"cand,2,1,superimpose" A)
 '((0 61 60 3/4 0) (0 65 62 3/4 0) (0 68 64 3/4 0)
   (3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 67 64 1 0)
   (7/4 62 61 1 0) (7/4 66 63 1 0) (7/4 69 65 1 0)))
(setf
 (gethash '"cand,2,2,superimpose" A)
 '((3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 72 67 1 0)))
(setq
 dataset-keys
 '("cand,1,1,superimpose" "cand,1,2,superimpose"
   "cand,2,1,superimpose" "cand,2,2,superimpose"))
(setq
 template-dataset
 '((0 60 60 3/4 0) (0 64 62 3/4 0) (0 67 64 3/4 0)
   (3/4 59 59 1 0) (3/4 62 61 1 0) (3/4 67 64 1 0)
   (7/4 60 60 1 0) (7/4 64 62 1 0) (7/4 67 64 1 0)))
(setq c-beats 12)
(min-max-abs-diffs-for-likelihood-profiles
 A dataset-keys template-dataset c-beats)
--> "cand,1,1,superimpose".
\end{verbatim}

\noindent This function takes a hash table consisting
of datasets and a list of keys for that hash table as
its first two arguments. Its third argument is the
dataset for a template. The idea is to compare each
of the likelihood profiles for the datasets
associated with the keys with the likelihood profile
of the template dataset, using the function abs-
differences-for-curves-at-points. Each comparison will
produce a maximal difference. The key of the dataset
that produces the minimum of the maximal differences
is returned, and the intuition is that this will be
the most plausible dataset, compared with the
template. |#

(defun min-max-abs-diffs-for-likelihood-profiles
       (hash-table-of-datasets dataset-keys
        template-dataset &optional (c-beats 12)
        (template-segments
         (if dataset-keys
           (segments-strict template-dataset 1 3)))
        (template-likelihood-profile
         (if template-segments
           (geom-mean-likelihood-of-states
            template-segments template-dataset
            c-beats)))
        (result nil)
        (candidate-dataset
         (if dataset-keys
           (gethash
            (first dataset-keys)
            hash-table-of-datasets)))
        (candidate-segments
         (if candidate-dataset
           (segments-strict candidate-dataset 1 3)))
        (candidate-likelihood-profile
         (if candidate-segments
           (geom-mean-likelihood-of-states
            candidate-segments candidate-dataset
            c-beats)))
        (max-abs-diff
         (if (and
              candidate-likelihood-profile
              template-likelihood-profile)
           (max-item
            (abs-differences-for-curves-at-points
             candidate-likelihood-profile
             template-likelihood-profile)))))
  (if (null dataset-keys)
    (nth
     (second
      (min-argmin (nth-list-of-lists 1 result)))
     (nth-list-of-lists 0 result))
    (min-max-abs-diffs-for-likelihood-profiles
     hash-table-of-datasets (rest dataset-keys)
     template-dataset c-beats template-segments
     template-likelihood-profile
     (if max-abs-diff
       (append
        result
        (list
         (list (first dataset-keys) max-abs-diff)))
       result))))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-hash-table :test #'equal))
(setf
 (gethash '"cand,1,1,superimpose" A)
 '((0 60 60 3/4 0) (0 64 62 3/4 0) (0 67 64 3/4 0)
   (3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 67 64 1 0)
   (7/4 60 60 1 0) (7/4 64 62 1 0) (7/4 67 64 1 0)))
(setf
 (gethash '"cand,1,2,superimpose" A)
 '((0 60 60 1 0) (3/4 64 62 1 0) (2 67 64 1 0)))
(setf
 (gethash '"cand,2,1,superimpose" A)
 '((0 61 60 3/4 0) (0 65 62 3/4 0) (0 68 64 3/4 0)
   (3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 67 64 1 0)
   (7/4 62 61 1 0) (7/4 66 63 1 0) (7/4 69 65 1 0)))
(setf
 (gethash '"cand,2,2,superimpose" A)
 '((3/4 60 60 1 0) (3/4 64 62 1 0) (3/4 72 67 1 0)))
(setq
 template-dataset
 '((0 60 60 3/4 0) (0 64 62 3/4 0) (0 67 64 3/4 0)
   (3/4 59 59 1 0) (3/4 62 61 1 0) (3/4 67 64 1 0)
   (7/4 60 60 1 0) (7/4 64 62 1 0) (7/4 67 64 1 0)))
(setq
 stm
 '(((7/4 (4 5)) "etc") ((1 (4 3)) "etc")
   ((11/4 (4 3 17)) "etc")))
(setq c-beats 12)
(most-plausible-join A 3/4 template-dataset stm 3 3 1)
--> "cand,1,1,superimpose".
\end{verbatim}

\noindent This function applies the function keys-of-
states-in-transition-matrix, followed by the function
min-max-abs-diffs-for-likelihood-profiles, to
determine which dataset, out of several candidates, is
the most plausible fit with the template dataset. |#

(defun most-plausible-join
       (hash-table-of-datasets join-at
        template-dataset stm &optional
        (duration-index 3) (beats-in-bar 4)
        (sort-index 1) (c-beats 12)
        (dataset-keys
         (disp-ht-key hash-table-of-datasets))
        (revised-keys
         (keys-of-states-in-transition-matrix
          hash-table-of-datasets dataset-keys
          join-at stm "beat-spacing-states"
          duration-index beats-in-bar sort-index)))
  (min-max-abs-diffs-for-likelihood-profiles
   hash-table-of-datasets revised-keys
   template-dataset c-beats))

#|
\noindent Example:
\begin{verbatim}
(remove-coincident-datapoints
 '((12 64 61 1) (14 63 62 1) (31/2 65 63 1/2))
 '((9 60 60 1) (10 64 62 3) (13 63 62 1)
   (13 72 67 5) (15 65 63 1) (16 65 63 2)) 1 3)
--> ((9 60 60 1) (13 63 62 1) (13 72 67 5)
     (16 65 63 2)).
\end{verbatim}

This function removes any datapoints (second argument)
that sound at the same time as datapoints provided as
the first argument. |#

(defun remove-coincident-datapoints
       (dominant-points subservient-points &optional
        (pitch-index 1) (duration-index 3))
  (if (or (null dominant-points)
          (null subservient-points))
    (identity subservient-points)
    (remove-coincident-datapoints
     (rest dominant-points)
     (remove-datapoints-coincident-with-datapoint
      (first dominant-points) subservient-points
      pitch-index duration-index))))

#|
\noindent Example:
\begin{verbatim}
(remove-datapoints-coincident-with-datapoint
 '(12 64 61 1)
 '((9 60 60 1) (10 64 62 3) (13 63 62 1)
   (13 72 67 5) (15 65 63 1) (16 65 63 2)) 1 3)
--> ((9 60 60 1) (13 63 62 1) (13 72 67 5)
     (15 65 63 1) (16 65 63 2)).
\end{verbatim}

This function removes any datapoints (second argument)
that sound at the same time as a datapoint provided as
the first argument. |#

(defun remove-datapoints-coincident-with-datapoint
       (datapoint datapoints &optional
        (pitch-index 1) (duration-index 3)
        (datapoints-with-offtimes
         (append-offtimes datapoints duration-index))
        (datapoint-offtime
         (+
          (first datapoint)
          (nth duration-index datapoint))))
  (if (null datapoints) ()
    (append
     (if (not
          (and
           (equalp
            (nth pitch-index datapoint)
            (nth pitch-index (first datapoints)))
           (and
            (> datapoint-offtime
               (first (first datapoints)))
            (< (first datapoint)
               (my-last
                (first datapoints-with-offtimes))))))
       (list (first datapoints)))
     (remove-datapoints-coincident-with-datapoint
      datapoint (rest datapoints) pitch-index
      duration-index (rest datapoints-with-offtimes)
      datapoint-offtime))))

#|
\noindent Example:
\begin{verbatim}
(remove-datapoints-with-nth-item<
 '((9 60) (10 64) (13 63) (13 72) (15 65) (16 65)) 10
 0)
--> ((10 64) (13 63) (13 72) (15 65) (16 65)).
\end{verbatim}

This function removes any datapoints whose nth-items
are less than the second argument. Datapoints are
assumed to be in lexicographic order. |#

(defun remove-datapoints-with-nth-item<
       (datapoints x n &optional
        (index
         (index-1st-sublist-item>=
          x (nth-list-of-lists n datapoints))))
  (if index (subseq datapoints index)))

#|
\noindent Example:
\begin{verbatim}
(remove-datapoints-with-nth-item<=
 '((9 60) (10 64) (13 63) (13 72) (15 65) (16 65)) 10
 0)
--> ((13 63) (13 72) (15 65) (16 65)).
\end{verbatim}

This function removes any datapoints whose nth-items
are less than or equal to the second argument.
Datapoints are assumed to be in lexicographic
order. |#

(defun remove-datapoints-with-nth-item<=
       (datapoints x n &optional
        (index
         (index-1st-sublist-item>
          x (nth-list-of-lists n datapoints))))
  (if index (subseq datapoints index)))

#|
\noindent Example:
\begin{verbatim}
(remove-datapoints-with-nth-item>
 '((9 60) (10 64) (13 63) (13 72) (15 65) (16 65)) 15
 0)
--> ((9 60) (10 64) (13 63) (13 72) (15 65)).
\end{verbatim}

This function removes any datapoints whose nth-items
are greater than the second argument. Datapoints are
assumed to be in lexicographic order. |#

(defun remove-datapoints-with-nth-item>
       (datapoints x n &optional
        (index
         (index-1st-sublist-item>
          x (nth-list-of-lists n datapoints))))
  (if index (subseq datapoints 0 index) datapoints))

#|
\noindent Example:
\begin{verbatim}
(remove-datapoints-with-nth-item>=
 '((9 60) (10 64) (13 63) (13 72) (15 65) (16 65)) 15
 0)
--> ((9 60) (10 64) (13 63) (13 72)).
\end{verbatim}

This function removes any datapoints whose nth-items
are greater than or equal to the second argument.
Datapoints are assumed to be in lexicographic
order. |#

(defun remove-datapoints-with-nth-item>=
       (datapoints x n &optional
        (index
         (index-1st-sublist-item>=
          x (nth-list-of-lists n datapoints))))
  (if index (subseq datapoints 0 index) datapoints))

#|
\noindent Example:
\begin{verbatim}
(state-in-transition-matrixp
 '((0 60 60 1 0) (0 64 62 1 0) (0 67 64 1 0))
 0
 '(((7/4 (4 5)) "etc") ((1 (4 3)) "etc")
   ((11/4 (4 3 17)) "etc"))
 "beat-spacing-states" 3 3 1)
--> T.
\end{verbatim}

\noindent This function checks a state, which exists
at a specified ontime in a given dataset, for
membership in a state-transition matrix. If it is a
member, T is returned, and NIL otherwise. |#

(defun state-in-transition-matrixp
       (dataset ontime stm state-fn &optional
        (duration-index 3) (beats-in-bar 4)
        (sort-index 1)
        (segmented
         (segments-strict
          dataset sort-index duration-index))
        (index
         (position
          ontime
          (nth-list-of-lists 0 segmented)
          :test #'equalp))
        (state
         (if index
           (nth
            index
            (if (string=
                 state-fn "spacing-holding-states")
              (spacing-holding-states
               dataset "no information"
               duration-index)
              (beat-spacing-states
               dataset "no information" beats-in-bar
               sort-index duration-index))))))
  (if state
    (if (assoc (first state) stm :test #'equalp)
      T NIL) T))

#|
\noindent Example:
\begin{verbatim}
(unite-datapoints
 '((9 60 60 1) (10 64 62 2) (13 63 62 1) (14 60 60 2)
   (15 65 63 2))
 '((27/2 60 60 1/2) (14 60 60 1) (14 63 62 1)
   (31/2 65 63 1/2) (16 64 62 1) (17 59 59 1))
 14 "superimpose")
--> ((9 60 60 1) (10 64 62 2) (13 63 62 1)
     (14 60 60 2) (14 63 62 1) (31/2 65 63 1/2)
     (16 64 62 1) (17 59 59 1)).
\end{verbatim}

This function unites two sets of datapoints. The third
argument, join-at, is the ontime at which they are
united (specified to avoid overhanging notes from
each set sounding during the other), and the fourth
argument, join-by, gives the option of superimposing
the sets, or letting the first or second set take
precedence. |#

(defun unite-datapoints
       (forwards-points backwards-points join-at
        &optional
        (join-by "superimpose")
        (pitch-index 1) (duration-index 3)
        (forwards-trimmed
         (remove-datapoints-with-nth-item>
          forwards-points join-at 0))
        (backwards-trimmed
         (remove-datapoints-with-nth-item<
          backwards-points join-at 0)))
  (if (string= join-by "superimpose")
    (sort-dataset-asc
     (append
      forwards-trimmed
      (remove-coincident-datapoints
       forwards-trimmed backwards-trimmed pitch-index
       duration-index)))
    (if (string= join-by "forwards-dominant")
      (sort-dataset-asc
       (append
        forwards-trimmed
        (remove-coincident-datapoints
         forwards-trimmed
         (remove-datapoints-with-nth-item<=
          backwards-trimmed join-at 0)
         pitch-index duration-index)))
      (sort-dataset-asc
       (append
        backwards-trimmed
        (remove-coincident-datapoints
         backwards-trimmed
         (remove-datapoints-with-nth-item>=
          forwards-trimmed join-at 0)
         pitch-index duration-index))))))

#| 24/1/2016. I wrote a slightly different version of
this function, which avoids lopping off notes at joins
by testing whether the last note in forwards-points
and first note in backwards-points are the same. If
they are not, there is no need to lop. Need to do docs
here. |#
(defun unite-datapoints-2
       (forwards-points backwards-points join-at
        &optional
        (join-by "superimpose")
        (pitch-index 1) (duration-index 3)
        (forwards-trimmed
         (remove-datapoints-with-nth-item>
          forwards-points join-at 0))
        (backwards-trimmed
         (remove-datapoints-with-nth-item<
          backwards-points join-at 0))
        #| Even if the instruction comes in to use
        forwards- or backwards-dominant processes, if
        the two point sets do not overlap (e.g., one
        ends with an ontime of 14 and the other begins
        with an ontime of 16, then revert to
        superimpose. |#
        (join-by
         (if (not
              (equalp
               (first (my-last forwards-points))
               (first (first backwards-points))))
           "superimpose" join-by)))
  (if (string= join-by "superimpose")
    (sort-dataset-asc
     (append
      forwards-trimmed
      (remove-coincident-datapoints
       forwards-trimmed backwards-trimmed pitch-index
       duration-index)))
    (if (string= join-by "forwards-dominant")
      (sort-dataset-asc
       (append
        forwards-trimmed
        (remove-coincident-datapoints
         forwards-trimmed
         (remove-datapoints-with-nth-item<=
          backwards-trimmed join-at 0)
         pitch-index duration-index)))
      (sort-dataset-asc
       (append
        backwards-trimmed
        (remove-coincident-datapoints
         backwards-trimmed
         (remove-datapoints-with-nth-item>=
          forwards-trimmed join-at 0)
         pitch-index duration-index))))))

