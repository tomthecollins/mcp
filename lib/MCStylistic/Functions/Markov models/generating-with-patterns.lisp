#| Copyright 2008-2013 Tom Collins
   Monday 25 October 2010
   Incomplete

\noindent The main function here is generate-beat-
spacing<->pattern-inheritance.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-beat-relative-MNN-for&back"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-beat-MNN-spacing-for&back"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "generating-with-patterns-preliminaries"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example: see Stylistic composition with
Racchmaninof-Jun2015
(Sec.~\ref{sec:ex:Racchmaninof-Jun2015}).

This function is at the heart of the model
named Racchmaninof-Oct2015 (standing for RAndom
Constrained CHain of Markovian Nodes with INheritance
Of Form). It takes ? mandatory arguments and
? optional arguments. The mandatory arguments
are initial-state and state-transition lists, and also
information pertaining to a so-called \emph{template
with patterns}. The optional arguments are mainly for
controlling various criteria like not too many
consecutive states from the same source.

STATE THE DIFF HERE BETWEEN
generate-beat-rel-MNN<->pattern-inheritance
and
generate-beat-spacing<->pattern-inheritance
...
|#

(defun generate-beat-rel-MNN<->pattern-inheritance
       (external-initial-states internal-initial-states
        stm-> external-final-states
        internal-final-states stm<- point-set-template
        template-tpc patterns-hash whole-piece-interval
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-forwards 3) (c-backwards 3) (ontime-index 0)
        (MNN-index 1) (MPN-index 2) (duration-index 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (interval-output-pairs nil)
        (existing-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (indexed-max-subset-score
         (indices-of-max-subset-score patterns-hash))
        (pattern-region
         (if indexed-max-subset-score
           (translation
            (gethash
             '"region"
             (nth
              (first indexed-max-subset-score)
              patterns-hash))
            (nth
             (second indexed-max-subset-score)
             (gethash
              '"translators"
              (nth
               (first indexed-max-subset-score)
               patterns-hash))))))
        (floor-ontime
         (floor
          (first
           (first
            (if pattern-region
              pattern-region point-set-template)))))
        (ceiling-ontime
         (ceiling
          (first
           (my-last
            (if pattern-region
              pattern-region point-set-template)))))
        (generation-intervals
         (generate-intervals
          floor-ontime ceiling-ontime
          existing-intervals beats-in-bar
          #| IMPORTANT!
          I need to make a decision here about whether
          to include beats-in-bar and so suppress
          intervals of less than one bar in length. |#
          ))
        (new-interval-output-pairs
         (if generation-intervals
           (generate-beat-rel-MNN-for-intervals
            generation-intervals whole-piece-interval
            interval-output-pairs
            external-initial-states
            internal-initial-states stm->
            external-final-states
            internal-final-states stm<-
            point-set-template template-tpc
            pattern-region checklist
            beats-in-bar c-failures c-sources
            c-forwards c-backwards ontime-index
            MNN-index MPN-index duration-index
            point-set-idx state-tonic-idx)))
        (new-translated-interval-output-pairs
         (if (and
              indexed-max-subset-score
              new-interval-output-pairs)
           (translate-to-other-occurrences
            new-interval-output-pairs
            interval-output-pairs
            indexed-max-subset-score patterns-hash
            existing-intervals point-set-idx))))
  (if (null indexed-max-subset-score)
    (merge-sort-by-vector<vector-car
     (append
      interval-output-pairs
      new-interval-output-pairs))
    (generate-beat-rel-MNN<->pattern-inheritance
     external-initial-states internal-initial-states
     stm-> external-final-states internal-final-states
     stm<- point-set-template template-tpc
     (progn
       (setf
        (gethash
         '"inheritance addressed"
         (nth
          (first indexed-max-subset-score)
          patterns-hash)) "Yes")
       patterns-hash)
     whole-piece-interval checklist beats-in-bar
     c-failures c-sources c-forwards c-backwards
     ontime-index MNN-index MPN-index duration-index
     point-set-idx state-tonic-idx
     (if new-interval-output-pairs
       (merge-sort-by-vector<vector-car
        (append
         interval-output-pairs
         new-interval-output-pairs
         new-translated-interval-output-pairs))
       interval-output-pairs))))

#|
\noindent Example: see Stylistic composition with
Racchmaninof-Oct2010
(Sec.~\ref{sec:ex:Racchmaninof-Oct2010}).

This function is at the heart of the model
named Racchmaninof-Oct2010 (standing for RAndom
Constrained CHain of Markovian Nodes with INheritance
Of Form). It takes nine mandatory arguments and
twenty-two optional arguments. The mandatory arguments
are initial-state and state-transition lists, and also
information pertaining to a so-called \emph{template
with patterns}. The optional arguments are mainly for
controlling various criteria like: not too many
consecutive states from the same source, the range
must be comparable with that of the template, and the
likelihood of the states must be comparable with that
of the template. |#

(defun generate-beat-spacing<->pattern-inheritance
       (external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template patterns-hash
        whole-piece-interval &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10)
        (c-sources 3) (c-bar 12) (c-min 7) (c-max 7)
        (c-beats 12) (c-prob 0.15) (c-forwards 3)
        (c-backwards 3) (sort-index 1)
        (duration-index 3)
        (interval-output-pairs nil)
        (existing-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (indexed-max-subset-score
         (indices-of-max-subset-score patterns-hash))
        (pattern-region
         (if indexed-max-subset-score
           (translation
            (gethash
             '"region"
             (nth
              (first indexed-max-subset-score)
              patterns-hash))
            (nth
             (second indexed-max-subset-score)
             (gethash
              '"translators"
              (nth
               (first indexed-max-subset-score)
               patterns-hash))))))
        (floor-ontime
         (floor
          (first
           (first
            (if pattern-region
              pattern-region dataset-template)))))
        (ceiling-ontime
         (ceiling
          (first
           (my-last
            (if pattern-region
              pattern-region dataset-template)))))
        (generation-intervals
         (generate-intervals
          floor-ontime ceiling-ontime
          existing-intervals))
        (new-interval-output-pairs
         (if generation-intervals
           (generate-beat-spacing-for-intervals
            generation-intervals whole-piece-interval
            interval-output-pairs
            external-initial-states
            internal-initial-states stm->
            external-final-states
            internal-final-states stm<-
            dataset-template pattern-region checklist
            beats-in-bar c-failures c-sources c-bar
            c-min c-max c-beats c-prob c-forwards
            c-backwards sort-index duration-index)))
        (new-translated-interval-output-pairs
         (if (and
              indexed-max-subset-score
              new-interval-output-pairs)
           (translate-to-other-occurrences
            new-interval-output-pairs
            interval-output-pairs
            indexed-max-subset-score patterns-hash
            existing-intervals))))
  (if (null indexed-max-subset-score)
    (merge-sort-by-vector<vector-car
     (append
      interval-output-pairs
      new-interval-output-pairs))
    (generate-beat-spacing<->pattern-inheritance
     external-initial-states internal-initial-states
     stm-> external-final-states internal-final-states
     stm<- dataset-template
     (progn
       (setf
        (gethash
         '"inheritance addressed"
         (nth
          (first indexed-max-subset-score)
          patterns-hash)) "Yes")
       patterns-hash)
     whole-piece-interval checklist beats-in-bar
     c-failures c-sources c-bar c-min c-max c-beats
     c-prob c-forwards c-backwards sort-index
     duration-index
     (if new-interval-output-pairs
       (merge-sort-by-vector<vector-car
        (append
         interval-output-pairs
         new-interval-output-pairs
         new-translated-interval-output-pairs))
       interval-output-pairs))))

#|
\noindent Example:
\begin{verbatim}
; Setup the variables.
(progn
  (setq
   *rs*
   #.(CCL::INITIALIZE-MRG31K3P-STATE 1670407120
      1951543909 504008720 2023269502 1149040660
      1259341928))
  (setq generation-interval '(24 44))
  (setq whole-piece-interval '(0 44))
  (setq A (make-hash-table :test #'equal))
  (setf
   (gethash "united,3,2,forwards-dominant" A)
   '((((1 (-12 7 12 16))
       ("bachChoraleBWV350R360"
        ((8 46 52 1 3) (8 65 63 1 2) (8 70 66 1 1)
         (8 74 68 1 0)) (58 59) (-2 0)))
      ((2 (-10 5 11 17))
       ("bachChoraleBWV39R67"
        ((33 45 51 1 3) (33 60 60 1 2)
         (33 66 63 1/2 1) (33 72 67 1 0)) (55 57)
        (1 0)))
      ((5/2 (-10 5 9 17))
       ("bachChoraleBWV350R360"
        ((9 48 53 1 3) (9 63 62 1 2) (9 75 69 1 0)
         (19/2 67 64 1/2 1)) (58 59) (-2 0)))
      ((3 (-8 4 7 19))
       ("bachChoraleBWV350R360"
        ((26 50 54 1 3) (26 62 61 1/2 2)
         (26 65 63 1 1) (26 77 70 1 0)) (58 59)
        (-2 0)))
      ((7/2 (-8 2 7 19))
       ("bachChoraleBWV55p5R95"
        ((58 50 54 1 3) (58 65 63 1 1) (58 77 70 1 0)
         (117/2 60 60 1/2 2)) (58 59) (-2 0)))
      ((4 (-3 0 9 17))
       ("bachChoraleBWV55p5R95"
        ((59 55 57 1/2 3) (59 58 59 1 2)
         (59 67 64 1 1) (59 75 69 1/2 0)) (58 59)
        (-2 0)))
      ((9/2 (-1 2 9 17))
       ("bachChoraleBWV226R69"
        ((55 64 62 1 1) (55 72 67 1 0)
         (111/2 54 56 1/2 3) (111/2 57 58 1/2 2))
        (55 57) (1 0)))
      ((1 (0 4 7 16))
       ("bachChoraleBWV226R69"
        ((56 55 57 1/2 3) (56 59 59 1/2 2)
         (56 62 61 1/2 1) (56 71 66 1 0)) (55 57)
        (1 0)))
      ((2 (2 7 16))
       ("bachChoraleBWV10p7R358"
        ((40 65 63 3 1) (41 60 60 1 2) (41 60 60 1 3)
         (41 74 68 1 0)) (58 59) (1 5)))
      ((3 (0 4 7 16))
       ("bachChoraleBWV10p7R358"
        ((40 65 63 3 1) (42 58 59 1 2) (42 62 61 3 3)
         (42 74 68 1 0)) (58 59) (1 5)))
      ((1 (-15 -3 4 12))
       ("bachChoraleBWV154p8R152"
        ((36 47 52 1/2 3) (36 59 59 1 2)
         (36 66 63 1 1) (36 74 68 1 0)) (62 61)
        (2 0)))
      ((3/2 (-13 -3 4 12))
       ("bachChoraleBWV154p8R152"
        ((36 59 59 1 2) (36 66 63 1 1) (36 74 68 1 0)
         (73/2 49 53 1/2 3)) (62 61) (2 0)))
      ((2 (-12 -3 4 12))
       ("bachChoraleBWV244R98"
        ((9 50 54 1/2 3) (9 59 59 1 2) (9 66 63 1 1)
         (9 74 68 1 0)) (62 61) (5 5)))
      ((5/2 (-10 -3 4 12))
       ("bachChoraleBWV244R98"
        ((25 59 59 1 2) (25 66 63 1 1) (25 74 68 1 0)
         (51/2 52 55 1/2 3)) (62 61) (5 5)))
      ((3 (-8 -3 4 11))
       ("bachChoraleBWV330R33"
        ((5 57 58 3/2 2) (6 52 55 1/2 3)
         (6 64 62 1 1) (6 71 66 1 0)) (60 60) (3 5)))
      ((7/2 (-20 -4 4 11))
       ("bachChoraleBWV330R33"
        ((6 64 62 1 1) (6 71 66 1 0)
         (13/2 40 48 1/2 3) (13/2 56 57 1/2 2))
        (60 60) (3 5)))
      ((4 (-15 0 4 9))
       ("bachChoraleBWV353R269"
        ((35 43 50 1 3) (35 58 59 1/2 2)
         (35 62 61 1 1) (35 67 64 1 0)) (58 59)
        (1 5)))
      ((9/2 (-15 -3 4 9))
       ("bachChoraleBWV353R269"
        ((15 43 50 1 3) (15 62 61 1 1) (15 67 64 1 0)
         (31/2 55 57 1/2 2)) (58 59) (1 5)))
      ((1 (-8 -4 4 11))
       ("bachChoraleBWV364R174"
        ((24 50 54 2 3) (24 54 56 2 2) (24 62 61 2 1)
         (24 69 65 2 0)) (58 59) (1 5))))
     ((12 43 50 1 3) (12 62 61 1 2) (12 67 64 1 1)
      (12 71 66 1 0) (13 45 51 1 3) (13 60 60 1 2)
      (13 66 63 1/2 1) (13 72 67 1 0)
      (27/2 64 62 1/2 1) (14 47 52 1 3)
      (14 59 59 1/2 2) (14 62 61 1 1) (14 74 68 1 0)
      (29/2 57 58 1/2 2) (15 52 55 1/2 3)
      (15 55 57 1/2 2) (15 64 62 1 1)
      (15 72 67 1/2 0) (31/2 54 56 1/2 3)
      (31/2 57 58 1/2 2) (31/2 72 67 1/2 0)
      (16 55 57 1 3) (16 59 59 1 2) (16 62 61 1 1)
      (16 71 66 1 0) (17 57 58 1 2) (17 57 58 1 3)
      (17 62 61 2 1) (17 71 66 1 0) (18 55 57 1 2)
      (18 59 59 1 3) (18 71 66 1 0) (20 40 48 1/2 3)
      (20 52 55 5/2 2) (20 59 59 6 1) (20 67 64 2 0)
      (41/2 42 49 1/2 3) (21 43 50 1/2 3)
      (43/2 45 51 1/2 3) (22 47 52 1/2 3)
      (22 66 63 1 0) (45/2 35 45 1/2 3)
      (45/2 51 54 1/2 2) (23 40 48 1 3)
      (23 55 57 1/2 2) (23 64 62 1 0)
      (47/2 52 55 1/2 2) (24 47 52 2 3)
      (24 51 54 2 2) (24 66 63 2 0))))
  (setq
   interval-output-pairs
   (list
    (list
     (list 12 24)
     (list
      "united,3,2,forwards-dominant"
      (list
       nil ; Forwards candidates would be here.
       nil ; Backwards candidates would be here.
       A)))))
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
  (setq
   external-final-states
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "Racchman-Jun2015 example")
      :name "final-states" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*
     )))
  (setq internal-final-states nil)
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
   (second (gethash "united,3,2,forwards-dominant" A)))
  (setq checklist '("originalp"))
  (setq beats-in-bar 4)
  (setq c-failures 10)
  (setq c-sources 5))
; Call the function and append the output.
(setq
 interval-output-pairs
 (append
  interval-output-pairs
  (list
   (generate-beat-rel-MNN-for-interval
    generation-interval whole-piece-interval
    interval-output-pairs external-initial-states
    internal-initial-states stm->
    external-final-states internal-final-states stm<-
    point-set-template template-tpc pattern-region
    checklist beats-in-bar c-failures c-sources))))
; Save it for auditioning.
(progn
  (setq
   point-set
   (interval-output-pairs2dataset
    interval-output-pairs 1))
  (saveit
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "Racchman-Jun2015 example")
     :name "Racchman-Jun2015-sample-output-2"
     :type "mid")
    *MCStylistic-MonthYear-example-files-results-path*)
   (modify-to-check-dataset
    (translation
     point-set
     (list
      (- 0 (first (first point-set))) 0 0 0 0))
    1500)))
\end{verbatim}

\noindent This function takes an ontime interval
$[a, b]$ as its first argument, which is a subset of an
ontime interval $[c, d]$ for the whole piece (second
argument). The third argument is a list of
interval-output pairs
$\{ \( [c, b_1], U_1 \),
    \( [a_2, b_2], U_2 \),\ldots,
    \( [a_n, d], U_1 \) \}$
for which material has already been generated. The role
of this function is to work out how the interval
$[a, b] = [a_i, b_i]$ relates to intervals $[a_j, b_j]$
with $j \neq i$ for which material has already been
generated. If for instance $b_i = a_j$, then there is
already material at the interval's end, and this needs
to be passed as a kind of constraint to the funcion
\ref{fun:generate-beat-rel-MNN-forced<->} (in this
case via the variable \verb+terminal<-p+.

Unlike the similar function
\ref{fun:generate-beat-spacing-for-interval}, which
calls \fun{beat-spacing-states} and
\fun{beat-spacing-states<-}, this function does not
call those subfunctions in order to define the variable 
context-states&datapoints. It just was not felt to be
necessary, and because of the change to a state
representation relative to tonal centre, converting
small collections of absolute MNNs to relative MNNs via
key estimations is an error-prone process.

15/5/2015. In the example code, we ask for material to
be generated for the interval $[24, 44]$, having
already generated material for the interval $[12, 24]$
in a piece with overall interval $[0, 44]$. The output
seems to stop just short of 44, which seems a bit
weird to me, but I'll reserve judgment (and
alterations) until I've incorporated this function in
generate-beat-rel-MNN-for-intervals and
generate-beat-rel-MNN<->pattern-inheritance. |#
    
(defun generate-beat-rel-MNN-for-interval
       (generation-interval whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- point-set-template template-tpc
        pattern-region &optional
        (checklist (list "originalp")) (beats-in-bar 4)
        (c-failures 10) (c-sources 3) (c-forwards 3)
        (c-backwards 3) (ontime-index 0) (MNN-index 1)
        (MPN-index 2) (duration-index 3)
        (point-set-idx 1) (state-tonic-idx 2)
        (left-side (first generation-interval))
        (right-side (second generation-interval))
        (trimmed-template
         (remove-datapoints-with-nth-item>
          (remove-datapoints-with-nth-item<
           point-set-template left-side 0)
          right-side 0))
        #| A workaround for when there is no
        pattern-region (so original generate
        functions are called), so first and last
        ontimes have to be floored and ceilinged
        respectively. |#
        (rounded-template
         (if (null pattern-region)
           (append
            (list
             (append
              (list
               (floor
                (first (first trimmed-template))))
              (rest (first trimmed-template))))
            (rest (butlast trimmed-template))
            (list
             (append
              (list
               (ceiling
                (first (my-last trimmed-template))))
              (rest (my-last trimmed-template)))))
           trimmed-template))
        (generated-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (generated-left-sides
         (nth-list-of-lists 0 generated-intervals))
        (generated-right-sides
         (nth-list-of-lists 1 generated-intervals))
        (terminal->p
         (equalp
          left-side (first whole-piece-interval)))
        (terminal<-p
         (equalp
          right-side (second whole-piece-interval)))
        (index-context->
         (position
          left-side generated-right-sides
          :test #'equalp))
        (index-context<-
         (position
          right-side generated-left-sides
          :test #'equalp))
        (context-most-plausible->
         (if index-context->
           (first
            (second
             (nth
              index-context->
              interval-output-pairs)))))
        (context-most-plausible<-
         (if index-context<-
           (first
            (second
             (nth
              index-context<-
              interval-output-pairs)))))
        (context-states&datapoints->
         (if context-most-plausible->
           (gethash
            context-most-plausible->
            (third
             (second
              (second
               (nth
                index-context->
                interval-output-pairs)))))))
        (context-states&datapoints<-
         (if context-most-plausible<-
           (gethash
            context-most-plausible<-
            (third
             (second
              (second
               (nth
                index-context<-
                interval-output-pairs)))))))
        (state-context-pair->
         (if context-states&datapoints->
           (my-last
            (first context-states&datapoints->))))
        (state-context-pair<-
         (if context-states&datapoints<-
           (first
            (first context-states&datapoints<-))))
        (output
         (generate-beat-rel-MNN-forced<->
          generation-interval terminal->p terminal<-p
          external-initial-states
          internal-initial-states stm->
          external-final-states internal-final-states
          stm<- rounded-template template-tpc
          pattern-region state-context-pair->
          state-context-pair<- checklist beats-in-bar
          c-failures c-sources c-forwards c-backwards
          ontime-index MNN-index MPN-index
          duration-index point-set-idx
          state-tonic-idx)))
  (list
   generation-interval
   (list
    (most-plausible-join-rel
     (third output) (/ (+ left-side right-side) 2)
     stm-> beats-in-bar ontime-index MNN-index
     MPN-index duration-index)
    output)))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   external-initial-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/initial-states.txt")))
  (setq
   internal-initial-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/internal-initial-states.txt")))
  (setq
   stm->
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/transition-matrix.txt")))
  (setq
   external-final-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/final-states.txt")))
  (setq
   internal-final-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/internal-final-states.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/transition-matrix<-.txt")))
  (setq
   dataset-all
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-56-1-ed.txt")))
  (setq
   dataset-template
   (subseq dataset-all 0 132))
  "Data imported.")
(progn
  (setq generation-interval '(12 24))
  (setq
   whole-piece-interval
   (list
    (floor (first (first dataset-all)))
    (ceiling (first (my-last dataset-all)))))
  (setq A (make-hash-table :test #'equal))
  (setf
   (gethash
    '"united-candidates,1,1,superimpose" A)
   '((9 70 66 3 0) (9 79 71 1/2 0) (19/2 74 68 3/2 0)
     (10 55 57 1 1) (10 62 61 1 1) (11 55 57 1 1)
     (11 62 61 1 1) (12 38 47 1 1) (12 69 65 1/2 0)))
  (setq B (make-hash-table :test #'equal))
  (setf
   (gethash
    '"united-candidates,2,3,forwards-dominant" B)
   '((24 38 47 1 1) (49/2 66 63 1/2 0) (25 50 54 1 1)
     (25 57 58 1 1) (25 60 60 1 1) (25 62 61 1 0)
     (26 50 54 1 1) (26 57 58 1 1) (26 60 60 1 1)
     (26 62 61 1 1) (26 66 63 1 0) (26 70 66 3/4 0)
     (107/4 69 65 1/4 0) (27 43 50 1 1)
     (27 67 64 2 0)))
  (setq
   interval-output-pairs
   (list
    (list
     (list 9 12)
     (list
      "united-candidates,1,1,superimpose"
      (list nil nil A)))
    (list
     (list 24 27)
     (list
      "united-candidates,2,3,forwards-dominant"
      (list nil nil B)))))
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
  "Argument instances defined.")
(progn
  (setq
   checklist
   (list "originalp" "mean&rangep" "likelihoodp"))
  (setq beats-in-bar 3) (setq c-failures 10)
  (setq c-sources 4) (setq c-bar 48) (setq c-min 38)
  (setq c-max 38) (setq c-beats 38) (setq c-prob 1)
  (setq c-forwards 3) (setq c-backwards 3)
  (setq
   *rs* #.(CCL::INITIALIZE-RANDOM-STATE 56302 14832))
  "Parameters set.")
(progn
  (setq
   interval-output-pair
   (generate-beat-spacing-for-interval
    generation-interval whole-piece-interval
    interval-output-pairs external-initial-states
    internal-initial-states stm->
    external-final-states internal-final-states stm<-
    dataset-template pattern-region checklist
    beats-in-bar c-failures c-sources c-bar c-min
    c-max c-beats c-prob c-forwards c-backwards))
--> ((12 24)
     ("united,2,1,backwards-dominant"
      (#<HASH-TABLE
       :TEST EQUAL size 3/60 #x30004340CE3D>
       #<HASH-TABLE
       :TEST EQUAL size 3/60 #x30004340C82D>
       #<HASH-TABLE
       :TEST EQUAL size 27/60 #x30004340C21D>)))
\end{verbatim}

This function generates material for a specified time
interval, by calling the function
generate-beat-spacing-forced$<$-$>$. |#

(defun generate-beat-spacing-for-interval
       (generation-interval whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (sort-index 1) (duration-index 3)
        (left-side (first generation-interval))
        (right-side (second generation-interval))
        (trimmed-template
         (remove-datapoints-with-nth-item>
          (remove-datapoints-with-nth-item<
           dataset-template left-side 0)
          right-side 0))
        #| A workaround for when there is no
        pattern-region (so original generate
        functions are called), so first and last
        ontimes have to be floored and ceilinged
        respectively. |#
        (rounded-template
         (if (null pattern-region)
           (append
            (list
             (append
              (list
               (floor
                (first (first trimmed-template))))
              (rest (first trimmed-template))))
            (rest (butlast trimmed-template))
            (list
             (append
              (list
               (ceiling
                (first (my-last trimmed-template))))
              (rest (my-last trimmed-template)))))
           trimmed-template))
        (generated-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (generated-left-sides
         (nth-list-of-lists 0 generated-intervals))
        (generated-right-sides
         (nth-list-of-lists 1 generated-intervals))
        (terminal->p
         (equalp
          left-side (first whole-piece-interval)))
        (terminal<-p
         (equalp
          right-side (second whole-piece-interval)))
        (index-context->
         (position
          left-side generated-right-sides
          :test #'equalp))
        (index-context<-
         (position
          right-side generated-left-sides
          :test #'equalp))
        (context-most-plausible->
         (if index-context->
           (first
            (second
             (nth
              index-context->
              interval-output-pairs)))))
        (context-most-plausible<-
         (if index-context<-
           (first
            (second
             (nth
              index-context<-
              interval-output-pairs)))))
        (context-datapoints->
         (if context-most-plausible->
           (gethash
            context-most-plausible->
            (third
             (second
              (second
               (nth
                index-context->
                interval-output-pairs)))))))
        (context-datapoints<-
         (if context-most-plausible<-
           (gethash
            context-most-plausible<-
            (third
             (second
              (second
               (nth
                index-context<-
                interval-output-pairs)))))))
        (state-context-pair->
         (if context-datapoints->
           (list
            (first
             (my-last
              (beat-spacing-states
               context-datapoints-> "no information"
               beats-in-bar sort-index
               duration-index)))
            (list
             nil nil "no information"
             (fourth
            (second
             (my-last
              (beat-spacing-states
               context-datapoints-> "no information"
               beats-in-bar sort-index
               duration-index))))))))
        (state-context-pair<-
         (if context-datapoints<-
           (list
            (first
             (first
              (beat-spacing-states<-
               context-datapoints<- "no information"
               beats-in-bar sort-index
               duration-index)))
            (list
             nil nil "no information"
             (fourth
              (second
               (first
                (beat-spacing-states<-
                 context-datapoints<- "no information"
                 beats-in-bar sort-index
                 duration-index))))))))
        (output
         (generate-beat-spacing-forced<->
          generation-interval terminal->p terminal<-p
          external-initial-states
          internal-initial-states stm->
          external-final-states internal-final-states
          stm<- rounded-template pattern-region
          state-context-pair-> state-context-pair<-
          checklist beats-in-bar c-failures c-sources
          c-bar c-min c-max c-beats c-prob c-forwards
          c-backwards sort-index duration-index)))
  (list
   generation-interval
   (list
    (most-plausible-join
     (third output) (/ (+ left-side right-side) 2)
     rounded-template stm-> duration-index
     beats-in-bar sort-index c-beats)
    output)))

#|
\noindent Example: see example for
\nameref{fun:generate-beat-rel-MNN-for-interval}.
\vspace{0.5cm}

\noindent This function applies the function
generate-beat-rel-MNN-for-interval to each member of
a list called \texttt{interval-output-pairs}. |#

(defun generate-beat-rel-MNN-for-intervals
       (generation-intervals whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- point-set-template template-tpc
        pattern-region &optional
        (checklist (list "originalp")) (beats-in-bar 4)
        (c-failures 10) (c-sources 3) (c-forwards 3)
        (c-backwards 3) (ontime-index 0) (MNN-index 1)
        (MPN-index 2) (duration-index 3)
        (point-set-idx 1) (state-tonic-idx 2))
  (mapcar
   #'(lambda (x)
       (generate-beat-rel-MNN-for-interval
        x whole-piece-interval interval-output-pairs
        external-initial-states internal-initial-states
        stm-> external-final-states
        internal-final-states stm<- point-set-template
        template-tpc pattern-region checklist
        beats-in-bar c-failures c-sources c-forwards
        c-backwards ontime-index MNN-index MPN-index
        duration-index point-set-idx state-tonic-idx))
   generation-intervals))

#|
\noindent Example: see example for
\nameref{fun:generate-beat-spacing-for-interval}.
\vspace{0.5cm}

\noindent This function applies the function
generate-beat-spacing-for-interval to each member of
a list called \texttt{interval-output-pairs}. |#

(defun generate-beat-spacing-for-intervals
       (generation-intervals whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (sort-index 1) (duration-index 3))
  (mapcar
   #'(lambda (x)
       (generate-beat-spacing-for-interval
        x whole-piece-interval interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        checklist beats-in-bar c-failures c-sources
        c-bar c-min c-max c-beats c-prob c-forwards
        c-backwards sort-index duration-index))
   generation-intervals))

#|
\noindent Example: see example for
\nameref{fun:unite-datapoints}.
\vspace{0.5cm}

\noindent This function applies the function
unite-datapoints, to convert the output for various
intervals into a dataset. |#

(defun interval-output-pairs2dataset
       (interval-output-pairs &optional
        (point-set-index nil)
        (first-pair (first interval-output-pairs))
        (datapoints
         (if first-pair
           (if point-set-index
             (nth
              point-set-index
              (gethash
               (first (second first-pair))
               (third (second (second first-pair)))))
             (gethash
              (first (second first-pair))
              (third (second (second first-pair))))))))
  (if (null first-pair) ()
    (unite-datapoints-2
     datapoints
     (interval-output-pairs2dataset
      (rest interval-output-pairs) point-set-index)
     (second (first first-pair))
     "backwards-dominant")))

#|
\noindent Example:
\begin{verbatim}
(translate-to-other-occurrence
 nil '(36 12 7) '((12 24))
 (make-hash-table :test #'equal)
 (translation
  '((12 38 47 1/2 1) (49/4 64 62 1/4 0)
    (25/2 50 54 1 1) (25/2 55 57 1 1) (25/2 58 59 1 1)
    (25/2 62 61 1 0)) '(36 12 7 0 0)))
--> (((48 60)
      ("translated material"
       (NIL NIL
        #<HASH-TABLE :TEST EQUAL
        size 1/60 #x3000436FF23D>))))
\end{verbatim}

\noindent This function takes a list of interval-
output pairs as its argument (from one iteration of
generate-beat-spacing<->pattern-inheritance) Its
second argument is a translation vector, by which
each of the output datasets must be translated. |#

(defun translate-to-other-occurrence
       (new-interval-output-pairs translation-vector
        &optional
        (point-set-idx nil)
        (interval-output-pair
         (first new-interval-output-pairs))
        (A (make-hash-table :test #'equal))
        (translated-datapoints
         (if interval-output-pair
           (translation
            (if point-set-idx
              (nth
               point-set-idx
               (gethash
                (first (second interval-output-pair))
                (third
                 (second
                  (second interval-output-pair)))))
              (gethash
               (first (second interval-output-pair))
               (third
                (second (second interval-output-pair)))))
            (append translation-vector (list 0 0)))))
        (states
         (if (and interval-output-pair point-set-idx)
           (first
            (gethash
             (first (second interval-output-pair))
             (third
              (second
               (second interval-output-pair)))))))
        )
  (if (null interval-output-pair) ()
    (cons
     (list
      (list
       (+
        (first (first interval-output-pair))
        (first translation-vector))
       (+
        (second (first interval-output-pair))
        (first translation-vector)))
      (list
       "translated material"
       (list
        nil
        nil
        (progn
          (setf
           (gethash '"translated material" A)
           (if point-set-idx
             (list states translated-datapoints)
             translated-datapoints))
          A))))
     (translate-to-other-occurrence
      (rest new-interval-output-pairs)
      translation-vector point-set-idx))))

#|
\noindent Example: see example for
\nameref{fun:translate-to-other-occurrence}.
\vspace{0.5cm}

\noindent This function applies the function
translate-to-other-occurrence to each member of
a list called \texttt{translators}. It first
determines whether the location to which material will
be translated has been addressed. |#

(defun translate-to-other-occurrences
       (new-interval-output-pairs
        existing-interval-output-pairs
        indices-of-max-subset-score patterns-hash
        &optional
        (existing-intervals
         (nth-list-of-lists
          0 existing-interval-output-pairs))
        (point-set-idx nil)
        (translators
         (gethash
          '"translators"
          (nth
           (first indices-of-max-subset-score)
           patterns-hash)))
        (altered-translators
         (remove-nth
          (second indices-of-max-subset-score)
          (subtract-list-from-each-list
           translators
           (nth
            (second indices-of-max-subset-score)
            translators))))
        (n (length altered-translators)) (i 0)
        (translated-intervals
         (if (< i n)
           (mapcar
            #'(lambda (x)
                (list
                 (+
                  (first x)
                  (first (nth i altered-translators)))
                 (+
                  (second x)
                  (first
                   (nth i altered-translators)))))
            (nth-list-of-lists
             0 new-interval-output-pairs))))
        (generation-possiblep
         (if (< i n)
           (equalp
            (generate-intervals
             (first (first translated-intervals))
             (my-last (my-last translated-intervals))
             existing-intervals)
            translated-intervals))))
  (if (>= i n) ()
    (append
     (if generation-possiblep
       (translate-to-other-occurrence
        new-interval-output-pairs
        (nth i altered-translators) point-set-idx))
     (translate-to-other-occurrences
      new-interval-output-pairs
      existing-interval-output-pairs
      indices-of-max-subset-score patterns-hash
      existing-intervals point-set-idx translators
      altered-translators n (+ i 1)))))
