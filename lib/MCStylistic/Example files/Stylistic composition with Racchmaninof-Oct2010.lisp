#| Copyright 2008-2011 Tom Collins
   Friday 12 August 2011

Stylistic composition with Racchmaninof-Oct2010

This code demonstrates the use of a model for
automated stylistic composition called
Racchmaninof-Oct2010, standing for RAndom Constrained
CHain of MArkovian Nodes with INheritance Of Form. A
date stamp is added in case it is superseded by future
work. Chapters 8 and 9 of Collins (2011) provide a
full explanation of Racchmaninof-Oct2010. The main
difference between Racchman-Oct2010 and
Racchmaninof-Oct2010 is that the latter includes
pattern inheritance. This means the temporal and
registral positions of discovered repeated patterns
from an existing piece are used as a template to 
guide the generation of a new passage of music. Both
models are similar in spirit to the databases and
programs referred to collectively as Experiments in
Musical Intelligence (EMI), as outlined by Cope (1996,
2001, 2005).

Here I will exemplify the building of
Racchmaninof-Oct2010 for generating a passage of music
in the style of a mazurka by Frederic Chopin (1810-
1849). The building of the model takes about two hours
on a 2.33 GHz machine with 3 GB RAM, so for users not
wishing to wait that long, the resulting files have
been placed in the Example files folder, in a folder
called `Racchmaninof-Oct2010 example'. The passage
generation takes about 260 seconds. More discussion of
this code can be found in the Documentation, Sec. 3.3.

References

Tom Collins. Improved methods for pattern discovery in
music, with applications in automated stylistic
composition. PhD thesis, Faculty of Mathematics,
Computing and Technology, The Open University, 2011.

David Cope. Experiments in musical intelligence. The
Computer Music and Digital Audio Series. A-R Editions,
Madison, WI, 1996.

David Cope. Virtual music: computer synthesis of
musical style. MIT Press, Cambridge, MA, 2001.
(Includes essays by Douglas Hofstadter, Eleanor
Selfridge-Field, Bernard Greenberg, Steve Larson,
Jonathan Berger, and Daniel Dennett).
 
David Cope. Computer models of musical creativity. MIT
Press, Cambridge, MA, 2005. |#


#| Step 1 - Import the dataset representations of
39 Chopin mazurkas. |#
(progn
  (setq
   *variable-names*
   '(C-6-1 C-6-3 C-6-4 C-7-1 C-7-2 C-7-3 C-7-5 C-17-1
     C-17-2 C-17-3 C-17-4 C-24-2 C-24-3 C-30-1 C-30-2
     C-30-3 C-30-4 C-33-1 C-33-2 C-33-3 C-41-1 C-41-2
     C-41-3 C-50-1 C-50-2 C-50-3 C-56-1 C-56-2 C-56-3
     C-59-1 C-59-2 C-63-1 C-63-2 C-67-2 C-67-3 C-67-4
     C-68-2 C-68-3 C-68-4))
  (setq
   *catalogue*
   '("C-6-1" "C-6-3" "C-6-4" "C-7-1" "C-7-2" "C-7-3"
     "C-7-5" "C-17-1" "C-17-2" "C-17-3" "C-17-4"
     "C-24-2" "C-24-3" "C-30-1" "C-30-2" "C-30-3"
     "C-30-4" "C-33-1" "C-33-2" "C-33-3" "C-41-1"
     "C-41-2" "C-41-3" "C-50-1" "C-50-2" "C-50-3"
     "C-56-1" "C-56-2" "C-56-3" "C-59-1" "C-59-2" 
     "C-63-1" "C-63-2" "C-67-2" "C-67-3" "C-67-4"
     "C-68-2" "C-68-3" "C-68-4"))
  (setq
   *chopin-mazurka-datasets*
   (merge-pathnames
    (make-pathname
     :directory '(:relative "chopinMazurkas" "lisp"))
    *MCStylistic-MonthYear-data-path*))
  (setq
   C-6-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-6-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-6-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-6-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-6-4
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-6-4-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-7-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-7-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-7-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-7-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-7-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-7-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-7-5
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-7-5-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-17-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-17-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-17-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-17-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-17-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-17-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-17-4
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-17-4-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-24-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-24-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-24-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-24-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-30-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-30-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-30-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-30-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-30-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-30-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-30-4
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-30-4-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-33-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-33-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-33-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-33-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-33-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-33-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-41-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-41-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-41-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-41-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-41-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-41-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-50-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-50-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-50-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-50-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-50-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-50-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-56-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-56-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-56-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-56-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-56-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-56-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-59-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-59-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-59-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-59-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-63-1
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-63-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-63-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-63-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-67-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-67-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-67-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-67-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-67-4
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-67-4-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-68-2
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-68-2-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-68-3
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-68-3-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   C-68-4
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-68-4-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  "Datasets imported.")

#| Step 2 - Create lists of initial/final state-
context pairs, and transition lists. |#
(progn
  (setq
   *external-initial-states*
   (construct-initial-states
    *variable-names* *catalogue* "beat-spacing-states"
    10 3 3 1))
  (write-to-file
   *external-initial-states*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Example results"
       "Racchmaninof-Oct2010 example")
     :name "initial-states" :type "txt")
    *MCStylistic-MonthYear-example-files-path*))
  (setq
   *external-final-states*
   (construct-final-states
    *variable-names* *catalogue* "beat-spacing-states"
    10 3 3 1))
  (write-to-file
   *external-final-states*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Example results"
       "Racchmaninof-Oct2010 example")
     :name "final-states" :type "txt")
    *MCStylistic-MonthYear-example-files-path*))
  "Initial/final state-context pairs exported.")
  
(progn
  (setq *transition-matrix* nil)
  (construct-stm
   *variable-names* *catalogue* "beat-spacing-states"
   3 3 1)
  (write-to-file
   *transition-matrix*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Example results"
       "Racchmaninof-Oct2010 example")
     :name "transition-matrix" :type "txt")
    *MCStylistic-MonthYear-example-files-path*))
  (setq *transition-matrix* nil)
  (construct-stm<-
   *variable-names* *catalogue* "beat-spacing-states"
   3 3 1)
  (write-to-file
   *transition-matrix*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Racchmaninof-Oct2010 example")
     :name "transition-matrix<-" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*))
  "Transition lists exported.")

#| Step 3 - Define parameter values for
Racchmaninof-Oct2010. |#
(progn
  (setq *beats-in-bar* 3) (setq *c-absrb* 10)
  (setq *c-src* 4) (setq *c-bar* 19) (setq *c-min* 19)
  (setq *c-max* 19) (setq *c-beat* 12)
  (setq *c-prob* 0.2) (setq *c-for* 3)
  (setq *c-back* 3)
  (setq
   *checklist*
   (list "originalp" "mean&rangep" "likelihoodp"))
  "Rachmaninof-Oct2010 parameters defined.")

#| Step 4 - Import lists of initial/final state-
context pairs, and transition lists. It should be
noted that some variables, such as
*internal-initial-states*, are not required for this
example, so their import code is commented out. |#
(progn
  (setq
   *external-initial-states*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "initial-states" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *internal-initial-states*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "internal-initial-states" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *internal-final-states*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "internal-final-states" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *external-final-states*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "final-states" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *stm->*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "transition-matrix" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *stm<-*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "transition-matrix<-" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (concatenate
   'string
   "Initial state/context pairs and transition lists"
   " imported."))

#| Step 5 - Import the dataset of an existing Chopin
mazurka (op.68 no.1) to be used as a template with
patterns. A template with patterns (cf. Def. 9.3 in
Collins, 2011) consists of basic information, such as
tempo and the pitch of the lowest-sounding note of the
first chord, but also information to do with
discovered patterns, such as the ontimes of first and
last datapoints, translators, and subset scores. The
second chunk of code here runs SIACT, rates the
discovered patterns, and performs some filtering as
described in Sec. 7.3.1 of Collins (2011). |#
(progn
  (setq
   *dataset-all*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "C-68-1-ed" :type "txt")
     *chopin-mazurka-datasets*)))
  (setq
   *dataset-template*
   (subseq *dataset-all* 0 231))
  (setq
   *dataset-projected*
   (orthogonal-projection-unique-equalp
    *dataset-template* '(1 1 1 0 0)))
  "Template imported and projected.")

(progn
  (setq time-a (get-internal-real-time))
  (setq *compact-thresh* 4/5)
  (setq *cardina-thresh* 3)
  (setq *region-type* "lexicographic")
  (setq *duration-thresh* 3)
  (SIA-reflected-merge-sort
   *dataset-projected*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Racchmaninof-Oct2010 example")
     :name "C-68-1 (1 1 1 0 0) SIA" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*))
  (setq
   *SIA-output*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "C-68-1 (1 1 1 0 0) SIA" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (compactness-trawler
   *SIA-output* *dataset-projected*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Racchmaninof-Oct2010 example")
     :name "C-68-1 (1 1 1 0 0) CT" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*)
   *compact-thresh* *cardina-thresh*
   *region-type*)
  (setq
   *SIACT-output*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative
        "Racchmaninof-Oct2010 example")
      :name "C-68-1 (1 1 1 0 0) CT" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    ))
  (setq
   *patterns-hash*
   (prepare-for-pattern-inheritance
    *SIACT-output* *dataset-projected*
    *duration-thresh*))
  (write-to-file-balanced-hash-table
   *patterns-hash*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "Racchmaninof-Oct2010 example")
     :name "C-68-1 (1 1 1 0 0) PH" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*))
  (setq time-b (get-internal-real-time))
  (float
   (/
    (- time-b time-a)
    internal-time-units-per-second)))
; 0.979296 sec. Patterns discovered, rated, filtered.

#| Step 6 - Generate candidate passages using
Racchmaninof-Oct2010 and select one. |#
(progn
  (setq
   *rs*
   #.(CCL::INITIALIZE-MRG31K3P-STATE
      1480006552 490947557 697061576 1760965485
      2015184206 904512324))
  (setq
   *whole-piece-interval*
   (list
    (floor (first (first *dataset-all*)))
    (ceiling (first (my-last *dataset-all*)))))
  (setq time-a (get-internal-real-time))
  (setq
   *interval-output-pairs*
   (generate-beat-spacing<->pattern-inheritance
    *external-initial-states*
    *internal-initial-states* *stm->*
    *external-final-states* *internal-final-states*
    *stm<-* *dataset-template* *patterns-hash*
    *whole-piece-interval* *checklist* *beats-in-bar*
    *c-absrb* *c-src* *c-bar* *c-min* *c-max* *c-beat*
    *c-prob* *c-for* *c-back*))
  (setq time-b (get-internal-real-time))
  (float
   (/
    (- time-b time-a)
    internal-time-units-per-second)))
; 1.326446 seconds.

#| Step 7 - Export the generated passage to MIDI and
text files. |#
(progn
  (setq
   *output-datapoints*
   (interval-output-pairs2dataset
    *interval-output-pairs*))
  (saveit
   (merge-pathnames
    (make-pathname
     :name "generated-passage2" :type "mid")
    *MCStylistic-MonthYear-example-files-results-path*)
   (modify-to-check-dataset
    (translation
     *output-datapoints*
     (list
      (- 0 (first (first *output-datapoints*)))
      0 0 0 0)) 850))
  (write-to-file
   *output-datapoints*
   (merge-pathnames
    (make-pathname
     :name "generated-passage2" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*))
  (concatenate
   'string
   "Generated passage exported to MIDI and text"
   " files."))
