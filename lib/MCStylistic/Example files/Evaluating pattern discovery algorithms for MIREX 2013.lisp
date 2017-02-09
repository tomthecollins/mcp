#| Copyright 2008-2011 Tom Collins
   Friday 12 August 2011

Evaluating pattern discovery algorithms for MIREX 2013

This code demonstrates the use of functions for
evaluating a pattern discovery algorithm against the
contents of an annotated ground truth. It will
calculate metrics such as precision and recall, as
well as more robust versions of these metrics, which
reward discoveries that are very similar (but not
exactly equal) to ground truth patterns. The functions
are intended to help participants in the 2013 MIREX
Pattern Discovery Task to evaluate their
algorithms. |#

#| Step 1 - Set the paths for the locations of output
patterns and ground truth patterns. |#
(setq
 *algorithms-output-root*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "MIREX 2013 pattern discovery task"))
  *MCStylistic-Mar2013-example-files-path*))
(setq *task-version* "polyphonic")
(setq
 *annotations-poly*
 (list
  "bruhn" "barlowAndMorgensternRevised"
  "sectionalRepetitions" "schoenberg" "tomcollins"))
(setq
 *annotations-mono*
 (list
  "bruhn" "barlowAndMorgenstern"
  "barlowAndMorgensternRevised" "sectionalRepetitions"
  "schoenberg" "tomcollins"))
(setq
 *ground-truth-paths*
 (list
  (merge-pathnames
   (make-pathname
    :directory
    '(:relative "beethovenOp2No1Mvt3"))
  *jkuPattsDevDB-Mar2013-gtr-path*)
  (merge-pathnames
   (make-pathname
    :directory
    '(:relative "gibbonsSilverSwan1612"))
  *jkuPattsDevDB-Mar2013-gtr-path*)))
#|
(setq
 *ground-truth-paths*
 (cl-fad:list-directory
  *jkuPattsDevDB-Mar2013-gtr-path*))
|#
(setq
 *algorithm-output-paths*
 (cl-fad:list-directory *algorithms-output-root*))
; Save the calculated metrics to this csv file.
(setq
 *csv-save-path&name*
 (merge-pathnames
  (make-pathname
   :name "calculated-metrics" :type "csv")
  *MCStylistic-Mar2013-example-files-results-path*))

#| Step 2 - List metrics to calculate and any
parameters. |#
(setq
 *metrics-to-calculate*
 (list
  "precision" "recall" "precision-est-card"
  "recall-est-card" "precision-occ-card"
  "recall-occ-card"))
#|
(setq
 *metrics-to-calculate*
 (list
  "precision-est-card" "recall-est-card"
  "precision-occ-card" "recall-occ-card"))
(setq
 *metrics-to-calculate*
 (list
  "precision-est-card" "recall-est-card"
  "precision-occ-card" "recall-occ-card"
  "precision-est-match" "recall-est-match"))
|#
(setq
 *metric-parameters*
 (list
  (list "score-thresh" .75) (list "tolp" t)
  (list "translationp" nil) (list "card-limit" 150)))
(setq *file-type* "csv")

#| Step 3 - Calculate the metrics. |#
(setq
 *ans*
 (pattern-discovery-metrics
  *algorithm-output-paths* *ground-truth-paths*
  *csv-save-path&name* *task-version*
  *annotations-poly* *metrics-to-calculate*
  *metric-parameters* *file-type*))
