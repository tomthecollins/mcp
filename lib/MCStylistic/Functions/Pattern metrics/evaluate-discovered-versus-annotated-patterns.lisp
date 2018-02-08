#| Copyright 2008-2013 Tom Collins
   Tuesday 5 February 2013
   Incomplete

\noindent The functions below are metrics for
evaluating the extent to which different ontime-pitch
pairs have been output by an algorithm.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "csv-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Third party" "cl-fad")
   :name "fad2"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern metrics")
   :name "robust-metrics"
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

#|
\noindent Example:
\begin{verbatim}
(setq
 *example-files-path*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "MIREX 2013 pattern discovery task"))
  *MCStylistic-MonthYear-example-files-path*))
(setq
 *algorithm-path*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "MIREX 2013 pattern discovery task"
     "algorithm3output"))
  *MCStylistic-MonthYear-example-files-path*))
(setq
 *piece-path*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "groundTruth" "beethovenOp2No1Mvt3"))
  *jkuPattsDevDB-MonthYear-path*))
(metrics-for-algorithm&piece
 *algorithm-path* *piece-path*)
--> (.364 .571 .729 .967 .947 0.967)
\end{verbatim}

\noindent This function lists a pattern collection
(for example the patterns annotated in a fugue by
Bruhn, or the patterns output by an algorithm), and
loads all occurrences of all these patterns as nested
lists. |#

(defun metrics-for-algorithm&piece
       (algorithm-path piece-path &optional
        (algorithm-idx 0) (piece-idx 0)
        (task-version "polyphonic")
        (annotations-to-use
         (list
          "bruhn" "barlowAndMorgensternRevised"
          "sectionalRepetitions" "schoenberg"
          "tomCollins"))
        (metrics-to-calculate
         (list
          "precision" "recall" "precision-est-card"
          "recall-est-card" "precision-occ-card"
          "recall-occ-card"))
        (metric-parameters
         (list
          (list "score-thresh" .75)
          (list "tolp" t)))
        (file-type "csv")
        (score-thresh
         (if (find
              "score-thresh"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "score-thresh"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))
           .75))
        (tolp
         (if (find
              "tolp"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "tolp"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))))
        (translationp
         (if (find
              "translationp"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "translationp"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))))
        (card-limit
         (if (find
              "card-limit"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "card-limit"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))
           100))
        (piece-name
         (pathname-name
          (pathname-as-file piece-path)))
        (alg-out-path
         (remove-if-not
          #'directory-pathname-p
          (list-directory
           (merge-pathnames
            (make-pathname
             :directory (list :relative piece-name))
            algorithm-path))))
        (Q-patt&occ
         (read-patts&occs alg-out-path file-type))
        (annotation-paths
         (mapcar
          #'(lambda (x)
              (merge-pathnames
               (make-pathname
                :directory
                (list
                 :relative
                 task-version "repeatedPatterns" x))
               piece-path))
          annotations-to-use))
        (P-patt&occ
         (read-ground-truth-for-piece
          annotation-paths file-type))
        (nmetric (length metrics-to-calculate))
        (metric-idx 0))
  (if (>= metric-idx nmetric) ()
    (if Q-patt&occ
      (progn
        (format
         t
         (concatenate
          'string "Calculating for algorithm ~D,"
          " piece ~D, metric ~D~%")
         (+ algorithm-idx 1) (+ piece-idx 1)
         (+ metric-idx 1))
        (cons
         (if (string=
              (nth metric-idx metrics-to-calculate)
              "precision")
           (establishment-metric
            P-patt&occ Q-patt&occ #'precision-matrix
            #'equalp-score tolp)
           (if (string=
                (nth metric-idx metrics-to-calculate)
                "recall")
             (establishment-metric
              P-patt&occ Q-patt&occ #'recall-matrix
              #'equalp-score tolp)
             (if (string=
                  (nth
                   metric-idx metrics-to-calculate)
                  "precision-est-card")
               (establishment-metric
                P-patt&occ Q-patt&occ
                #'precision-matrix #'cardinality-score
                tolp translationp)
               (if (string=
                    (nth
                     metric-idx metrics-to-calculate)
                    "recall-est-card")
                 (establishment-metric
                  P-patt&occ Q-patt&occ
                  #'recall-matrix #'cardinality-score
                  tolp translationp)
                 (if (string=
                      (nth
                       metric-idx
                       metrics-to-calculate)
                      "precision-occ-card")
                   (occurrence-metric
                    P-patt&occ Q-patt&occ
                    #'precision-matrix
                    #'cardinality-score tolp
                    translationp score-thresh)
                   (if (string=
                        (nth
                         metric-idx
                         metrics-to-calculate)
                        "recall-occ-card")
                     (occurrence-metric
                      P-patt&occ Q-patt&occ
                      #'recall-matrix
                      #'cardinality-score tolp
                      translationp score-thresh)
                     (if (string=
                          (nth
                           metric-idx
                           metrics-to-calculate)
                          "precision-est-match")
                       (establishment-metric
                        P-patt&occ Q-patt&occ
                        #'precision-matrix
                        #'matching-score tolp
                        translationp card-limit)
                       (if (string=
                            (nth
                             metric-idx
                             metrics-to-calculate)
                            "recall-est-match")
                         (establishment-metric
                          P-patt&occ Q-patt&occ
                          #'recall-matrix
                          #'matching-score
                          tolp translationp
                          card-limit)
                         (if (string=
                              (nth
                               metric-idx
                               metrics-to-calculate)
                              "precision-occ-match")
                           (occurrence-metric
                            P-patt&occ Q-patt&occ
                            #'precision-matrix
                            #'matching-score tolp
                            translationp card-limit
                            score-thresh)
                           (if (string=
                                (nth
                                 metric-idx
                                 metrics-to-calculate)
                                "recall-occ-match")
                             (occurrence-metric
                              P-patt&occ Q-patt&occ
                              #'recall-matrix
                              #'matching-score tolp
                              translationp card-limit
                              score-thresh)))))))))))
         (metrics-for-algorithm&piece
          algorithm-path piece-path algorithm-idx
          piece-idx task-version annotations-to-use
          metrics-to-calculate metric-parameters
          file-type score-thresh tolp translationp
          card-limit piece-name alg-out-path
          Q-patt&occ annotation-paths P-patt&occ
          nmetric (+ metric-idx 1))))
      (format
         t
         (concatenate
          'string "Results missing for algorithm ~D,"
          " piece ~D, skipping~%")
         (+ algorithm-idx 1) (+ piece-idx 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 *algorithm-path*
 (merge-pathnames
  (make-pathname
   :directory
   (list
    :relative "Example files"
    "MIREX 2013 pattern discovery task" 
    "algorithm5output" "beet_op002_no1_mv1"))
  *MCStylistic-MonthYear-path*))
(setq
 *piece-path*
 (merge-pathnames
  (make-pathname
   :directory
   (list
    :relative "Example files"
    "MIREX 2013 pattern discovery task"
    "exampleGroundTruth" "beet_op002_no1_mv1"))
  *MCStylistic-MonthYear-path*))
(metrics-for-algorithm&piece-all-patt-all-occ
 *algorithm-path* *piece-path*)
--> (.364 .571 .729 .967 .947 0.967)
\end{verbatim}

\noindent This function is a slight variant on
metrics-for-algorithm&piece. It assumes that the
entire algorithm output and for one piece (movement)
is contained in one text file, in a nested list.
Similarly for the ground truth. |#

(defun metrics-for-algorithm&piece-all-patt-all-occ
       (algorithm-path piece-path &optional
        (algorithm-idx 0) (piece-idx 0)
        (metrics-to-calculate
         (list
          "precision" "recall" "precision-est-card"
          "recall-est-card" "precision-occ-card"
          "recall-occ-card"))
        (metric-parameters
         (list
          (list "score-thresh" .75)
          (list "tolp" t)))
        (score-thresh
         (if (find
              "score-thresh"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "score-thresh"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))
           .75))
        (tolp
         (if (find
              "tolp"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "tolp"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))))
        (translationp
         (if (find
              "translationp"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "translationp"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))))
        (card-limit
         (if (find
              "card-limit"
              (nth-list-of-lists 0 metric-parameters)
              :test #'equalp)
           (nth
            (position
             "card-limit"
             (nth-list-of-lists 0 metric-parameters)
             :test #'equalp)
            (nth-list-of-lists 1 metric-parameters))
           100))
        (piece-name
         (pathname-name
          (pathname-as-file piece-path)))
        (alg-out-path
         (merge-pathnames
            (make-pathname
             :directory (list :relative piece-name)
             :name "all_patt_all_occ" :type "txt")
            algorithm-path))
        (Q-patt&occ
         (first (read-from-file alg-out-path)))
        (P-patt&occ
         (first
          (read-from-file
           (merge-pathnames
            (make-pathname
             :name "all_patt_all_occ" :type "txt")
            piece-path))))
        (nmetric (length metrics-to-calculate))
        (metric-idx 0))
  (if (>= metric-idx nmetric) ()
    (if Q-patt&occ
      (progn
        (format
         t
         (concatenate
          'string "Calculating for algorithm ~D,"
          " piece ~D, metric ~D~%")
         (+ algorithm-idx 1) (+ piece-idx 1)
         (+ metric-idx 1))
        (cons
         (if (string=
              (nth metric-idx metrics-to-calculate)
              "precision")
           (establishment-metric
            P-patt&occ Q-patt&occ #'precision-matrix
            #'equalp-score tolp)
           (if (string=
                (nth metric-idx metrics-to-calculate)
                "recall")
             (establishment-metric
              P-patt&occ Q-patt&occ #'recall-matrix
              #'equalp-score tolp)
             (if (string=
                  (nth
                   metric-idx metrics-to-calculate)
                  "precision-est-card")
               (establishment-metric
                P-patt&occ Q-patt&occ
                #'precision-matrix #'cardinality-score
                tolp translationp)
               (if (string=
                    (nth
                     metric-idx metrics-to-calculate)
                    "recall-est-card")
                 (establishment-metric
                  P-patt&occ Q-patt&occ
                  #'recall-matrix #'cardinality-score
                  tolp translationp)
                 (if (string=
                      (nth
                       metric-idx
                       metrics-to-calculate)
                      "precision-occ-card")
                   (occurrence-metric
                    P-patt&occ Q-patt&occ
                    #'precision-matrix
                    #'cardinality-score tolp
                    translationp score-thresh)
                   (if (string=
                        (nth
                         metric-idx
                         metrics-to-calculate)
                        "recall-occ-card")
                     (occurrence-metric
                      P-patt&occ Q-patt&occ
                      #'recall-matrix
                      #'cardinality-score tolp
                      translationp score-thresh)
                     (if (string=
                          (nth
                           metric-idx
                           metrics-to-calculate)
                          "precision-est-match")
                       (establishment-metric
                        P-patt&occ Q-patt&occ
                        #'precision-matrix
                        #'matching-score tolp
                        translationp card-limit)
                       (if (string=
                            (nth
                             metric-idx
                             metrics-to-calculate)
                            "recall-est-match")
                         (establishment-metric
                          P-patt&occ Q-patt&occ
                          #'recall-matrix
                          #'matching-score
                          tolp translationp
                          card-limit)
                         (if (string=
                              (nth
                               metric-idx
                               metrics-to-calculate)
                              "precision-occ-match")
                           (occurrence-metric
                            P-patt&occ Q-patt&occ
                            #'precision-matrix
                            #'matching-score tolp
                            translationp card-limit
                            score-thresh)
                           (if (string=
                                (nth
                                 metric-idx
                                 metrics-to-calculate)
                                "recall-occ-match")
                             (occurrence-metric
                              P-patt&occ Q-patt&occ
                              #'recall-matrix
                              #'matching-score tolp
                              translationp card-limit
                              score-thresh)))))))))))
         (metrics-for-algorithm&piece-all-patt-all-occ
          algorithm-path piece-path algorithm-idx
          piece-idx metrics-to-calculate
          metric-parameters score-thresh tolp
          translationp card-limit piece-name
          alg-out-path Q-patt&occ P-patt&occ
          nmetric (+ metric-idx 1))))
      (format
         t
         (concatenate
          'string "Results missing for algorithm ~D,"
          " piece ~D, skipping~%")
         (+ algorithm-idx 1) (+ piece-idx 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 *algorithms-output-root*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "MIREX 2013 pattern discovery task"))
  *MCStylistic-MonthYear-example-files-path*))
(setq *task-version* "polyphonic")
(setq
 *annotations-poly*
 (list
  "bruhn" "barlowAndMorgensternRevised"
  "sectionalRepetitions" "schoenberg" "tomcollins"))
(setq
 *ground-truth-paths*
 (list
  (merge-pathnames
   (make-pathname
    :directory
    '(:relative "groundTruth" "beethovenOp2No1Mvt3"))
  *jkuPattsDevDB-MonthYear-path*)
  (merge-pathnames
   (make-pathname
    :directory
    '(:relative
      "groundTruth" "gibbonsSilverSwan1612"))
  *jkuPattsDevDB-MonthYear-path*)))
(setq
 *algorithm-output-paths*
 (firstn
  2
  (list-directory *algorithms-output-root*)))
; Save the calculated metrics to this csv file.
(setq
 *csv-save-path&name*
 (merge-pathnames
  (make-pathname
   :name "calculated-metrics" :type "csv")
  *MCStylistic-MonthYear-example-files-path*))
(setq
 *metrics-to-calculate*
 (list
  "precision" "recall" "precision-est-card"
  "recall-est-card" "precision-occ-card"
  "recall-occ-card"))
(setq
 *metric-parameters*
 (list
  (list "score-thresh" .75) (list "tolp" t)
  (list "translationp" nil) (list "card-limit" 150)))
(setq *file-type* "csv")
(setq
 *ans*
 (pattern-discovery-metrics
  *algorithm-output-paths* *ground-truth-paths*
  *csv-save-path&name* *task-version*
  *annotations-poly* *metrics-to-calculate*
  *metric-parameters* *file-type*))
--> Writes a file to the specified location.

(setq
 *algorithms-output-root*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "MIREX 2013 pattern discovery task"))
  *MCStylistic-MonthYear-example-files-path*))
(setq
 *ground-truth-paths*
 (list
  (merge-pathnames
   (make-pathname
    :directory
    '(:relative
      "MIREX 2013 pattern discovery task"
      "exampleGroundTruth" "beet_op002_no1_mv1"))
  *MCStylistic-MonthYear-example-files-path*)))
(setq
 *algorithm-output-paths*
 (subseq
  (list-directory *algorithms-output-root*)
  4 6))
; Save the calculated metrics to this csv file.
(setq
 *csv-save-path&name*
 (merge-pathnames
  (make-pathname
   :name "calculated-metrics2" :type "csv")
  *MCStylistic-MonthYear-example-files-results-path*))
(setq
 *metrics-to-calculate*
 (list
  "precision" "recall" "precision-est-card"
  "recall-est-card"))
(setq
 *metric-parameters*
 (list
  (list "score-thresh" .75) (list "tolp" t)
  (list "translationp" nil) (list "card-limit" 150)))
(setq *file-type* nil)
(setq *file-structure* "all-patt-all-occ")
(setq
 *ans*
 (pattern-discovery-metrics
  *algorithm-output-paths* *ground-truth-paths*
  *csv-save-path&name* nil nil *metrics-to-calculate*
  *metric-parameters* nil *file-structure*))
--> Writes a file to the specified location.
\end{verbatim}

\noindent This function loops over algorithm output
and annotated ground truth patterns. It computes a
list of metrics for each (algorithm output, annotation
ground truth) pair, and according to an optional
argument, writes the results to a csv table. If there
is no algorithm output for a particular piece, it will
be skipped and an empty row will appear in the
table. |#

(defun pattern-discovery-metrics
       (algorithm-output-paths ground-truth-paths
        &optional
        (csv-save-path&name nil)
        (task-version "polyphonic")
        (annotations-to-use
         (list
          "bruhn" "barlowAndMorgensternRevised"
          "sectionalRepetitions" "schoenberg"
          "tomCollins"))
        (metrics-to-calculate
         (list
          "precision-est-card" "recall-est-card"
          "precision-occ-card" "recall-occ-card"))
        (metric-parameters
         (list
          (list "score-thresh" .75)
          (list "tolp" t)))
        (file-type "csv")
        (file-structure "sep-patt-sep-occ")
        (nalg (length algorithm-output-paths))
        (ngtr (length ground-truth-paths))
        (nmet (length metrics-to-calculate))
        (metric-mtx
         (loop for i from 0 to (- nalg 1) collect
           (loop for j from 0 to (- ngtr 1) collect
             (if (string=
                  file-structure "all-patt-all-occ")
         (metrics-for-algorithm&piece-all-patt-all-occ
                (nth i algorithm-output-paths)
                (nth j ground-truth-paths) i j
                metrics-to-calculate
                metric-parameters)
               (metrics-for-algorithm&piece
                (nth i algorithm-output-paths)
                (nth j ground-truth-paths) i j
                task-version annotations-to-use
                metrics-to-calculate metric-parameters
                file-type)))))
        (file
         (if csv-save-path&name
           (open
            csv-save-path&name
            :direction :output :if-does-not-exist
            :create :if-exists :overwrite))))
  (if (null csv-save-path&name)
    metric-mtx
    (progn
      (loop for i from 0 to (- nalg 1) do
        (progn
          (format
           file "~S~%"
           (nth i algorithm-output-paths))
          (format file "piece,")
          (loop for k from 0 to (- nmet 2) do
             (format
              file "~S,"
              (nth k metrics-to-calculate)))
          (format
           file "~S~%" (my-last metrics-to-calculate))
          (loop for j from 0 to (- ngtr 1) do
            (progn
              (format
               file "~S," (nth j ground-truth-paths))
              (loop for k from 0 to (- nmet 2) do
                (if (nth j (nth i metric-mtx))
                  (format
                   file "~5$,"
                   (nth
                    k (nth j (nth i metric-mtx))))
                  (format file ",")))
              (if (nth j (nth i metric-mtx))
                (format
                 file "~5$~%"
                 (my-last
                  (nth j (nth i metric-mtx))))
                (format file "~%")))))
        (format file "~%~%"))
      (close file))))

#|
\noindent Example:
\begin{verbatim}
(setq
 *annotation-collection*
 (merge-pathnames
  (make-pathname
   :directory
   (list
    :relative "groundTruth" "bachBWV889Fg"
    *task-version* "repeatedPatterns" "bruhn"))
  *jkuPattsDevDB-MonthYear-path*))
(setq
 *pattern-paths*
 (remove-if-not
  #'directory-pathname-p
  (list-directory *annotation-collection*)))
(read-patts&occs *pattern-paths*)
--> ((((1 64) (2 60) (3 65) (4 56) (6 62) (7 59)
       (7 64) (8 60))
      ((21 76) (22 72) (23 77) (24 68) (26 74) (27 71)
       (27 76) (28 72))...
      ((66 60) (67 65) (68 56) (70 62) (71 59) (71 64)
       (72 60)))
     (((10 64) (10 62) (10 60) (10 59) (11 57) (11 60)
       (11 59) (11 57) (11 55) (12 54) (12 55) (12 57)
       (12 55) (12 54) (12 52) (13 51) (13 52) (13 54)
       (13 52) (13 51) (13 49) (14 47) (14 49) (14 51)
       (15 51) (15 49) (15 51) (16 52))
      ((22 69) (22 67) (22 65) (22 64) (23 62) (23 65)
       (23 64) (23 62) (23 60) (24 59) (24 60) (24 62)
       (24 60) (24 59) (24 57) (25 56) (25 57) (25 59)
       (25 57) (25 56) (25 54) (26 52) (26 54) (26 56)
       (27 56) (27 54) (27 56) (28 57))...
      ((88 50) (88 52) (88 53) (88 52) (88 50) (88 48)
       (89 47) (89 48) (89 50) (89 48) (89 47) (89 45)
       (90 43) (90 45) (90 47) (91 47) (91 45) (91 47)
       (92 48)))
     (((24 53) (25 47) (25 50) (26 44) (26 47) (27 52)
       (28 45))
      ((52 60) (53 54) (53 57) (54 51) (54 54) (55 59)
       (56 52))...
      ((88 69) (89 62) (89 65) (90 59) (90 74)
       (91 67))))
\end{verbatim}

\noindent This function lists a pattern collection
(for example the patterns annotated in a fugue by
Bruhn, or the patterns output by an algorithm), and
loads all occurrences of all these patterns as nested
lists. |#

(defun read-patts&occs
       (pattern-paths &optional (file-type "csv")
        (curr-occs-paths
         (if pattern-paths
           (remove-if-not
            #'pathname-typesp
            (list-directory
             (merge-pathnames
              (make-pathname
               :directory
               (list
                :relative "occurrences" file-type))
              (first pattern-paths))))))
        (curr-occs
         (if curr-occs-paths
           (mapcar
            #'(lambda (x)
                (if (string= file-type "csv")
                  (csv2dataset x) (read-from-file x)))
            curr-occs-paths))))
  (if (null pattern-paths) ()
    (cons
     curr-occs
     (read-patts&occs
      (rest pattern-paths) file-type))))

#|
\noindent Example:
\begin{verbatim}
(setq
 *annotations-poly*
 (list
  "bruhn" "barlowAndMorgensternRevised"
  "sectionalRepetitions" "schoenberg" "tomCollins"))
(setq
 *annotation-paths*
 (mapcar
  #'(lambda (x)
      (merge-pathnames
       (make-pathname
        :directory
        (list
         :relative "groundTruth" "beethovenOp2No1Mvt3"
         *task-version* "repeatedPatterns" x))
       *jkuPattsDevDB-MonthYear-path*))
  *annotations-poly*))
(read-ground-truth-for-piece *annotation-paths*)
--> (;pattern 1
     (
      ;occurrence 1
      ((-1 60) (-1 68)...(10 68))
      ...
      ;occurrence m1
      ((449 63) (449 72)..(460 72)))
     ;pattern 2
     (
      ;occurrence 1
      ((239 60) (240 53)...(251 65))
      ...
      ;occurrence m2
      ((414 53) (414 69)...(425 65)))
     ...
     ;pattern n
     (
      ;occurrence 1
      ((41 60) (41 68)...(159 53))
      ...
      ;occurrence mn
      ((437 60) (437 68)...(555 53))))
\end{verbatim}

\noindent This function lists all annotated patterns
for a piece (for example the patterns annotated in a
sonata by Barlow and Morgenstern, and sectional
repetitions), and loads all occurrences of all these
patterns as nested lists. |#

(defun read-ground-truth-for-piece
       (annotation-paths &optional (file-type "csv"))
  (if (null annotation-paths) ()
    (append
     (read-patts&occs
      (remove-if-not
       #'directory-pathname-p
       (list-directory
        (first annotation-paths))) file-type)
     (read-ground-truth-for-piece
      (rest annotation-paths)))))
