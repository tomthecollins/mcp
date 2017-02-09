#| Copyright 2008-2013 Tom Collins
   Thursday 4 August 2011

Discovering and rating musical patterns

This code demonstrates the use of a pattern discovery
algorithm, applied to bars 1-19 of the Sonata in C
minor L10 by Domenico Scarlatti (1685-1757). The
algorithm is the Structure Induction Algorithm with
Compactness Trawling (SIACT), as defined by Collins,
Thurlow, Laney, Willis, and Garthwaite (2010). It
combines the Strucutre Induction Algorithm (SIA) with
the concept of compactness (Meredith, Lemstrom, and
Wiggins, 2002, 2003). The formula for rating
discovered patterns was developed by Collins, Laney,
Willis, and Garthwaite (2011). More discussion of this
code can be found in the Documentation, Sec. 3.1.

Please note: it is assumed that the package
MCStylistic has been loaded, and that the variable
*MCStylistic-Oct2013-example-files-path* has been
defined appropriately. Files are imported from and
exported to subfolders at the location specified by
this variable.

References

Tom Collins, Jeremy Thurlow, Robin Laney, Alistair
Willis, and  Paul H. Garthwaite. A comparative
evaluation of algorithms for discovering translational
patterns in Baroque keyboard works. In J. Stephen
Downie and Remco Veltkamp, editors, Proceedings of the
International Symposium on Music Information
Retrieval, pages 3-8, Utrecht, 2010. International
Society for Music Information Retrieval.

Tom Collins, Robin Laney, Alistair Willis, and Paul H.
Garthwaite. Modelling pattern importance in Chopin's
mazurkas. Music Perception, 28(4):387-414, 2011.

David Meredith, Kjell Lemstrom, and Geraint A.
Wiggins. Algorithms for discovering repeated patterns
in multidimensional representations of polyphonic
music. Journal of New Music Research, 31(4):321–345,
2002.

David Meredith, Kjell Lemstrom, and Geraint A.
Wiggins. Algorithms for discovering repeated patterns
in multidimensional representations of polyphonic
music. In Cambridge Music Processing Colloquium, 11
pages, Cambridge, UK, 2003. Department of Engineering,
University of Cambridge. Retrieved 10 August, 2009
from http://www.titanmusic.com/papers/public
/cmpc2003.pdf. |#


#| Step 1 - Set the parameters. |#
(setq *compact-thresh* 2/3)
(setq *cardina-thresh* 3)
(setq *region-type* "lexicographic")

#| Step 2 - Load dataset and create projections. |#
(progn
  (setq
   *dataset*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "scarlatti-L10-bars1-19" :type "txt")
     *MCStylistic-MonthYear-example-files-data-path*)))
  (setq
   *dataset-1-1-0-1-0*
   (orthogonal-projection-unique-equalp
    *dataset* '(1 1 0 1 0)))
  (setq
   *dataset-1-0-1-0-0*
   (orthogonal-projection-unique-equalp
    *dataset* '(1 0 1 0 0)))
  "Dataset loaded and projections created")

#| Step 3 - Run SIA on projection (1 1 0 1 0). |#
(time
 (SIA-reflected-merge-sort
  *dataset-1-1-0-1-0*
  (merge-pathnames
   (make-pathname
    :name "L 10 (1 1 0 1 0) SIA" :type "txt")
   *MCStylistic-MonthYear-example-files-results-path*)))
; 0.585303 seconds.

#| Step 4 - Run SIACT on projection (1 1 0 1 0). |#
(progn
  (setq
   *SIA-1-1-0-1-0*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 1 0 1 0) SIA" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
     ))
  (time
   (compactness-trawler
    *SIA-1-1-0-1-0* *dataset-1-1-0-1-0*
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 1 0 1 0) SIACT" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
    *compact-thresh* *cardina-thresh* *region-type*)))
; 0.559605 seconds.

#| Step 5 - Rate discovered patterns for projection
(1 1 0 1 0). |#
(progn
  (setq
   *SIACT-1-1-0-1-0*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 1 0 1 0) SIACT" :type "txt")
     *MCStylistic-MonthYear-example-files-results-path*)
     ))
  (time
   (setq
    *hash-1-1-0-1-0*
    (evaluate-variables-of-patterns2hash
     *SIACT-1-1-0-1-0* *dataset-1-1-0-1-0*)))
  (write-to-file-balanced-hash-table
   *hash-1-1-0-1-0*
   (merge-pathnames
    (make-pathname
     :name "L 10 (1 1 0 1 0) hash" :type "txt")
    *MCStylistic-MonthYear-example-files-results-path*))
  (concatenate
   'string
   "Discovered patterns have been rated and placed in"
   " in the hash table *hash-1-1-0-1-0*. They have"
   " also been written to a text file for future"
   " reference."))
; 2.633121 seconds.

#| Here are the details for pattern A, as annotated in
the Documentation, Fig. 3.1. |#
(disp-ht-el (nth 13 *hash-1-1-0-1-0*))
--> (("name" . "pattern 24") ("compactness" . 1)
     ("expected occurrences" . 35.375904)
     ("rating" . 7.539052)
     ("pattern"
      (1/2 60 1/2) (1 63 1/2) (3/2 67 1/2) (2 72 1/2)
      (5/2 75 1/2) (3 79 1/2) (7/2 84 1/2) (4 79 1/2)
      (9/2 75 1/2) (5 72 1/2) (11/2 67 1/2) (6 60 1))
     ("translators" (0 0 0) (6 -12 0)) ("index" . 24)
     ("cardinality" . 12) ("MTP vectors" (6 -12 0))
     ("compression ratio" . 24/13)
     ("region"
      (1/2 60 1/2) (1 63 1/2) (3/2 67 1/2) (2 72 1/2)
      (5/2 75 1/2) (3 79 1/2) (7/2 84 1/2) (4 79 1/2)
      (9/2 75 1/2) (5 72 1/2) (11/2 67 1/2) (6 60 1))
     ("occurrences" . 2))

#| Step 6 - Run SIA on projection (1 0 1 0 0). |#
(time
 (SIA-reflected-merge-sort
  *dataset-1-0-1-0-0*
  (merge-pathnames
   (make-pathname
    :name "L 10 (1 0 1 0 0) SIA" :type "txt")
   *MCStylistic-Mar2013-example-files-results-path*)))
; 0.445292 seconds.

#| Step 7 - Run SIACT on projection (1 0 1 0 0). |#
(progn
  (setq
   *SIA-1-0-1-0-0*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 0 1 0 0) SIA" :type "txt")
     *MCStylistic-Mar2013-example-files-results-path*)
     ))
  (time
   (compactness-trawler
    *SIA-1-0-1-0-0* *dataset-1-0-1-0-0*
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 0 1 0 0) SIACT" :type "txt")
     *MCStylistic-Mar2013-example-files-results-path*)
    *compact-thresh* *cardina-thresh* *region-type*)))
; 0.623635 seconds.

#| Step 8 - Rate discovered patterns for projection
(1 0 1 0 0). |#
(progn
  (setq
   *SIACT-1-0-1-0-0*
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "L 10 (1 0 1 0 0) SIACT" :type "txt")
     *MCStylistic-Mar2013-example-files-results-path*)
    ))
  (time
   (setq
    *hash-1-0-1-0-0*
    (evaluate-variables-of-patterns2hash
     *SIACT-1-0-1-0-0* *dataset-1-0-1-0-0*)))
  (write-to-file-balanced-hash-table
   *hash-1-0-1-0-0*
   (merge-pathnames
    (make-pathname
     :name "L 10 (1 0 1 0 0) hash" :type "txt")
    *MCStylistic-Mar2013-example-files-results-path*))
  (concatenate
   'string
   "Discovered patterns have been rated and placed in"
   " in the hash table *hash-1-0-1-0-0*. They have"
   " also been written to a text file for future"
   " reference."))
; 3.440220 seconds.

#| Here are the details for pattern B, as annotated in
the Documentation, Fig. 3.1. |#
(disp-ht-el (nth 2 *hash-1-0-1-0-0*))
--> (("name" . "pattern 40") ("compactness" . 13/15)
     ("expected occurrences" . 44.16958)
     ("rating" . 8.927441)
     ("pattern"
      (73/4 71) (37/2 72) (75/4 73) (19 69) (19 74)
      (39/2 68) (39/2 73) (20 69) (20 74) (41/2 68)
      (41/2 73) (21 69) (21 74))
     ("translators"
      (-6 0) (-3 -7) (0 0) (3 -7) (15 -10) (18 -17)
      (24 -11) (30 -12))
     ("index" . 40) ("cardinality" . 13)
     ("MTP vectors"
      (36 -12) (30 -11) (30 -12) (24 -11) (24 -17)
      (18 -17) (15 -2) (12 -3) (9 -1) (9 -7) (3 -7)
      (3 -7))
     ("compression ratio" . 26/5)
     ("region"
      (73/4 71) (37/2 72) (75/4 73) (19 64) (19 69)
      (19 74) (39/2 68) (39/2 73) (20 64) (20 69)
      (20 74) (41/2 68) (41/2 73) (21 69) (21 74))
     ("occurrences" . 8))

#| Pattern C, as annotated in the Fig. 3.1 of the
Documentation, is not discovered by SIACT. Close
inspection of the music reveals that C is not a
translational pattern for either of the projections
considered above.

Pattern D, as annotated in Fig. 3.1 of the
Documentation, is not discovered by SIACT. The
following code will give details of a pattern that
contains D. |#
(disp-ht-el (nth 10 *hash-1-0-1-0-0*))
