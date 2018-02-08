#| Copyright Tom Collins 2/8/2018.

Script for loading a symbolic version of a piece, as
well as the audio_time_to_symbolic_time information,
and outputting a new file that contains the symbolic
version of the piece and an extra column for the
estimated audio time. |#

#| Set some paths. |#
(setq
 *music-data-root*
 (make-pathname
  :directory
  '(:absolute
     "Users" "tomthecollins" "Shizz" "repos" "mcp"
     "assignments" "1" "visualization")))
(setq
 *path&name*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "example"))
  *music-data-root*))
(setq *piece-name* "Fantaisie-impromptu_Op.66_-_Chopin")
(setq *synch-name* "Fantaisie-impromptu_Op.66_-_Chopin_synch")

#| Symbolic version, bar2beat, and
audioTime2symbolicTime information. |#
(setq
 symbolic-source
 (merge-pathnames
  (make-pathname
   :name *piece-name* :type "mid") *path&name*))
(setq
 audioTime2symbolicTime-source
 (merge-pathnames
  (make-pathname
   :name "annotation_layer"
   :type "txt") *path&name*))
(setq
 dataset-destination
 (merge-pathnames
  (make-pathname
   :name *synch-name* :type "txt") *path&name*))
(setq
 csv-destination
 (merge-pathnames
  (make-pathname
   :name *synch-name* :type "csv") *path&name*))

#| Read in the symbolic version of the piece. |#
(progn
  (setq
   dataset
   (load-midi-file symbolic-source))
  "Yes!")
; Round up durations because they are annoying!
(progn
  (setq
   dataset
   (mapcar
    #'(lambda (x)
        (append
         (firstn 2 x)
         (list (+ (third x) 1/480))
         (lastn 2 x)))
    dataset))
   "Yes!")

#| Read in audioTime2symbolicTime information. |#
(progn
  (setq
   as-info-raw
   (read-from-file audioTime2symbolicTime-source))
  "Yes!")
#| Create a list of knot-value pairs. |#
(progn
  (setq
   knot-value-pairs
   (loop for i from 0 to (- (length as-info-raw) 1)
     collect
     (list
      (* 4 i) (float (nth i as-info-raw)))))
  "Yes!")

#| Estimate the onsets and offsets. |#
(progn
  (setq
   on-est
   (linearly-interpolate-x-values
    (nth-list-of-lists 0 dataset) knot-value-pairs))
  (setq
   off-est
   (linearly-interpolate-x-values
    (add-two-lists
     (nth-list-of-lists 0 dataset)
     (nth-list-of-lists 2 dataset)) knot-value-pairs))
  (setq
   dataset-synch
   (mapcar
    #'(lambda (x y z)
        (append x (list y) (list z)))
    dataset on-est off-est))
  (setq
   dataset-synch
   (mapcar
    #'(lambda (x)
        (list
         (first x) (second x)
         (guess-morphetic (second x))
         (third x) (fourth x) (sixth x)
         (seventh x)))
    dataset-synch))
  "Yes!")

#| Now write the symbolic-audio synchronised version
of the piece to text and csv files as usual. |#
(progn
  (write-to-file
   dataset-synch dataset-destination)
  (dataset2csv dataset-synch csv-destination))
