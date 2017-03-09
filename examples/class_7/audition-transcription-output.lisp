#| Copyright Tom Collins 3/9/2017.

Script for loading a text file output by Boeck's
transcription algorithm and converting it to a MIDI
file for audition. |#

#| Set some paths. |#
(setq *user* "?")
(if (string= *user* "?")
  (setq
   *text-file-dir*
   (make-pathname
    :directory
    '(:absolute
      "?" "??")))
  (setq
   *text-file-dir*
   (make-pathname
    :directory
    '(:absolute
      "Users" "tomthecollins" "Shizz" "repos" "mcp"
      "examples" "class_7"))))

(setq *piece-name* "Labyrinth")

(setq
 text-source
 (merge-pathnames
  (make-pathname
   :name *piece-name* :type "txt") *text-file-dir*))
(setq
 midi-out
 (merge-pathnames
  (make-pathname
   :name *piece-name* :type "mid") *text-file-dir*))

#| Read in the transcribed version of the piece. |#
(progn
  (setq
   dataset
   (mapcar
    #'(lambda (x)
        (tab-separated-reals2list x))
    (read-from-file-arbitrary text-source)))
  "Yes!")

; Output it to a MIDI file.
(saveit
 midi-out
 (mapcar
    #'(lambda (x)
        (list
         (* 1000 (first x))
         (second x)
         1000
         1
         80))
    dataset))

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
      i (float (nth i as-info-raw)))))
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
           (guess-morphetic (second x) '(4 5))
           (third x) (fourth x) (sixth x) (seventh x)))
 dataset-synch))
  "Yes!")

#| Now write the symbolic-audio synchronised version
of the piece to text and csv files as usual. |#
(progn
  (write-to-file
   dataset-synch dataset-destination)
  (dataset2csv dataset-synch csv-destination))
