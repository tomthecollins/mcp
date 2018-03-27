#| Copyright Tom Collins 2/11/2018.

Script for loading a tab-separated text file of onsets
and MIDI note numbers and saving to a MIDI file. |#

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
(setq *piece-name* "Jaymes Young - Don't You Know")

; Import.
(progn
  (setq
   data
   (read-from-file-arbitrary
    (merge-pathnames
     (make-pathname
      :name *piece-name* :type "txt") *path&name*)))
  (setq
   dataset
   (mapcar
    #'(lambda (x)
        (tab-separated-string2list x))
    data))
  (setq
   dataset
   (mapcar
    #'(lambda (x)
        (list
         (round
          (* 1000 (read-from-string (first x))))
         (parse-integer (second x))
         1000 1 84))
    dataset))
  "Yes!")

; Export.
(saveit
 (merge-pathnames
  (make-pathname
   :name *piece-name* :type "mid") *path&name*)
 dataset)

