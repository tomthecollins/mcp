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

