(progn
  (setq *print-length* nil)
  (setq *print-pretty* nil)
  (defvar
      *MCStylistic-MonthYear-path*
    (make-pathname
     :directory
     '(:absolute
       "Users" "Shared")))
  #| Begin user selection with conditional, so
  multiple setups are possible with one setup file. |#
  (setq *user* "tom mac")
  (setq
      *MCStylistic-MonthYear-path*
    (cond
     ((string= *user* "")
      #| If user uses path above, then no changes
      necessary here. |#
      *MCStylistic-MonthYear-path*)
     ((string= *user* "tom mac")
      (make-pathname
       :directory
       '(:absolute
         "Users" "tomthecollins" "Shizz" "repos"
         "mcp" "lib" "MCStylistic")))
     ((string= *user* "ak mac")
      (make-pathname
       :directory
       '(:absolute
         "Users" "ak" "sources" "collcodeinit"
         "private" "core" "lisp" "MCStylistic-curr")))
     ((string= *user* "tom linux")   
      (make-pathname
       :directory
       '(:absolute
         "home" "tcolli07" "repos" "collcodeinit"
         "private" "core" "lisp" "MCStylistic-curr")))
     ((string= *user* "tom linux camel")   
      (make-pathname
       :directory
       '(:absolute
         "home" "tcolli07" "repos" "collCodeInit"
         "private" "core" "lisp" "MCStylistic-curr")))
     ((string= *user* "brian mac")   
      (make-pathname
       :directory
       '(:absolute
         "Users" "otterteeth" "Development" "PhD"
         "collCodeInit" "private" "core" "lisp"
         "MCStylistic-curr")))
     ((string= *user* "robin mac")   
      (make-pathname
       :directory
       '(:absolute
         "Users" "robin" "directoryname"
         "subdirectory" "collCodeInit" "private"
         "core" "lisp" "MCStylistic-curr")))
     ((string= *user* "katrien mac")
      (make-pathname
       :directory
       '(:absolute
         "Users" "tomthecollins" "Shizz" "repos"
         "collcodeinit" "private" "core" "lisp"
         "MCStylistic-curr")))
      ((string= *user* "michael mac")
       (make-pathname
        :directory
        '(:absolute
          "Users" "michael" "Documents" "repos"
          "collcodeinit" "private" "core" "lisp"
          "MCStylistic-curr")))
    ))
  (defvar
      *MCStylistic-MonthYear-data-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Data"))
     *MCStylistic-MonthYear-path*))
  (defvar
      *MCStylistic-MonthYear-functions-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Functions"))
     *MCStylistic-MonthYear-path*))
  (defvar
      *MCStylistic-MonthYear-example-files-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Example files"))
     *MCStylistic-MonthYear-path*))
  (defvar
      *MCStylistic-MonthYear-example-files-data-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Example data"))
     *MCStylistic-MonthYear-example-files-path*))
  (defvar
    *MCStylistic-MonthYear-example-files-results-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "Example results"))
     *MCStylistic-MonthYear-example-files-path*))
  (defvar
      *jkuPattsDevDB-MonthYear-path*
    (make-pathname
     :directory
     '(:absolute
       "Users" "tomthecollins" "Shizz" "JKU"
       "Projects" "MIREX2013" "JKUPDD-MonthYear")
     #|
     '(:absolute
       "Users" "Shared" "jkuPattsDevDB-MonthYear"
       "MCStylistic-MonthYear")
     |#))
  (defvar
      *jkuPattsDevDB-MonthYear-gtr-path*
    (merge-pathnames
     (make-pathname
      :directory '(:relative "groundTruth"))
     *jkuPattsDevDB-MonthYear-path*))
  (format t "MCStylistic-MonthYear paths defined.~%")
  (defvar *equality-tolerance* 1e-5)
  (defvar *pitch-mod* 12)
  (defvar *rs* (make-random-state t))
  (defvar *transition-matrix* ())
  (defvar *chunk-type* ())
  (defvar *chunk-length* 0)
  (defvar *midi-file-format* 0)
  (defvar *midi-file-ntrks* 0)
  (defvar *midi-file-granularity* 24)
  (defvar *track-time* 0)
  (defvar *running-status* 0)
  (defvar *track-end* t)
  (defvar
      *sequence-strings*
    (make-array
     1
     :initial-contents #("sequence-strings")
     :fill-pointer t :adjustable t))
  (defvar
      *sequence-tempo-map*
    (make-array
     1
     :element-type 'list :initial-element '(0 0 1)
     :fill-pointer t :adjustable t))
  (defvar
      *sequence-notes*
    (make-array
     0
     :element-type 'list :initial-element '(0 0 0 0 0)
     :fill-pointer t :adjustable t))
  (defconstant
      kMThdHeaderBytes '(#x4D #x54 #x68 #x64))
  (defconstant kMThdHeaderLenBytes '(0 0 0 6))
  (defconstant kMThdHeaderFormatBytes '(0 1))
  (defconstant
      kMTrkHeaderBytes '(#x4D #x54 #x72 #x6B))
  (defconstant kMTrkEndBytes '(#x00 #xFF #x2F #x00))
  (defconstant kPPQNValue #x30)
  (defvar *events* nil)
  (defvar *channel-no-1* 1)
  (defvar *channel-no-2* 2)
  (defvar *channel-no-3* 3)
  (defvar *channel-no-4* 4)
  (defvar *channel-no-5* 5)
  (defvar *channel-no-6* 6)
  (defvar *channel-no-7* 7)
  (defvar *channel-no-8* 8)
  (defvar *channel-no-9* 9)
  (defvar *channel-no-10* 10)
  (defvar *channel-no-11* 11)
  (defvar *channel-no-12* 12)
  (defvar *channel-no-13* 13)
  (defvar *channel-no-14* 14)
  (defvar *channel-no-15* 15)
  (defvar *channel-no-16* 16)
  (defvar *overwrite-if-exists* t)
  (format
   t
   (concatenate
    'string
    "MCStylistic-MonthYear variables defined.~%"
    "Now loading functions, please be patient..."))

  ;Debug - BB 06.24.15
  #|(format
   t
   (concatenate
    'string
    "MCStylistic--MonthYear-functions-path: ~a~%" 
    *MCStylistic-MonthYear-functions-path*
    ))
  |#

  (load
   (merge-pathnames
    (make-pathname
   :directory '(:relative "Third party" "cl-fad")
   :name "fad2" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "list-processing" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 10\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "locating-indices" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "real-interval-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "set-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 20\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "merge-sort-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "interpolation" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "sort-by" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "vector-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 30\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "geometric-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern rating")
     :name "projection" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "stats-sampling" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 40\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "text-files" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "midi-import" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "midi-export" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "structural-induction-mod" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern rating")
     :name "empirical-preliminaries" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern rating")
     :name "musical-properties" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 50\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern rating")
     :name "evaluation-heuristics" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "structural-induction-merge"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "director-musices"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "kern"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
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
     :directory '(:relative "File conversion")
     :name "hash-tables"
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
     :directory '(:relative "File conversion")
     :name "kern-by-col"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 60\%...")
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
     :directory '(:relative "Query staff notation")
     :name "artic-dynam-lyrics-utilities"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "pitches-intervals-durations"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "analytic-string-manipulations"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "kern-rests"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "kern-articulation"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 70\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "spacing-states" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "keyscape" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "beat-rel-MNN-states" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "markov-analyse" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "markov-analyse-backwards" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "markov-compose" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 80\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "realising-states" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "realising-states-backwards" :type "lisp")
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
     :directory '(:relative "Markov models")
     :name "generating-beat-MNN-spacing-backwards"
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
     :name "generating-beat-relative-MNN"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "generating-beat-relative-MNN-backwards"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
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
     :directory '(:relative "Pattern discovery")
     :name "evaluation-for-SIACT"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "pattern-inheritance-preliminaries"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 90\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "generating-with-patterns" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "evaluation-for-SIA+" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "superdiagonals" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "further-structural-induction-algorithms"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "compactness-trawl" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "ontimes-signatures" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "chord-labelling" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "inner-metric-analysis" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "neo-riemannian-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "kern-to-staff-features" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "texture" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "c@merata-processing" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern metrics")
     :name "matching-score" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern metrics")
     :name "robust-metrics" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern metrics")
     :name
     (concatenate
      'string "evaluate-discovered-versus-"
      "annotated-patterns") :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 100\%~%")
  (format t "Welcome to MCStylistic-Mar2017.")
  )

#|
(in-package :common-lisp-user)
|#

















#| Needs finishing off.
  
  #| This is a temporary load command. I expect it to
  be replaced by a call to some MNN-relative version
  of generating-with-patterns. |#
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "generating-beat-relative-MNN-for&back"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 10\%...")
  ; End temporary load command.
  
  
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "pattern-inheritance-preliminaries"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 20\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "further-structural-induction-algorithms"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "markov-analyse-backwards" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 30\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "midi-import" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "compactness-trawl" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern discovery")
     :name "evaluation-for-SIACT" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 40\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "csv-files" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "geometric-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Maths foundation")
     :name "locating-indices" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "kern-articulation" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 50\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "inner-metric-analysis" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "keyscape" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "neo-riemannian-operations" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 60\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Pattern metrics")
     :name
     (concatenate
      'string "evaluate-discovered-versus-annotated"
      "-patterns") :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "beat-rel-MNN-states" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 70\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "ontimes-signatures" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "analytic-string-manipulations"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Harmony and metre")
     :name "chord-labelling" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 80\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "kern-to-staff-features" :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "texture"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 90\%...")
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "File conversion")
     :name "kern-rests"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Query staff notation")
     :name "c@merata-processing"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
  (format t " 100\%~%")
  (format nil "Welcome to MCStylistic-Mar2017."))
|#