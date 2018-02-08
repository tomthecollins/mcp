#| Copyright 2008-2014 Tom Collins
   Tuesday 17 June 2014
   Incomplete

\noindent Top-level functions for the MediaEval 2014
C@merata task.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :name "kern-by-col"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Query staff notation")
   :name "kern-to-staff-features"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Harmony and metre")
   :name "ontimes-signatures"
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
   :directory '(:relative "Maths foundation")
   :name "real-interval-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "sort-by"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Query staff notation")
   :name "texture"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(setq question-number "004")
(setq
 question-path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1")
   :name "training_v1" :type "xml")
  *MCStylistic-MonthYear-data-path*))
(setq
 notation-path
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1"))
  *MCStylistic-MonthYear-data-path*))
(setq notation-name "f3")
(Stravinsqi-Jun2014
 question-number question-path&name
 notation-path notation-name)
--> (wrote the following to text file)
  <question number="004" music_file="f3.xml" divisions="4">
    <text>"F# followed by a major sixth"</text>
    <answer>
      <passage start_beats="3" start_beat_type="8"
               end_beats="3" end_beat_type="8"
               start_divisions="4" end_divisions="4"
               start_bar="2" start_offset="6"
               end_bar="2" end_offset="6" />
    </answer>
  </question>
\end{verbatim}

\noindent This function reads a question string from
an xml file, loads a piece/excerpt of music to which
the question refers, analyses the music so as to
answer the question, and writes the answer to a
different xml results file. |#

(defun Stravinsqi-Jun2014
       (question-number question-path&name
        notation-path notation-name
        &optional
        (notation-path&name
         (merge-pathnames
          (make-pathname
           :name notation-name :type "krn")
          notation-path))
        ; Get question string and division value.
        (question&division
         (c@merata2014-question-file2question-string
          question-number question-path&name
          notation-name))
        (division (second question&division))
        ; Split up compound questions.
        (questions
         (followed-by-splitter
          (first question&division)))
        #| Load staff names and names of the opening
        clefs on each staff. |#
        (staff&clef-names
         (staves-info2staff&clef-names
          notation-path&name))
        #| Load time-signature information and any
        changes across the piece. |#
        (ontimes-signatures
         (append-ontimes-to-time-signatures
          (kern-file2ontimes-signatures
           notation-path&name)))
        ; Load point-set representations.
        ;#|
        (artic-set
         (kern-file2points-artic-dynam-lyrics
          notation-path&name))
        ;|#
        (point-set
         (kern-file2dataset-by-col
          notation-path&name))
        (rest-set
         (kern-file2rest-set-by-col
          notation-path&name))
        (texture-set
         (texture-from-kern notation-path&name))
        (tie-set
         (kern-file2tie-set-by-col
          notation-path&name))
        #| Get time-interval answers to elemental
        questions. |#
        (ans-harmonic-interval
         (mapcar
          #'(lambda (x)
              (harmonic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        ; Mel. intervals to be prefaced by "melodic".
        (ans-melodic-interval
         (mapcar
          #'(lambda (x)
              (melodic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur-pitch
         (mapcar
          #'(lambda (x)
              (duration&pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-pitch
         (mapcar
          #'(lambda (x)
              (pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur
         (mapcar
          #'(lambda (x)
              (duration-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-nadir-apex
         (mapcar
          #'(lambda (x)
              (nadir-apex-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-triad
         (mapcar
          #'(lambda (x)
              (triad-time-intervals
               (first x) point-set))
          questions))
        ; Below, any triad, particular inversion:
        (ans-triad-inversion
         (mapcar
          #'(lambda (x)
              (triad-inversion-time-intervals
               (first x) point-set))
          questions))
        (ans-texture
         (mapcar
          #'(lambda (x)
              (texture-time-intervals
               (first x) texture-set
               staff&clef-names)) questions))
        (ans-cadence
         (mapcar
          #'(lambda (x)
              (cadence-time-intervals
               (first x) point-set
               ontimes-signatures))
          questions))
        ;#|
        (ans-word&event
         (mapcar
          #'(lambda (x)
              (word&event-time-intervals
               (first x) artic-set))
          questions))
        (ans-word
         (mapcar
          #'(lambda (x)
              (word-time-intervals
               (first x) artic-set)) questions))
        (ans-artic
         (mapcar
          #'(lambda (x)
              (articulation&event-time-intervals
               (first x) artic-set
               staff&clef-names))
          questions))
        ;|#
        (ans-dur-rest
         (mapcar
          #'(lambda (x)
              (rest-duration-time-intervals
               (first x) rest-set staff&clef-names))
          questions))
        (ans-tied&event
         (mapcar
          #'(lambda (x)
              (tied&event-time-intervals
               (first x) tie-set)) questions))
         
        
        ; Define the set of answers.
        (time-intervals
         (list
          ans-harmonic-interval
          ans-melodic-interval
          ans-dur-pitch
          ans-pitch
          ans-dur
          ans-nadir-apex
          ans-triad
          ans-triad-inversion
          ans-texture
          ans-cadence
          ans-word&event
          ans-word
          ans-artic
          ans-dur-rest
          ans-tied&event))
        
        #| Check within consecutive questions for
        consecutive answers. |#
        #|
        (time-intervals
         (collect-consecutive-answers
          (list
           ans-harmonic-interval
           ans-melodic-interval)))
        |#
        #| Check within simultaneous questions for
        simultaneous answers. |#
        
        #| Cross-check the timings of compound
        questions. |#
        
         
        (time-intervals
         (if (> (length questions) 1)
           (cross-check-compound-questions
            questions ontimes-signatures
            time-intervals)
           (if (index-item-1st-doesnt-occur
                (list nil) time-intervals)
             (first
              (nth
               (index-item-1st-doesnt-occur
                (list nil) time-intervals)
               time-intervals)))))
        )
  #| Convert time-interval answers to required
  format and write to file. |#
  (c@merata2014-write-answer
   (first question&division) division time-intervals
   ontimes-signatures question-number
   notation-path notation-name))

#|
\noindent Example:
\begin{verbatim}
(setq question-number "004")
(setq
 question-path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1")
   :name "training_v1" :type "xml")
  *MCStylistic-MonthYear-data-path*))
(setq
 notation-path
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1"))
  *MCStylistic-MonthYear-data-path*))
(setq notation-name "f3")
(Stravinsqi-Jun2015
 question-number question-path&name
 notation-path notation-name)
--> (wrote the following to text file)
  <question number="004" music_file="f3.xml" divisions="4">
    <text>"F# followed by a major sixth"</text>
    <answer>
      <passage start_beats="3" start_beat_type="8"
               end_beats="3" end_beat_type="8"
               start_divisions="4" end_divisions="4"
               start_bar="2" start_offset="6"
               end_bar="2" end_offset="6" />
    </answer>
  </question>
\end{verbatim}

\noindent This function reads a question string from
an xml file, loads a piece/excerpt of music to which
the question refers, analyses the music so as to
answer the question, and writes the answer to a
different xml results file.

The differences between Stravinsqi-Jun2015 and
Stravinsqi-Jun2014 are as follows:
1. A destination file path can be specified;
2. Stravinsqi-Jun2015 addresses a `followed-by' bug in
Stravinsqi-Jun2014 (see projects -> MediaEval -> 2014 ->
example-of-followed-by-not-working.lisp for details);
3. Simultaneities are query-able;
4. Specific chords are query-able;
5. Dynamics are query-able;
6. Delimiting to bar numbers (or bar number ranges);

|#

(defun Stravinsqi-Jun2015
       (question-number question-path&name
        notation-path notation-name
        destination-path&name &optional
        (notation-path&name
         (merge-pathnames
          (make-pathname
           :name notation-name :type "krn")
          notation-path))
        ; Get question string and division value.
        (question&division
         (c@merata2014-question-file2question-string
          question-number question-path&name
          notation-name))
        (division (second question&division))
        #| If the question involves delimiting the piece
        to a range of bars only, then extract and
        exploit this information early on (below),
        because it will lead to greater efficiency. |#
        (question&bar-limits
         (bar-delimiter (first question&division)))
        (bar-limits
         (if (> (length question&bar-limits) 1)
           (rest question&bar-limits)))
        ; Split up simultaneous questions.
        (questions-sim
         (simultaneous-splitter
          (first question&bar-limits)))
        ; (question-type
        ;  (if (and
        ;       (null question-type)
        ;       (> (length questions) 1)) "simultaneous"))
        
        ; Split up compound questions.
        (questions-flw
         (loop for q in questions-sim collect
           (followed-by-splitter-Jun2015 q)))
        ; (question-type
        ;  (if (> (length questions) 1) "followed-by"))
        
        #| Load staff names and names of the opening
        clefs on each staff, load time-signature
        information and any changes across the piece,
        and load point-set representations, constraining
        the latter by bar limits if specified. |#
        (point-sets-etc
         (c@merata2015-point-set-loader
          notation-path&name bar-limits))
        (staff&clef-names (first point-sets-etc))
        (ontimes-signatures (second point-sets-etc))
        (artic-set (third point-sets-etc))
        (point-set (fourth point-sets-etc))
        (rest-set (fifth point-sets-etc))
        (texture-set (sixth point-sets-etc))
        (tie-set (seventh point-sets-etc))
        #| Get time-interval answers to elemental
        questions. |#
        (ans-flw
         (loop for qs in questions-flw collect
           (time-intervals-for-question-elements-Jun2015
            qs staff&clef-names ontimes-signatures
            artic-set point-set rest-set texture-set
            tie-set)))
        
        #| Check within simultaneous questions for
        simultaneous answers. |#
        (ans-sim
         (if (equalp (length ans-flw) 2)
           (loop for i from 0
             to (- (length (first ans-flw)) 1) append
             (loop for j from 0
               to (- (length (second ans-flw)) 1)
               when
               (interval-strict-intersectionp
                (nth i (first ans-flw))
                (nth j (second ans-flw)))
               collect
               (interval-strict-intersection
                (nth i (first ans-flw))
                (nth j (second ans-flw)))))
           (first ans-flw)))
        #| This command could go in several places. I
        also considered whether to combine contiguous
        intervals here, but decided against it. |#
        (ans-sim
         (remove-duplicates ans-sim :test #'equalp))
        )
  #| Convert time-interval answers to required
  format and write to file. |#
  (c@merata2015-write-answer
   (first question&division) division ans-sim
   ontimes-signatures question-number
   notation-path notation-name destination-path&name))

#| Old version. Does not incorporate simultaneities.
(defun Stravinsqi-Jun2015
       (question-number question-path&name
        notation-path notation-name
        destination-path&name &optional
        (notation-path&name
         (merge-pathnames
          (make-pathname
           :name notation-name :type "krn")
          notation-path))
        ; Get question string and division value.
        (question&division
         (c@merata2014-question-file2question-string
          question-number question-path&name
          notation-name))
        (division (second question&division))
        #| If the question involves delimiting the piece
        to a range of bars only, then extract and
        exploit this information early on (below),
        because it will lead to greater efficiency. |#
        (question&bar-limits
         (bar-delimiter (first question&division)))
        (bar-limits
         (if (> (length question&bar-limits) 1)
           (rest question&bar-limits)))
        ; Split up compound questions.
        (questions
         (followed-by-splitter
          (first question&bar-limits)))
        ; (question-type
        ;  (if (> (length questions) 1) "followed-by"))
        ; Split up simultaneous questions.
        (questions
         (if (null question-type)
           (simultaneous-splitter
            (first question&bar-limits)) questions))
        (question-type
         (if (and
              (null question-type)
              (> (length questions) 1)) "simultaneous"))
              
        #| Load staff names and names of the opening
        clefs on each staff, load time-signature
        information and any changes across the piece,
        and load point-set representations, constraining
        the latter by bar limits if specified. |#
        (point-sets-etc
         (c@merata2015-point-set-loader
          notation-path&name bar-limits))
        (staff&clef-names (first point-sets-etc))
        (ontimes-signatures (second point-sets-etc))
        (artic-set (third point-sets-etc))
        (point-set (fourth point-sets-etc))
        (rest-set (fifth point-sets-etc))
        (texture-set (sixth point-sets-etc))
        (tie-set (seventh point-sets-etc))
        #| Get time-interval answers to elemental
        questions. |#
        (ans-harmonic-interval
         (mapcar
          #'(lambda (x)
              (harmonic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        ; Mel. intervals to be prefaced by "melodic".
        (ans-melodic-interval
         (mapcar
          #'(lambda (x)
              (melodic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur-pitch
         (mapcar
          #'(lambda (x)
              (duration&pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-pitch
         (mapcar
          #'(lambda (x)
              (pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur
         (mapcar
          #'(lambda (x)
              (duration-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-nadir-apex
         (mapcar
          #'(lambda (x)
              (nadir-apex-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-triad
         (mapcar
          #'(lambda (x)
              (triad-time-intervals
               (first x) point-set))
          questions))
        ; Below, any triad, particular inversion:
        (ans-triad-inversion
         (mapcar
          #'(lambda (x)
              (triad-inversion-time-intervals
               (first x) point-set))
          questions))
        (ans-texture
         (mapcar
          #'(lambda (x)
              (texture-time-intervals
               (first x) texture-set
               staff&clef-names)) questions))
        (ans-cadence
         (mapcar
          #'(lambda (x)
              (cadence-time-intervals
               (first x) point-set
               ontimes-signatures))
          questions))
        ;#|
        (ans-word&event
         (mapcar
          #'(lambda (x)
              (word&event-time-intervals
               (first x) artic-set))
          questions))
        (ans-word
         (mapcar
          #'(lambda (x)
              (word-time-intervals
               (first x) artic-set)) questions))
        (ans-artic
         (mapcar
          #'(lambda (x)
              (articulation&event-time-intervals
               (first x) artic-set))
          questions))
        ;|#
        (ans-dur-rest
         (mapcar
          #'(lambda (x)
              (rest-duration-time-intervals
               (first x) rest-set staff&clef-names))
          questions))
        (ans-tied&event
         (mapcar
          #'(lambda (x)
              (tied&event-time-intervals
               (first x) tie-set)) questions))
         
        
        ; Define the set of answers.
        (time-intervals
         (list
          ans-harmonic-interval
          ans-melodic-interval
          ans-dur-pitch
          ans-pitch
          ans-dur
          ans-nadir-apex
          ans-triad
          ans-triad-inversion
          ans-texture
          ans-cadence
          ans-word&event
          ans-word
          ans-artic
          ans-dur-rest
          ans-tied&event))
        #| For each question element, determine where
        the most specific answer is located (to avoid
        followed-by errors), and restrict time-intervals
        to these. |#
        (time-interval-idxs
         (loop for i from 0
           to (- (length questions) 1) collect
           (if (index-item-1st-doesnt-occur
                nil (nth-list-of-lists i time-intervals))
             (list
              (index-item-1st-doesnt-occur
               nil (nth-list-of-lists i time-intervals))
              i))))
        (time-intervals
         (loop for i from 0
           to (- (length time-intervals) 1) collect
           (loop for j from 0
             to (- (length questions) 1) collect
             (if (find
                  (list i j) time-interval-idxs
                  :test #'equalp)
               (nth j (nth i time-intervals))))))
        
        #| Check within consecutive questions for
        consecutive answers. |#
        #|
        (time-intervals
         (collect-consecutive-answers
          (list
           ans-harmonic-interval
           ans-melodic-interval)))
        |#
        #| Check within simultaneous questions for
        simultaneous answers. |#
        
        #| Cross-check the timings of compound
        questions. |#
        (time-intervals
         (if (string= question-type "followed-by")
           (if (> (length questions) 1)
             (cross-check-compound-questions
              questions ontimes-signatures
              time-intervals)
             (if (first time-interval-idxs)
               (nth
                (nth 1 (nth 0 time-interval-idxs))
                (nth
                 (nth 0 (nth 0 time-interval-idxs))
                 time-intervals))))
           time-intervals))
        )
  #| Convert time-interval answers to required
  format and write to file. |#
  (c@merata2015-write-answer
   (first question&division) division time-intervals
   ontimes-signatures question-number
   notation-path notation-name destination-path&name))
|#

#|
\noindent Example:
\begin{verbatim}
(setq question-number "004")
(setq
 question-path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1")
   :name "training_v1" :type "xml")
  *MCStylistic-MonthYear-data-path*))
(setq notation-name "f3")
(c@merata-question-file2question-string
 question-number question-path&name notation-name)
--> ("harmonic interval of a minor sixth" 4)
\end{verbatim}

\noindent This function retrieves a natural-language
question string from an xml file, and also retrieves
the value stored in the divisions field. |#

(defun c@merata2014-question-file2question-string
       (question-number question-path&name
        notation-name &optional
        (raw
         (read-from-file-arbitrary
          question-path&name))
        (rel-idx
         (first
          (loop for i from 0 to (- (length raw) 1)
            when
            (and
             (search notation-name (nth i raw))
             (search
              (concatenate
               'string "number=\"" question-number
               "\"") (nth i raw)))
            collect i)))
        (divisions
         (if rel-idx
           (parse-integer
            (subseq
             (nth rel-idx raw)
             (+
              (search
               "divisions=\"" (nth rel-idx raw)) 11))
            :junk-allowed t)))
        (question
         (if rel-idx
           (string-trim
            '(#\Space #\return #\linefeed)
            (replace-all
             (replace-all
              (nth (+ rel-idx 1) raw) "<text>" "")
             "</text>" "")))))
  (list question divisions))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "F# followed two crotchets later by a G")
(setq division 1)
(setq time-intervals '((4 8) (8 10)))
(setq ontimes-signatures '((1 4 4 0) (5 3 8 16)))
(setq question-number "004")
(setq
 notation-path
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1"))
  *MCStylistic-MonthYear-data-path*))
(setq notation-name "f3")
(c@merata2014-write-answer
 question-string division time-intervals
 ontimes-signatures question-number
 notation-path notation-name)
--> (wrote the following to text file)
  <question number="004" music_file="f3.xml" divisions="1">
    <text>"F# followed two crotchets later by a G"</text>
    <answer>
      <passage start_beats="4" start_beat_type="4"
               end_beats="4" end_beat_type="4"
               start_divisions="1" end_divisions="1"
               start_bar="2" start_offset="1"
               end_bar="2" end_offset="4" />
      </passage>
      <passage start_beats="4" start_beat_type="4"
               end_beats="4" end_beat_type="4"
               start_divisions="1" end_divisions="1"
               start_bar="3" start_offset="1"
               end_bar="3" end_offset="2" />
      </passage>
    </answer>
  </question>
\end{verbatim}

\noindent This function takes a list of time intervals
as its main argument and writes them to an xml-style
file, conforming to the MediaEval 2014 C@amerata
standard. |#

(defun c@merata2014-write-answer
       (question-string division time-intervals
        ontimes-signatures question-number
        notation-path notation-name
        &optional
        (start-bar&beats ; x.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (first x) ontimes-signatures))
          time-intervals))
        (start-time-sigs ; y.
         (mapcar
          #'(lambda (x)
              (if (>= (first x) 0)
                (row-of-max-ontime<=ontime-arg
                 (first x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (end-bar&beats ; z.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (- (my-last x) (/ 1 division))
               ontimes-signatures))
          time-intervals))
        (end-time-sigs ; w.
         (mapcar
          #'(lambda (x)
              (if (>= (my-last x) 0)
                (row-of-max-ontime<=ontime-arg
                 (my-last x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (answer-lists
         (mapcar
          #'(lambda (x y z w)
              (list
               ; start_beats.
               (second y)
               ; start_beat_type.
               (third y)
               ; end_beats.
               (second w)
               ; end_beat_type.
               (third w)
               ; start_divisions.
               division
               ; end_divisions.
               division
               ; start_bar.
               (first x)
               ; start_offset.
               (+ (* (- (second x) 1) division) 1)
               ; end_bar.
               (first z)
               ; end_offset.
               (+ (* (- (second z) 1) division) 1)))
          start-bar&beats start-time-sigs
          end-bar&beats end-time-sigs))
        (answer-path&name
         (merge-pathnames
          (make-pathname
           :name "dmun01" :type "txt")
          notation-path))
        (file
         (open
          answer-path&name
          :direction :output :if-does-not-exist
          :create :if-exists :append)))
  (progn
    (format
     file "  <question number=~s" question-number)
    (format
     file
     " music_file=~s"
     (concatenate 'string notation-name ".xml"))
    (format file " divisions=\"~s\">~%" division)
    (format
     file "    <text>~a</text>~%" question-string)
    (format file "    <answer>~%")
    (loop for i from 0 to
      (- (length time-intervals) 1) when
      (and
       #| Makes use of the given division value, to
       check that integer offsets are returned. |#
       (integerp (nth 7 (nth i answer-lists)))
       (integerp (nth 9 (nth i answer-lists))))
      do
      (format file "      <passage ")
      (format
       file "start_beats=\"~s\""
       (nth 0 (nth i answer-lists)))
      (format
       file " start_beat_type=\"~s\""
       (nth 1 (nth i answer-lists)))
      (format
       file " end_beats=\"~s\""
       (nth 2 (nth i answer-lists)))
      (format
       file " end_beat_type=\"~s\""
       (nth 3 (nth i answer-lists)))
      (format
       file " start_divisions=\"~s\""
       (nth 4 (nth i answer-lists)))
      (format
       file " end_divisions=\"~s\""
       (nth 5 (nth i answer-lists)))
      (format
       file " start_bar=\"~s\""
       (nth 6 (nth i answer-lists)))
      (format
       file " start_offset=\"~s\""
       (nth 7 (nth i answer-lists)))
      (format
       file " end_bar=\"~s\""
       (nth 8 (nth i answer-lists)))
      (format
       file " end_offset=\"~s\"/>~%"
       (nth 9 (nth i answer-lists)))
      ; (format file "       </passage>~%")
      )
    (format file "    </answer>~%")
    (format file "  </question>~%~%")
    (close file)))

#| Old version. Unnecessary linebreaks, and passages
should occur within a single answer, but they don't
here, with one passage per answer.
(defun c@merata2014-write-answer
       (question-string division time-intervals
        ontimes-signatures question-number
        notation-path notation-name
        &optional
        (start-bar&beats ; x.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (first x) ontimes-signatures))
          time-intervals))
        (start-time-sigs ; y.
         (mapcar
          #'(lambda (x)
              (if (>= (first x) 0)
                (row-of-max-ontime<=ontime-arg
                 (first x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (end-bar&beats ; z.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (- (my-last x) (/ 1 division))
               ontimes-signatures))
          time-intervals))
        (end-time-sigs ; w.
         (mapcar
          #'(lambda (x)
              (if (>= (my-last x) 0)
                (row-of-max-ontime<=ontime-arg
                 (my-last x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (answer-lists
         (mapcar
          #'(lambda (x y z w)
              (list
               ; start_beats.
               (second y)
               ; start_beat_type.
               (third y)
               ; end_beats.
               (second w)
               ; end_beat_type.
               (third w)
               ; start_divisions.
               division
               ; end_divisions.
               division
               ; start_bar.
               (first x)
               ; start_offset.
               (+ (* (- (second x) 1) division) 1)
               ; end_bar.
               (first z)
               ; end_offset.
               (+ (* (- (second z) 1) division) 1)))
          start-bar&beats start-time-sigs
          end-bar&beats end-time-sigs))
        (answer-path&name
         (merge-pathnames
          (make-pathname
           :name "dmun01" :type "txt")
          notation-path))
        (file
         (open
          answer-path&name
          :direction :output :if-does-not-exist
          :create :if-exists :append)))
  (progn
    (format
     file "  <question number=~s" question-number)
    (format
     file
     " music_file=~s"
     (concatenate 'string notation-name ".xml"))
    (format file " divisions=\"~s\">~%" division)
    (format
     file "    <text>~a</text>~%" question-string)
    (loop for i from 0 to
      (- (length time-intervals) 1) when
      (and
       #| Makes use of the given division value, to
       check that integer offsets are returned. |#
       (integerp (nth 7 (nth i answer-lists)))
       (integerp (nth 9 (nth i answer-lists))))
      do
      (format file "    <answer>~%")
      (format file "      <passage ")
      (format
       file "start_beats=\"~s\""
       (nth 0 (nth i answer-lists)))
      (format
       file " start_beat_type=\"~s\"~%"
       (nth 1 (nth i answer-lists)))
      (format
       file "               end_beats=\"~s\""
       (nth 2 (nth i answer-lists)))
      (format
       file " end_beat_type=\"~s\"~%"
       (nth 3 (nth i answer-lists)))
      (format
       file "               start_divisions=\"~s\""
       (nth 4 (nth i answer-lists)))
      (format
       file " end_divisions=\"~s\"~%"
       (nth 5 (nth i answer-lists)))
      (format
       file "               start_bar=\"~s\""
       (nth 6 (nth i answer-lists)))
      (format
       file " start_offset=\"~s\"~%"
       (nth 7 (nth i answer-lists)))
      (format
       file "               end_bar=\"~s\""
       (nth 8 (nth i answer-lists)))
      (format
       file " end_offset=\"~s\" />~%"
       (nth 9 (nth i answer-lists)))
      (format file "    </answer>~%"))
    (format file "  </question>~%~%")
    (close file)))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 notation-path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "camerata_questions_2014")
   :name "bach_minuet_in_g_bwv_anh114"
   :type "krn")
  *MCStylistic-MonthYear-data-path*))
(setq bar-limits '(7 8))
(c@merata2015-point-set-loader
 notation-path&name bar-limits)
--> ((("left hand" "bass clef")
      ("right hand" "treble clef"))
     ((1 3 4 0))
     ((18 62 61 1 1 ("(") NIL NIL)
      (18 66 63 1 0 NIL NIL NIL)
      ...
      (47/2 57 58 1/2 1 (")") NIL NIL))
     ((18 62 61 1 1) (18 66 63 1 0) ...
      (47/2 57 58 1/2 1))
     NIL
     ((0 24 "melody with accompaniment" 1.2569444))
     ((43/2 69 65 1/2 0 "[") (22 69 65 2 0 "]"))).
\end{verbatim}

\noindent This function loads several varialbes that
are helpful for analysing a piece of music for the
presence of a given query/concept: staff&clef-names,
which contains the staff names and names of the opening
clefs on each staff; ontime-signatures, which contains
time-signature information and any changes across the
piece; a whole bunch of point sets (articulation,
dynamics, and lyrics set, the regular five-column
point set, a rest set, a texture set, and a tie set.
If bar-delimiting information has been extracted from
the question string, then this is also sent here, to
limit the output of the function to those events that
occurs within the bar limits. |#

(defun c@merata2015-point-set-loader
       (notation-path&name &optional (bar-limits nil)
        (ontime-idx 0) (texture-offtime-idx 1)
        #| Load staff names and names of the opening
        clefs on each staff. |#
        (staff&clef-names
         (staves-info2staff&clef-names
          notation-path&name))
        #| Load time-signature information and any
        changes across the piece. |#
        (ontimes-signatures
         (append-ontimes-to-time-signatures
          (kern-file2ontimes-signatures
           notation-path&name)))
        (ontime-interval
         (if bar-limits
           (list
            (my-last
             (ontime-of-bar&beat-number
              (first bar-limits) 1
              ontimes-signatures))
            (my-last
             (ontime-of-bar&beat-number
              ; This addition for handy use of < below.
              (+ (second bar-limits) 1) 1
              ontimes-signatures)))))
         ; Load point-set representations.
        ;#|
        (artic-set
         (kern-file2points-artic-dynam-lyrics
          notation-path&name))
        (artic-set
         (if bar-limits
           (restrict-dataset-in-nth-to-tests 
            artic-set ontime-idx (list #'>= #'<)
            ontime-interval)
           artic-set))
        ;|#
        (point-set
         (kern-transp-file2dataset-by-col
          notation-path&name t))
        (point-set
         (if bar-limits
           (restrict-dataset-in-nth-to-tests 
            point-set ontime-idx (list #'>= #'<)
            ontime-interval)
           point-set))
        (rest-set
         (kern-file2rest-set-by-col
          notation-path&name))
        (rest-set
         (if bar-limits
           (restrict-dataset-in-nth-to-tests 
            rest-set ontime-idx (list #'>= #'<)
            ontime-interval) rest-set))
        (texture-set
         (texture-from-kern notation-path&name))
        (texture-set
         (if bar-limits
           (loop for txtr in texture-set when
             (interval-strict-intersectionp
              (list
               (nth ontime-idx txtr)
               (nth texture-offtime-idx txtr))
              ontime-interval) collect txtr)
           texture-set))
        (tie-set
         (kern-file2tie-set-by-col
          notation-path&name t))
        (tie-set
         (if bar-limits
           (restrict-dataset-in-nth-to-tests 
            tie-set ontime-idx (list #'>= #'<)
            ontime-interval) tie-set)))
  (list
   staff&clef-names ontimes-signatures artic-set
   point-set rest-set texture-set tie-set))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "F# followed two crotchets later by a G")
(setq division 1)
(setq time-intervals '((4 8) (8 10)))
(setq ontimes-signatures '((1 4 4 0) (5 3 8 16)))
(setq question-number "004")
(setq
 notation-path
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "C@merata2014" "training_v1"))
  *MCStylistic-MonthYear-data-path*))
(setq notation-name "f3")
(c@merata2015-write-answer
 question-string division time-intervals
 ontimes-signatures question-number
 notation-path notation-name)
--> (wrote the following to text file)
  <question number="004" music_file="f3.xml" divisions="1">
    <text>"F# followed two crotchets later by a G"</text>
    <answer>
      <passage start_beats="4" start_beat_type="4"
               end_beats="4" end_beat_type="4"
               start_divisions="1" end_divisions="1"
               start_bar="2" start_offset="1"
               end_bar="2" end_offset="4" />
      </passage>
      <passage start_beats="4" start_beat_type="4"
               end_beats="4" end_beat_type="4"
               start_divisions="1" end_divisions="1"
               start_bar="3" start_offset="1"
               end_bar="3" end_offset="2" />
      </passage>
    </answer>
  </question>
\end{verbatim}

\noindent This function takes a list of time intervals
as its main argument and writes them to an xml-style
file, conforming to the MediaEval 2015 C@amerata
standard. |#

(defun c@merata2015-write-answer
       (question-string division time-intervals
        ontimes-signatures question-number
        notation-path notation-name
        &optional
        (answer-path&name
         (merge-pathnames
          (make-pathname
           :name "dmun01" :type "txt")
          notation-path))
        (start-bar&beats ; x.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (first x) ontimes-signatures))
          time-intervals))
        (start-time-sigs ; y.
         (mapcar
          #'(lambda (x)
              (if (>= (first x) 0)
                (row-of-max-ontime<=ontime-arg
                 (first x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (end-bar&beats ; z.
         (mapcar
          #'(lambda (x)
              (bar&beat-number-of-ontime
               (- (my-last x) (/ 1 division))
               ontimes-signatures))
          time-intervals))
        (end-time-sigs ; w.
         (mapcar
          #'(lambda (x)
              (if (>= (my-last x) 0)
                (row-of-max-ontime<=ontime-arg
                 (my-last x) ontimes-signatures)
                (first ontimes-signatures)))
          time-intervals))
        (answer-lists
         (mapcar
          #'(lambda (x y z w)
              (list
               ; start_beats.
               (second y)
               ; start_beat_type.
               (third y)
               ; end_beats.
               (second w)
               ; end_beat_type.
               (third w)
               ; start_divisions.
               division
               ; end_divisions.
               division
               ; start_bar.
               (first x)
               ; start_offset.
               (+ (* (- (second x) 1) division) 1)
               ; end_bar.
               (first z)
               ; end_offset.
               (+ (* (- (second z) 1) division) 1)))
          start-bar&beats start-time-sigs
          end-bar&beats end-time-sigs))
        (file
         (open
          answer-path&name
          :direction :output :if-does-not-exist
          :create :if-exists :append)))
  (progn
    (format
     file "  <question number=~s" question-number)
    (format
     file
     " music_file=~s"
     (concatenate 'string notation-name ".xml"))
    (format file " divisions=\"~s\">~%" division)
    (format
     file "    <text>~a</text>~%" question-string)
    (format file "    <answer>~%")
    (loop for i from 0 to
      (- (length time-intervals) 1) when
      #| Makes use of the given division value, to
      check that integer offsets are returned. Tried
      removing it for 2015 v2, to see the difference.
      This was because it seemed the "against" questions
      weren't working. Tried removin it for 2015 v3 only
      if it was an "against" question.
      v1.
      (and
       (integerp (nth 7 (nth i answer-lists)))
       (integerp (nth 9 (nth i answer-lists))))
      v2.
      t
      v3... see below.
      
      |#
      (if (or
           (search "against" question-string)
           (search "simultaneously" question-string)
           (search "simultaneous" question-string)
           (search "accompanied by" question-string)
           (search "at the same time" question-string))
        t
        (and
         (integerp (nth 7 (nth i answer-lists)))
         (integerp (nth 9 (nth i answer-lists)))))
           
      do
      (format file "      <passage ")
      (format
       file "start_beats=\"~s\""
       (nth 0 (nth i answer-lists)))
      (format
       file " start_beat_type=\"~s\""
       (nth 1 (nth i answer-lists)))
      (format
       file " end_beats=\"~s\""
       (nth 2 (nth i answer-lists)))
      (format
       file " end_beat_type=\"~s\""
       (nth 3 (nth i answer-lists)))
      (format
       file " start_divisions=\"~s\""
       (nth 4 (nth i answer-lists)))
      (format
       file " end_divisions=\"~s\""
       (nth 5 (nth i answer-lists)))
      (format
       file " start_bar=\"~s\""
       (nth 6 (nth i answer-lists)))
      (format
       file " start_offset=\"~s\""
       (nth 7 (nth i answer-lists)))
      (format
       file " end_bar=\"~s\""
       (nth 8 (nth i answer-lists)))
      (format
       file " end_offset=\"~s\"/>~%"
       (nth 9 (nth i answer-lists)))
      ; (format file "       </passage>~%")
      )
    (format file "    </answer>~%")
    (format file "  </question>~%~%")
    (close file)))

#|
\noindent Example:
\begin{verbatim}
; questions, ontimes-signatures, and ans-list as in
; cross-check-compound-questions
(setq iq 0)
(setq ia 2)
(setq it 0)
(cross-check-compound-question
 questions ontimes-signatures ans-list na iq ia it)
--> ((0 2 0) ((1 0 1)))
\end{verbatim}

\noindent This function is a helper function for
\nameref{fun:cross-check-compound-questions}. It takes
a time-interval answer for question component $i$
(where the question is of the compound variety) and
looks at the time offset specified for question
component $i + 1$ to calculate the target ontime at
which an event should occur. It has to be quite
subtle because this offset could be expressed as a
quantity of note values or bar numbers. |#

(defun cross-check-compound-question
       (questions ontimes-signatures ans-list na iq
        ia it &optional
        (probe-interval
         (nth it (nth iq (nth ia ans-list))))
        (bar&beat-probe-ontime
         (if (string=
              (nth 2 (nth (+ iq 1) questions))
              "bars")
           (bar&beat-number-of-ontime
            (first probe-interval)
            ontimes-signatures)))
        (next-offset (nth 1 (nth (+ iq 1) questions)))
        (simple-offset
         (if (and next-offset (zerop next-offset))
           (second probe-interval) next-offset))
        (target-ontime
         (if simple-offset
           (if bar&beat-probe-ontime
             (my-last
              (ontime-of-bar&beat-number
               (+
                (first bar&beat-probe-ontime)
                simple-offset)
               (second bar&beat-probe-ontime)
               ontimes-signatures))
             (if (> next-offset 0)
               (+
                (first probe-interval)
                simple-offset) simple-offset)))))
  (cons
   (list iq ia it)
   (list
    (loop for ja from 0 to na append
      (loop for jt from 0 to
        (-
         (length (nth (+ iq 1) (nth ja ans-list))) 1)
        append
        (if (equalp
             (first
              (nth
               jt (nth (+ iq 1) (nth ja ans-list))))
             target-ontime)
          (list (list (+ iq 1) ja jt))
          #|
          (nth
          jt (nth (+ iq 1) (nth ja ans-list)))
          |#
          ))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 questions
 '(("F#" nil) ("crotchet" 2 "bars")
   ("perfect 5th" 3)))
(setq ontimes-signatures '((1 4 4 0) (5 3 8 16)))
(setq
 ans-list
 '(
   (; ans-fn1 - duration-fn
    ; q1 - pitch
    nil
    (; q2 - duration
     (0 1) (1 2) (8 9))
    (; q3 - harmonic interval
     (1 7) (2 4)))
   (; ans-fn2 melodic-interval-fn
    ; q1 - pitch
    nil
    ; q2 - duration
    nil
    (; q3 - harmonic interval
     (4 7) (12 14)))
   (; ans-fn3 - pitch-fn
    (; q1 - pitch
     (0 2) (2 5))
    (; q2 - duration
     (8 9) (12 14) (14 15))
    ; q3 - harmonic interval
    nil)
   (; ans-fn4 - harmonic-interval-fn
    ; q1 - pitch
    nil
    (; q2 - duration
     (0 12) (12 14) (14 15))
    (; q3 - harmonic interval
     (4 7) (11 13) (14 15)))))
; (0 2) (8 9) (11 13) Candidate time intervals
; (0 2 0) (1 0 2) (2 3 1) Candidate indices in ans-list
(cross-check-compound-questions
 questions ontimes-signatures ans-list)
--> ((0 13))

(setq
 questions
 '(("F" NIL) ("crotchet" 0) ("perfect fifth" 3)))
(setq
 ans-list
 '(
   (; ans-fn1 - duration-fn
    (; q1 - pitch
     (1 2) (7 8) (9 10))
    (; q2 - duration
     (0 1) (2 3) (8 9))
    (; q3 - harmonic interval
     (1 7) (2 4)))
   (; ans-fn2 melodic-interval-fn
    (; q1 - pitch
     (0 2))
    ; q2 - duration
    nil
    (; q3 - harmonic interval
     (4 7) (12 14)))
   (; ans-fn3 - pitch-fn
    (; q1 - pitch
     (0 2) (6 8))
    (; q2 - duration
     (8 9) (12 14) (14 15))
    ; q3 - harmonic interval
    nil)
   (; ans-fn4 - harmonic-interval-fn
    ; q1 - pitch
    nil
    (; q2 - duration
     (0 12) (12 14) (14 15))
    (; q3 - harmonic interval
     (5 7) (11 13) (14 15)))))
; (0 2) (2 3) (6 8) Candidate time intervals
; (0 2 0) (1 0 1) (2 3 0) Candidate indices in ans-list
(cross-check-compound-questions
 questions ontimes-signatures ans-list)
--> ((1 7) (7 13) (0 7) (0 7) (6 13))

(setq
 questions
 '(("word \"While\"" 0) ("word \"love\"" 0)
   ("word \"doth\"" 0)))
(setq ontimes-signatures '((1 3 4 0)))
(setq
 ans-list
 '(
   (; ans-fn1 - word-fn
    (; q1 - word
     (6 7) (6 15/2))
    (; q2 - word
     (7 8) (15/2 8))
    (; q3 - word
     (8 17/2) (8 9)))
   (; ans-fn2 melodic-interval-fn
    (; q1 - word
     nil)
    ; q2 - word
     nil)
    (; q3 - word
     nil)))
(cross-check-compound-questions
 questions ontimes-signatures ans-list)
--> ((6 17/2) (6 17/2))
\end{verbatim}

\noindent This function looks across an answer list
for individual components that could be combined to
form answers to compound questions. The answer list
is ordered first by answer function, then by question
number, then by time interval answering a question. |#

(defun cross-check-compound-questions
       (questions ontimes-signatures ans-list
        &optional
        (nq (- (length questions) 1))
        (na (- (length ans-list) 1))
        ; Determine plausible "followed-by"s
        (cross-check-assoc-list
         (if (> nq 0)
           (loop for iq from 0 to (- nq 1) append
             (loop for ia from 0 to na append
               (loop for it from 0 to
                 (-
                  (length
                   (nth iq (nth ia ans-list))) 1)
                 ;collect it
                 collect
                 (cross-check-compound-question
                  questions ontimes-signatures
                  ans-list na iq ia it))))))
        #| Look for plausible routes with length equal
        to the number of compounds in the question. |#
        (matching-pursuits
         (mapcar
          #'(lambda (x)
              (if (zerop (first (first x)))
                (matching-pursuit
                 cross-check-assoc-list (first x))))
              cross-check-assoc-list))
        (rel-idxs
         (loop for i from 0 to
           (- (length matching-pursuits) 1) when
           (equalp
            (length (nth i matching-pursuits))
            (+ nq 1))
           collect (nth i matching-pursuits))))
  (loop for i from 0 to (- (length rel-idxs) 1)
    #| For conformant routes, make a new interval
    that begins with the ontime of the first interval
    and ends with the offtime of the last interval. |#
    collect
    (list
     (first
      (nth
       (third (first (nth i rel-idxs)))
       (nth
        (first (first (nth i rel-idxs)))
        (nth
         (second
          (first (nth i rel-idxs))) ans-list))))
     (my-last
      (nth
       (third (my-last (nth i rel-idxs)))
       (nth
        (first (my-last (nth i rel-idxs)))
        (nth
         (second
          (my-last
           (nth i rel-idxs))) ans-list)))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 assoc-list
 '(((0 0 0) NIL)
   ((0 0 1) ((1 0 2) (1 2 0)))
   ((0 0 2) ((1 0 2) (1 2 0)))
   ((0 1 0) ((1 0 2) (1 2 0)))
   ((0 2 0) ((1 0 2) (1 2 0)))
   ((0 2 1) NIL)
   ((1 0 0) NIL)
   ((1 0 1) ((2 1 0) (2 3 0)))
   ((1 0 2) ((2 3 1)))
   ((1 2 0) ((2 3 1)))
   ((1 2 1) NIL)
   ((1 2 2) NIL)
   ((1 3 0) NIL)
   ((1 3 1) NIL)
   ((1 3 2) NIL)))
(matching-pursuit assoc-list '(0 2 0))
--> ((0 2 0) (1 0 2) (2 3 1))
\end{verbatim}

\noindent This function `pursues' a probe list across
an assoc list, returning subsequent entries so long
as the chain of probes continues. For instance, in the
example, list \texttt{(1 0 2)} is in the list indexed
by \texttt{(0 2 0)}, so we return the latter then look
it up and see \texttt{(2 3 1)} is in the list indexed
by \texttt{(0 2 0)}, so we return it etc.

I am concerned that this function does not do
everything it ought. For example, how would
\texttt{((0 2 0) (1 0 2) (2 3 1))} emerge: it is a
legitimate chain but I do not see how it is returned.
More testing required! |#

(defun matching-pursuit
       (assoc-list &optional
        (probe (first (first assoc-list)))
        (growing-list nil)
        (targets
         (if probe
           (second
            (assoc
             probe assoc-list :test #'equalp)))))
  (if (null probe) (identity growing-list)
    (if
        (null targets)
      (matching-pursuit
       assoc-list
       (second targets)
       (append growing-list (list probe))
       (rest targets))
      (matching-pursuit
        assoc-list (first targets)
        (append growing-list (list probe))))))

; This needs documenting. It's real important!

(defun time-intervals-for-question-elements-Jun2015
       (questions staff&clef-names ontimes-signatures
        artic-set point-set rest-set texture-set
        tie-set &optional
        (ans-chord-interval
         (mapcar
          #'(lambda (x)
              (chord-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-harmonic-interval
         (mapcar
          #'(lambda (x)
              (harmonic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        ; Mel. intervals to be prefaced by "melodic".
        (ans-melodic-interval
         (mapcar
          #'(lambda (x)
              (melodic-interval-of-a
               (first x) point-set staff&clef-names))
          questions))
        (ans-artic
         (mapcar
          #'(lambda (x)
              (articulation&event-time-intervals
               (first x) artic-set staff&clef-names))
          questions))
        (ans-dynam
         (mapcar
          #'(lambda (x)
              (dynamic&event-time-intervals
               (first x) artic-set staff&clef-names))
          questions))
        #| Next bit needs more careful thought. Let's
        say we can't find any dur-pitch combos using
        the ordinary point set, then we can try the
        tied point set instead. That's what I've done
        below (and for ans-dur), but it could perhaps be
        made more compact. |#
        (ans-dur-pitch
         (mapcar
          #'(lambda (x)
              (duration&pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur-pitch
         (if (or
              (equalp
               ans-dur-pitch
               (constant-vector
                nil (length questions)))
              (equalp
               ans-dur-pitch
               (constant-vector
                "val and pair were true"
                (length questions))))
           (mapcar
            #'(lambda (x)
                (duration&pitch-class-time-intervals
                 (first x) tie-set staff&clef-names))
            questions) ans-dur-pitch))
        (ans-pitch
         (mapcar
          #'(lambda (x)
              (pitch-class-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        #| Next bit needs more careful thought. Let's
        say we can't find any dur-pitch combos using
        the ordinary point set, then we can try the
        tied point set instead. |#
        (ans-dur
         (mapcar
          #'(lambda (x)
              (duration-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-dur
         (if (equalp
              ans-dur
              (constant-vector
               nil (length questions)))
           (mapcar
            #'(lambda (x)
                (duration-time-intervals
                 (first x) tie-set staff&clef-names))
            questions) ans-dur))
        #| Let's say there's a question "quaver D" for a
        piece, and the piece contains no "quaver D"s
        but plenty of quavers. Well, ans-dur-pitch will
        be empty, and so all the quavers will be found
        by ans-dur, and these will be returned
        (erroneously). So I modified ans-dur-pitch to
        return "val and pair were true" if val and pair
        were true (meaning the query contained a
        duration and pitch) but none were found. We
        should loop through ans-dur and ans-pitch and
        remove nil-ify any answers where ans-dur-pitch
        contains "val and pair were true". |#
        (ans-dur
         (loop for i from 0 to (- (length questions) 1)
           collect
           (if (and
                (stringp (nth i ans-dur-pitch))
                (string=
                 (nth i ans-dur-pitch)
                 "val and pair were true"))
             nil
             (nth i ans-dur))))
        (ans-pitch
         (loop for i from 0 to (- (length questions) 1)
           collect
           (if (and
                (stringp (nth i ans-dur-pitch))
                (string=
                 (nth i ans-dur-pitch)
                 "val and pair were true"))
             nil
             (nth i ans-pitch))))
        (ans-dur-pitch
         (loop for i from 0 to (- (length questions) 1)
           collect
           (if (and
                (stringp (nth i ans-dur-pitch))
                (string=
                 (nth i ans-dur-pitch)
                 "val and pair were true"))
             nil
             (nth i ans-dur-pitch))))
        
        (ans-nadir-apex
         (mapcar
          #'(lambda (x)
              (nadir-apex-time-intervals
               (first x) point-set staff&clef-names))
          questions))
        (ans-triad
         (mapcar
          #'(lambda (x)
              (triad-time-intervals
               (first x) point-set))
          questions))
        ; Below, any triad, particular inversion:
        (ans-triad-inversion
         (mapcar
          #'(lambda (x)
              (triad-inversion-time-intervals
               (first x) point-set))
          questions))
        (ans-texture
         (mapcar
          #'(lambda (x)
              (texture-time-intervals
               (first x) texture-set
               staff&clef-names)) questions))
        (ans-cadence
         (mapcar
          #'(lambda (x)
              (cadence-time-intervals
               (first x) point-set
               ontimes-signatures))
          questions))
        ;#|
        (ans-word&event
         (mapcar
          #'(lambda (x)
              (word&event-time-intervals
               (first x) artic-set))
          questions))
        (ans-word
         (mapcar
          #'(lambda (x)
              (word-time-intervals
               (first x) artic-set)) questions))
        ;|#
        (ans-dur-rest
         (mapcar
          #'(lambda (x)
              (rest-duration-time-intervals
               (first x) rest-set staff&clef-names))
          questions))
        (ans-tied&event
         (mapcar
          #'(lambda (x)
              (tied&event-time-intervals
               (first x) tie-set)) questions))
       
        ; Define the set of answers.
        (time-intervals
         (list
          ans-chord-interval
          ans-harmonic-interval
          ans-melodic-interval
          ans-artic
          ans-dynam
          ans-dur-pitch
          ans-pitch
          ans-dur
          ans-nadir-apex
          ans-triad
          ans-triad-inversion
          ans-texture
          ans-cadence
          ans-word&event
          ans-word
          ans-dur-rest
          ans-tied&event))
        #| For each question element, determine where
        the most specific answer is located (to avoid
        followed-by errors), and restrict time-intervals
        to these. |#
        (time-interval-idxs
         (loop for i from 0
           to (- (length questions) 1) collect
           (if (index-item-1st-doesnt-occur
                nil (nth-list-of-lists i time-intervals))
             (list
              (index-item-1st-doesnt-occur
               nil (nth-list-of-lists i time-intervals))
              i))))
        (time-intervals
         (loop for i from 0
           to (- (length time-intervals) 1) collect
           (loop for j from 0
             to (- (length questions) 1) collect
             (if (find
                  (list i j) time-interval-idxs
                  :test #'equalp)
               (nth j (nth i time-intervals)))))))
  #| Cross-check the timings of compound
  questions. |#
  (if (> (length questions) 1)
    (cross-check-compound-questions
     questions ontimes-signatures
     time-intervals)
    (if (first time-interval-idxs)
      (nth
       (nth 1 (nth 0 time-interval-idxs))
       (nth
        (nth 0 (nth 0 time-interval-idxs))
        time-intervals)))))





















