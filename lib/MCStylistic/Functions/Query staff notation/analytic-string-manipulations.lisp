#| Copyright 2008-2014 Tom Collins
   Tuesday 17 June 2014
   Incomplete

\noindent The functions below are for converting
string-based representations of quantity into
numeric representations, and for splitting question
strings into $n$ components.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(append-list-of-lists
 '(("yes" 0)
   (("crotchet" 0) ("crotchet" 0) ("crotchet" 0))
   ("no" 4)))
--> (("yes" 0) ("crotchet" 0) ("crotchet" 0)
     ("crotchet" 0) ("no" 4))
(append-list-of-lists '(("yes" 0)))
--> (("yes" 0))
(append-list-of-lists '(("yes" 0) ("no" 0)))
--> (("yes" 0) ("no" 0))
(append-list-of-lists 
 '((("crotchet" 0) ("crotchet" 0))))
--> (("crotchet" 0) ("crotchet" 0))
\end{verbatim}

\noindent In a list this function identifies elements
that are lists of lists, and removes one structural
level from these lists. |#

(defun append-list-of-lists (a-list)
  (if (null a-list) ()
    (if (stringp (first (first a-list)))
      (cons
       (first a-list)
       (append-list-of-lists (rest a-list)))
      (append
       (first a-list)
       (append-list-of-lists (rest a-list))))))

#|
\noindent Example:
\begin{verbatim}
(setq question-string "C4 in the left hand in bars 1 to 44")
(bar-delimiter question-string)
--> ("C4 in the left hand" 1 44)
(setq question-string "G4 in bars 16-17")
(bar-delimiter question-string)
--> ("G4" 16 17)
(setq question-string "C major chord in measure 12")
(bar-delimiter question-string)
--> ("C major chord" 12 12)
(setq
 question-string
 (concatenate
  'string
  "ten yes in measures 1-60 followed by two no"))
(bar-delimiter question-string)
--> ("ten yes followed by two no" 1 60)
\end{verbatim}

\noindent This function extracts bar-delimiting
information from a question string. |#

(defun bar-delimiter
       (question-string &optional
        (bar-delimiters
         (list
          "in bars " "in measures" "in bar "
          "in measure "))
        (hyphen-string
         (if (search "-" question-string) "-" " to "))
        (indices-of-bar-delimiters
         (loop for bd in bar-delimiters collect
           (search bd question-string)))
        (bar-delimiterp
         (index-item-1st-doesnt-occur
          nil indices-of-bar-delimiters))
        (bar-delimiting-phrase
         (if bar-delimiterp
           (nth bar-delimiterp bar-delimiters)))
        (question-b4&after
         (if bar-delimiterp
           (string-separated-string2list
            bar-delimiting-phrase
            question-string)))
        ;#|
        (question-string
         (if bar-delimiterp
           (first question-b4&after) question-string))
        ;|#
        #|
        (question-string
         (if bar-delimiterp
           (replace-all
            question-string bar-delimiting-phrase "")
           question-string))
        |#
        (bar-limits
         (if bar-delimiterp
           (mapcar
            #'(lambda (x)
                (parse-integer x :junk-allowed t))
            (string-separated-string2list
             hyphen-string
             (second question-b4&after)))))
        (bar-limits
         (if (and
              bar-delimiterp
              (equalp (length bar-limits) 1))
           (cons (first bar-limits) bar-limits)
           bar-limits))
        #| If either of the bar limits values are zero,
        replace with one for now to prevent errors. |#
        (bar-limits
         (if bar-delimiterp
           (loop for bl in bar-limits collect
             (if (zerop bl) 1 bl)) bar-limits))
        (postfix
         (if bar-limits
           (second
            (string-separated-string2list
             (write-to-string (second bar-limits))
             (second question-b4&after)))))
        (question-string
         (if postfix
           (concatenate
            'string
            question-string " " postfix)
           question-string))
        )
  (if bar-delimiterp
    (cons (string-trim " " question-string) bar-limits)
    (list question-string)))

#|
\noindent Example:
\begin{verbatim}
(consecutive-question2list "three ascending seconds")
--> (("ascending second" 0) ("ascending second" 0)
     ("ascending second" 0))
(consecutive-question2list "three crotchets in a row")
--> (("crotchet" 0) ("crotchet" 0) ("crotchet" 0))
(consecutive-question2list "two consecutive kittens")
--> (("kitten" 0) ("kitten" 0))
(consecutive-question2list "perfect cadence")
--> ("perfect cadence" 0)
(consecutive-question2list "semiquaver rest")
--> ("semiquaver rest" 0)
(consecutive-question2list
 '("consecutive fifths" NIL))
--> (("fifth" 0) ("fifth" 0))
(consecutive-question2list
 '("consecutive fifths" 2 "bars"))
--> (("fifth" 2 "bars") ("fifth" 0))
(consecutive-question2list "two fifths")
--> (("fifth" 0) ("fifth" 0))
(consecutive-question2list "two fifths in violins")
--> (("fifths in violins" 0) ("fifths in violins" 0))
\end{verbatim}

\noindent This function turns a question string
containing `consecutive', `in a row', or some
implicit reference to consecutive events such as
`three crotchets' into the appropriate number of
question strings. A call to this function is
embedded in the function
\nameref{fun:followed-by-splitter}. |#

(defun consecutive-question2list
       (question-string&time-interval
        &optional
        (question-string
         (if (stringp question-string&time-interval)
           question-string&time-interval
           (first question-string&time-interval)))
        (time-interval
         (if (and
              (listp question-string&time-interval)
              (second question-string&time-interval))
           #| The list structure here looks a bit
           heavy-handed, but it is because there may
           be a bars modifier involved. |#
           (rest question-string&time-interval)
           (list 0)))
        #| Does the string contain "consecutive" or
        "in a row"? |#
        (poss-consec
         (search "consecutive" question-string))
        (poss-in-a-row
         (search
          "in a row" question-string :from-end t))
        #| If so, it may be that the word
        "consecutive" or words "in a row" are being
        used without explicit reference to an amount
        (e.g., "consecutive fifths" really means
        "two consecutive fifths"). Here we identify if
        this is the case, and, if so, insert the
        string "two ". |#
        (question-string
         (if (or
              (equalp poss-consec 0)
              (and
               poss-in-a-row
               (equalp
                (length
                 (number-string2numberless-string
                  question-string))
                (length question-string))))
           (concatenate
            'string "two " question-string)
           question-string))
        #| It may also be that "or more" is in the
        question string (e.g., "two or more" or
        "three or more"). This will be removed here,
        because these time intervals will be returned
        anyway. |#
        (question-string
         (replace-all
          question-string "or more " ""))
        (no-consec
         (if (or
              (search
               "violins" question-string
               :test #'char-equal)
              (search
               "violas" question-string
               :test #'char-equal)
              (search
               "cellos" question-string
               :test #'char-equal)
              (search
               "double basses" question-string
               :test #'char-equal)
              (search
               "contrabasses" question-string
               :test #'char-equal))
           (replace-all
            (replace-all
             question-string "in a row" "")
            "consecutive" "")
           (string-right-trim
            '(#\Space #\s)
            (replace-all
             (replace-all
              question-string "in a row" "")
             "consecutive" ""))))
        (nos-consec
         (if no-consec
           (number-string2numeric no-consec)))
        (numberless-question-string
         (if no-consec
           (number-string2numberless-string
            no-consec))))
  (if (> nos-consec 0)
    (loop for i from 0 to (- nos-consec 1)
      collect
      (append
       (list numberless-question-string)
       (if (zerop i) time-interval (list 0))))
    (append (list question-string) time-interval)))

#|
\noindent Example:
\begin{verbatim}
(followed-by-splitter-Jun2015 "chord C3 F#4")
--> (("chord C3 F#4" 0))
(followed-by-splitter-Jun2015 "chord C3, F#4")
--> (("chord C3, F#4" 0))
(followed-by-splitter-Jun2015 "C3 F#4")
--> (("C3" 0) ("F#4" 0))
(followed-by-splitter-Jun2015 "C3, F#4")
--> (("C3" 0) ("F#4" 0))
(followed-by-splitter-Jun2015 "crotchet, quaver")
--> (("crotchet" 0) ("quaver" 0))
; NB these last two split by different functions.
(followed-by-splitter-Jun2015
 "C followed by Eb in the bass clef")
--> (("C" 0) ("by Eb in the bass clef" 0))
(followed-by-splitter-Jun2015
 "F# followed two crotchets later by a G")
--> (("F#" 0) ("G" 2))
(followed-by-splitter-Jun2015
 "F#, then two quavers later a G")
--> (("F#" 0) ("G" 1))
(followed-by-splitter-Jun2015
 "F# then two bars later a G")
--> (("F#" 0) ("G" 2 "bars"))
(followed-by-splitter-Jun2015 "F# then a G")
--> (("F#" 0) ("G" 0))
(followed-by-splitter-Jun2015
 (concatenate
  'string "three ascending seconds followed by a fall"
  " of a third"))
--> (("ascending second" 0) ("ascending second" 0)
     ("ascending second" 0) ("fall of third" 0))
(followed-by-splitter-Jun2015
 (concatenate
  'string "two falling seconds followed three"
  " quarter notes later by a rise of a third"))
--> (("falling second" 0) ("falling second" 0)
     ("rise of third" 3))
(followed-by-splitter-Jun2015 "consecutive fifths")
--> (("fifth" 0) ("fifth" 0))
(followed-by-splitter-Jun2015
 (concatenate
  'string "two falling seconds followed three"
  " quarter notes later by a crotchet tied with a"
  " minim"))
(followed-by-splitter-Jun2015 "2 quavers in the violins")
\end{verbatim}

\noindent If a question string refers to a sequence of
events (e.g., `F then two crotchets later a G'), this
function splits it into two separate questions, also
returning the time relation that must pertain between
the two events.

The only difference between followed-by-splitter and
followed-by-splitter-Jun2015 is that the former calls
pitch-class-sequential-expression2list whereas the latter
calls the (hopefully) more widely applicable and robust
pitch-sequential-expression2list. |#

(defun followed-by-splitter-Jun2015
       (question-string &optional
        #| If the question string contains "G-clef",
        "F-clef", or "C-clef" as opposed to "treble",
        bass, or alto, then replace the former with
        the latter. This has to be done early, because
        the call to pitch-class-sequential-expression
        further down relies on uppercase letters
        being used to represent pitch classes only.
        This could probably be moved into the function
        modify-question-by-staff-restriction, as the
        aforementioned function is more robust now. |#
        (question-string
         (replace-all
          (replace-all
           (replace-all
            question-string "G-clef" "treble clef")
           "F-clef" "bass clef")
          "C-clef" "alto clef"))
        #| If the question string begins with "the
        words" or the "lyrics", then we will convert
        this to a followed-by question string. This
        chunk of code could be turned into its own
        little function, but has been left in this
        function for now. |#
        (question-split
         (if (or
              (search "the words " question-string)
              (search "the lyrics " question-string))
           (if (search "&quot;" question-string)
             (string-separated-string2list
              "&quot;" question-string)
             (string-separated-string2list
              "\"" question-string))))
        (lyric-queries
         (if question-split
           (space-bar-separated-string2list
            (second question-split))))
        (question-string
         (if lyric-queries
           (concat-strings
            (append
             (loop for i from 0 to
               (- (length lyric-queries) 2)
               collect
               (concatenate
                'string "word \""
                (nth i lyric-queries)
                "\" followed by "))
             (list
              "word \"" (my-last lyric-queries)
              "\""))) question-string))
        ; Now on to the function proper.   
        (question-strings
         (cond
          ((search "followed" question-string)
           (string-separated-string2list
            "followed" question-string))
          ((search ", then" question-string)
           (string-separated-string2list
            ", then" question-string))
          ((search " then" question-string)
           (string-separated-string2list
            " then" question-string))
          ((and
            (not
             (loop for c in
               (list #\A #\B #\C #\D #\E #\F #\G) thereis
               (>
                (count
                 c question-string :test #'equal) 0)))
            (search ", " question-string))
           (string-separated-string2list
            ", " question-string))
          ))
        (question-strings-mod
         (if question-strings
           (mapcar
            #'(lambda (x) (modify-by-later x))
            question-strings)
           (list (list question-string nil))))
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (consecutive-question2list x))
           question-strings-mod)))
        #| Split mention of tied notes into
        separate questions. |#
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (tied-question2list x))
           initial-question-list)))
        #| Split the sequential expression of pitch
        classes (possibly followed by other
        information into separate questions). |#
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (pitch-sequential-expression2list x))
           initial-question-list)))
        #| If the final question element contains a
        voice reference but the preceding elements do
        not, then this voice reference must be
        distributed to them, because it probably
        refers to them too. |#
        (voice-referencep
         (and
          (> (length initial-question-list) 1)
          (>
           (length
            (first
             (my-last initial-question-list))) 8)
          (or
           (and
            (string=
             (subseq
              (first (my-last initial-question-list))
              (-
               (length
                (first
                 (my-last initial-question-list)))
               6)) " voice")
            (not
             (string=
              (subseq
               (first (my-last initial-question-list))
               (-
                (length
                 (first
                  (my-last initial-question-list)))
                8)) " a voice")))
           (and
            (string=
             (subseq
              (first (my-last initial-question-list))
              (-
               (length
                (first
                 (my-last initial-question-list)))
               6)) " part")
            (not
             (string=
              (subseq
               (first (my-last initial-question-list))
               (-
                (length
                 (first
                  (my-last initial-question-list)))
                8)) " a part"))))))
        (voice-ref-str
         (if voice-referencep
           (subseq
            (first (my-last initial-question-list))
            (search
             "in the "
             (first
              (my-last initial-question-list)))))))
  (if voice-ref-str
    (append
     (loop for i from 0 to
       (- (length initial-question-list) 2) collect
       (list
        (concatenate
         'string
         (first (nth i initial-question-list))
         " " voice-ref-str)
        (second (nth i initial-question-list))))
     (last initial-question-list))
    initial-question-list))

(defun followed-by-splitter
       (question-string &optional
        #| If the question string contains "G-clef",
        "F-clef", or "C-clef" as opposed to "treble",
        bass, or alto, then replace the former with
        the latter. This has to be done early, because
        the call to pitch-class-sequential-expression
        further down relies on uppercase letters
        being used to represent pitch classes only.
        This could probably be moved into the function
        modify-question-by-staff-restriction, as the
        aforementioned function is more robust now. |#
        (question-string
         (replace-all
          (replace-all
           (replace-all
            question-string "G-clef" "treble clef")
           "F-clef" "bass clef")
          "C-clef" "alto clef"))
        #| If the question string begins with "the
        words" or the "lyrics", then we will convert
        this to a followed-by question string. This
        chunk of code could be turned into its own
        little function, but has been left in this
        function for now. |#
        (question-split
         (if (or
              (search "the words " question-string)
              (search "the lyrics " question-string))
           (if (search "&quot;" question-string)
             (string-separated-string2list
              "&quot;" question-string)
             (string-separated-string2list
              "\"" question-string))))
        (lyric-queries
         (if question-split
           (space-bar-separated-string2list
            (second question-split))))
        (question-string
         (if lyric-queries
           (concat-strings
            (append
             (loop for i from 0 to
               (- (length lyric-queries) 2)
               collect
               (concatenate
                'string "word \""
                (nth i lyric-queries)
                "\" followed by "))
             (list
              "word \"" (my-last lyric-queries)
              "\""))) question-string))
        ; Now on to the function proper.   
        (question-strings
         (cond
          ((search "followed" question-string)
           (string-separated-string2list
            "followed" question-string))
          ((search ", then" question-string)
           (string-separated-string2list
            ", then" question-string))
          ((search " then" question-string)
           (string-separated-string2list
            " then" question-string))))
        (question-strings-mod
         (if question-strings
           (mapcar
            #'(lambda (x) (modify-by-later x))
            question-strings)
           (list (list question-string nil))))
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (consecutive-question2list x))
           question-strings-mod)))
        #| Split mention of tied notes into
        separate questions. |#
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (tied-question2list x))
           initial-question-list)))
        #| Split the sequential expression of pitch
        classes (possibly followed by other
        information into separate questions). |#
        (initial-question-list
         (append-list-of-lists
          (mapcar
           #'(lambda (x)
               (pitch-class-sequential-expression2list
                x))
           initial-question-list)))
        #| If the final question element contains a
        voice reference but the preceding elements do
        not, then this voice reference must be
        distributed to them, because it probably
        refers to them too. |#
        (voice-referencep
         (and
          (> (length initial-question-list) 1)
          (>
           (length
            (first
             (my-last initial-question-list))) 8)
          (or
           (and
            (string=
             (subseq
              (first (my-last initial-question-list))
              (-
               (length
                (first
                 (my-last initial-question-list)))
               6)) " voice")
            (not
             (string=
              (subseq
               (first (my-last initial-question-list))
               (-
                (length
                 (first
                  (my-last initial-question-list)))
                8)) " a voice")))
           (and
            (string=
             (subseq
              (first (my-last initial-question-list))
              (-
               (length
                (first
                 (my-last initial-question-list)))
               6)) " part")
            (not
             (string=
              (subseq
               (first (my-last initial-question-list))
               (-
                (length
                 (first
                  (my-last initial-question-list)))
                8)) " a part"))))))
        (voice-ref-str
         (if voice-referencep
           (subseq
            (first (my-last initial-question-list))
            (search
             "in the "
             (first
              (my-last initial-question-list)))))))
  (if voice-ref-str
    (append
     (loop for i from 0 to
       (- (length initial-question-list) 2) collect
       (list
        (concatenate
         'string
         (first (nth i initial-question-list))
         " " voice-ref-str)
        (second (nth i initial-question-list))))
     (last initial-question-list))
    initial-question-list))

#| First attempt. Did not include any ability to
handle occurrences of "consecutive" or "in a row" in
queries.
(defun followed-by-splitter
       (question-string &optional
        (question-strings
         (cond
          #|
          ((search "followed by another" question-string)
           (string-separated-string2list
            "followed by another" question-string))
          ((search "followed by an" question-string)
           (string-separated-string2list
            "followed by an" question-string))
          ((search "followed by a" question-string)
           (string-separated-string2list
            "followed by a" question-string))
          ((search "followed by" question-string)
           (string-separated-string2list
            "followed by" question-string))
          |#
          ((search "followed" question-string)
           (string-separated-string2list
            "followed" question-string))
          ((search ", then" question-string)
           (string-separated-string2list
            ", then" question-string))
          ((search "then" question-string)
           (string-separated-string2list
            "then" question-string)))))
  (if question-strings
    (mapcar
     #'(lambda (x) (modify-by-later x))
     question-strings)
    (list (list question-string nil))))
|#

#|
\noindent Example:
\begin{verbatim}
(modify-by-later "two crotchets later by a G")
--> ("G" 2)
(modify-by-later "four eighth notes later by a G")
--> ("G" 2)
(modify-by-later "two measures later by a G")
--> ("G" 2 "bars")
(modify-by-later "two measures later a G")
--> ("G" 2 "bars")
(modify-by-later "F natural")
--> ("F natural" 0)
(modify-by-later "four consecutive quavers")
--> ("four consecutive quavers" 0)
(modify-by-later "four quavers in a row")
--> ("four quavers in a row" 0)
(modify-by-later "fermata quaver")
--> ("fermata quaver" 0)
\end{verbatim}

\noindent This function edits a question string that
refers to a subsequent musical event taking place a
specified amount of time later. The question string
is cleaned of the ``later'' reference and the amount
of time later is returned (measured in crotchet
beats). If it is a number of ``bars later'' then this
is not possible to measure in crotchet beats unless
the beginning ontime/bar and time signature (plus
changes) are known. So in this case the number of bars
is returned, plus the string ``bars'' for further
processing. |#

(defun modify-by-later
       (question-string &optional
        (later-idx
         (search "later" question-string))
        (bars-idx
         (or
          (search "bars" question-string)
          (search "measures" question-string)))
        (question-string2
         (string-trim
          " "
          (replace-all
           (replace-all
            (replace-all
             (replace-all
              (replace-all
               (subseq
                question-string
                (if later-idx (+ later-idx 5) 0))
               "by another" "")
              "by an" "")
             "by a" "") "by " "") " a " " "))))
  (cons
   question-string2
   (if bars-idx
     (list
      (number-string2numeric question-string) "bars")
     (list
      (number&note2time-interval question-string)))))

#| Old version.
(defun modify-by-later
       (question-string &optional
        (later-idx
         (search "later" question-string))
        (bars-idx
         (or
          (search "bars" question-string)
          (search "measures" question-string)))
        (question-string
         (string-trim
          " "
          (replace-all
           (replace-all
            (replace-all
             (replace-all
              (replace-all
               (subseq
                question-string
                (if later-idx (+ later-idx 5) 0))
               "by another" "")
              "by an" "")
             "by a" "") "by " "") "a " ""))))
  (cons
   question-string
   (if bars-idx
     (list
      (number-string2numeric question-string) "bars")
     (list
      (number&note2time-interval question-string)))))
|#

#|
\noindent Example:
\begin{verbatim}
(number&note2time-interval "two crotchets")
--> 2
(number&note2time-interval "two semiquavers")
--> 1/2
(number&note2time-interval
 "four sixteenth notes")
--> 1
(number&note2time-interval "eighth note")
--> 0
(number&note2time-interval "quarter note")
--> 0
(number&note2time-interval
 "three dotted half notes")
--> 9
(number&note2time-interval
 "two consecutive crotchets")
--> 0
(number&note2time-interval "crotchet")
--> 0
\end{verbatim}

\noindent This function converts a string-based
representation of quantity (expressed as a number and
a note value) into the numeric representation
measured in crotchet beats. |#

(defun number&note2time-interval
       (question-string &optional
        (single
         (duration-string2numeric question-string))
        (multiple
         (if single
           (number-string2numeric question-string))))
  (if (and single (numberp multiple))
    (* single (if multiple multiple 1)) 0))

#|
\noindent Example:
\begin{verbatim}
(number-string2numberless-string "six quarter notes")
--> "quarter notes"
(number-string2numberless-string "sixty quarter notes")
--> "quarter notes"
(number-string2numberless-string "seventh")
--> "seventh"
(number-string2numberless-string "90 quarter notes")
--> "quarter notes"
(number-string2numberless-string "16 16th notes")
--> "16th notes"
\end{verbatim}

\noindent This function returns a revised version of
the input string, removing any numeric quantity that
appears at the front. |#

(defun number-string2numberless-string
       (question-string &optional
        (probes
         '(("one" 1) ("two" 2) ("three" 3) ("four" 4)
           ("five" 5) ("six" 6) ("seven" 7) ("eight" 8)
           ("nine" 9) ("ten" 10) ("eleven" 11)
           ("twelve" 12)  ("thirteen" 13) ("fourteen" 14)
           ("fifteen" 15) ("sixteen" 16)
           ("seventeen" 17) ("eighteen" 18)
           ("nineteen" 19) ("twenty" 20) ("thirty" 30)
           ("forty" 40) ("fifty" 50) ("sixty" 60)
           ("seventy" 70) ("eighty" 80) ("ninety" 90)))
        #| Old probes from 2014.
        (probes
         (list
          "one" "two" "three" "four"
          "five" "six" "seven" "eight"
          "nine" "ten" "eleven" "twelve"
          "thirteen" "fourteen" "fifteen" "sixteen"
          "seventeen" "eighteen" "nineteen"
          "twenty"))
        |#
        (n (length question-string))
        (rel-idx
         (first
          (loop for i from 0 to (- (length probes) 1)
            when
            (and
             (string=
              (subseq
               question-string
               0
               (min-item
                (list (length (first (nth i probes))) n)))
              (first (nth i probes)))
             (if (equalp i 3)
               (and
                (not
                 (string=
                  (subseq
                   question-string 0 (min 6 n))
                  "fourth"))
                (not
                 (string=
                  (subseq
                   question-string 0 (min 10 n))
                  "fourteenth"))) t)
             (if (equalp i 5)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 5 n))
                  "sixty"))
                (not
                 (string=
                  (subseq question-string 0 (min 5 n))
                  "sixth"))
                (not
                 (string=
                  (subseq question-string 0 (min 9 n))
                  "sixteenth"))) t)
             (if (equalp i 6)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 7 n))
                  "seventy"))
                (not
                 (string=
                  (subseq question-string 0 (min 7 n))
                  "seventh"))
                (not
                (string=
                 (subseq question-string 0 (min 11 n))
                 "seventeenth"))) t)
             (if (equalp i 7)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "eighty"))
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "eighth"))
                (not
                 (string=
                  (subseq
                   question-string 0 (min 10 n))
                  "eighteenth"))) t)
             (if (equalp i 8)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "ninety"))
                (not
                 (string=
                  (subseq question-string 0 (min 10 n))
                  "nineteenth"))) t)
             (if (equalp i 9)
               (not
                (string=
                 (subseq question-string 0 (min 5 n))
                 "tenth")) t)
             (if (equalp i 10)
               (not
                (string=
                 (subseq question-string 0 (min 8 n))
                 "eleventh")) t)
             (if (equalp i 12)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "thirteenth")) t)
             (if (equalp i 13)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "fourteenth")) t)
             (if (equalp i 14)
               (not
                (string=
                 (subseq question-string 0 (min 9 n))
                 "fifteenth")) t)
             (if (equalp i 15)
               (not
                (string=
                 (subseq question-string 0 (min 9 n))
                 "sixteenth")) t)
             (if (equalp i 16)
               (not
                (string=
                 (subseq question-string 0 (min 11 n))
                 "seventeenth")) t)
             (if (equalp i 17)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "eighteenth")) t)
             (if (equalp i 18)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "nineteenth")) t))
            collect i)))
        (digits
         (parse-integer
          question-string
          :junk-allowed t))
        (digits
         (if (or
              (equalp (search "8th" question-string) 0)
              (equalp (search "16th" question-string) 0)
              (equalp (search "32nd" question-string) 0)
              (equalp (search "64th" question-string) 0))
           nil digits))
        )
  (if rel-idx
    (string-trim
     " "
     (replace-all
      question-string
      (first (nth rel-idx probes)) ""))
    (if digits
      (string-trim
       " "
       (replace-once
        question-string
        (write-to-string digits) ""))
      question-string)))

#|
\noindent Example:
\begin{verbatim}
(number-string2numeric "two crotchets")
--> 2
(number-string2numeric "quarter note")
--> 0
(number-string2numeric "eight crotchets")
--> 8
(number-string2numeric "sixteenth note")
--> 0
(number-string2numeric "six quarter notes")
--> 6
(number-string2numeric "nine consecutive crotchets")
--> "consecutive"
(number-string2numeric "nine crotchets")
--> 9
(number-string2numeric "nine crotchets in a row")
--> "consecutive"
(number-string2numeric "ninety crotchets")
--> 90
(number-string2numeric "90 crotchets")
--> 90
(number-string2numeric "three bars")
--> 3
(number-string2numeric "crotchet")
--> 0
(number-string2numeric "8th note C")
--> 0
(number-string2numeric "8 8th notes")
--> 8
\end{verbatim}

\noindent This function returns a natural number, a
string, or nil, representing the quantity mentioned
at the beginning of the input string argument. The
most common use case for this function will be a
string containing a quantity of note or bar values
that signify a time interval (e.g., two crotchets
later). If, however, a number of consecutive events is
referred to (e.g., two crotchets in a row), this must
be recognised and the string ``consecutive'' returned
instead. |#

(defun number-string2numeric
       (question-string &optional
        #| This is a good long-term solution, but the
        exception handling below needs more careful
        thought/revision.
        (probes
         (loop for i from 1 to 100 collect
           (format nil "~r" i)))
        |#
        (probes
         '(("one" 1) ("two" 2) ("three" 3) ("four" 4)
           ("five" 5) ("six" 6) ("seven" 7) ("eight" 8)
           ("nine" 9) ("ten" 10) ("eleven" 11)
           ("twelve" 12)  ("thirteen" 13) ("fourteen" 14)
           ("fifteen" 15) ("sixteen" 16)
           ("seventeen" 17) ("eighteen" 18)
           ("nineteen" 19) ("twenty" 20) ("thirty" 30)
           ("forty" 40) ("fifty" 50) ("sixty" 60)
           ("seventy" 70) ("eighty" 80) ("ninety" 90)))
        (n (length question-string))
        (rel-idx
         (first
          (loop for i from 0 to (- (length probes) 1)
            when
            (and
             (string=
              (subseq
               question-string
               0
               (min-item
                (list (length (first (nth i probes))) n)))
              (first (nth i probes)))
             (if (equalp i 3)
               (and
                (not
                 (string=
                  (subseq
                   question-string 0 (min 6 n))
                  "fourth"))
                (not
                 (string=
                  (subseq
                   question-string 0 (min 10 n))
                  "fourteenth"))) t)
             (if (equalp i 5)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 5 n))
                  "sixty"))
                (not
                 (string=
                  (subseq question-string 0 (min 5 n))
                  "sixth"))
                (not
                 (string=
                  (subseq question-string 0 (min 9 n))
                  "sixteenth"))) t)
             (if (equalp i 6)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 7 n))
                  "seventy"))
                (not
                 (string=
                  (subseq question-string 0 (min 7 n))
                  "seventh"))
                (not
                (string=
                 (subseq question-string 0 (min 11 n))
                 "seventeenth"))) t)
             (if (equalp i 7)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "eighty"))
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "eighth"))
                (not
                 (string=
                  (subseq
                   question-string 0 (min 10 n))
                  "eighteenth"))) t)
             (if (equalp i 8)
               (and
                (not
                 (string=
                  (subseq question-string 0 (min 6 n))
                  "ninety"))
                (not
                 (string=
                  (subseq question-string 0 (min 10 n))
                  "nineteenth"))) t)
             (if (equalp i 9)
               (not
                (string=
                 (subseq question-string 0 (min 5 n))
                 "tenth")) t)
             (if (equalp i 10)
               (not
                (string=
                 (subseq question-string 0 (min 8 n))
                 "eleventh")) t)
             (if (equalp i 12)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "thirteenth")) t)
             (if (equalp i 13)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "fourteenth")) t)
             (if (equalp i 14)
               (not
                (string=
                 (subseq question-string 0 (min 9 n))
                 "fifteenth")) t)
             (if (equalp i 15)
               (not
                (string=
                 (subseq question-string 0 (min 9 n))
                 "sixteenth")) t)
             (if (equalp i 16)
               (not
                (string=
                 (subseq question-string 0 (min 11 n))
                 "seventeenth")) t)
             (if (equalp i 17)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "eighteenth")) t)
             (if (equalp i 18)
               (not
                (string=
                 (subseq question-string 0 (min 10 n))
                 "nineteenth")) t))
            collect i)))
        (digits
         (parse-integer
          question-string
          :junk-allowed t))
        (digits
         (if (or
              (equalp (search "8th" question-string) 0)
              (equalp (search "16th" question-string) 0)
              (equalp (search "32nd" question-string) 0)
              (equalp (search "64th" question-string) 0))
           nil digits)))
  (if (or
       #|
       (string=
        (subseq
         question-string
         (min-item
          (list
           (+ (length (nth rel-idx probes)) 1) n))
         (min-item
          (list
           (+ (length (nth rel-idx probes)) 12) n)))
        "consecutive")
       |#
       (search "consecutive" question-string)
       (search "in a row" question-string))
    "consecutive"
    (if rel-idx
      (second (nth rel-idx probes))
      (if digits digits 0))))

#|
\noindent Example:
\begin{verbatim}
(pitch-class-sequential-expression2list
 (list "A sharp B flat Db" 2 "bars"))
--> (("A sharp" 2 "bars") ("B flat" 0) ("Db" 0))
(pitch-class-sequential-expression2list
 (list "asecnding G B D" 1))
--> (("G" 1) ("B" 0) ("D" 0))
(pitch-class-sequential-expression2list
 (list "A sharp crotchet B flat Db" 2))
--> (("A sharp crotchet" 2) ("B flat" 0) ("Db" 0))
(pitch-class-sequential-expression2list
 (list "ascending A sharp crotchet" 2))
--> ("ascending A sharp crotchet" 2)
(pitch-class-sequential-expression2list
 (list "quarter note F in the Alto" 0))
--> ("quarter note F in the Alto" 0)
\end{verbatim}

\noindent This function identifies pitch classes in a
question string, such as might be provided
sequentially (e.g., C E$\flat$ G). It splits the
question into separate elements consisting of these
pitch classes (and possibly other events).

11/6/2015. This function has been replaced by
\nameref{fun:pitch-sequential-expression2list}, which
handles pitch and octave usage also. |#

(defun pitch-class-sequential-expression2list
       (question-string&time-interval &optional
        (question-string
         (first question-string&time-interval))
        (time-interval
         (if (second question-string&time-interval)
           (rest question-string&time-interval)
           (list 0)))
        #| Does the string contain a reference to
        "chord"? If so, we probably don't want to
        split it up, because the sequence refers to
        members of a simultaneity. |#
        (chordp (search "chord" question-string))
        #| Does the string contain a sequence of
        pitch-class references? |#
        (positions-pc
         (if (not chordp)
           (positions-char
            #\_
            (substitute-if
             #\_ #'upper-case-p question-string))))
        #| If each of these is followed by a sharp or
        flat sign, or a space, then keep it. Otherwise
        lose it. |#
        (l-question-string (length question-string))
        (positions-pc
         (loop for i from 0 to
           (- (length positions-pc) 1) when
           (if (<
                (nth i positions-pc)
                (- l-question-string 1))
             (and
              (cond
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\A) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\B) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\C) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\D) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\E) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\F) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\G) t))
              (cond
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\b) t)
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\#) t)
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\Space)
                t))) t)
           collect (nth i positions-pc))))
  (if (> (length positions-pc) 1)
    (loop for i from 0 to (- (length positions-pc) 1)
      collect
      (append
       (list
        (if (equalp i (- (length positions-pc) 1))
          (subseq
           question-string (nth i positions-pc))
          (subseq
           question-string
           (nth i positions-pc)
           (- (nth (+ i 1) positions-pc) 1))))
       (if (zerop i) time-interval (list 0))))
    (append (list question-string) time-interval)))

#|
\noindent Example:
\begin{verbatim}
(pitch-sequential-expression2list
 '("quavers C#4, A3, Eb3, Gb3 in the X part" 3))
--> (("quaver C#4" 3) ("quaver A3" 0) ("quaver Eb3" 0)
     ("quaver Gb3 in the X part" 0))
(pitch-sequential-expression2list
 '("A sharp B flat Db" 2 "bars" 3))
--> (("A sharp" 2 "bars") ("B flat" 0) ("Db" 0))
(pitch-sequential-expression2list
 '("ascending G B D" 1))
--> (("G" 1) ("B" 0) ("D" 0))
(pitch-sequential-expression2list
 '("A sharp crotchet B flat Db" 2))
--> (("A sharp crotchet" 2) ("B flat" 0) ("Db" 0))
(pitch-sequential-expression2list
 '("ascending A sharp crotchet" 2))
--> ("ascending A sharp crotchet" 2)
(pitch-sequential-expression2list
 '("quarter note F in the Alto" 0))
--> ("quarter note F in the Alto" 0)
(pitch-sequential-expression2list
 '("dotted minim on the word &quot;Hi&quot;" 0))
--> (("dotted minim on the word &quot;Hi&quot;" 0))
(pitch-sequential-expression2list
 '("quarter note" 0))
--> (("quarter note" 0))
(pitch-sequential-expression2list
 (list
  (concatenate
   'string "E4 E5 G#4 B4 in sixteenth notes in the"
   " treble clef") 0))
--> (("E4" 0) ("E5" 0) ("G#4" 0)
     ("B4 in sixteenth notes in the treble clef" 0))
(pitch-sequential-expression2list
 '("quavers in the violins" 0))
--> (("quavers in the violins" 0))
\end{verbatim}

\noindent This function identifies pitches in a question
string, such as might be provided sequentially (e.g.,
C4 E$\flat$4 G3). It splits the question into separate
elements consisting of these pitches (and possibly other
events). |#

(defun pitch-sequential-expression2list
       (question-string&time-interval &optional
        (question-string
         (first question-string&time-interval))
        (time-interval
         (if (second question-string&time-interval)
           (rest question-string&time-interval)
           (list 0)))
        #| Does the string contain a reference to
        "chord"? If so, we probably don't want to
        split it up, because the sequence refers to
        members of a simultaneity. |#
        (chordp (search "chord" question-string))
        #| Get the positions of uppercase letters. |#
        (positions-pc
         (if (not chordp)
           (positions-char
            #\_
            (substitute-if
             #\_ #'upper-case-p question-string))))
        #| Separate prefix and postfix. |#
        (prefix-dirty
         (if (not chordp)
           (subseq
            question-string 0
            (first positions-pc))))
        (prefix-dirty
         (if (string= prefix-dirty "")
           nil prefix-dirty))
        (prefix-clean
         (if (and (not chordp) prefix-dirty)
           (if (or
                (search
                 "violins" question-string
                 :test #'char-equal)
                (search
                 "violas" question-string
                 :test #'char-equal)
                (search
                 "cellos" question-string
                 :test #'char-equal)
                (search
                 "double basses" question-string
                 :test #'char-equal)
                (search
                 "contrabasses" question-string
                 :test #'char-equal))
             (string-right-trim
              '(#\Space) prefix-dirty)
             (string-right-trim
              '(#\Space #\s) prefix-dirty))))
        (prefix-durationp
         (duration-string2numeric prefix-clean))
        (postfix
         (if (and
              (not chordp)
              (search " in the " question-string))
           (subseq
            question-string
            (search " in the " question-string))))
        (question-string-orig question-string)
        (question-string
         (if prefix-dirty
           (replace-all
            question-string prefix-dirty "")
           question-string))
        (question-string
         (if postfix
           (replace-all
            question-string postfix "")
           question-string))
        #| Get the positions of uppercase letters. |#
        (positions-pc
         (if (not chordp)
           (positions-char
            #\_
            (substitute-if
             #\_ #'upper-case-p question-string))))
        #|
        (string-separated-string2list
            (if (search ", " question-string)
              ", " " ")
            (replace-all
             question-string "chord" ""))
        |#
        #| If each of these is followed by a sharp or
        flat sign, an integer, or a space or comma,
        then keep it. Otherwise lose it. |#
        (l-question-string (length question-string))
        (positions-pc
         (loop for i from 0 to
           (- (length positions-pc) 1) when
           (if (<
                (nth i positions-pc)
                (- l-question-string 1))
             (and
              (cond
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\A) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\B) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\C) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\D) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\E) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\F) t)
               ((equal
                 (char
                  question-string
                  (nth i positions-pc)) #\G) t))
              (cond
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\b) t)
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\#) t)
               ((parse-integer
                 (subseq
                  question-string
                  (+ (nth i positions-pc) 1))
                 :junk-allowed t) t)
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\Space)
                t)
               ((equal
                 (char
                  question-string
                  (+ (nth i positions-pc) 1)) #\,)
                t))) t)
           collect (nth i positions-pc)))
        #| Collect the substrings. |#
        (subs-nearly-ready
         (if (> (length positions-pc) 1)
           (loop for i from 0
             to (- (length positions-pc) 1)
             collect
             (append
              (list
               (if (equalp
                    i (- (length positions-pc) 1))
                 (subseq
                  question-string (nth i positions-pc))
                 (subseq
                  question-string
                  (nth i positions-pc)
                  (- (nth (+ i 1) positions-pc) 1))))
              (if (zerop i) time-interval (list 0))))
           (list
            (append
             (list question-string-orig)
             time-interval))))
        #| Remove any remaining commas and re-insert
        any prefix. |#
        (subs-ready
         (if (> (length positions-pc) 1)
           (loop for i from 0
             to (- (length positions-pc) 1)
             collect
             (if prefix-durationp
               (cons
                (concatenate
                 'string
                 prefix-clean " "
                 (string-right-trim
                  '(#\,)
                  (first (nth i subs-nearly-ready))))
                (rest (nth i subs-nearly-ready)))
               (cons
                (string-right-trim
                 '(#\,)
                 (first (nth i subs-nearly-ready)))
                (rest (nth i subs-nearly-ready)))))
           subs-nearly-ready)))
  #| Finally, reinsert any postfix, but only in the
  last element, because it is redistributed to all
  elements by a later function. |#
  (if (and postfix (> (length positions-pc) 1))
    (loop for i from 0
      to (- (length positions-pc) 1)
      collect
      (if (equalp i (- (length positions-pc) 1))
        (cons
         (concatenate
          'string
          (first (nth i subs-ready)) postfix)
         (rest (nth i subs-ready)))
        (nth i subs-ready))) subs-ready))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 (concatenate
  'string "minim in the left hand against"
  " a crotchet in the right hand"))
(followed-by-splitter question-string)
--> (("minim in the left hand against a crotchet in the right hand" 0))
(simultaneous-splitter question-string)
--> (("minim in the left hand")
     ("crotchet in the right hand"))
(setq
 question-string
 (concatenate
  'string "simultaneous minim in the left hand and"
  " a crotchet in the right hand"))
(followed-by-splitter question-string)
--> (("simultaneous minim in the left hand and a crotchet in the right hand" 0))
(simultaneous-splitter question-string)
--> ("minim in the left hand" "crotchet in the right hand")
(setq
 question-string
 (concatenate
  'string "minim in the left hand accompanied by"
  " a crotchet in the right hand"))
(followed-by-splitter question-string)
--> (("minim in the left hand accompanied by a crotchet in the right hand" 0))
(simultaneous-splitter question-string)
--> ("minim in the left hand" "crotchet in the right hand")
\end{verbatim}

\noindent This function parses queries that contain
information about simultaneous events. It splits the
query into multiple queries according to those events,
and will also remove (now) unnecessary simultaniety
delimiters and extra `a's or `an's. |#

(defun simultaneous-splitter
       (question-string &optional
        (sim-delimiters
         '(("against" "against")
           ("simultaneously" " and ")
           ("simultaneous" " and ")
           ("accompanied by" "accompanied by")
           ("at the same time" " and ")))
        (indices-of-sim-delims
         (loop for sd in sim-delimiters collect
           (search (first sd) question-string)))
        (sim-delimiterp
         (index-item-1st-doesnt-occur
          nil indices-of-sim-delims))
        (sim-phrase&join
         (if sim-delimiterp
           (nth sim-delimiterp sim-delimiters)))
        (questions
         (if sim-delimiterp
           (string-separated-string2list
            (second sim-phrase&join)
            (if (string=
                 (first sim-phrase&join)
                 (second sim-phrase&join))
              question-string
              (replace-all
               question-string
               (first sim-phrase&join) "")))
           (list question-string)))
        (questions-tidy
         (mapcar
          #'(lambda (x)
              (if (and
                   (>= (length x) 3)
                   (string= (subseq x 0 3) "an "))
                (subseq x 3)
                (if (and
                     (>= (length x) 2)
                     (string= (subseq x 0 2) "a "))
                  (subseq x 2) x)))
          questions)))
  questions-tidy)

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string&time-interval
 (list
  (concatenate
   'string
   "dotted crotchet tied with a crotchet tied with a "
   "quaver") 2 "bars"))
(tied-question2list question-string&time-interval)
--> (("[ dotted crotchet" 2 "bars") ("[] crotchet" 0)
     ("] quaver" 0))
(setq
 question-string&time-interval
 (list
  (concatenate
   'string "dotted crotchet tied with a crotchet") 1))
(tied-question2list question-string&time-interval)
--> (("[ dotted crotchet" 1) ("] crotchet" 0))
(setq
 question-string&time-interval
 (list
  (concatenate
   'string "dotted crotchet") 1))
(tied-question2list question-string&time-interval)
--> ("dotted crotchet" 1)
\end{verbatim}

\noindent This function unpacks an expression such as
`crotchet tied with a quaver' into the constituents
`crotchet tied forward' and `quaver tied back'. |#

(defun tied-question2list
       (question-string&time-interval &optional
        (question-strings
         (string-separated-string2list
          "tied with"
          (first question-string&time-interval))))
  (if (> (length question-strings) 1)
    (append
     (list
      (append
       (list
        (concatenate
         'string "[ " (first question-strings)))
       (rest question-string&time-interval)))
     (loop for i from 1 to
       (- (length question-strings) 2) collect
       (list
        (concatenate
         'string "[] " (nth i question-strings)) 0))
     (list
      (list
       (concatenate
        'string "] " (my-last question-strings)) 0)))
    question-string&time-interval))
