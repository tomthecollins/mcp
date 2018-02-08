#| Copyright 2008-2014 Tom Collins
   Friday 9 May 2014
   Incomplete

\noindent The functions below will parse a point set
representation of a piece/excerpt of music and
return times at which pitches, intervals or
durations specified by a string argument occur.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "projection"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
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

(defvar
 *pitch-class-MNN-MPN-mod-assoc*
  '(("B#" (12 6)) ("C" (0 0)) ("Dbb" (0 1))
    ("B##" (13 6)) ("C#" (1 0)) ("Db" (1 1))
    ("C##" (2 0)) ("D" (2 1)) ("Ebb" (2 2))
    ("D#" (3 1)) ("Eb" (3 2)) ("Fbb" (3 3))
    ("D##" (4 1)) ("E" (4 2)) ("Fb" (4 3))
    ("E#" (5 2)) ("F" (5 3)) ("Gbb" (5 4))
    ("E##" (6 2)) ("F#" (6 3)) ("Gb" (6 4))
    ("F##" (7 3)) ("G" (7 4)) ("Abb" (7 5))
    ("G#" (8 4)) ("Ab" (8 5))
    ("G##" (9 4)) ("A" (9 5)) ("Bbb" (9 6))
    ("Cbb" (10 0)) ("A#" (10 5))
    ("Bb" (10 6))
    ("A##" (11 5)) ("B" (11 6))
    ("Cb" (11 0))))

#|
\noindent Example:
\begin{verbatim}
(setq question-string "fermata on an F")
(setq
 artic-set
 '((3/2 59 59 1/2 2 NIL NIL NIL)
   (2 50 54 1 3 NIL NIL NIL)
   (2 57 58 1 2 NIL NIL NIL)
   (2 65 63 1 0 (";") NIL NIL)
   (2 66 63 1 1 NIL NIL NIL)))
(articulation&event-time-intervals
 question-string artic-set staff&clef-names)
--> ((2 3)).
\end{verbatim}

\noindent This function looks for expressive
markings in the articulation dimension of an
articulation point set and events specified in the
question string. It returns time intervals
corresponding to notes that are set to the
expressive marking specified in the question
string and that instantiate the specified event. |#

(defun articulation&event-time-intervals
       (question-string artic-set staff&clef-names
        &optional
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4) (artic-idx 5)
        (q-split&artic-event&rel-points
         (articulation-points
          question-string artic-set artic-idx))
        (question-split
         (first q-split&artic-event&rel-points))
        (probe-artic
         (second q-split&artic-event&rel-points))
        (relevant-points
         (third q-split&artic-event&rel-points))
        (probe-event
         (if question-split
             (second question-split)))
        (rel-intervals
         (if probe-artic
           (remove-duplicates
            (loop for rp in relevant-points when
              (position
               probe-artic (nth artic-idx rp)
               :test #'search)
              collect
              (list
               (nth ontime-idx rp)
               (+
                (nth ontime-idx rp)
                (nth duration-idx rp))))
            :test #'equalp)))
        (ordinary-point-set
         (if probe-artic
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-idx x)
                 (nth MNN-idx x)
                 (nth MPN-idx x)
                 (nth duration-idx x)
                 (nth staff-idx x)))
            relevant-points)))
        (ans-dur-pitch
         (if probe-artic
           (duration&pitch-class-time-intervals
            probe-event ordinary-point-set
            staff&clef-names)))
        (ans-pitch
         (if probe-artic
           (pitch-class-time-intervals
            probe-event ordinary-point-set
            staff&clef-names)))
        (ans-dur
         (if probe-artic
           (duration-time-intervals
            probe-event ordinary-point-set
            staff&clef-names))))
  (cond
   ((not (null ans-dur-pitch))
    (intersection-multidimensional
     rel-intervals ans-dur-pitch))
   ((not (null ans-pitch))
    (intersection-multidimensional
     rel-intervals ans-pitch))
   ((not (null ans-dur))
    (intersection-multidimensional
     rel-intervals ans-dur))
   (t rel-intervals)))

(defun dynamic&event-time-intervals
       (question-string artic-set staff&clef-names
        &optional
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4) (dynam-idx 6)
        (q-split&artic-event&rel-points
         (dynamics-points
          question-string artic-set dynam-idx))
        (question-split
         (first q-split&artic-event&rel-points))
        (probe-artic
         (second q-split&artic-event&rel-points))
        (relevant-points
         (third q-split&artic-event&rel-points))
        (probe-event
         (if question-split
             (second question-split)))
        (rel-intervals
         (if probe-artic
           (remove-duplicates
            (loop for rp in relevant-points when
              (position
               probe-artic (nth dynam-idx rp)
               :test #'search)
              collect
              (list
               (nth ontime-idx rp)
               (+
                (nth ontime-idx rp)
                (nth duration-idx rp))))
            :test #'equalp)))
        (ordinary-point-set
         (if probe-artic
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-idx x)
                 (nth MNN-idx x)
                 (nth MPN-idx x)
                 (nth duration-idx x)
                 (nth staff-idx x)))
            relevant-points)))
        (ans-dur-pitch
         (if probe-artic
           (duration&pitch-class-time-intervals
            probe-event ordinary-point-set
            staff&clef-names)))
        (ans-pitch
         (if probe-artic
           (pitch-class-time-intervals
            probe-event ordinary-point-set
            staff&clef-names)))
        (ans-dur
         (if probe-artic
           (duration-time-intervals
            probe-event ordinary-point-set
            staff&clef-names))))
  (cond
   ((not (null ans-dur-pitch))
    (intersection-multidimensional
     rel-intervals ans-dur-pitch))
   ((not (null ans-pitch))
    (intersection-multidimensional
     rel-intervals ans-pitch))
   ((not (null ans-dur))
    (intersection-multidimensional
     rel-intervals ans-dur))
   (t rel-intervals)))

#| 1st attempt.
(defun articulation&event-time-intervals
       (question-string artic-set &optional
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4) (artic-idx 5)
        (question-split
         (cond
          ((search "staccato " question-string)
           (string-separated-string2list
            "staccato " question-string))
          ((search "marcato " question-string)
           (string-separated-string2list
            "marcato " question-string))
          ((search "fermata on an " question-string)
           (string-separated-string2list
            "fermata on an " question-string))
          ((search "fermata on a " question-string)
           (string-separated-string2list
            "fermata on a " question-string))
          ((search "fermata " question-string)
           (string-separated-string2list
            "fermata " question-string))))
        (probe-artic
         (cond
          ((search "staccato " question-string) "'")
          ((search "marcato " question-string) "^")
          ((search "fermata "
            question-string) ";")))
        (probe-event
         (if question-split
           (second question-split)))
        (rel-intervals
         (if probe-artic
           (remove-duplicates
            (loop for i from 0 to
              (- (length artic-set) 1) when
              (position
               probe-artic
               (nth artic-idx (nth i artic-set))
               :test #'search)
              collect
              (list
               (nth ontime-idx (nth i artic-set))
               (+
                (nth ontime-idx (nth i artic-set))
                (nth
                 duration-idx (nth i artic-set)))))
            :test #'equalp)))
        (ordinary-point-set
         (if probe-artic
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-idx x)
                 (nth MNN-idx x)
                 (nth MPN-idx x)
                 (nth duration-idx x)
                 (nth staff-idx x)))
            artic-set)))
        (ans-dur-pitch
         (if probe-artic
           (duration&pitch-class-time-intervals
            probe-event ordinary-point-set nil)))
        (ans-pitch
         (if probe-artic
           (pitch-class-time-intervals
            probe-event ordinary-point-set nil)))
        (ans-dur
         (if probe-artic
           (duration-time-intervals
            probe-event ordinary-point-set nil))))
  (cond
   ((not (null ans-dur-pitch))
    (intersection-multidimensional
     rel-intervals ans-dur-pitch))
   ((not (null ans-pitch))
    (intersection-multidimensional
     rel-intervals ans-pitch))
   ((not (null ans-dur))
    (intersection-multidimensional
     rel-intervals ans-dur))))
|#

#|
\noindent Example:
\begin{verbatim}
(duration&pitch-class-time-intervals
 "dotted minim C sharp"
 '((0 37 46 1 1) (1/3 68 64 1/3 0) (2/3 66 63 1/3 0)
   (1 49 53 3 1) (1 56 57 1 1) (1 59 59 1 1)
   (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 3 1) (2 50 54 3 1))
  '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((1 4) (2 5))
(duration&pitch-class-time-intervals
 "dotted minim C sharp in the piano right hand"
 '((0 37 46 1 1) (1/3 68 64 1/3 0) (2/3 66 63 1/3 0)
   (1 49 53 3 1) (1 56 57 1 1) (1 59 59 1 1)
   (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 3 1) (2 50 54 3 1))
  '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> nil
\end{verbatim}

\noindent This function returns (ontime, offtime)
pairs of points (notes) that have the duration and
pitch class specified by the first string argument.
Durations can be in the format ``dotted minim'' or
``dotted half note'', for instance. Pitc classes can
be in the format  The function does not look for
dotted notes in the case of the word dotted, but
adds one half of the value to the corresponding note
type and looks for the numeric value. |#

(defun duration&pitch-class-time-intervals
       (question-string point-set staff&clef-names
        &optional
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3) (staff-idx 4)
        (val
         (if (not
              (or
               (search "[" question-string)
               (search "]" question-string)))
         (duration-string2numeric question-string)))
        ; Edit out the duration of question string.
        (question-string
         (edit-out-duration-of-question-string
          question-string))
        ; Edit the pitch class of question string.
        (question-string
         (replace-all
          question-string " double flat" "bb"
          :test #'string=))
        (question-string
         (replace-all
          question-string " double sharp" "##"
          :test #'string=))
        (question-string
         (replace-all
          question-string " flat" "b"
          :test #'string=))
        (question-string
         (replace-all
          question-string " sharp" "#"
          :test #'string=))
        (question-string
         (replace-all
          question-string " natural" ""
          :test #'string=))
        ; modify question by any staff restriction.
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        ; Restrict the point set too.
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-idx staff-restriction)
           point-set))
        (pair-no-mod
         (if (parse-integer
              (my-last-string question-string)
              :junk-allowed t)
           (pitch&octave2MIDI-morphetic-pair
            (replace-all
             question-string " " ""
             :test #'string=))))
        (pair
         (if (null pair-no-mod)
           (second
            (assoc
             question-string
             *pitch-class-MNN-MPN-mod-assoc*
             :test #'string=))
           pair-no-mod))
       ; Set of ontime, mod MNN & MPN, dur, offtime.
        (point-set-mod-n
         (if (null pair-no-mod)
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-index x)
                 (mod (nth MNN-index x) 12)
                 (mod (+ (nth MPN-index x) 3) 7)
                 (nth duration-index x)
                 (+
                  (nth ontime-index x)
                  (nth duration-index x))))
            point-set)
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-index x)
                 (nth MNN-index x)
                 (nth MPN-index x)
                 (nth duration-index x)
                 (+
                  (nth ontime-index x)
                  (nth duration-index x))))
            point-set)))
        #| Get the points with the relevant duration
        and pitch. |#
        #| Comment 28/11/2014. There should probably
        be a remove-duplicates command in here
        somewhere. |#
        (relevant-points
         (if (and val pair)
           (restrict-dataset-in-nth-to-xs
            ; Testing MPN.
            (restrict-dataset-in-nth-to-xs
             ; Testing MNN.
             (restrict-dataset-in-nth-to-xs
              ; Testing duration.
              point-set-mod-n duration-index
              (list val))
             MNN-index (list (first pair)))
            MPN-index (list (second pair))))))
  (if relevant-points
    (mapcar
     #'(lambda (x)
         (list (first x) (fifth x)))
     relevant-points)
    (if (and val pair)
      "val and pair were true")))

#|
\noindent Example:
\begin{verbatim}
(duration-string2numeric "four hemidemisemiquavers")
--> 1/16
(duration-string2numeric "dotted quaver")
--> 3/4
(duration-string2numeric "double dotted quaver")
--> 7/8
(duration-string2numeric "dotted yeah")
--> NIL
\end{verbatim}

\noindent This function converts a duration
expressed in string format into a numeric format.

10/12/2015. Made an alteration because `dotted
triplet quavers' (for example) were being processed
as one, say `dotted', but not the other
`triplet'. |#

(defun duration-string2numeric
       (question-string &optional
        (base-dur
         (if (or
              (search
               "hemidemisemiquaver"
               question-string)
              (search
               "sixty-fourth note"
               question-string)) 1/16
           (if (or
                (search
                 "demisemiquaver"
                 question-string)
                (search
                 "thirty-second note"
                 question-string)) 1/8
             (if (or
                  (search
                   "semiquaver"
                   question-string)
                  (search
                   "sixteenth note"
                   question-string)) 1/4
               (if (or
                    (search
                     "quaver"
                     question-string)
                    (search
                     "eighth note"
                     question-string)) 1/2
                 (if (or
                      (search
                       "crotchet"
                       question-string)
                      (search
                       "quarter note"
                       question-string)) 1
                   (if (or
                        (search
                         "minim"
                         question-string)
                        (search
                         "half note"
                         question-string)) 2
                     (if (or
                          (search
                           "semibreve"
                           question-string)
                          (search
                           "whole note"
                         question-string)) 4))))))))
        (base-dur
         (cond
          ((and
            base-dur
            (search
             "triple dotted" question-string))
           (* base-dur 15/8))
          ((and
            base-dur
            (search
             "double dotted" question-string))
           (* base-dur 7/4))
          ((and
            base-dur
            (search
             "dotted" question-string))
           (* base-dur 3/2))
          (t
           base-dur))))
  (if (and
       base-dur
       (search "triplet" question-string))
    (* base-dur 2/3)
    (if (and
         base-dur
         (search "quintuplet" question-string))
      (* base-dur 2/5)
      (if (and
           base-dur
           (search "septuplet" question-string))
        (* base-dur 2/7)
        (identity base-dur)))))

#|
\noindent Example:
\begin{verbatim}
(duration-time-intervals
 "dotted minim"
 '((0 37 46 1 1) (1/3 68 64 1/3 0) (2/3 66 63 1/3 0)
   (1 49 53 3 1) (1 56 57 1 1) (1 59 59 1 1)
   (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 3 1) (2 50 54 3 1))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((1 4) (2 5))
(duration-time-intervals
 "quarter note in the piano left hand"
 '((0 37 46 1 1) (1/3 68 64 1/3 0) (2/3 66 63 1/3 0)
   (1 49 53 3 1) (1 56 57 1 1) (1 59 59 1 1)
   (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 3 1) (2 50 54 3 1))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((0 1) (1 2))
\end{verbatim}

\noindent This function returns (ontime, offtime)
pairs of points (notes) that have the duration
specified by the first string argument It can be in
the format ``dotted minim'' or ``dotted half note'',
for instance. The function does not look for dotted
notes in the case of the word dotted, but adds one
half of the value to the corresponding note type and
looks for the numeric value. |#

(defun duration-time-intervals
       (question-string point-set staff&clef-names
        &optional
        (ontime-index 0) (duration-index 3)
        (staff-idx 4)
        #| Check that the question string does not
        contain strings such as "staccato",
        "marcato", "fermata", "rest", or a tie
        symbol. If it does, this is not the function
        to use. This function might still get called
        when processing articulation or rests, but
        it will have had these aspects removed and
        processed by a different function. |#
        (artic-lyric-rest-tie-tf
         (cond
          ((search "staccato" question-string) t)
          ((search "marcato" question-string) t)
          ((search "fermata" question-string) t)
          ((search " word" question-string) t)
          ((search " rest" question-string) t)
          ((search "[" question-string) t)
          ((search "]" question-string) t)))
        ; Convert the duration into numeric format.
        (val
         (if (not artic-lyric-rest-tie-tf)
           (duration-string2numeric
            question-string)))
        ; modify question by any staff restriction.
        (question-string&staff-idx
         (if val
           (modify-question-by-staff-restriction
            question-string staff&clef-names)))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        ; Restrict the point set too.
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-idx staff-restriction)
           point-set))
        ; Append the offtimes.
        (point-set-off-app
         (if val
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-index x)
                 (nth duration-index x)
                 (+
                  (nth ontime-index x)
                  (nth duration-index x))))
            point-set)))
        ; Get the points with the relevant duration.
        (relevant-points
         (if val
           (restrict-dataset-in-nth-to-xs
             ; Testing duration.
             point-set-off-app 1 (list val)))))
  (progn
    ; This next line simply to suppress warning msg.
    (length question-string)
    (remove-duplicates
     (mapcar
      #'(lambda (x)
          (list (first x) (third x)))
      relevant-points)
     :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(edit-out-duration-of-question-string
 "dotted crotchet C4")
--> "C4"
(edit-out-duration-of-question-string
 "C4 dotted crotchet in the oboe")
--> "C4   in the oboe"
\end{verbatim}

\noindent This function, new for Stravinsqi-Jun2015,
removes any mention of a musical duration from an input
string, returning whatever remains of the string. As the
second example shows, it could be improved by removing
extra white spaces, which (might) otherwise lead to
errors in subsequent processing. |#

(defun edit-out-duration-of-question-string
       (question-string &optional
        (question-string
         (replace-all
          question-string "triple dotted" ""
          :test #'string=))        
        (question-string
         (replace-all
          question-string "double dotted" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "dotted" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "triplet" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quintuplet" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "septuplet" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "hemidemisemiquaver" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "sixty-fourth note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "demisemiquaver" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "thirty-second note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "semiquaver" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "sixteenth note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quaver" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "eighth note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "crotchet" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quarter note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "minim" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "half note" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "semibreve" ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "whole note" ""
          :test #'string=)))
  (string-trim " " question-string))

#| Old version with specific space.
(defun edit-out-duration-of-question-string
       (question-string &optional
        (question-string
         (replace-all
          question-string "triple dotted " ""
          :test #'string=))        
        (question-string
         (replace-all
          question-string "double dotted " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "dotted " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "triplet " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quintuplet " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "septuplet " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "hemidemisemiquaver " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "sixty-fourth note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "demisemiquaver " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "thirty-second note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "semiquaver " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "sixteenth note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quaver " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "eighth note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "crotchet " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "quarter note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "minim " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "half note " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "semibreve " ""
          :test #'string=))
        (question-string
         (replace-all
          question-string "whole note " ""
          :test #'string=)))
  question-string)
|#

#|
\noindent Example:
\begin{verbatim}
(harmonic-interval-of-a "third"
 '((0 60 60 3 0) (2 63 62 1 0) (5 63 62 1 0)
   (5 67 64 1/2 0)))
--> '((2 3) (5 11/2))
(harmonic-interval-of-a "major 3rd"
 '((0 60 60 3 0) (2 63 62 1 0) (5 63 62 1 0)
   (5 67 64 1/2 0)))
--> ((5 11/2))
(harmonic-interval-of-a "quaver rest"
 '((0 60 60 3 0) (2 63 62 1 0) (5 63 62 1 0)
   (5 67 64 1/2 0)))
--> nil
\end{verbatim}

\noindent The first argument is a string; the second
is a point set. The function returns a list of raw
ontime-offtime pairs during which the harmonic
interval specified by the string is sounding. If an
ontime-offtime pair is $(a, b)$, it should be noted
that the interval sounds in the interval $[a, b)$.

One of the training questions mentioned simultaneous
intervals. I will need to write a function that
looks for the word "simultaneous", splits the string
into the requested intervals, calculates
ontime-offtime pairs for each interval, then finds
the intersection of these. |#

(defun harmonic-interval-of-a
       (question-string point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        (question-string
         (replace-all
          (replace-all
           question-string
           "harmonic interval of a " ""
           :test #'string=)
          "harmonic " "" :test #'string=))
        #| Identify restrictions to any particular
        staves or voices, identify the numerical
        index of the relevant staff and edit the
        question string appropriately. |#
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        #| Standard variables. |#
        (MNN-idx 1) (staff-idx 4) (duration-idx 3)
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-idx staff-restriction)
           point-set))
        (vectors
         #| If the question contains "in a voice",
         it probably refers to melodic rather than
         harmonic intervals, so nil-ify the vectors
         variable to avoid returning harmonic
         results. |#
         (if (search " in a voice" question-string)
           nil
           (interval-string2MNN-MPN-mods
            question-string)))
        ; Segment the point set.
        (segs
         (if (not (null vectors))
           (segments-strict
            point-set MNN-idx duration-idx)))
        ; Within each segment, project & return MTP.
        (lower-points
         (loop for i from 0 to
           (- (length segs) 1) collect
           (loop for j from 0 to
             (- (length vectors) 1) append
             (maximal-translatable-pattern
              (nth j vectors)
           (orthogonal-projection-not-unique-equalp
               (nth i (nth-list-of-lists 1 segs))
               (list 0 1 1 0 0)))))))
  (harmonic-interval-segments2raw-times
   (nth-list-of-lists 0 segs) lower-points))

#| Second attempt.
(defun harmonic-interval-of-a
       (question-string point-set &optional
        (question-string
         (replace-all
          (replace-all
           question-string
           "harmonic interval of a " ""
           :test #'string=)
          "harmonic " "" :test #'string=))
        (vectors
         #| If the question contains "in a voice",
         it probably refers to melodic rather than
         harmonic intervals, so nil-ify the vectors
         variable to avoid returning harmonic
         results. |#
         (if (search " in a voice" question-string)
           nil
           (interval-string2MNN-MPN-mods
            question-string)))
        ; Segment the point set.
        (segs
         (if (not (null vectors))
           (segments-strict
            point-set 1 3)))
        ; Within each segment, project & return MTP.
        (lower-points
         (loop for i from 0 to
           (- (length segs) 1) collect
           (loop for j from 0 to
             (- (length vectors) 1) append
             (maximal-translatable-pattern
              (nth j vectors)
           (orthogonal-projection-not-unique-equalp
               (nth i (nth-list-of-lists 1 segs))
               (list 0 1 1 0 0)))))))
  (harmonic-interval-segments2raw-times
   (nth-list-of-lists 0 segs) lower-points))
|#

#| First attempt.
(defun harmonic-interval-of-a
       (question-string point-set &optional
        (question-string
         (replace-all
          (replace-all
           question-string
           "harmonic interval of a " ""
           :test #'string=)
          "harmonic " "" :test #'string=))
        (vector
         (interval-string2MNN-MPN-mod
          question-string))
        ; Segment the point set.
        (segs
         (if (not (null vector))
           (segments-strict
            point-set 1 3)))
        ; Within each segment, project & return MTP.
        (lower-points
         (mapcar
          #'(lambda (x)
              (maximal-translatable-pattern
               vector
   (orthogonal-projection-not-unique-equalp
    x (list 0 1 1 0 0))))
          (nth-list-of-lists 1 segs))))
  (harmonic-interval-segments2raw-times
   (nth-list-of-lists 0 segs) lower-points))
|#

#|
\noindent Example:
\begin{verbatim}
(harmonic-interval-segments2raw-times
 '(-1 0 1/3 2/3 1 3/2 2 11/4 3)
 '(NIL NIL NIL NIL ((56 57)) ((56 57)) NIL ((59 59))
   NIL))
--> '((1 2) (11/4 3))
\end{verbatim}

\noindent The first argument is a sequence of
ontimes; the second is a list of MNN-MPN pairs of
the same length as the first argument. The function
unites consecutive windows and returns a list of
time windows in which there are non-null items. |#

(defun harmonic-interval-segments2raw-times
       (seg-ons harm-int-lower-points &optional
        (curr-lower-point
         (if (not (null seg-ons))
           (first (first harm-int-lower-points))))
        (raw-on
         (if (not (null curr-lower-point))
           (first seg-ons)))
        (nxt-lower-point
         (if (not (null (second seg-ons)))
           (first (second harm-int-lower-points))))
        (raw-off
         (if (equalp
              curr-lower-point nxt-lower-point)
           (identity nil)
           (if (not (null (second seg-ons)))
             (second seg-ons)))))
  (if (null seg-ons) ()
    (append
     (if (and (numberp raw-on) (numberp raw-off))
       (list (list raw-on raw-off)))
     (if (and (numberp raw-on) (null raw-off))
       (harmonic-interval-segments2raw-times
        (rest seg-ons) (rest harm-int-lower-points)
        curr-lower-point raw-on)
       (harmonic-interval-segments2raw-times
        (rest seg-ons)
        (rest harm-int-lower-points))))))

#|
\noindent Example:
\begin{verbatim}
(interval-string2MNN-MPN-mods "perfect 5th")
--> ((7 4))
\end{verbatim}

\noindent This function converts a string
representation of a harmonic or melodic interval to
a list of pairs of MIDI note and morphetic pitch
numbers modulo twelve and seven respectively. For
instance, a perfect fifth is the interval of 7 MNN
and 5 MPN. |#

(defun interval-string2MNN-MPN-mods
       (interval-string)
  (cond
   ((or
     (string= interval-string "unison")
     (string= interval-string "1st"))
    (list (list 0 0)))
   ((or
     (string= interval-string "second")
     (string= interval-string "2nd"))
    (list (list 1 1) (list 2 1)))
   ((or
     (string= interval-string "semitone")
     (string= interval-string "half-step"))
    (list (list 1 0) (list 1 1)))
   ((or
     (string= interval-string "minor second")
     (string= interval-string "minor 2nd"))
    (list (list 1 1)))
   ((or
     (string= interval-string "major second")
     (string= interval-string "major 2nd"))
    (list (list 2 1)))
   ((or
     (string= interval-string "tone")
     (string= interval-string "step"))
    (list (list 2 0) (list 2 1)))
   ((or
     (string= interval-string "third")
     (string= interval-string "3rd"))
    (list (list 3 2) (list 4 2)))
   ((or
     (string= interval-string "minor third")
     (string= interval-string "minor 3rd"))
    (list (list 3 2)))
   ((or
     (string= interval-string "major third")
     (string= interval-string "major 3rd"))
    (list (list 4 2)))
   ((or
     (string=
      interval-string "diminished fourth")
     (string= interval-string "diminished 4th"))
    (list (list 4 3)))
   ((or
     (string= interval-string "perfect fourth")
     (string= interval-string "perfect 4th")
     (string= interval-string "fourth")
     (string= interval-string "4th"))
    (list (list 5 3)))
   ((or
     (string=
      interval-string "augmented fourth")
     (string= interval-string "augmented 4th")
     (string= interval-string "tritone"))
    (list (list 6 3)))
   ((or
     (string= interval-string "diminished fifth")
     (string= interval-string "diminished 5th")
     (string= interval-string "tritone"))
    (list (list 6 4)))
   ((or
     (string= interval-string "perfect fifth")
     (string= interval-string "perfect 5th")
     (string= interval-string "fifth")
     (string= interval-string "5th"))
    (list (list 7 4)))
   ((or
     (string= interval-string "augmented fifth")
     (string= interval-string "augmented 5th"))
    (list (list 8 4)))
   ((or
     (string= interval-string "sixth")
     (string= interval-string "6th"))
    (list (list 8 5) (list 9 5)))
   ((or
     (string= interval-string "minor sixth")
     (string= interval-string "minor 6th"))
    (list (list 8 5)))
   ((or
     (string= interval-string "major sixth")
     (string= interval-string "major 6th"))
    (list (list 9 5)))
   ((or
     (string= interval-string "seventh")
     (string= interval-string "7th"))
    (list (list 10 6) (list 11 6)))
   ((or
     (string= interval-string "minor seventh")
     (string= interval-string "minor 7th"))
    (list (list 10 6)))
   ((or
     (string= interval-string "major seventh")
     (string= interval-string "major 7th"))
    (list (list 11 6)))
   ((string= interval-string "octave")
    (list (list 12 7)))
   ((or
     (string= interval-string "ninth")
     (string= interval-string "9th"))
    (list (list 13 8) (list 14 8)))
   ((or
     (string= interval-string "minor ninth")
     (string= interval-string "minor 9th"))
    (list (list 13 8)))
   ((or
     (string= interval-string "major ninth")
     (string= interval-string "major 9th"))
    (list (list 14 8)))
   ((or
     (string= interval-string "tenth")
     (string= interval-string "10th"))
    (list (list 15 9)))
   ((or
     (string= interval-string "minor tenth")
     (string= interval-string "minor 10th"))
    (list (list 15 9)))
   ((or
     (string= interval-string "major tenth")
     (string= interval-string "major 10th"))
    (list (list 16 9)))))

#| First attempt.
(defun interval-string2MNN-MPN-mod (interval-string)
  (cond
   ; unisons?
   ((or
     (string= interval-string "minor second")
     (string= interval-string "minor 2nd")
     (string= interval-string "semitone"))
    (list 1 1))
   ((or
     (string= interval-string "major second")
     (string= interval-string "major 2nd")
     (string= interval-string "second")
     (string= interval-string "tone"))
    (list 2 1))
   ((or
     (string= interval-string "minor third")
     (string= interval-string "minor 3rd"))
    (list 3 2))
   ((or
     (string= interval-string "major third")
     (string= interval-string "major 3rd")
     (string= interval-string "third"))
    (list 4 2))
   ((or
     (string=
      interval-string "diminished fourth")
     (string= interval-string "diminished 4th"))
    (list 4 3))
   ((or
     (string= interval-string "perfect fourth")
     (string= interval-string "perfect 4th")
     (string= interval-string "fourth")
     (string= interval-string "4th"))
    (list 5 3))
   ((or
     (string=
      interval-string "augmented fourth")
     (string= interval-string "augmented 4th")
     (string= interval-string "tritone"))
    (list 6 3))
   ((or
     (string= interval-string "diminished fifth")
     (string= interval-string "diminished 5th")
     (string= interval-string "tritone"))
    (list 6 4))
   ((or
     (string= interval-string "perfect fifth")
     (string= interval-string "perfect 5th")
     (string= interval-string "fifth")
     (string= interval-string "5th"))
    (list 7 4))
   ((or
     (string= interval-string "augmented fifth")
     (string= interval-string "augmented 5th"))
    (list 8 4))
   ((or
     (string= interval-string "minor sixth")
     (string= interval-string "minor 6th"))
    (list 8 5))
   ((or
     (string= interval-string "major sixth")
     (string= interval-string "major 6th")
     (string= interval-string "sixth"))
    (list 9 5))
   ((or
     (string= interval-string "minor seventh")
     (string= interval-string "minor 7th")
     (string= interval-string "seventh"))
    (list 10 6))
   ((or
     (string= interval-string "major seventh")
     (string= interval-string "major 7th"))
    (list 11 6))
   ((string= interval-string "octave")
    (list 12 7))
   ((or
     (string= interval-string "minor ninth")
     (string= interval-string "minor 9th"))
    (list 13 8))
   ((or
     (string= interval-string "major ninth")
     (string= interval-string "major 9th")
     (string= interval-string "ninth"))
    (list 14 8))
   ((or
     (string= interval-string "minor tenth")
     (string= interval-string "minor 10th"))
    (list 15 9))
   ((or
     (string= interval-string "major tenth")
     (string= interval-string "major 10th")
     (string= interval-string "tenth"))
    (list 16 9))))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 point-set
 '((0 52 55 1/2 1) (1/4 76 69 1/2 0)
   (1/2 54 56 1/2 1) (3/4 75 68 1/2 0)
   (1 56 57 1/2 1) (5/4 74 68 1/2 0)
   (3/2 57 58 1/2 1) (7/4 73 67 1/2 0)
   (2 59 59 1/2 1) (9/4 71 66 1/2 0)
   (5/2 61 60 1/2 1) (11/4 69 65 1/2 0)
   (3 63 61 1/2 1) (13/4 68 64 1/2 0)
   (7/2 64 62 1/4 1) (15/4 63 61 1/4 1)
   (15/4 66 63 1/2 0) (4 61 60 1/4 1)
   (17/4 59 59 1/4 1) (17/4 68 64 1/8 0)
   (35/8 69 65 1/8 0) (9/2 64 62 1/2 1)
   (9/2 68 64 1/4 0) (19/4 71 66 1/8 0)
   (39/8 69 65 1/8 0) (5 52 55 1/2 1)
   (5 71 66 1/4 0)))
(setq question-string "melodic fourth")
(melodic-interval-of-a question-string point-set)
--> ((17/4 9/2 5))
(setq
 question-string
 "perfect melodic fourth in the bass clef")
(melodic-interval-of-a question-string point-set)
--> ((17/4 9/2 5))
(setq
 question-string "melodic 4th in the treble clef")
(melodic-interval-of-a question-string point-set)
--> nil
(setq
 question-string "4th in the bass clef")
(melodic-interval-of-a question-string point-set)
--> nil
(setq
 question-string "melodic minor 2nd")
(melodic-interval-of-a question-string point-set)
--> ((1/4 3/4 5/4) (5/4 7/4 9/4) (11/4 13/4 15/4)
     (17/4 35/8 9/2) (35/8 9/2 19/4) (1 3/2 2)
     (3 7/2 15/4) (7/2 15/4 4))
(setq
 question-string "rising melodic minor 2nd")
(melodic-interval-of-a question-string point-set)
--> ((17/4 35/8 9/2) (1 3/2 2) (3 7/2 15/4))
(setq
 question-string "melodic descending minor 2nd")
(melodic-interval-of-a question-string point-set)
--> ((1/4 3/4 5/4) (5/4 7/4 9/4) (11/4 13/4 15/4)
     (35/8 9/2 19/4) (7/2 15/4 4))
(setq
 question-string
 "melodic rising minor 2nd in the left hand")
(melodic-interval-of-a question-string point-set)
--> ((1 3/2 2) (3 7/2 15/4))
(melodic-interval-of-a
 "octave leap" '((0 60 60 1 0) (2 72 67 1 0)))
--> ((0 2 3))
\end{verbatim}

\noindent The first argument is a string; the second
is a point set. The function returns a list of raw
ontime1-ontime2-offtime2 triples subtended by the
melodic interval specified by the string. The task
description suggests that a melodic interval
pertains from the ontime of the first note to the
offtime of the second note. This causes problems for
identifying consecutive melodic intervals, however,
because for instance, in the melody C-D-E,
technically the second rising melodic second (D-E)
begins before the first rising melodic second (C-D)
ends. Thus the ontime of the second note is output
as well, so that this can be used to identify
consecutive intervals if required.

The training questions mention that melodic
intervals can only occur `within staff' (unlike
harmonic intervals), so this is how the function has
been implemented. It also handles requests to
restrict returned results to particular staves,
whereas the function
\nameref{fun:harmonic-interval-of-a} does not at
present. |#

(defun melodic-interval-of-a
       (question-string point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        (melodic-idx
         (or
          (search "melodic" question-string)
          (search "rising" question-string)
          (search "rise" question-string)
          (search "ascending" question-string)
          (search "fall" question-string)
          (search "descending" question-string)
          (search "leap" question-string)
          (search "repeated note" question-string)
          (search "note repeated" question-string)
          (search "in a voice" question-string)))
        ; Replace instances of leap/asc with rising.
        (question-string
         (replace-all
          question-string
          "leap of a" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "leap of" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "rise of a" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "rise of" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "ascending" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "octave leap" "rising melodic octave"
          :test #'string=))
        #| Sometimes a melodic interval is expressed
        with leap or fall at the end of the phrase,
        such as augmented 4th leap. Handle these
        cases here. |#
        (question-string
         (if (and
              (> (length question-string) 5)
              (string=
               (subseq
                question-string
                (- (length question-string) 5))
               " leap"))
           (concatenate
            'string
            "rising "
            (replace-all
             question-string
             " leap" "" :test #'string=))
           question-string))
        ; Replace instances of fall with descending.
        (question-string
         (replace-all
          question-string
          "fall of a" "descending"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "fall of" "descending"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "falling" "descending"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "octave fall" "descending melodic octave"
          :test #'string=))
        #| Sometimes a melodic interval is expressed
        with leap or fall at the end of the phrase,
        such as augmented 4th fall. Handle these
        cases here. |#
        (question-string
         (if (and
              (> (length question-string) 5)
              (string=
               (subseq
                question-string
                (- (length question-string) 5))
               " fall"))
           (concatenate
            'string
            "descending "
            (replace-all
             question-string
             " fall" "" :test #'string=))
           question-string))
        ; Put rising in here to avoid double search:
        (question-string
         (replace-all
          question-string
          "repeated note" "rising unison"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "a note repeated twice" "rising unison"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "a note repeated" "rising unison"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "note repeated" "rising unison"
          :test #'string=))
        #| Remove non-specific references to voices,
        such as "in a voice". |#
        (question-string
         (if (search " in a voice" question-string)
           (string-right-trim
            '(#\Space #\s)
            (replace-all
             question-string
             "in a voice" "" :test #'string=))
           question-string))
        (up-down-either
         (if (search "rising" question-string)
           "up"
           (if (search "descending" question-string)
             "down" "either")))
        ; Strip out "melodic", etc.
        (question-string
         (if melodic-idx
           (replace-all
            (replace-all
             (replace-all
              (replace-all
               (replace-all
                (replace-all
                 (replace-all
                  (replace-all
                   (replace-all
                    question-string
                    "melodic interval of a " ""
                    :test #'string=)
                   "interval of a melodic " ""
                   :test #'string=)
               "melodic rising " "" :test #'string=)
               "rising melodic " "" :test #'string=)
                "melodic descending " ""
                :test #'string=)
               "descending melodic " ""
               :test #'string=)
              "rising " "" :test #'string=)
             "descending " "" :test #'string=)
            "melodic " "" :test #'string=)))
        #| Identify restrictions to any particular
        staves or voices, identify the numerical
        index of the relevant staff and edit the
        question string appropriately. |#
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        #| Convert string interval to numeric. |#
        (vectors
         (interval-string2MNN-MPN-mods
          question-string))
        #| Standard variables. |#
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (staff-idx 4) (duration-idx 3)
        (point-sets
         (if (not (null vectors))
           (split-point-set-by-staff
            (if staff-restriction
              (restrict-dataset-in-nth-to-xs
               point-set staff-idx
               staff-restriction)
              point-set) staff-idx)))
        (unique-ontimes
         (if (not (null vectors))
           (mapcar
            #'(lambda (x)
                (remove-duplicates
                 (sort
                  (nth-list-of-lists
                   ontime-idx x) #'<)
                 :test #'equalp))
            point-sets)))
        (list-of-pairs
         (if point-sets
           (append-list
            (mapcar
             #'(lambda (x y)
                 (pairs-forming-melodic-interval-of
                  x vectors up-down-either
                  ontime-idx
                  MNN-idx MPN-idx y))
             point-sets unique-ontimes)))))
  (mapcar
   #'(lambda (x)
       (list
        (nth ontime-idx (first x))
        (nth ontime-idx (second x))
        (+
         (nth ontime-idx (second x))
         (nth duration-idx (second x)))))
   list-of-pairs))

#| First attempt.
(defun melodic-interval-of-a
       (question-string point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        (melodic-idx
         (or
          (search "melodic" question-string)
          (search "rising" question-string)
          (search "ascending" question-string)
          (search "falling" question-string)
          (search "descending" question-string)
          (search "octave leap" question-string)
          (search "octave fall" question-string)))
        ; Replace instances of leap/asc with rising.
        (question-string
         (replace-all
          question-string
          "ascending" "rising"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "octave leap" "rising melodic octave"
          :test #'string=))
        ; Replace instances of fall with descending.
        (question-string
         (replace-all
          question-string
          "falling" "descending"
          :test #'string=))
        (question-string
         (replace-all
          question-string
          "octave fall" "descending melodic octave"
          :test #'string=))
        (up-down-either
         (if (search "rising" question-string)
           "up"
           (if (search "descending" question-string)
             "down" "either")))
        ; Strip out "melodic", etc.
        (question-string
         (if melodic-idx
           (replace-all
            (replace-all
             (replace-all
              (replace-all
               (replace-all
                (replace-all
                 (replace-all
                  (replace-all
                   question-string
                   "melodic interval of a " ""
                   :test #'string=)
          "melodic rising " "" :test #'string=)
          "rising melodic " "" :test #'string=)
          "melodic descending " "" :test #'string=)
          "descending melodic " "" :test #'string=)
              "rising " "" :test #'string=)
             "descending " "" :test #'string=)
            "melodic " "" :test #'string=)))
         #| Identify restrictions to any particular
        staves or voices, identify the numerical
        index of the relevant staff and edit the
        question string appropriately. |#
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        #| Convert string interval to numeric. |#
        (vector
         (interval-string2MNN-MPN-mod
          question-string))
        #| Standard variables. |#
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (staff-idx 4) (duration-idx 3)
        (point-sets
         (if (not (null vector))
           (if staff-restriction
             (list
              (dataset-restricted-to-m-in-nth
               point-set staff-restriction
               staff-idx))
             (split-point-set-by-staff
              point-set staff-idx))))
        (unique-ontimes
         (if (not (null vector))
           (mapcar
            #'(lambda (x)
                (remove-duplicates
                 (sort
                  (nth-list-of-lists
                   ontime-idx x) #'<)
                   :test #'equalp))
            point-sets)))
        (list-of-pairs
         (if point-sets
           (append-list
            (mapcar
             #'(lambda (x y)
                 (pairs-forming-melodic-interval-of
                  x vector up-down-either ontime-idx
                  MNN-idx MPN-idx y))
             point-sets unique-ontimes)))))
  (mapcar
   #'(lambda (x)
       (list
        (nth ontime-idx (first x))
        (nth ontime-idx (second x))
        (+
         (nth ontime-idx (second x))
         (nth duration-idx (second x)))))
   list-of-pairs))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "melodic minor 2nd in the bass clef")
(setq
 staff&clef-names
 '(("piano left hand" "bass clef") 
   ("piano right hand" "treble clef")))
(modify-question-by-staff-restriction
 question-string staff&clef-names)
--> ("melodic minor 2nd" (1))
(setq question-string "melodic minor 2nd")
(modify-question-by-staff-restriction
 question-string staff&clef-names)
--> ("melodic minor 2nd" NIL)
(setq
 question-string
 "perfect fifth between bass and alto voices")
(setq
 staff&clef-names
 '(("Bass" "bass clef") ("Tenor" "tenor clef")
   ("Alto" "treble clef")
   ("Soprano II" "treble clef")
   ("Soprano I" "treble clef")))
(modify-question-by-staff-restriction
 question-string staff&clef-names)
--> ("perfect fifth" (4 2))
(modify-question-by-staff-restriction
 "minim in the &quot;vocal&quot; part"
 '(("bass" "bass clef") ("vocal" "treble clef")))
--> ("minim" (0))
\end{verbatim}

\noindent This function modifies a question string
according to the presence of a substring that
restricts a question to a particular staff or voice.
The numerical index of the relevant staff or voice
is identified (recall that the left-most spines in a
parsed kern file have the highest staff numbers) and
returned, along with the modified question string
(modified to have the substring removed for ease of
subsequent processing).

12/6/2015. This function doesn't work if the staff
names are one letter long, and that letter occurs in
the other bits of the question string. This should
be fixed. |#

(defun modify-question-by-staff-restriction
       (question-string staff&clef-names &optional
        (n-stave (- (length staff&clef-names) 1))
        #| If the question string contains "left
        hand" or "right hand", then it will be
        necessary to modify the staff and clef names
        variable, if it contains just a generic
        "piano" label for both hands, because this
        will not be specific enough. |#
        (staff&clef-names
         (if (or
              (search "left hand" question-string)
              (search "right hand" question-string))
           (substitute
            (list "right hand" "treble clef")
            (list "piano" "treble clef")
            (substitute
             (list "left hand" "bass clef")
             (list "piano" "bass clef")
             staff&clef-names :test #'equalp)
            :test #'equalp) staff&clef-names))
        #| Search on clef first, because this is
        what the training data refers to. |#
        (staff-restriction-string&idxs
         ;(first
         (loop for i from 0 to n-stave when
           (search
            (second (nth i staff&clef-names))
            question-string :test #'equalp)
           collect
           (list
            (second (nth i staff&clef-names))
            (- n-stave i))));)
        #| Search on staff next, in case the
        question refers to this instead. |#
        (staff-restriction-string&idxs
         (append
          staff-restriction-string&idxs
          ;(first
          (loop for i from 0 to
            n-stave when
            (search
             (first (nth i staff&clef-names))
             question-string :test #'equalp)
            collect
            (list
             (first (nth i staff&clef-names))
             (- n-stave i)))));)
        #| Now remove staff restriction information
        from the question string. |#
        (question-string
         (if staff-restriction-string&idxs
           (remove-staff-restriction-from-q-string
            question-string
            staff-restriction-string&idxs)
           question-string)))
  ; Returned edited question string and staff index.
  (list
   question-string
   (remove-duplicates
    (mapcar
     #'(lambda (x)
         (second x))
     staff-restriction-string&idxs)
    :test #'equalp)))

#| First attempt.
(defun modify-question-by-staff-restriction
       (question-string staff&clef-names &optional
        (n-stave (- (length staff&clef-names) 1))
        #| Search on clef first, because this is
        what the training data refers to. |#
        (staff-restriction-string&idx
         (first
          (loop for i from 0 to n-stave when
            (search
             (second (nth i staff&clef-names))
             question-string :test #'equalp)
            collect
            (list
             (second (nth i staff&clef-names))
             (- n-stave i)))))
        #| Search on staff next, in case the
        question refers to this instead. |#
        (staff-restriction-string&idx
         (if (null staff-restriction-string&idx)
           (first
            (loop for i from 0 to
              n-stave when
              (search
               (first (nth i staff&clef-names))
               question-string :test #'equalp)
              collect
              (list
               (first (nth i staff&clef-names))
               (- n-stave i))))
           staff-restriction-string&idx))
        #| Now remove staff restriction information
        from the question string. |#
        (question-string
         (if staff-restriction-string&idx
           (replace-all
            (replace-all
             (replace-all
              (replace-all 
               question-string
               (concatenate
                'string " in the "
              (first staff-restriction-string&idx))
               "" :test #'equalp)
              (concatenate
               'string
          (first staff-restriction-string&idx) " ")
              "" :test #'equalp)
             " part" "" :test #'string=)
            " voice" "" :test #'string=)
           question-string)))
  ; Returned edited question string and staff index.
  (list
   question-string
   (second staff-restriction-string&idx)))
|#

#|
\noindent Example:
\begin{verbatim}
(my-last-string "F#5")
--> "5"
(my-last-string "")
--> ""
\end{verbatim}

\noindent This function returns the last element of a
string as a string. |#

(defun my-last-string
       (a-string &optional (n (length a-string)))
  (if (zerop (length a-string)) ""
    (subseq a-string (- n 1))))

#|
\noindent Example:
\begin{verbatim}
(nadir-apex-time-intervals
 "nadir in Soprano I voice"
 '((91 67 64 1 0) (92 66 63 3/2 0)
   (187/2 64 62 1/2 0) (94 62 61 1 0) (95 62 61 1 0)
   (96 74 68 1 0) (97 74 68 1 0) (98 75 69 3 0))
 '(("Soprano II" "treble clef")
   ("Soprano I" "treble clef")))
--> ((94 95))
(nadir-apex-time-intervals
 "melodic interval of a second"
 '((91 67 64 1 0) (92 66 63 3/2 0) (98 75 69 3 0))
 '(("Soprano II" "treble clef")
   ("Soprano I" "treble clef")))
--> nil
\end{verbatim}

\noindent This function locates the lowest- or
highest-sounding note, usually in a specified part
or voice. |#

(defun nadir-apex-time-intervals
       (question-string point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        (nadir-apex-idx
         (or
          (search "nadir" question-string)
          (search "apex" question-string)
          (search "lowest note" question-string)
          (search "highest note" question-string)))
        ; Question-string replacements.
        (question-string
         (replace-all
          question-string
          "lowest note" "nadir" :test #'string=))
        (question-string
         (replace-all
          question-string
          "highest note" "apex" :test #'string=))
        (nadir-apex-either
         (if nadir-apex-idx
           (if (search "nadir" question-string)
             "nadir" "apex")))
        #| Identify restrictions to any particular
        staves or voices, identify the numerical
        index of the relevant staff and edit the
        question string appropriately. |#
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        #| Not necessary.
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        |#
        #| Standard variables. |#
        (ontime-idx 0) (MNN-idx 1) (staff-idx 4)
        (duration-idx 3)
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-idx staff-restriction)
           point-set))
        (extremum-idx
         (if nadir-apex-either
           (if (string= nadir-apex-either "nadir")
             (second
              (min-nth-argmin MNN-idx point-set))
             (second
              (max-nth-argmax
               MNN-idx point-set))))))
  (if extremum-idx
    (list
     (list
      (nth ontime-idx (nth extremum-idx point-set))
      (+
       (nth ontime-idx (nth extremum-idx point-set))
       (nth
        duration-idx
        (nth extremum-idx point-set)))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 point-set
 '((1/4 76 69 1/2 0) (3/4 75 68 1/2 0)
   (5/4 73 67 1/2 0)))
(setq MNN-MPN-mods '((1 1) (2 1)))
(pairs-forming-melodic-interval-of
 point-set MNN-MPN-mods "down")
--> (((1/4 76 69 1/2 0) (3/4 75 68 1/2 0))
     ((3/4 75 68 1/2 0) (5/4 73 67 1/2 0)))
(setq
 point-set
 '((1/4 76 69 1/2 0) (3/4 75 68 1/2 0)
   (3/4 77 70 1/2 0)
   (5/4 74 68 1/2 0) (7/4 73 67 1/2 0)
   (9/4 71 66 1/2 0) (11/4 69 65 1/2 0)
   (13/4 68 64 1/2 0) (15/4 66 63 1/2 0)
   (17/4 68 64 1/8 0) (35/8 69 65 1/8 0)))
(setq MNN-MPN-mods '((1 1)))
(pairs-forming-melodic-interval-of
 point-set MNN-MPN-mods "either")
--> (((1/4 76 69 1/2 0) (3/4 75 68 1/2 0))
     ((1/4 76 69 1/2 0) (3/4 77 70 1/2 0))
     ((5/4 74 68 1/2 0) (7/4 73 67 1/2 0))
     ((11/4 69 65 1/2 0) (13/4 68 64 1/2 0))
     ((17/4 68 64 1/8 0) (35/8 69 65 1/8 0)))
(pairs-forming-melodic-interval-of
 point-set MNN-MPN-mods "up")
--> (((1/4 76 69 1/2 0) (3/4 77 70 1/2 0))
     ((17/4 68 64 1/8 0) (35/8 69 65 1/8 0))))
(pairs-forming-melodic-interval-of
 point-set MNN-MPN-mods "down")
--> (((1/4 76 69 1/2 0) (3/4 75 68 1/2 0))
     ((5/4 74 68 1/2 0) (7/4 73 67 1/2 0))
     ((11/4 69 65 1/2 0) (13/4 68 64 1/2 0)))
\end{verbatim}

\noindent This function takes a point set as its
first argument and a list of pairs of MIDI note and
morphetic pitch numbers (mod twelve and seven
respectively) as its second argument. It returns
pairs of points that give the melodic interval
(rising or falling) specified by the MNN-MPN pairs.
The interval between the point pair is strictly
melodic, meaning that if the first point in the pair
has ontime $x$ and the second point in the pair has
ontime $y$, there can be no other point with ontime
$z < y$ (although $z = y$ is permissible). |#

(defun pairs-forming-melodic-interval-of
       (point-set MNN-MPN-mods &optional
        (up-down-either "either")
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (unique-ontimes
         (remove-duplicates
          (sort
           (nth-list-of-lists
            ontime-idx point-set) #'<)
          :test #'equalp))
        (pairs nil)
        (point (first point-set))
        (unique-ontime-idx
         (if point
           (index-1st-sublist-item>
            (nth ontime-idx point) unique-ontimes)))
        (unique-ontime
         (if unique-ontime-idx
           (nth unique-ontime-idx unique-ontimes)))
        (candidate-points
         (if unique-ontime
           (restrict-dataset-in-nth-to-xs
            point-set ontime-idx
            (list unique-ontime))))
        (relevant-points
         (if unique-ontime
           (loop for j from 0 to
             (- (length MNN-MPN-mods) 1) append
             (loop for i from 0 to
               (-
                (length candidate-points) 1) append
               (if (equalp
                    (mapcar
                     #'(lambda (x)
                         (if (string=
                              up-down-either
                              "either")
                           (abs x) x))
                     (subtract-two-lists
                      (nth-list
                       (list MNN-idx MPN-idx)
                       (nth i candidate-points))
                      (nth-list
                       (list MNN-idx MPN-idx)
                       point)))
                    (if (string=
                         up-down-either "down")
                      (multiply-list-by-constant
                        (nth j MNN-MPN-mods) -1)
                      (nth j MNN-MPN-mods)))
               (list (nth i candidate-points))))))))
  (if (null point-set) (identity pairs)
    (pairs-forming-melodic-interval-of
     (rest point-set) MNN-MPN-mods up-down-either
     ontime-idx MNN-idx MPN-idx unique-ontimes
     (if relevant-points
       (append
        pairs
        (mapcar
         #'(lambda (x)
             (list point x)) relevant-points))
       (identity pairs)))))

#| First attempt.
(defun pairs-forming-melodic-interval-of
       (point-set MNN-MPN-mod &optional
        (up-down-either "either")
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (unique-ontimes
         (remove-duplicates
          (sort
           (nth-list-of-lists
            ontime-idx point-set) #'<)
          :test #'equalp))
        (pairs nil)
        (point (first point-set))
        (unique-ontime-idx
         (if point
           (index-1st-sublist-item>
            (nth ontime-idx point) unique-ontimes)))
        (unique-ontime
         (if unique-ontime-idx
           (nth unique-ontime-idx unique-ontimes)))
        (candidate-points
         (if unique-ontime
           (restrict-dataset-in-nth-to-xs
            point-set ontime-idx
            (list unique-ontime))))
        (relevant-points
         (if unique-ontime
           (loop for i from 0 to
             (- (length candidate-points) 1) append
             (if (equalp
                  (mapcar
                   #'(lambda (x)
                       (if (string=
                            up-down-either "either")
                         (abs x) x))
                   (subtract-two-lists
                    (nth-list
                     (list MNN-idx MPN-idx)
                     (nth i candidate-points))
                    (nth-list
                     (list MNN-idx MPN-idx) point)))
                  (if (string=
                       up-down-either "down")
                    (multiply-list-by-constant
                     MNN-MPN-mod -1)
                    MNN-MPN-mod))
               (list (nth i candidate-points)))))))
  (if (null point-set) (identity pairs)
    (pairs-forming-melodic-interval-of
     (rest point-set) MNN-MPN-mod up-down-either
     ontime-idx MNN-idx MPN-idx unique-ontimes
     (if relevant-points
       (append
        pairs
        (mapcar
         #'(lambda (x)
             (list point x)) relevant-points))
       (identity pairs)))))
|#

#|
\noindent Example:
\begin{verbatim}
(pitch-class-time-intervals
 "F sharp"
 '((-1 66 63 4/3 0) (0 37 46 1 1) (1/3 68 64 1/3 0)
   (2/3 66 63 1/3 0) (1 49 53 1 1) (1 56 57 1 1)
   (1 59 59 1 1) (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 1 1))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((-1 1/3) (2/3 1) (3/2 2))
(pitch-class-time-intervals
 "G sharp in the piano left hand"
 '((-1 66 63 4/3 0) (0 37 46 1 1) (1/3 68 64 1/3 0)
   (2/3 66 63 1/3 0) (1 49 53 1 1) (1 56 57 1 1)
   (1 59 59 1 1) (1 65 62 1/2 0) (3/2 66 63 1/2 0)
   (2 49 53 1 1))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((1 2))
\end{verbatim}

\noindent This function returns (ontime, offtime)
pairs of points (notes) that have the pitch class
specified by the first string argument It can be in
the format "G double flat" or "Gbb", for
instance. |#

(defun pitch-class-time-intervals
       (question-string point-set staff&clef-names
        &optional
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3) (staff-idx 4)
        #| Check the question string does not
        contain reference to a tie. If it does, set
        the question string to empty. Events related
        to ties may still be parsed by this
        function, but references to a tie will have
        been removed by another function first. |#
        (question-string
         (if (cond
              ((search "[" question-string) t)
              ((search "]" question-string) t))
           " " question-string))
        ; Edit the question string.
        (question-string
         (replace-all
          question-string " double flat" "bb"
          :test #'string=))
        (question-string
         (replace-all
          question-string " double sharp" "##"
          :test #'string=))
        (question-string
         (replace-all
          question-string " flat" "b"
          :test #'string=))
        (question-string
         (replace-all
          question-string " sharp" "#"
          :test #'string=))
        (question-string
         (replace-all
          question-string " natural" ""
          :test #'string=))
        ; modify question by any staff restriction.
        (question-string&staff-idx
         (modify-question-by-staff-restriction
          question-string staff&clef-names))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        ; Restrict the point set too.
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-idx staff-restriction)
           point-set))
        (pair-no-mod
         (if (parse-integer
              (my-last-string question-string)
              :junk-allowed t)
           (pitch&octave2MIDI-morphetic-pair
            (replace-all
             question-string " " ""
             :test #'string=))))
        (pair
         (if (null pair-no-mod)
           (second
            (assoc
             question-string
             *pitch-class-MNN-MPN-mod-assoc*
             :test #'string=))
           pair-no-mod))
        ; Set of ontime, mod MNN & MPN, offtime.
        (point-set-mod-n
         (if (null pair-no-mod)
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-index x)
                 (mod (nth MNN-index x) 12)
                 (mod (+ (nth MPN-index x) 3) 7)
                 (+
                  (nth ontime-index x)
                  (nth duration-index x))))
            point-set)
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-index x)
                 (nth MNN-index x)
                 (nth MPN-index x)
                 (+
                  (nth ontime-index x)
                  (nth duration-index x))))
            point-set)))
        ; Get the points with the relevant pitch.
        (relevant-points
         (if pair
           (restrict-dataset-in-nth-to-xs
            ; Testing MPN.
            (restrict-dataset-in-nth-to-xs
             ; Testing MNN.
             point-set-mod-n MNN-index
             (list (first pair)))
            MPN-index (list (second pair))))))
  (mapcar
   #'(lambda (x)
       (list
        (first x) (fourth x))) relevant-points))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "perfect fifth between bass and alto voices")
(setq
 staff-restriction-string&idxs
 '(("Bass" 4) ("Alto" 2)))
(remove-staff-restriction-from-q-string
 question-string staff-restriction-string&idxs)
--> "perfect fifth"
\end{verbatim}

\noindent Given a question string that contains one
or more references to voices, which have been
identified already and are contained in the variable
staff-restriction-string\&idxs, this function
removes those references from the question string,
so that it can be parsed without error in subsequent
analyses.

12/6/2015. Added " in" to the list of
replace-alls. |#

(defun remove-staff-restriction-from-q-string
    (question-string staff-restriction-string&idxs)
  (if (null staff-restriction-string&idxs)
    (string-trim '(#\Space) question-string)
    (remove-staff-restriction-from-q-string
     (replace-all
      (replace-all
       (replace-all
        (replace-all
         (replace-all
          (replace-all
           (replace-all
            (replace-all
             (replace-all
              (replace-all
        #| Comment 5/12/2014. At the core of the
        replace-alls is a command to replace the
        staff restriction with "". This is followed
        by (outside) a command to replace the staff
        restriction + " " with "". This doesn't
        make sense - this outer call can never be
        invoked. |#
               (replace-all
                (replace-all 
                 question-string
                 (first
                  (first
                   staff-restriction-string&idxs))
                 "" :test #'equalp)
                (concatenate
                 'string
                 (first
                  (first
                   staff-restriction-string&idxs))
                 " ") "" :test #'equalp)
               " in the" "" :test #'string=)
              ; Subtle addition here in 2015.
              " in" "" :test #'string=)
             " between the" "" :test #'string=)
            " between" "" :test #'string=)
           " and" "" :test #'string=)
          " parts" "" :test #'string=)
         " voices" "" :test #'string=)
        " part" "" :test #'string=)
       " voice" "" :test #'string=)
      ; Subtle 2015 addition - some names in quotes.
      "&quot;" "" :test #'string=)
     (rest staff-restriction-string&idxs))))

#|
\noindent Example:
\begin{verbatim}
(setq question-string "[] F sharp crotchet")
(setq
 tie-set
 '((0 66 63 1 0 "[") (1 66 63 1 0 "[]")
   (2 66 63 1 0 "]") (3 67 64 1 0 "[")
   (4 67 64 1 0 "[]") (5 67 64 1 0 "]")))
(tied&event-time-intervals question-string tie-set)
--> ((1 2))
\end{verbatim}

\noindent This function looks for ties in the
corresponding dimension of an unresolved-tie point
set. It returns time intervals of tied notes that
also instantiate some other musical event, such as a
duration or pitch. |#

(defun tied&event-time-intervals
       (question-string tie-set &optional
        (tie-idx 5)
        (tie-type
         (cond
          ((search "[]" question-string) "[]")
          ((search "[" question-string) "[")
          ((search "]" question-string) "]")))
        (probe-event
         (if tie-type
           (replace-all
            (replace-all
             question-string
             (concatenate 'string tie-type " ") "")
            "a " "")))
        (rel-points
         (if tie-type
           (dataset-restricted-to-m-in-nth
            tie-set tie-type tie-idx)))
        (ans-dur-pitch
         (if rel-points
           (duration&pitch-class-time-intervals
            probe-event rel-points nil)))
        (ans-pitch
         (if rel-points
           (pitch-class-time-intervals
            probe-event rel-points nil)))
        (ans-dur
         (if rel-points
           (duration-time-intervals
            probe-event rel-points nil))))
  (cond
   ((not (null ans-dur-pitch)) ans-dur-pitch)
   ((not (null ans-pitch)) ans-pitch)
   ((not (null ans-dur)) ans-dur)))

#|
\noindent Example:
\begin{verbatim}
(setq question-string "the word \"we\"")
(setq
 artic-set
 '((0 55 57 1 4 NIL NIL ("Sing"))
   (0 62 61 1 3 NIL NIL ("Sing"))
   (1 55 57 1 4 NIL NIL ("we"))
   (1 59 59 2 3 NIL NIL ("we"))))
(word-time-intervals question-string artic-set)
--> ((1 3) (1 2))
\end{verbatim}

\noindent This function looks for words in the
lyrics dimension of an articulation point set. It
returns time intervals corresponding to notes that
are set to the word specified in the question
string. The use of the functions reverse and
\nameref{fun:sort-dataset-asc} could be improved. |#

(defun word-time-intervals
       (question-string artic-set &optional
        (ontime-idx 0) (duration-idx 3)
        (lyrics-idx 7)
        (question-split
         (if (or
              (search "word " question-string)
              (search "lyric " question-string))
           (if (search "&quot;" question-string)
             (string-separated-string2list
              "&quot;" question-string)
             (string-separated-string2list
              "\"" question-string))))
        (probe-word
         (if question-split
             (second question-split))))
  (if probe-word
    #| Use of reverse and sort-dataset-asc here is a
    fairly crude workaround: if there are two time
    intervals [a, b], [a, c] with b < c, it seems
    the function cross-check-compound-questions
    works most effectively if the two intervals are
    in descending order [a, c], [a, b]. But this
    needs further checking especially with regards
    to lyrics queries. |#
    (reverse
     (sort-dataset-asc
      (remove-duplicates
       (loop for i from 0 to
         (- (length artic-set) 1) when
         (position
          probe-word
          (nth lyrics-idx (nth i artic-set))
          :test #'search)
         collect
         (list
          (nth ontime-idx (nth i artic-set))
          (+
           (nth ontime-idx (nth i artic-set))
           (nth
            duration-idx (nth i artic-set)))))
       :test #'equalp)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 artic-set
 '((0 46 52 4 3 NIL NIL ("might."))
   (0 58 59 2 2 NIL NIL ("might."))
   (3 58 59 1 3 NIL NIL ("Be-"))
   (3 70 66 1 2 NIL NIL ("Be-"))
   (4 69 65 1 1 NIL NIL ("-hold"))
   (4 72 67 1 0 NIL NIL ("-hold"))))
(setq
 question-string
 "word &quot;Be-&quot; on a B flat")
(word&event-time-intervals
 question-string artic-set)
--> ((3 4))
(setq
 question-string "word &quot;Ja&quot; on an A flat")
(word&event-time-intervals
 question-string artic-set)
--> nil
(setq
 question-string "Bb on the word &quot;Be-&quot;")
(word&event-time-intervals
 question-string artic-set)
--> ((3 4))
\end{verbatim}

\noindent This function looks for words in the
lyrics dimension of an articulation point set and
events specified in the question string. It returns
time intervals corresponding to notes that are set
to the word specified in the question string and
that instantiate the specified event. |#

(defun word&event-time-intervals
       (question-string artic-set &optional
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4)
        (lyrics-idx 7)
        (question-split
         (if (or
              (search "word " question-string)
              (search "lyric " question-string))
           (if (search "&quot;" question-string)
             (string-separated-string2list
              "&quot;" question-string)
             (string-separated-string2list
              "\"" question-string))))
        (probe-word
         (if question-split
             (second question-split)))
        (probe-event
         (if question-split
           (if (string= (third question-split) "")
             (replace-all
              (first question-split) " on the word"
              "")
             (replace-all
              (replace-all
               (third question-split) "on an "
               "") "on a " ""))))
    #| Analysis should only proceed if there really
    is an event to accompany the word. So use
    result of probe-event to set probe-word to nil
    if there is no event. |#
        (probe-word
         (if (or
              (string= probe-event "word")
              (string= probe-event "lyric"))
           nil probe-word))
        (rel-intervals
         (if probe-word
           (remove-duplicates
            (loop for i from 0 to
              (- (length artic-set) 1) when
              (position
               probe-word
               (nth lyrics-idx (nth i artic-set))
               :test #'search)
              collect
              (list
               (nth ontime-idx (nth i artic-set))
               (+
                (nth ontime-idx (nth i artic-set))
                (nth
                 duration-idx (nth i artic-set)))))
            :test #'equalp)))
        (ordinary-point-set
         (if probe-word
           (mapcar
            #'(lambda (x)
                (list
                 (nth ontime-idx x)
                 (nth MNN-idx x)
                 (nth MPN-idx x)
                 (nth duration-idx x)
                 (nth staff-idx x)))
            artic-set)))
        (ans-dur-pitch
         (if probe-word
           (duration&pitch-class-time-intervals
            probe-event ordinary-point-set nil)))
        (ans-pitch
         (if probe-word
           (pitch-class-time-intervals
            probe-event ordinary-point-set nil)))
        (ans-dur
         (if probe-word
           (duration-time-intervals
            probe-event ordinary-point-set nil))))
  (cond
   ((not (null ans-dur-pitch))
    (intersection-multidimensional
     rel-intervals ans-dur-pitch))
   ((not (null ans-pitch))
    (intersection-multidimensional
     rel-intervals ans-pitch))
   ((not (null ans-dur))
    (intersection-multidimensional
     rel-intervals ans-dur))))
