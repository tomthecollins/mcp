#| Copyright 2008-2013 Tom Collins
   Tuesday 17 November 2009
   Incomplete

; REQUIRED PACAKGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-merge"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#| Example:
(check-pitch&octave "C3")
gives
"C3".

This function tests whether a supplied pitch&octave
is in an acceptable format and range. I was intending
to allow pitches from C0 to C8 (MNNs 12 to 108) but
this function will not allow C8, so could be adjusted
in future. If acceptable the pitch&octave is returned,
and nil otherwise. |#

(defun check-pitch&octave
       (a-string &optional (l (length a-string)))
  (if (and
       (> (length a-string) 1)
       (find
	(subseq a-string 0 (- l 1))
	'("B#" "C" "Dbb" "B##" "C#" "Db" "C##" "D"
	  "Ebb" "D#" "Eb" "D##" "E" "Fbb" "E#" "F"
	  "Gbb" "E##" "F#" "Gb" "F##" "G" "Abb" "G#"
	  "Ab" "G##" "A" "Bbb" "Cbb" "A#" "Bb" "A##"
	  "B" "Cb") :test #'string=)
       (parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t)
       (>=
	(parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t) 0)
       (<=
	(parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t) 7))
    (identity a-string) ()))

#| Example:
(director-musice2datapoint
 7 1
 '(bar 1 n ("C3" 1/2) key "C" modus "maj" mm 192
   meter (2 2))
 3 "C3" 1/2)
gives
(7 48 53 1/2 1 nil).

This function converts one line of a director
musices file into a datapoint consisting of ontime,
MIDI note number, morphetic pitch number, duration,
stave, and T if the note is tied over. |#

(defun director-musice2datapoint
       (ontime stave dm-note index
	pitches duration &optional
	(pitches
	 (if (listp pitches)
	   (identity pitches) (list pitches)))
	(MIDI-morphetic-pair
	 (if (first pitches)
	   (pitch&octave2MIDI-morphetic-pair
	    (first pitches)))))
  (if (null (first pitches)) ()
    (cons
     (list
      ontime
      (first MIDI-morphetic-pair)
      (second MIDI-morphetic-pair)
      duration
      stave
      (if (equalp (length pitches) 1)
	(string= (nth (+ index 1) dm-note) 'tie)))
     (director-musice2datapoint
      ontime stave dm-note index (rest pitches)
      duration))))

#| Example:
(director-musices2dataset
 (concatenate
  'string
  "/Users/tomcollins/Open/Music/Director musices/"
  "example.txt"))
gives
((0 50 54 5/4 1) (5/4 52 55 1/8 1) (11/8 54 56 1/8 1)
 (3/2 55 57 1/8 1) (13/8 57 58 1/8 1)
 (7/4 59 59 1/8 1) (15/8 61 60 1/8 1) (2 62 61 1 1)
 (3 61 60 3/4 1) (15/4 59 59 1/8 1) (31/8 61 60 1/8 1)
 (4 62 61 1 1) (4 62 61 5/4 0) (5 59 59 1 1)
 (21/4 64 62 1/8 0) (43/8 66 63 1/8 0)
 (11/2 67 64 1/8 0) (45/8 69 65 1/8 0)
 (23/4 71 66 1/8 0) (47/8 73 67 1/8 0) (6 55 57 1 1)
 (6 74 68 1 0) (7 57 58 1 1) (7 73 67 3/4 0)
 (31/4 71 66 1/8 0) (63/8 73 67 1/8 0)).

This function converts a piece of music represented in
the director-musices format into a dataset where each
datapoint consists of an ontime, MIDI note number,
morphetic pitch number, duration and stave. |#

(defun director-musices2dataset (path&name)
  (resolve-ties
   (director-musices2dataset-chunked
    path&name)))

#| Example:
(director-musices2dataset-chunked
 (concatenate
  'string
  "/Users/tomcollins/Open/Music/Director musices/"
  "example.txt"))
gives
((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
 (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
 (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
 (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)
 (6 74 68 1 0 NIL) (7 73 67 3/4 0 NIL)
 (31/4 71 66 1/8 0 NIL) (63/8 73 67 1/8 0 NIL)
 (0 50 54 1 1 T) (1 50 54 1/4 1 NIL)
 (5/4 52 55 1/8 1 NIL) (11/8 54 56 1/8 1 NIL)
 (3/2 55 57 1/8 1 NIL) (13/8 57 58 1/8 1 NIL)
 (7/4 59 59 1/8 1 NIL) (15/8 61 60 1/8 1 NIL)
 (2 62 61 1 1 NIL) (3 61 60 3/4 1 NIL)
 (15/4 59 59 1/8 1 NIL) (31/8 61 60 1/8 1 NIL)
 (4 62 61 1 1 NIL) (5 59 59 1 1 NIL) (6 55 57 1 1 NIL)
 (7 57 58 1 1 NIL)).

This function converts a piece of music represented in
the director-musices format into a chunked dataset,
chunked in the sense that ties still have to be
resolved. |#

(defun director-musices2dataset-chunked
       (path&name &optional
	(dm-notes
	 (read-from-file path&name))
	(result nil)
	(ontime 0)
	(stave -1)
	(dm-note (first dm-notes))
	(index
	 (if (and (listp dm-note) dm-note)
	   (if (string= (first dm-note) 'n)
	     (identity 1) (identity 3))))
	(pitches
	 (if index
	   (first (nth index dm-note))))
	(duration
	 (if index
	     (* 4 (second (nth index dm-note))))))
  (if (null dm-notes)
    (identity result)
    (if index
      (director-musices2dataset-chunked
       path&name (rest dm-notes)
       (append
	result
	(director-musice2datapoint
	 ontime stave dm-note index pitches duration))
       (+ ontime duration) stave)
      (director-musices2dataset-chunked
       path&name (rest (rest dm-notes))
       result 0 (+ stave 1)))))

#|
\noindent Example:
\begin{verbatim}
(guess-morphetic 63 '(4 0))
--> 61.
(guess-morphetic 63 '(-2 0))
--> 62.
(guess-morphetic 70 '(5 5))
--> 65.
(guess-morphetic 70 '(1 5))
--> 66.
\end{verbatim}

\noindent This function takes a MIDI note number and
a key (represented by steps on the cycle of fiths, and
mode). It attempts to guess the corresponding
morphetic pitch number, given the key. |#

(defun guess-morphetic
       (y &optional (fifths-step-mode '(0 0))
        (trans
         (assoc
          fifths-step-mode
          #| In this list, first number is steps on
             the cycle of fifths. Second number is the
             interval in semitones for transposing key
             to C major. Third numer is the number of
             steps on the staff. |#
          '(; Major keys.
            ((-6 0) 6 4) ((-5 0) -1 -1) ((-4 0) 4 2)
            ((-3 0) -3 -2) ((-2 0) 2 1) ((-1 0) -5 -3)
            ((0 0) 0 0) ((1 0) 5 3) ((2 0) -2 -1)
            ((3 0) 3 2) ((4 0) -4 -2) ((5 0) 1 1)
            ((6 0) -6 -4)
            ; Minor keys.
            ((-3 5) 6 4) ((-2 5) -1 -1) ((-1 5) 4 2)
            ((0 5) -3 -2) ((1 5) 2 1) ((2 5) -5 -3)
            ((3 5) 0 0) ((4 5) 5 3) ((5 5) -2 -1)
            ((6 5) 3 2) ((-6 5) 3 2) ((7 5) -4 -2)
            ((-5 5) -4 -2) ((8 5) 1 1) ((-4 5) 1 1)
            ((9 5) -6 -4)) :test #'equalp))
        (z (+ y (second trans)))
        (w (guess-morphetic-in-C-major z)))
  (- w (third trans)))

#|
\noindent Example:
\begin{verbatim}
(guess-morphetic-in-C-major 68)
--> 65.
\end{verbatim}

\noindent This function takes a MIDI note number as
its only argument. It attempts to guess the
corresponding morphetic pitch number, assuming a key
of or close to C major. |#

(defun guess-morphetic-in-C-major
       (y &optional
        (octave (floor (- (/ y 12) 1)))
        (MIDI-residue (- y (* 12 (+ octave 1))))
        (morphetic-residue
         (second
	  (assoc
	   MIDI-residue
	   '((0 0) (1 0) (2 1) (3 2) (4 2) (5 3) (6 3)
             (7 4) (8 4) (9 5) (10 6) (11 6))
           :test #'equalp))))
  (+ morphetic-residue (+ (* 7 octave) 32)))

#| Example:
(index-of-1st-tie
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)))
gives
0.

This function returns the index of the first element
of a list of lists whose last value (indicating a tie)
is T. |#

(defun index-of-1st-tie
       (chunked-dataset &optional (index 0))
  (if (null chunked-dataset) ()
    (if (my-last (first chunked-dataset))
      (identity index)
      (index-of-1st-tie
       (rest chunked-dataset) (+ index 1)))))

#| Example:
(indices-of-ties
 '((4 62 61 1 0 T) (5 62 61 1 0 T)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)
   (6 62 61 1/4 0 NIL)) 0)
gives
(1 8).

This function returns the indices of elements that
have the same MIDI-morphetic pairs as the element
indicated by the second argument, so long as these
elements continue to be tied.

1 July 2010. I'm not sure this function is robust
enough. Try replacing the last MNN by 63 and it falls
apart. |#

(defun indices-of-ties
       (chunked-dataset index &optional
	(j (+ index 1))
	(MIDI-note-number
	 (second (nth index chunked-dataset)))
	(morphetic-pitch-number
	 (third (nth index chunked-dataset)))
	(indices nil))
  (if (and
       (equalp
	(second (nth j chunked-dataset))
	MIDI-note-number)
       (equalp
	(third (nth j chunked-dataset))
	morphetic-pitch-number))
    (if (sixth (nth j chunked-dataset))
      (indices-of-ties
       chunked-dataset index (+ j 1)
       MIDI-note-number morphetic-pitch-number
       (append indices (list j)))
      (append indices (list j)))
    (indices-of-ties
     chunked-dataset index (+ j 1)
     MIDI-note-number morphetic-pitch-number
     indices)))

#| Example:
(MIDI-morphetic-pair2pitch&octave '(70 65))
gives
"A#4".

This function returns the pitch and octave of an input
MIDI note number and morphetic pitch number. |#

(defun MIDI-morphetic-pair2pitch&octave
       (MIDI-morphetic-pair &optional
	(octave
	 (floor
	  (/ (- (second MIDI-morphetic-pair) 32) 7)))
	(MIDI-residue
	 (- (first MIDI-morphetic-pair)
	    (* 12 (+ octave 1))))
	(morphetic-residue
	 (- (second MIDI-morphetic-pair)
	    (+ (* 7 octave) 32)))
	(pitch
	 (second
	  (assoc
	   (list MIDI-residue morphetic-residue)
	   '(((12 6) "B#") ((0 0) "C") ((0 1) "Dbb")
	     ((13 6) "B##") ((1 0) "C#") ((1 1) "Db")
	     ((2 0) "C##") ((2 1) "D") ((2 2) "Ebb" )
	     ((3 1) "D#") ((3 2) "Eb") ((4 1) "D##")
	     ((4 2) "E") ((3 3) "Fbb") ((5 2) "E#")
	     ((5 3) "F") ((5 4) "Gbb") ((6 2) "E##")
	     ((6 3) "F#") ((6 4) "Gb") ((7 3) "F##")
	     ((7 4) "G") ((7 5) "Abb") ((8 4) "G#")
	     ((8 5) "Ab") ((9 4) "G##") ((9 5) "A")
	     ((9 6) "Bbb") ((-2 0) "Cbb")
	     ((10 5) "A#") ((10 6) "Bb")
	     ((11 5) "A##") ((11 6) "B")
	     ((-1 0) "Cb")) :test #'equalp))))
  (if pitch
    (concatenate
     'string
     pitch
     (write-to-string octave))))

#| Example:
(modify-to-check-dataset
 '((0 50 54 5/4 1) (5/4 52 55 1/8 1)
   (11/8 54 56 1/8 1) (3/2 55 57 1/8 1)
   (13/8 57 58 1/8 1) (7/4 59 59 1/8 1))) 
gives
((0 50 1250 1 90) (1250 52 125 1 90)
 (1375 54 125 1 90) (1500 55 125 1 90)
 (1625 57 125 1 90) (1750 59 125 1 90)).

This function converts standard vector representation
to events for saving as a MIDI file. Channel can be
set to a default value of 1 (piano sound) or channel
values can be maintained from the input variable. |#

(defun modify-to-check-dataset
       (dataset &optional
        (scale 1000) (channel 1) (velocity 90)
        (output
         (if (and
              (stringp channel)
              (string= channel "use channel"))
           (mapcar
            #'(lambda (x)
                (list
                 (* scale (first x)) (second x)
                 (* scale (fourth x))
                 (+ (fifth x) 1)))
            dataset)
           (mapcar
            #'(lambda (x)
                (list
                 (* scale (first x)) (second x)
                 (* scale (fourth x))
                 channel))
            dataset))))
  (if (and
       (stringp velocity)
       (string= velocity "use velocity"))
    (mapcar
     #'(lambda (x y)
         (append x (list (sixth y))))
     output dataset)
    (mapcar
     #'(lambda (x)
         (append x (list velocity)))
     output)))

#| Deprecated 18/2/2016, so that velocities could be
used if specified.
(defun modify-to-check-dataset
       (dataset &optional
        (scale 1000) (channel 1) (velocity 90))
  (if (and
       (stringp channel)
       (string= channel "use channel"))
    (mapcar
     #'(lambda (x)
         (list
          (* scale (first x)) (second x)
          (* scale (fourth x)) (+ (fifth x) 1)
          velocity))
     dataset)
    (mapcar
     #'(lambda (x)
         (list
          (* scale (first x)) (second x)
          (* scale (fourth x)) channel velocity))
     dataset)))
|#

#| Example:
(pitch&octave2MIDI-morphetic-pair "A#4")
gives
(70 65).

This function returns the MIDI note number and
morphetic pitch number of an input pitch and
octave.

11/6/2015. Made this more robust, so that it will
return nil rather than an error if it receives empty
strings "" or non-pitch-and-octave strings. |#

(defun pitch&octave2MIDI-morphetic-pair
       (pitch&octave &optional
	(length-arg (length pitch&octave))
	(pitch
         (if (> length-arg 0)
           (subseq
            pitch&octave
            0 (- length-arg 1))))
	(octave
         (if (> length-arg 0)
           (parse-integer
            (subseq
             pitch&octave
             (- length-arg 1)) :junk-allowed t)))
	(pair
         (if (and pitch octave)
           (second
            (assoc
             pitch
             '(
               ("B#" (12 6)) ("C" (0 0)) ("Dbb" (0 1))
              ("B##" (13 6)) ("C#" (1 0)) ("Db" (1 1))
               ("C##" (2 0)) ("D" (2 1)) ("Ebb" (2 2))
               ("D#" (3 1)) ("Eb" (3 2)) ("Fbb" (3 3))
               ("D##" (4 1)) ("E" (4 2)) ("Fb" (4 3))
               ("E#" (5 2)) ("F" (5 3)) ("Gbb" (5 4))
               ("E##" (6 2)) ("F#" (6 3)) ("Gb" (6 4))
               ("F##" (7 3)) ("G" (7 4)) ("Abb" (7 5))
               ("G#" (8 4)) ("Ab" (8 5))
               ("G##" (9 4)) ("A" (9 5)) ("Bbb" (9 6))
               ("Cbb" (-2 0)) ("A#" (10 5))
               ("Bb" (10 6))
               ("A##" (11 5)) ("B" (11 6))
               ("Cb" (-1 0))) :test #'string=)))))
  (if pair
    (list
     (+ (* 12 (+ octave 1)) (first pair))
     (+ (+ (* 7 octave) 32) (second pair)))))

#| Example:
(resolve-tie
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)))
gives
((4 62 61 5/4 0 NIL) (21/4 64 62 1/8 0 NIL)
 (43/8 66 63 1/8 0 NIL) (11/2 67 64 1/8 0 NIL)
 (45/8 69 65 1/8 0 NIL) (23/4 71 66 1/8 0 NIL)
 (47/8 73 67 1/8 0 NIL)).

This function locates notes relevant to a tie, creates
a single appropriately defined note, and removes the
redundant notes. |#

(defun resolve-tie
       (chunked-dataset &optional
	(index
	 (index-of-1st-tie chunked-dataset))
	(tied-forward
	 (if index
	   (nth index chunked-dataset)))
	(relevant-indices
	 (if index
	   (cons
	    index
	    (indices-of-ties
	     chunked-dataset index))))
	(tied-back
	 (if index
	   (nth (my-last relevant-indices)
		chunked-dataset))))
  (if index
    (cons
     (append
      (firstn 3 tied-forward)
      (list
       (- (+ (first tied-back) (fourth tied-back))
	  (first tied-forward)))
      (list (fifth tied-forward) nil))
     (remove-nth-list
      relevant-indices chunked-dataset
      (reverse relevant-indices)))
    (identity "all ties resolved")))

#| Example:
(resolve-ties
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (5 74 68 1 0 T)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)
   (6 74 68 1 0 T) (7 74 68 1/4 0 NIL)))
gives
((4 62 61 5/4 0) (5 74 68 9/4 0) (21/4 64 62 1/8 0)
 (43/8 66 63 1/8 0) (11/2 67 64 1/8 0)
 (45/8 69 65 1/8 0) (23/4 71 66 1/8 0)
 (47/8 73 67 1/8 0)).

This function applies the function resolve-tie
recursively until all ties have been resolved. At
this point the input dataset is projected to remove
the tie dimension. |#

(defun resolve-ties 
       (dataset &optional
	(unchunking-dataset
	 (resolve-tie dataset)))
  (if (stringp unchunking-dataset)
    (orthogonal-projection-unique-equalp
     dataset '(1 1 1 1 1 0))
    (resolve-ties
     unchunking-dataset)))
	
#| Old lists.
'(
  ("B#" (12 6))
  ("C" (0 0))
  ("Dbb" (0 1))
  ("B##" (13 6))
  ("C#" (1 0))
  ("Db" (1 1))
  ("C##" (2 0))
  ("D" (2 1))
  ("Ebb" (2 2))
  ("D#" (3 1))
  ("Eb" (3 2))
  ("D##" (4 1))
  ("E" (4 2))
  ("Fbb" (3 3))
  ("E#" (5 2))
  ("F" (5 3))
  ("Gbb" (5 4))
  ("E##" (6 2))
  ("F#" (6 3))
  ("Gb" (6 4))
  ("F##" (7 3))
  ("G" (7 4))
  ("Abb" (7 5))
  ("G#" (8 4))
  ("Ab" (8 5))
  ("G##" (9 4))
  ("A" (9 5))
  ("Bbb" (9 6))
  ("Cbb" (-2 0))
  ("A#" (10 5))
  ("Bb" (10 6))
  ("A##" (11 5))
  ("B" (11 6))
  ("Cb" (-1 0))
  )

'(
  ((12 6) "B#")
  ((0 0) "C")
  ((0 1) "Dbb")
  ((13 6) "B##")
  ((1 0) "C#")
  ((1 1) "Db")
  ((2 0) "C##")
  ((2 1) "D")
  ((2 2) "Ebb" )
  ((3 1) "D#")
  ((3 2) "Eb")
  ((4 1) "D##")
  ((4 2) "E")
  ((3 3) "Fbb")
  ((5 2) "E#")
  ((5 3) "F")
  ((5 4) "Gbb")
  ((6 2) "E##")
  ((6 3) "F#")
  ((6 4) "Gb")
  ((7 3) "F##")
  ((7 4) "G")
  ((7 5) "Abb")
  ((8 4) "G#")
  ((8 5) "Ab")
  ((9 4) "G##")
  ((9 5) "A")
  ((9 6) "Bbb")
  ((-2 0) "Cbb")
  ((10 5) "A#")
  ((10 6) "Bb")
  ((11 5) "A##")
  ((11 6) "B")
  ((-1 0) "Cb")
  )
|#