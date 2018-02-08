#| Copyright 2008-2014 Tom Collins
   Monday 16 June 2014
   Incomplete

\noindent The functions below will parse a kern file
(http://kern.ccarh.org/) by column and convert it to a
list representation in which notes appear as points
in pitch-time space, and performance directions such
as articulation, dynamic markings, and lyrics appear
as sublists of strings in later elements. The main
function is
\nameref{fun:kern-file2points-artic-dynam-lyrics}.
The functions were coded hastily and require further
testing.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :directory '(:relative "Maths foundation")
   :name "list-processing"
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
   :name "kern-by-col"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "set-operations"
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
|#

#|
\noindent Example:
\begin{verbatim}
(setq a-string "(''^")
(articulation-string2list a-string)
--> ("(" "''" "^")
\end{verbatim}

\noindent This function splits up a string of
concatenated articulation markings into a list of
articulation markings, taking care over elements such
as marcato markings ($^{\prime\prime}$). |#

(defun articulation-string2list
       (a-string &optional
        (marcato-tf
         (and
          (> (length a-string) 1)
          (string= (subseq a-string 0 1) "'")
          (string= (subseq a-string 1 2) "'"))))
  (if (equalp (length a-string) 0) ()
    (cons
     (if marcato-tf
       "''" (subseq a-string 0 1))
     (articulation-string2list
      (if marcato-tf
        (subseq a-string 2) (subseq a-string 1))))))

#|
\noindent Example:
\begin{verbatim}
(dynamics-string2list "p")
--> ("p")
(dynamics-string2list "pp<")
--> ("pp" "<")
\end{verbatim}

\noindent This function splits up a string of
concatenated dynamic markings into a list of dynamic
markings, taking care over elements such as pianissimo
markings ($pp$). |#

(defun dynamics-string2list
       (a-string &optional
        (permitted-markings
         (list
          "ppp" "pp" "sp" "p" "mp" "m" "mf" "sfz" "sf"
          "f" "ff" "fff" "<" ">"))
        (growing-list nil)
        (hit
         (search
          (first permitted-markings) a-string)))
  (if (or
       (equalp (length a-string) 0)
       (null permitted-markings))
    (identity growing-list)
    (dynamics-string2list
     (if hit
       (replace-all
        a-string (first permitted-markings) "")
       a-string)
     (rest permitted-markings)
     (if hit
       (append
        growing-list
        (list (first permitted-markings)))
       growing-list))))

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Kern")
   :name "C-6-1-ed" :type "txt")
  *MCStylistic-MonthYear-data-path*))
(firstn
 10 (kern-file2points-artic-dynam-lyrics path&name))
--> ((0 66 63 4/3 0 ("(" "^") ("p") NIL)
     (1 37 46 1 1 NIL NIL NIL)
     (4/3 68 64 1/3 0 NIL NIL NIL)
     (5/3 66 63 1/3 0 NIL NIL NIL)
     (2 49 53 1 1 NIL ("<") NIL)
     (2 56 57 1 1 NIL ("<") NIL)
     (2 59 59 1 1 NIL ("<") NIL)
     (2 65 62 1/2 0 NIL ("<") NIL)
     (5/2 66 63 1/2 0 NIL NIL NIL)
     (3 49 53 1 1 NIL NIL NIL))
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
    "C@merata2014" "training_v1")
   :name "f1" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(firstn
 10 (kern-file2points-artic-dynam-lyrics path&name))
--> ((0 46 52 4 3 NIL NIL ("place."))
     (0 58 59 2 2 NIL NIL ("place."))
     (0 65 63 2 1 NIL NIL ("place."))
     (0 70 66 2 0 NIL NIL ("place"))
     (3 58 59 1 2 NIL NIL ("With-"))
     (3 70 66 1 1 NIL NIL ("With-"))
     (3 74 68 1 0 NIL NIL ("With-"))
     (4 65 63 1 2 NIL NIL ("-in"))
     (4 69 65 1 1 NIL NIL ("-in"))
     (4 72 67 1 0 NIL NIL ("-in")))
\end{verbatim}

\noindent This function is similar to
\nameref{fun:kern-file2dataset-by-col}. As well as
converting a kern file to a point set, it includes
articulation, dynamics, and lyrics information with
each note/point to which they apply.

19/1/2015. Introduced a check to avoid trying to parse
root and harm spines in kern files with this
function. |#

(defun kern-file2points-artic-dynam-lyrics
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows))
        (staves-variable
         (staves-info2staves-variable-robust
          kern-rows))
        ; Determine if anacrusis present.
        (anacrusis
         (kern-anacrusis-correction
          path&name kern-rows kern-rows-sep
          (first staves-variable)))
        ; Determine if dynamics/lyrics present.
        (dynamics-idx
         (kern-file2dynamics-tf
          path&name kern-rows kern-rows-sep))
        (lyrics-idx
         (kern-file2lyrics-tf
          path&name kern-rows kern-rows-sep))
        (kern-rows
         (subseq
          kern-rows (second staves-variable)))
        (staves-variable (first staves-variable))
        (ontimes (list 0)) (i-stave 0)
        (n-stave (length staves-variable))
        (point-set nil)
        #| Test for presence of keyboard spines,
        which look like "**kern" "**kern" "**dynam",
        with dynamics applying to both hands. |#
        (key-spines-previous nil)
        (key-spines
         (and
          (< (+ i-stave 2) n-stave)
          (not
           (integerp
            (first
             (nth (+ i-stave 2) staves-variable))))
          dynamics-idx
          (test-all-true
           (mapcar
            #'(lambda (x y)
                (string=
                 (nth
                  (+ i-stave x)
                  (nth
                   dynamics-idx kern-rows-sep)) y))
            (list 0 1 2)
            (list "**kern" "**kern" "**dynam")))))
        #| Test for presence of a dynamics spine,
        which in non-keyboard case looks like
        "**kern" "**dynam". |#
        (dynam-spine
         (if dynamics-idx
           (string=
            (nth
             (+ i-stave 1)
             (nth dynamics-idx kern-rows-sep))
            "**dynam")))
        #| Test for presence of a lyrics spine,
        which looks like "**kern" "**text" or
        "**kern" "**dynam **text" depending on the
        presence of a dynamics spine. |#
        (lyrics-spine
         (if lyrics-idx
           (string=
            (nth
             (+
              i-stave
              (if dynam-spine 2 1))
             (nth lyrics-idx kern-rows-sep))
            "**text"))))
  (if (>= i-stave n-stave)
    ; Output point set, correcting for an anacrusis.
    (if (> anacrusis 0)
      (mapcar
       #'(lambda (x)
           (cons
            (- (first x) anacrusis) (rest x)))
       (sort-dataset-asc
        point-set #'vector<vector-realp))
      (sort-dataset-asc
       point-set #'vector<vector-realp))
    (kern-file2points-artic-dynam-lyrics
     path&name nil kern-rows-sep nil anacrusis
     dynamics-idx lyrics-idx kern-rows staves-variable
     ontimes
     #| Increment the i-stave variable according to
     whether the current spine title segment looks
     like "**kern", "**kern **dynam", "**kern **text",
     "**kern **dynam **text". |#
     (if key-spines
       (+ i-stave 1)
       (if (and dynam-spine lyrics-spine)
         (+ i-stave 3)
         (if (or
              key-spines-previous dynam-spine
              lyrics-spine)
           (+ i-stave 2) (+ i-stave 1))))
     n-stave
     (if (or
          (string=
           (nth
            i-stave
            (tab-separated-string2list
             (first kern-rows))) "**root")
          (string=
           (nth
            i-stave
            (tab-separated-string2list
             (first kern-rows))) "**harm"))
       point-set
       (append
        point-set
        (kern-cols2points-artic-dynam-lyrics
         ; Note list.
         (kern-rows2col
          kern-rows i-stave staves-variable)
         ; Articulation list.
         (kern-rows2col
          kern-rows i-stave staves-variable
          #'not-articulation-char-p)
         ; Dynamics list.
         (if key-spines
           (kern-rows2col
            kern-rows (+ i-stave 2) staves-variable
            #'not-dynamics-char-p)
           (if (or key-spines-previous dynam-spine)
             (kern-rows2col
              kern-rows (+ i-stave 1) staves-variable
              #'always-nil)))
         ; Lyrics list.
         (if lyrics-spine
           (kern-rows2col
            kern-rows (+ i-stave (if dynam-spine 2 1))
            staves-variable #'always-nil))
         (first (nth i-stave staves-variable))
         ontimes)))
     key-spines)))

#|
\noindent Example:
\begin{verbatim}
(setq
 note-list
 '((("[4f#" "4e#")) NIL (("12f#]")) (("12g#"))
   (("12f#")) (("8e#")) (("8f#")) (("8.g#")) (("16d"))
   NIL (("")) (("8c#") ("4r"))))
(setq
 artic-list
 '((("^" "(^")) NIL (("")) ((""))
   (("")) (("")) (("")) (("")) ((""))
   NIL (("^")) ((")") (""))))
(setq
 dynam-list
 '((("p")) NIL (("")) ((""))
   (("")) (("<")) (("")) (("")) ((""))
   NIL (("")) ((""))))
(setq lyrics-list nil)
(kern-cols2points-artic-dynam-lyrics
 note-list artic-list dynam-list lyrics-list 0 '(0))
--> ((0 65 62 1 0      ("(" "^") ("p") NIL)
     (0 66 63 4/3 0    ("^")     ("p") NIL)
     (4/3 68 64 1/3 0  NIL       NIL   NIL)
     (5/3 66 63 1/3 0  NIL       NIL   NIL)
     (2 65 62 1/2 0    NIL       ("<") NIL)
     (5/2 66 63 1/2 0  NIL       NIL   NIL)
     (3 68 64 3/4 0    NIL       NIL   NIL)
     (15/4 62 61 1/4 0 NIL       NIL   NIL)
     (4 61 60 1/2 0    (")")     NIL   NIL))
\end{verbatim}

\noindent This function combines a column of notes
from a kern file with corresponding columns of
articulation marks, dynamics, and lyrics. It is
called by the function
\nameref{fun:kern-file2points-artic-dynam-lyrics} and
performs a similar role to the function
\nameref{fun:kern-col2dataset} in the function
\nameref{fun:kern-file2dataset-by-col}. |#

(defun kern-cols2points-artic-dynam-lyrics
       (note-list artic-list dynam-list lyrics-list
        staff-index ontimes &optional
        (point-set nil) (tied-points nil)
        (an-item (first note-list))
        (n (length an-item))
        (ontimes
         (if (and
              an-item
              (not (equalp (length ontimes) n)))
           (constant-vector (first ontimes) n)
           (identity ontimes)))
        (results
         (if note-list
           (mapcar
             #'(lambda (x1 x2 x3 x4 x5)
                 (parse-kern-notes-artic-dynam-lyrics
                  x1 x2 x3 x4 staff-index x5 0))
             (first note-list)
             (if (null (first artic-list))
               (list nil)
               (first artic-list))
             (if (null (first dynam-list))
               (list nil)
               (first dynam-list))
             (if (null (first lyrics-list))
               (list nil)
               (first lyrics-list))
             ontimes))))
  (if (null note-list)
    (sort-dataset-asc
     (resolve-ties-kern tied-points point-set))
    (kern-cols2points-artic-dynam-lyrics
     (rest note-list) (rest artic-list)
     (rest dynam-list) (rest lyrics-list)
     staff-index
     (if results
       (mapcar
        #'(lambda (x y)
            (+ x (first y))) ontimes results)
       (identity ontimes))
     (append
      point-set
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 8)) results)))
     (append
      tied-points
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 9)) results))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Kern")
   :name "C-6-1-ed" :type "txt")
  *MCStylistic-MonthYear-data-path*))
(kern-file2dynamics-tf path&name)
--> 9
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
    "C@merata2014" "training_v1")
   :name "f7" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(kern-file2dynamics-tf path&name)
--> NIL
\end{verbatim}

\noindent This function determines whether the string
"**dynam" appears in the kern file for a piece of
music. If yes the function returns the index of the
kern row where this string appears, and NIL
otherwise. |#

(defun kern-file2dynamics-tf
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows)))
  ; Find row beginning **kern.
  (first
   (loop for i from 0
     to (- (length kern-rows-sep) 1)
     when
     (numberp
      (position
       "**dynam" (nth i kern-rows-sep)
       :test #'string=)) collect i)))

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "C@merata2014" "training_v1")
   :name "f6" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(kern-file2lyrics-tf path&name)
--> 0
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "C@merata2014" "training_v1")
   :name "f7" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(kern-file2lyrics-tf path&name)
--> NIL
\end{verbatim}

\noindent This function determines whether the string
"**text" appears in the kern file for a piece of
music. If yes the function returns the index of the
kern row where this string appears, and NIL
otherwise. |#

(defun kern-file2lyrics-tf
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows)))
  ; Find row beginning **kern.
  (first
   (loop for i from 0
     to (- (length kern-rows-sep) 1)
     when
     (numberp
      (position
       "**text" (nth i kern-rows-sep)
       :test #'string=)) collect i)))

#|
\noindent Example:
\begin{verbatim}
(parse-kern-notes-artic-dynam-lyrics
 '("[4f#" "4e#") '("^" "(^") '("p") '("-fraid" "laid")
 0 3 0)
--> (1
     ((3 66 63 1 0 ("^") ("p") ("-fraid" "laid") "[")
      (3 65 62 1 0 ("(" "^") ("p") ("-fraid" "laid")))
\end{verbatim}

\noindent This function converts a kern row consisting
of spaced notes into a list of points, and also
returns the minimum duration of those notes. It is
assumed that any irrelevant symbols have already
been removed via the code
\begin{verbatim}
(remove-if #'not-tie-dur-pitch-char-p *kern-note*)
\end{verbatim}
Non-notes/rests should then result in '(0 NIL) being
returned. A lone crotchet rest should result in
'(1 NIL) being returned, etc. |#

(defun parse-kern-notes-artic-dynam-lyrics
       (note-list artic-list dynam-list lyrics-list
        staff-index ontime minimum-duration
        &optional
        (points nil)
        (dynam-items
         (dynamics-string2list (first dynam-list)))
        (half-data
         (if note-list
           (kern-tie-dur-pitch2list
            (first note-list))))
        (MIDI-morphetic-pair
         (if half-data
           (if (and
                (not
                 (string= (first half-data) "rest"))
                (not (string= (first half-data) "["))
                (not (string= (first half-data) "]")))
             (pitch&octave2MIDI-morphetic-pair
              (first half-data)))))
        (artic-items
         (articulation-string2list
          (first artic-list))))
  (if (null note-list)
    (list minimum-duration points)
    (if (null half-data)
      (parse-kern-notes-artic-dynam-lyrics
       (rest note-list) (rest artic-list) dynam-list
       lyrics-list staff-index ontime
       minimum-duration points dynam-items)
      (if (null MIDI-morphetic-pair)
        (parse-kern-notes-artic-dynam-lyrics
         (rest note-list) (rest artic-list) nil
         lyrics-list staff-index ontime
         (if (second half-data)
           (if (zerop minimum-duration)
             (second half-data)
             (min
              minimum-duration (second half-data)))
           minimum-duration)
         points dynam-items)
        (parse-kern-notes-artic-dynam-lyrics
         (rest note-list) (rest artic-list) nil
         lyrics-list staff-index ontime
         (if (zerop minimum-duration)
           (second half-data)
           (min minimum-duration (second half-data)))
         (if (equalp (length half-data) 3)
           (append
            points
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index
              artic-items dynam-items lyrics-list
              (third half-data))))
           (append
            points
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index
              artic-items dynam-items lyrics-list))))
         dynam-items)))))




