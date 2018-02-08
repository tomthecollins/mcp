#| Copyright 2008-2014 Tom Collins
   Friday 9 May 2014
   Incomplete

\noindent The functions below will parse a kern
file (http://kern.ccarh.org/) and identify various
aspects of it. For instance,
\nameref{fun:kern-file2ontimes-signatures} will
identify all the time signature changes in a kern
file and convert them to a list of bar numbers where
they occur, and what they consist of.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Kern")
   :name "C-41-1-ed" :type "txt")
  *MCStylistic-MonthYear-data-path*))
(firstn 5 (kern2clef-changes path&name))
--> ((0 "clefF4" 1) (0 "clefG2" 0) (121 "clefG2" 1)
     (126 "clefF4" 1) (127 "clefG2" 1)
     (132 "clefF4" 1) (133 "clefG2" 1))
\end{verbatim}

\noindent This function parses a kern file and returns
a list consisting of triples: the first element in each
triple is ontime; the second is a string specifying
the clef change that occurs at that point in time;
the third is the staff number to which the clef
change belongs. |#

(defun kern2clef-changes
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (staves-variable
         (staves-info2staves-variable-robust
          kern-rows))
        (kern-rows
         (subseq
          kern-rows (second staves-variable)))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows))
        (staves-variable (first staves-variable))
        ; Determine if anacrusis present.
        (anacrusis
         (kern-anacrusis-correction
          path&name kern-rows kern-rows-sep
          staves-variable))
        (ontimes (list 0)) (i-stave 0)
        (n-stave (length staves-variable))
        (dataset nil))
  (if (>= i-stave n-stave)
    (if (> anacrusis 0)
      (mapcar
       #'(lambda (x)
           (cons
            (- (first x) anacrusis) (rest x)))
       (sort-dataset-asc
        dataset #'vector<vector-realp))
      (sort-dataset-asc
       dataset #'vector<vector-realp))
    (kern2clef-changes
     path&name nil nil kern-rows nil
     staves-variable anacrusis ontimes (+ i-stave 1)
     n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (kern-col2staff-changes
         (kern-rows2col-preserving-clefs
          kern-rows i-stave staves-variable)
         (first (nth i-stave staves-variable))
         ontimes))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 a-list
 '(NIL NIL NIL (("*clefG2")) NIL NIL (("16r"))
   (("16B" "16c")) (("16d")) (("16e")) (("*clefGv2"))
   (("16f")) (("16d")) (("16e")) (("16c"))))
(kern-col2staff-changes a-list 0 (list 0))
--> ((0 "clefG2") (1 "clefGv2"))
\end{verbatim}

\noindent This function plays a similar role as the
function \nameref{fun:kern-col2dataset} in the
function \nameref{fun:kern-file2dataset-by-col}. It
keeps a running total of ontime in the spine of a
kern file, and uses this to populate any encountered
clef changes with the appropriate ontime where the
clef change first takes effect. |#

(defun kern-col2staff-changes
       (a-list staff-index ontimes &optional
        (clef-set nil)
        (an-item (first a-list))
        (n (length an-item))
        (ontimes
         (if (and
              an-item
              (not (equalp (length ontimes) n)))
           (constant-vector (first ontimes) n)
           (identity ontimes)))
        (clef-change
         (if (search "clef" (first (first an-item)))
           (list
            (first ontimes)
            (replace-all
             (first (first an-item)) "*" "")
            staff-index)))
        (results
         (if (and (not clef-change) a-list)
           (mapcar
             #'(lambda (x y)
                 (parse-kern-spaced-notes
                  x staff-index y 0))
             (first a-list) ontimes))))
  (if (null a-list)
    (identity clef-set)
    (kern-col2staff-changes
     (rest a-list) staff-index
     (if results
       (mapcar
        #'(lambda (x y)
            (+ x (first y))) ontimes results)
       (identity ontimes))
     (if clef-change
       (append
        clef-set
        (list clef-change))
       clef-set))))

#|
\noindent Example:
\begin{verbatim}
(setq
 kern-rows-sep
 '(("**kern" "**kern" "**dynam" "**kern")
   ("*staff3" "*staff2" "*staff2" "*staff1")
   ("*clefF4" "*clefG2" "*clefG2" "*clefG2")
   ("*k[f#c#g#]" "*k[f#c#g#]" "*k[f#c#g#]"
    "*k[f#c#g#]")
   ("*M6/8" "*M6/8" "*M6/8" "*M6/8")
   ("8r" "8a/" "f" "8r")
   ("=1" "=1" "=1" "=1")
   ("2.r" "4.dd\\" "." "2.r")
   ("." "4.d/" "." ".")
   ("=2" "=2" "=2" "=2")
   ("2.r" "4.g#/" "." "2.r")
   ("." "4r" "." ".")
   ("." "8g#/" "." ".")
   ("=3" "=3" "=3" "=3")
   ("*M4/4" "*M4/4" "*M4/4" "*M4/4")
   ("4BB\\ 4c#\\" "4a/" "." "4g#\\ 4ff#\\")
   ("4Cn\\ 4B-\\" "4a/" "." "4fn/ 4dd/")
   ("2BB\\ 2cn\\" "2a/" "." "2gn\\ 2ffn\\")
   ("=4" "=4" "=4" "=4")
   ("1r" "1r" "." "1r")
   ("==" "==" "==" "==")
   ("*-" "*-" "*-" "*-")))
(kern-file2ontimes-signatures "blah" kern-rows-sep)
--> ((1 6 8) (3 4 4)).
\end{verbatim}

\noindent This function parses a kern file looking
for lines beginning with ``*M'', which denote
changes in time signature. Apart from the first such
instance, which it is assumed belongs to bar 1 of a
piece, it then looks immediately before these lines
to parse the bar numbers at which time signature
changes occur. Call the function
append-ontimes-to-time-signatures afterwards to
include ontimes as well. |#

(defun kern-file2ontimes-signatures
       (path&name &optional
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          (read-from-file-arbitrary path&name)))
        #| Return index of each row containing *M,
        and the time signature specified. |#
        (time-sigs&idx
         (kern-rows-sep2time-sigs&idx
          kern-rows-sep)))
  (cons
   #| By definition, first entry in time-sigs&idx
   must be for bar 1. |#
   (list
    1 (first (first (first time-sigs&idx)))
    (second (first (first time-sigs&idx))))
   (mapcar
    #'(lambda (x)
        (list
         (if (string=
              (subseq
               (first
                (nth
                 (- (second x) 1) kern-rows-sep))
               0 1) "=")
           (parse-integer
            (subseq
             (first
              (nth
               (- (second x) 1) kern-rows-sep)) 1)
            :junk-allowed T)
           "Error: could not find bar number.")
         (first (first x)) (second (first x))))
    (rest time-sigs&idx))))

#|
\noindent Example:
\begin{verbatim}
(setq
 rows
 '("**kern	**kern"
   "*staff2	*staff1"
   "=1-	=1-"
   "*clefF4	*clefG2"
   "*k[]	*k[]"
   "*M4/4	*M4/4"
   "2r	16r"
   ".	16B/LL 16c/LL"
   ".	16d/"
   ".	16e/JJ"
   "*	*clefGv2"
   ".	16f/LL"
   ".	16d/"
   ".	16e/"
   ".	16c/JJ"))
(kern-rows2col-preserving-clefs rows 1 '((2 1) (1 1)))
--> (NIL NIL NIL (("*clefG2")) NIL NIL (("16r"))
     (("16B" "16c")) (("16d")) (("16e"))
     (("*clefGv2")) (("16f")) (("16d")) (("16e"))
     (("16c")))
\end{verbatim}

\noindent This function plays a similar role as the
function \nameref{fun:kern-rows2col} in the function
\nameref{fun:kern-file2dataset-by-col}. It keeps
information relating to clefs as well as note
information, and removes everything else. |#

(defun kern-rows2col-preserving-clefs
       (rows i staves-variable &optional
        (fn #'not-tie-dur-pitch-char-p)
        (indices
         (cons
          0
          (fibonacci-list
           (nth-list-of-lists 1 staves-variable))))
        (start-index (nth i indices))
        (end-index (nth (+ i 1) indices))
        (row (first rows))
        (parsed-row
         (if (search "*clef" row)
           (mapcar
            #'(lambda (x)
                (mapcar
                 #'(lambda (y)
                     (remove-if #'always-nil y))
                 (space-bar-separated-string2list x)))
            (tab-separated-string2list row))
           (if (and
                row
                (not (string= (subseq row 0 1) "!"))
                (not (string= (subseq row 0 1) "="))
                (if (string= (subseq row 0 1) "*")
                  (recognised-spine-commandp row) T))
             (mapcar
              #'(lambda (x)
                  (mapcar
                   #'(lambda (y)
                       (remove-if fn y))
                   (space-bar-separated-string2list
                    x)))
              (tab-separated-string2list row)))))
        (result
         (if (<= end-index (length parsed-row)) 
           (subseq
            parsed-row start-index end-index))))
  (if (null row) ()
    (cons
     result
     (kern-rows2col-preserving-clefs
      (rest rows) i
      (update-staves-variable staves-variable row)
      fn))))

#|
\noindent Example:
\begin{verbatim}
(setq
 kern-rows-sep
 '(("*M6/8" "*M6/8" "*M6/8" "*M6/8")
   ("8r" "8a/" "f" "8r")
   ("=1" "=1" "=1" "=1")
   ("2.r" "4.dd\\" "." "2.r")
   ("." "4.d/" "." ".")
   ("=2" "=2" "=2" "=2")
   ("*M4/4" "*M4/4" "*M4/4" "*M4/4")
   ("4BB\\ 4c#\\" "4a/" "." "4g#\\ 4ff#\\")
   ("4Cn\\ 4B-\\" "4a/" "." "4fn/ 4dd/")
   ("2BB\\ 2cn\\" "2a/" "." "2gn\\ 2ffn\\")
   ("==" "==" "==" "==")
   ("*-" "*-" "*-" "*-")))
(kern-rows-sep2time-sigs&idx kern-rows-sep)
--> (((6 8) 0) ((4 4) 6)).
\end{verbatim}

\noindent This function parses rows from a kern file
that have already been converted from tab-separated
text into lists of strings. It looks for lists that
begin with the substring ``*M'', which specifies a
change in time signature, converts the rest of such
strings to the upper and lower number of the
following time signature (for instance ``*M8/8''
maps to 6 and 8), and returns this in a list along
with the index of the row. |#

(defun kern-rows-sep2time-sigs&idx
       (kern-rows-sep &optional (i 0)
        (a-kern-string (first (first kern-rows-sep)))
        (bgn-idx
         (search "*M" a-kern-string :test #'string=))
        (sig-string
         (if bgn-idx
           (subseq a-kern-string (+ bgn-idx 2))))
        (slash-idx
         (if sig-string
           (position
            #\/ sig-string :test #'equalp)))
        (upper-number
         (if slash-idx
           (parse-integer
            (subseq sig-string 0 slash-idx)
            :junk-allowed T)))
        (lower-number
         (if slash-idx
           (parse-integer
            (subseq sig-string (+ slash-idx 1))
            :junk-allowed T))))
  (if (null kern-rows-sep) ()
    (append
     (if slash-idx
       (list
        (list (list upper-number lower-number) i)))
     (kern-rows-sep2time-sigs&idx
      (rest kern-rows-sep) (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(staves-info2staff&clef-names
 '("!!!COM: Chopin, Frederic"
   "**kern	**kern	**dynam"
   "*thru	*thru	*thru"
   "*staff2	*staff1	*staff1/2"
   "*>A	*>A	*>A"
   "*clefF4	*clefG2	*clefG2"))
--> (("piano left hand" "bass clef")
     ("piano right hand" "treble clef"))
(staves-info2staff&clef-names
 '("!!!COM: Beethoven, Ludwig van"
   "!!!CDT: 1770///-1827///"
   "**kern	**dynam"
   "*Ipiano	*Ipiano"
   "*clefG2	*clefG2"
   "*k[b-]	*k[b-]"
   "*F:	*F:" "*M3/4	*M3/4" "*MM40	*MM40"
   "8.c/L	." "16c/Jk	." "=1	=1" "*	*"))
--> (("piano right hand" "treble clef"))
(staves-info2staff&clef-names
 '("!!!AGN:	chorale"
   "**kern	**kern	**kern	**kern"
   "*ICvox	*ICvox	*ICvox	*ICvox"
   "*Ibass	*Itenor	*Ialto	*Isoprn"
   "*I\"Bass	*I\"Tenor	*I\"Alto	*I\"Soprano"
   "*>[A,A,B]	*>[A,A,B]	*>[A,A,B]	*>[A,A,B]"
   "*>norep[A,B]	*>norep[A,B]	*>norep[A,B]	*>norep[A,B]"
   "*>A	*>A	*>A	*>A"
   "*clefF4	*clefGv2	*clefG2	*clefG2"
   "*k[f#]	*k[f#]	*k[f#]	*k[f#]"))
--> (("bass" "bass clef") ("tenor" "tenor clef")
     ("alto" "treble clef") ("soprn" "treble clef"))
(staves-info2staff&clef-names
 '("**kern	**kern"
   "*staff2	*staff1"
   "=1-	=1-"
   "*clefF4	*clefG2"
   "*k[f#c#g#d#]	*k[f#c#g#d#]"
   "*M3/8	*M3/8"))
--> (("piano left hand" "bass clef")
     ("piano right hand" "treble clef"))
(staves-info2staff&clef-names
 '("**kern	**text	**kern	**text	**kern	**text	**kern	**text"
   "*staff4	*staff4	*staff3	*staff3	*staff2	*staff2	*staff1	*staff1"
   "=1-	=1-	=1-	=1-	=1-	=1-	=1-	=1-"
   "*clefF4	*	*clefGv2	*	*clefG2	*	*clefG2	*"
   "*k[b-e-]	*	*k[b-e-]	*	*k[b-e-]	*	*k[b-e-]	*"
   "*M4/4	*	*M4/4	*	*M4/4	*	*M4/4	*"
   "1BB-	place.	2B-\	place.	2f/	place.	2b-\	place"))
--> (("staff4" "bass clef") ("staff3" "tenor clef")
     ("staff2" "treble clef")
     ("staff1" "treble clef"))
\end{verbatim}

\noindent This function parses rows supplied from a
kern file and returns a list of string pairs: the first
element in each pair is a label for a staff in the
score. If none is provided, ``staff$x$'' will appear;
the second element is a label for the clef type with
which the staff begins (e.g., ``bass clef''). This is
useful because often users refer to ``bass clef''
when they mean the lower part of the keyboard staff,
for instance. |#

(defun staves-info2staff&clef-names
       (path&name &optional
        (rows
         (if (listp path&name)
           path&name
           (read-from-file-arbitrary path&name)))
        (row1 (first rows))
        (parsed-row1
         (if row1 (tab-separated-string2list row1)))
        (rel-idx
         (if parsed-row1
           (positions
            "**kern" parsed-row1 #'string=)))
        (staff-idx
         (first
          (loop for i from 1 to 10 when
            (or
             (search "*staff" (nth i rows))
             (and
              (nth i rows)
              (> (length (nth i rows)) 3)
              (string=
               (subseq (nth i rows) 0 2) "*I")
              (not
               (string=
                (subseq (nth i rows) 0 3) "*IC"))))
            collect i)))
        (staff-row
         (if (and rel-idx staff-idx)
           (mapcar
            #'(lambda (x)
                (replace-all
                 (replace-all x "*I" "") "*" ""))
            (nth-list
             rel-idx
             (tab-separated-string2list
              (nth staff-idx rows))))))
        (clef-idx
         (first
          (loop for i from 1 to 10 when
            (search "*clef" (nth i rows))
            collect i)))
        (clef-row
         (if (and rel-idx clef-idx)
           (mapcar
            #'(lambda (x)
                (replace-all x "*" ""))
            (nth-list
             rel-idx
             (tab-separated-string2list
              (nth clef-idx rows)))))))
  (if rel-idx
    (if (or
         (and
          (equalp (length rel-idx) 2)
          (equalp clef-row (list "clefF4" "clefG2")))
         (and
          (equalp staff-row '("piano" "piano"))
          (or
           (equalp clef-row (list "clefG2" "clefG2"))
           (equalp clef-row (list "clefF4" "clefF4")))))
      (list
       (list "left hand" "bass clef")
       (list "right hand" "treble clef"))
      (mapcar
       #'(lambda (x y)
           (list
            (if (and (stringp x) (string= x ""))
              "none" x)
            (second
             (assoc
              y
              (list 
               (list "clefF4" "bass clef")
               (list "clefGv2" "tenor clef")
               (list "clefG2" "treble clef")
               (list "clefC3" "alto clef"))
              :test #'string=))))
       staff-row clef-row))
    (staves-info2staff&clef-names nil (rest rows))))

