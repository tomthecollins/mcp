#| Copyright 2008-2014 Tom Collins
   Saturday 17 March 2012
   Incomplete

The functions below will parse a kern file
(http://kern.ccarh.org/) by column and convert it to a
dataset. The main function is
\nameref{fun:kern-file2dataset-by-col}. Conflicts
between kern's relative encoding and the absolute
parsing (which affected the function
\nameref{fun:kern-file2dataset}) have been resolved.

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
   :directory '(:relative "Markov models")
   :name "generating-with-patterns-preliminaries"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "interpolation"
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
   :directory '(:relative "Maths foundation")
   :name "list-processing"
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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "vector-operations"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(append-list '(("4cc" "4dd") (7.2 -5 6) (".")))
--> ("4cc" "4dd" 7.2 -5 6 ".")
\end{verbatim}

\noindent Removes one structural level from a list. |#

(defun append-list (a-list)
  (if (null a-list) ()
    (append
     (first a-list) (append-list (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 kern-rows
 '("!!!COM: Beethoven, Ludwig van"
   "!!!OPR: Symphony No. 3 in E-flat Major"
   "!!!ONM: Opus 55" "!!!OTL:" "!!!OMV: 1"
   "**kern	**dynam"
   "*I:Corno 1, 2 in Eb	*I:Corno 1, 2 in Eb"
   "*trvc 3 2	*" "*>[A,B,B,C]	*" "*clefG2	*"
   "*k[]	*" "*C:	*" "*M3/4	*"
   "*tb24	*" "=1	=1" "(2d/	pp"
   "*>2nd ending	*"))
(setq staves-variable '(((0 1) (-1/2 1)) 5))
(header2trans-vec kern-rows staves-variable)
--> ((3 2) NIL)
\end{verbatim}

\noindent This function looks for a line in the kern
file containing the string trvc, short for
transposition vector. The format \verb+*trvc 3 2+
means that this instrument sounds three semitones and
two stave steps higher than it is notated. The
corresponding points in the point set could be
translated by this amount, otherwise the MIDI and
point-set encodings will contain unintended
bitonalities, which is problematic for probabilistic
calculations on the notes as heard, and for checking
data by auditioning MIDI files. |#

(defun header2trans-vec
       (kern-rows staves-variable &optional
        (nsearch 50)
        (nspine (length (first staves-variable)))
        (trvc-idx
         (index-item-1st-occurs
          nil
          (mapcar
           #'(lambda (x)
               (not
                (search "trvc" x :test #'string=)))
           (firstn nsearch kern-rows))))
        (trans-row
         (if trvc-idx (nth trvc-idx kern-rows)))
        (trans-row-proc
         (if trans-row
           (mapcar
            #'(lambda (x)
                (mapcar
                 #'(lambda (y)
                     (remove-if
                      #'not-tie-dur-pitch-char-p y))
                 (space-bar-separated-string2list x)))
            (tab-separated-string2list trans-row)))))
  (loop for i from 0 to (- nspine 1) collect
    (if (equalp (length (nth i trans-row-proc)) 3)
      (list
       (parse-integer
        (second (nth i trans-row-proc))
        :junk-allowed t)
       (parse-integer
        (third (nth i trans-row-proc))
        :junk-allowed t)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "chopinMazurkas" "Kern")
   :name "C-6-1-ed" :type "txt")
  *MCStylistic-MonthYear-data-path*))
(kern-anacrusis-correction path&name)
--> 1
\end{verbatim}

\noindent This function begins parsing a kern file up
to bar one (usually indicated by ``=1'' or ``=1-'').
Any notes appearing before bar one are considered to be
an anacrusis. The duration of the anacrusis is
calculated (using the ontime of the first note(s) in
bar one) and returned. Zero is returned otherwise.

On 11 December 2014 I was converting some kern files
that did not contain ``=1'' or ``=1-''; nor did they
contain anacruses. The absence of these strings
caused an error, because \texttt{index-bar1} was null.
To remedy this I put in some extra logic when defining
\texttt{mini-dataset}. |#

(defun kern-anacrusis-correction
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows))
        (staves-variable
         (first
          (staves-info2staves-variable-robust
           kern-rows)))
        (ontime-idx 0)
        (dur-idx 3)
        (index-bar1
         (first
          (loop for i from 0
            to (- (length kern-rows-sep) 1)
            when
            (or
             (numberp
              (position
               "=1" (nth i kern-rows-sep)
               :test #'string=))
             (numberp
              (position
               "=1-" (nth i kern-rows-sep)
               :test #'string=)))
            collect i)))
        (mini-dataset
         (if index-bar1
           (kern-col2dataset-no-tie-resolution
            (kern-rows2col
             (firstn index-bar1 kern-rows) 0
             staves-variable) 0 (list 0))))
        #| If the lowest staff rests during the
        anacrusis, we import this instead. |#
        (mini-dataset
         (if (and index-bar1 (null mini-dataset))
           (kern-col2rest-set
            (kern-rows2col
             (firstn index-bar1 kern-rows) 0
             staves-variable) 0 (list 0))
           mini-dataset)))
  (if mini-dataset
    ; Return the maximal offtime in the anacrusis.
    (max-item
     (mapcar
      #'(lambda (x)
          (+
           (nth ontime-idx x)
           (nth dur-idx x))) mini-dataset)) 0))

#|
\noindent Example:
\begin{verbatim}
(kern-col2dataset
 '((("31")) (("8E")) ((".")) (("8G")) ((".")) (("8F"))
   ((".")) (("8A-")) ((".")) (("8BB")) (("."))
   (("8D")) ((".")) (("8C")) ((".")) (("8E-")) (("."))
   (("32")) (("")) (("2r") ("4DD")) NIL ((".") ("."))
   ((".") (".")) NIL ((".") ("4EE-")) NIL
   ((".") (".")) ((".") (".")) NIL (("12d") ("4FF"))
   (("12A-") (".")) (("12F") (".")) (("12c") ("4FF#"))
   (("12A") (".")) (("12E-") ("."))
   (("33") ("33")) (("12c") ("8GGG")) (("12G") ("."))
   ((".") ("8GG")) (("12E-") (".")) (("12c") ("8GGG"))
   (("12G") (".")) ((".") ("8GG")) (("12E-") ("."))
   (("12B") ("8GGG")) (("12G") (".")) ((".") ("8GG"))
   (("12D") (".")) (("12B") ("8GGG")) (("12G") ("."))
   ((".") ("8GG")) (("12D") (".")))
  0 (list 0))
--> ((0 52 55 1/2 0) (1/2 55 57 1/2 0) (1 53 56 1/2 0)
     (3/2 56 58 1/2 0) (2 47 52 1/2 0)
     (5/2 50 54 1/2 0) (3 48 53 1/2 0)
     (7/2 51 55 1/2 0) (4 38 47 1 0) (5 39 48 1 0)
     (6 41 49 1 0) (6 62 61 1/3 0) (19/3 56 58 1/3 0)
     (20/3 53 56 1/3 0) (7 42 49 1 0) (7 60 60 1/3 0)
     (22/3 57 58 1/3 0) (23/3 51 55 1/3 0)
     (8 31 43 1/2 0) (8 60 60 1/3 0)
     (25/3 55 57 1/3 0) (17/2 43 50 1/2 0)
     (26/3 51 55 1/3 0) (9 31 43 1/2 0)
     (9 60 60 1/3 0) (28/3 55 57 1/3 0)
     (19/2 43 50 1/2 0) (29/3 51 55 1/3 0)
     (10 31 43 1/2 0) (10 59 59 1/3 0)
     (31/3 55 57 1/3 0) (21/2 43 50 1/2 0)
     (32/3 50 54 1/3 0) (11 31 43 1/2 0)
     (11 59 59 1/3 0) (34/3 55 57 1/3 0)
     (23/2 43 50 1/2 0) (35/3 50 54 1/3 0))
\end{verbatim}

\noindent This function converts a kern column
consisting of spaced notes into a dataset, and also
returns the minimum duration of those notes. It is
assumed that any irrelevant symbols have already been
removed via the code
\begin{verbatim}
(remove-if #'not-tie-dur-pitch-char-p *kern-note*)
\end{verbatim}
Non-notes/rests should then result in '(0 NIL) being
returned. A lone crotchet rest should result in
'(1 NIL) being returned, etc. |#

(defun kern-col2dataset
       (a-list staff-index ontimes &optional
        (dataset nil) (tied-datapoints nil)
        (an-item (first a-list)) (n (length an-item))
        (ontimes
         (if (and
              an-item
              (not (equalp (length ontimes) n)))
           (constant-vector (first ontimes) n)
           (identity ontimes)))
        (results
         (if a-list
           (mapcar
             #'(lambda (x y)
                 (parse-kern-spaced-notes
                  x staff-index y 0))
             (first a-list) ontimes))))
  (if (null a-list)
    (sort-dataset-asc
     (resolve-ties-kern tied-datapoints dataset))
    (kern-col2dataset
     (rest a-list) staff-index
     (if results
       (mapcar
        #'(lambda (x y)
            (+ x (first y))) ontimes results)
       (identity ontimes))
     (append
      dataset
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 5)) results)))
     (append
      tied-datapoints
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 6)) results))))))

#|
\noindent Example:
\begin{verbatim}
(kern-col2dataset-no-tie-resolution
 '(NIL (("[4f#"))) 0 (list 0))
--> ((0 66 63 1 0 "["))
\end{verbatim}

\noindent This function is very similar to the function
\nameref{fun:kern-col2dataset}. The difference is that
no attempt is made to resolve ties. This is useful for
calculating the length of any anacrusis: if every note
in the anacrusis is tied into the first full bar of the
piece, then resolving ties can lead to incorrect
calculation of the anacrusis length. |#

(defun kern-col2dataset-no-tie-resolution
       (a-list staff-index ontimes &optional
        (dataset nil) (an-item (first a-list))
        (n (length an-item))
        (ontimes
         (if (and
              an-item
              (not (equalp (length ontimes) n)))
           (constant-vector (first ontimes) n)
           (identity ontimes)))
        (results
         (if a-list
           (mapcar
             #'(lambda (x y)
                 (parse-kern-spaced-notes
                  x staff-index y 0))
             (first a-list) ontimes))))
  (if (null a-list)
    ;(sort-dataset-asc
     (mapcar
      #'(lambda (x)
          (firstn 6 x))
      dataset);)
    (kern-col2dataset-no-tie-resolution
     (rest a-list) staff-index
     (if results
       (mapcar
        #'(lambda (x y)
            (+ x (first y))) ontimes results)
       (identity ontimes))
     (append
      dataset
      (append-list
       (mapcar
        #'(lambda (x)
            (second x)) results))))))

#|
\noindent Example:
\begin{verbatim}
(kern-col2rest-set '((("4r")) (("8r"))) 0 (list 0))
--> ((0 "rest" "rest" 1 0) (1 "rest" "rest" 1/2 0))
\end{verbatim}

\noindent This function is similar to the function
\nameref{fun:kern-col2dataset}. Rather than converting
written notes in a particular voice to points, it
converts written rests in a particular voice to
points. The output is a point set, where each point
consists of an ontime, two `rest' strings
(placeholders for MIDI note and morphetic
pitch numbers), duration, and staff number. |#

(defun kern-col2rest-set
       (a-list staff-index ontimes &optional
        (dataset nil) (tied-datapoints nil)
        (an-item (first a-list)) (n (length an-item))
        (ontimes
         (if (and
              an-item
              (not (equalp (length ontimes) n)))
           (constant-vector (first ontimes) n)
           (identity ontimes)))
        (results
         (if a-list
           (mapcar
             #'(lambda (x y)
                 (parse-kern-spaced-rests
                  x staff-index y 0))
             (first a-list) ontimes))))
  (if (null a-list)
    dataset
    (kern-col2rest-set
     (rest a-list) staff-index
     (if results
       (mapcar
        #'(lambda (x y)
            (+ x (first y))) ontimes results)
       (identity ontimes))
     (append
      dataset
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 5)) results)))
     (append
      tied-datapoints
      (append-list
       (mapcar
        #'(lambda (x)
            (return-lists-of-length-n
             (second x) 6)) results))))))

#|
\noindent Example:
\begin{verbatim}
(kern-file2dataset-by-col
  (merge-pathnames
   (make-pathname
    :name "C-6-1-small" :type "krn")
   *MCStylistic-MonthYear-example-files-data-path*))
--> ((0 66 63 4/3 0) (1 37 46 1 1) (4/3 68 64 1/3 0)
     (5/3 66 63 1/3 0) (2 49 53 1 1) (2 56 57 1 1)
     (2 59 59 1 1) (2 65 62 1/2 0) (5/2 66 63 1/2 0)
     (3 49 53 1 1) (3 53 55 1 1) (3 59 59 1 1)
     (3 68 64 3/4 0) (15/4 62 61 1/4 0) (4 42 49 1 1)
     (4 61 60 1/2 0) (19/4 66 63 1/4 0) (5 54 56 1 1)
     (5 61 60 1 1) (5 69 65 1 0) (6 54 56 1 1)
     (6 61 60 1 1))
\end{verbatim}

\noindent This function is a more robust version of
the function kern-file2dataset. It converts a text
file in the kern format into a dataset, where each
datapoint consists of an ontime, MIDI note number,
morphetic pitch number, duration, and staff number.

It is more robust because kern-file2dataset parsed
a kern score by row only, so sometimes whitespace in a
score was misinterpreted. For example, rows such as
\begin{verbatim}
*staff2	*staff1
4.c	4g
.	4a
8b.	.
\end{verbatim}
would be interpreted as
\begin{verbatim}
((0 60 3/2) (0 67 1) (1 69 1) (2 59 1/2))
\end{verbatim}
rather than
\begin{verbatim}
((0 60 3/2) (0 67 1) (1 69 1) (3/2 59 1/2))
\end{verbatim}
The example at the top of this function's
documentation is a case in point. |#

(defun kern-file2dataset-by-col
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
       (sort-dataset-asc dataset))
      (sort-dataset-asc dataset))
    (kern-file2dataset-by-col
     path&name nil nil kern-rows nil
     staves-variable anacrusis ontimes (+ i-stave 1)
     n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (kern-col2dataset
         (kern-rows2col
          kern-rows i-stave staves-variable)
         (first (nth i-stave staves-variable))
         ontimes))))))

#|
\noindent Example:
\begin{verbatim}
(kern-file2tie-set-by-col
  (merge-pathnames
   (make-pathname
    :name "C-6-1-small" :type "krn")
   *MCStylistic-MonthYear-example-files-data-path*)
  t)
--> ((-1 66 63 1 0 "[") (0 66 63 1/3 0 "][")
     (1/3 66 63 1/3 0 "]") (5 66 63 1 0 "[")
     (5 69 65 1 0 "["))
\end{verbatim}

\noindent This function converts any notes that are
tied in a score into points, using `[' to mean tied
forward, `]' to mean tied back, and `[]' to mean tied
both. |#

(defun kern-file2tie-set-by-col
       (path&name &optional (correct-trans-instr nil)
        (kern-rows
         (read-from-file-arbitrary path&name))
        (staves-variable
         (staves-info2staves-variable-robust
          kern-rows))
        
        (trans-vec
         (if correct-trans-instr
           (header2trans-vec
            kern-rows staves-variable)))
        
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
       (mapcar
        #'(lambda (y)
            (append (first y) (second y)))
        (merge-sort-by-vector<vector-car
         (loop for i from 0 to
           (- (length dataset) 1) when
           (stringp (sixth (nth i dataset)))
           collect
           (list
            (butlast (nth i dataset))
            (last (nth i dataset)))))))
      #| Introduced a fix here because the previous
      method of calling sort-dataset-asc could throw
      errors if points matched up to non-real elements
      such as "[" symbols. |#
      (mapcar
       #'(lambda (x)
           (append (first x) (second x)))
       (merge-sort-by-vector<vector-car
        (loop for i from 0 to
          (- (length dataset) 1) when
          (stringp (sixth (nth i dataset)))
          collect
          (list
           (butlast (nth i dataset))
           (last (nth i dataset)))))))
    (kern-file2tie-set-by-col
     path&name correct-trans-instr nil nil trans-vec
     kern-rows nil staves-variable anacrusis ontimes
     (+ i-stave 1) n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (if (and
             correct-trans-instr
             (nth i-stave trans-vec))
          (mapcar
           #'(lambda (x)
               (append
                (list (first x))
                (list
                 (+
                  (second x)
                  (first (nth i-stave trans-vec))))
                (list
                 (+
                  (third x)
                  (second (nth i-stave trans-vec))))
                (subseq x 3)))
           (kern-col2dataset-no-tie-resolution
            (kern-rows2col
             kern-rows i-stave staves-variable)
            (first (nth i-stave staves-variable))
            ontimes))
          (kern-col2dataset-no-tie-resolution
           (kern-rows2col
            kern-rows i-stave staves-variable)
           (first (nth i-stave staves-variable))
           ontimes)))))))

#| Old version did not handle transposing
instruments.
(defun kern-file2tie-set-by-col
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
       (mapcar
        #'(lambda (y)
            (append (first y) (second y)))
        (merge-sort-by-vector<vector-car
         (loop for i from 0 to
           (- (length dataset) 1) when
           (stringp (sixth (nth i dataset)))
           collect
           (list
            (butlast (nth i dataset))
            (last (nth i dataset)))))))
      #| Introduced a fix here because the previous
      method of calling sort-dataset-asc could throw
      errors if points matched up to non-real elements
      such as "[" symbols. |#
      (mapcar
       #'(lambda (x)
           (append (first x) (second x)))
       (merge-sort-by-vector<vector-car
        (loop for i from 0 to
          (- (length dataset) 1) when
          (stringp (sixth (nth i dataset)))
          collect
          (list
           (butlast (nth i dataset))
           (last (nth i dataset)))))))
    (kern-file2tie-set-by-col
     path&name nil nil kern-rows nil
     staves-variable anacrusis ontimes (+ i-stave 1)
     n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (kern-col2dataset-no-tie-resolution
         (kern-rows2col
          kern-rows i-stave staves-variable)
         (first (nth i-stave staves-variable))
         ontimes))))))
|#

#|
\noindent Example:
\begin{verbatim}
(setq
 rows
 '("(4b-/	2.f\	[4.gg\	."
   "4cc/ 4dd/	.	.	."
   ".	.	(16.ggS\LL]	."
   ".	.	32aa\JJk	."
   "4dd-/)	.	16ccc\LL	."
   ".	.	16bb-\	."
   ".	.	16gg\	."
   ".	.	16ee\JJ)	."
   "=60	=60	=60	=60"
   "(4b-/ 4dd-/	4.f\	(4ee\ 4gg\	pp"
   "8a/ 8cc/)	.	8ff\)	."))
(setq staves-variable '((1 2) (0 1) (-1/2 1)))
(setq i 0)
(kern-rows2col rows i staves-variable)
--> ((("4b-") ("2.f")) (("4cc" "4dd") ("."))
((".") (".")) ((".") (".")) (("4dd-") ("."))
((".") (".")) ((".") (".")) ((".") ("."))
(("60") ("60")) (("4b-" "4dd-") ("4.f"))
(("8a" "8cc") (".")))

(setq
 rows
 (list
  "*staff2	*staff1	*staff1/2"
  "=58	=58	=58" "8f/ 8a/	8ff\\	."
  "8r	8r	." "4r	16ccS\\LL	pp"
  ".	(32dd'\\L	." ".	32ee'\\	."
  ".	32ff'\\	." ".	32gg'\\	." ".	32aa'\\	."
  ".	32bb-'\\JJJ)	."
  "4c/ 4e/ 4g/ 4b-/	(32ccc\\LLL	."
  ".	32bb\\	." ".	32ccc\\	." ".	32ddd\\	."
  ".	32ccc\\	." ".	32bb-\\	." ".	32aa\\	."
  ".	32gg\\JJJ)	." "=59	=59	=59"
  "*^	*	*" "(4b-/	2.f\\	[4.gg\\	."
  "4cc/	.	.	."
  ".	.	(16.ggS\\LL]	."
  ".	.	32aa\\JJk	."
  "4dd-/)	.	16ccc\\LL	."
  ".	.	16bb-\\	." ".	.	16gg\\	."
  ".	.	16ee\\JJ)	."
  "=60	=60	=60	=60"
  "(4b-/ 4dd-/	4.f\\	(4ee\\ 4gg\\	pp"
  "8a/ 8cc/)	.	8ff\\)	."
  "8r	4.ry	8r	." "4r	.	4r	."
  "*v	*v	*	*" "*clefF4	*	*"
  "=61	=61	=61"
  (concatenate
   'string
   "8C'\\ 8E'\\ 8G'\\ 8c'\\	"
   "8g'\\ 8b-'\\ 8cc'\\ 8ee'\\ 8gg'\\	pp")
  "8r	8r	."
  (concatenate
   'string
   "8FF'/ 8AA'/ 8C'/ 8F'/	"
   "8a'\\ 8cc'\\ 8ff'\\	.")
  "8r;	8r;	." "==	==	=="))
(setq staves-variable '((1 1) (0 1) (-1/2 1)))
(setq i 0)
(kern-rows2col rows i staves-variable)
--> (NIL NIL (("8f" "8a")) (("8r")) (("4r")) (("."))
     ((".")) ((".")) ((".")) ((".")) (("."))
     (("4c" "4e" "4g" "4b-")) ((".")) ((".")) (("."))
     ((".")) ((".")) ((".")) ((".")) NIL ((""))
     (("4b-") ("2.f")) (("4cc") (".")) ((".") ("."))
     ((".") (".")) (("4dd-") (".")) ((".") ("."))
     ((".") (".")) ((".") (".")) NIL
     (("4b-" "4dd-") ("4.f")) (("8a" "8cc") ("."))
     (("8r") ("4.r")) (("4r") (".")) (("") ("")) NIL
     NIL (("8C" "8E" "8G" "8c")) (("8r"))
     (("8FF" "8AA" "8C" "8F")) (("8r")) NIL)
\end{verbatim}

\noindent This function focuses on events in kern
rows that occur on the ith stave, and returns only
those events as datapoints.

Encountered some kern files that used spine commands
(beginning $\ast$) to encode information other than
splitting ($\ast\wedge$) or collapsing ($\ast\vee$).
Introduced a third test to the and condition, checking
whether the first character in a kern row is $\ast$.
If so the function recognised-spine-commandp checks
the row for recognised spine commands, and the row is
ignored if none are found.

Updated 9/6/2014 to allow the passing of a function as
the first optional argument. This enables more control
over removal of irrelevant characters (e.g., some
characters might be irrelevant to processing pitch but
relevant to parsing articulation). |#

(defun kern-rows2col
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
                 (space-bar-separated-string2list x)))
            (tab-separated-string2list row))))
        (result
         (if (<= end-index (length parsed-row)) 
           (subseq
             parsed-row start-index end-index))))
  (if (null row) ()
    (cons
     result
     (kern-rows2col
      (rest rows) i
      (update-staves-variable staves-variable row)
      fn))))

#| Old version.
(defun kern-rows2col
       (rows i staves-variable &optional
        (indices
         (cons
          0
          (fibonacci-list
           (nth-list-of-lists 1 staves-variable))))
        (start-index (nth i indices))
        (end-index (nth (+ i 1) indices))
        (row (first rows))
        (parsed-row
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
                     (remove-if
                      #'not-tie-dur-pitch-char-p y))
                 (space-bar-separated-string2list x)))
            (tab-separated-string2list row))))
        (result
         (if (<= end-index (length parsed-row)) 
           (subseq
             parsed-row start-index end-index))))
  (if (null row) ()
    (cons
     result
     (kern-rows2col
      (rest rows) i
      (update-staves-variable staves-variable row)))))
|#

#|
\noindent Example:
\begin{verbatim}
(kern-transp-file2dataset-by-col
  (merge-pathnames
   (make-pathname
    :name "B-55-1-small" :type "krn")
   *MCStylistic-MonthYear-example-files-data-path*)
  t)
--> ((0 65 63 2 0))
\end{verbatim}

\noindent This function is very similar to
kern-file2dataset-by-col. The difference is that this
function is intended to be run on kern files that
contain transposing instruments in them. It is assumed
that the kern file will contain a string such as
\verb+"*trvc 3 2	*"+ soon after the
announcement of staves
\verb+"**kern	**dynam"+, to indicate for instance
that notes on the first stave will sound three
semitones and two staff steps higher than written. |#

(defun kern-transp-file2dataset-by-col
       (path&name &optional
        (correct-trans-instr nil)
        (kern-rows
         (read-from-file-arbitrary path&name))
        (staves-variable
         (staves-info2staves-variable-robust
          kern-rows))
        (trans-vec
         (if correct-trans-instr
           (header2trans-vec
            kern-rows staves-variable)))
        (kern-rows
         (subseq
          kern-rows (second staves-variable)))
        (kern-rows-sep
         (mapcar
          #'(lambda (x)
              (tab-separated-string2list x))
          kern-rows))
        (staves-variable (first staves-variable))
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
       (sort-dataset-asc dataset))
      (sort-dataset-asc dataset))
    (kern-transp-file2dataset-by-col
     path&name correct-trans-instr nil nil trans-vec 
     kern-rows kern-rows-sep staves-variable
     anacrusis ontimes (+ i-stave 1) n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (if (and
             correct-trans-instr
             (nth i-stave trans-vec))
          (mapcar
           #'(lambda (x)
               (append
                (list (first x))
                (list
                 (+
                  (second x)
                  (first (nth i-stave trans-vec))))
                (list
                 (+
                  (third x)
                  (second (nth i-stave trans-vec))))
                (subseq x 3)))
           (kern-col2dataset
            (kern-rows2col
             kern-rows i-stave staves-variable)
            (first (nth i-stave staves-variable))
            ontimes))
          (kern-col2dataset
           (kern-rows2col
            kern-rows i-stave staves-variable)
           (first (nth i-stave staves-variable))
           ontimes)))))))

#|
\noindent Example:
\begin{verbatim}
(setq a-list '("4C#" "4r" "4B" "8g#"))
(setq staff-index 0)
(setq ontime 3)
(setq minimum-duration 0)
(parse-kern-spaced-rests
 a-list staff-index ontime minimum-duration)
--> (1/2
     ((3 "rest" "rest" 1 0) (3 59 59 1 0)
      (3 68 64 1/2 0)))
\end{verbatim}

\noindent This function is the rests equivalent of
the function \nameref{fun:parse-kern-spaced-notes}. |#

(defun parse-kern-spaced-rests
       (a-list staff-index ontime minimum-duration
        &optional (datapoints nil)
        (half-data
         (if a-list
           (kern-tie-dur-pitch2list
            (first a-list))))
        (MIDI-morphetic-pair
         (if half-data
           (if (and
                (stringp (first half-data))
                (string= (first half-data) "rest"))
             (list "rest" "rest")
               (if
                   (and
                    (not
                     (string= (first half-data) "["))
                    (not
                     (string= (first half-data) "]")))
                 (pitch&octave2MIDI-morphetic-pair
                  (first half-data)))))))
  (if (null a-list)
    (list minimum-duration datapoints)
    (if (null half-data)
      (parse-kern-spaced-rests
       (rest a-list) staff-index ontime
       minimum-duration datapoints)
      (if (null MIDI-morphetic-pair)
        (parse-kern-spaced-rests
         (rest a-list) staff-index ontime
         (if (second half-data)
           (if (zerop minimum-duration)
             (second half-data)
             (min
              minimum-duration (second half-data)))
           minimum-duration)
         datapoints)
        (parse-kern-spaced-rests
         (rest a-list) staff-index ontime
         (if (zerop minimum-duration)
           (second half-data)
           (min minimum-duration (second half-data)))
         (if (equalp (length half-data) 3)
           (append
            datapoints
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index
              (third half-data))))
           (append
            datapoints
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index)))))))))
