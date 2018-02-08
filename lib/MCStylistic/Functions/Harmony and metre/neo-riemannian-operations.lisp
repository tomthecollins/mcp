#| Copyright 2008-2013 Tom Collins
   Sunday 24 February 2013
   Incomplete

\noindent The functions below are implementations of
neo-Riemannian operations \citep*{lewin1987,cohn1998}.
These include leading-note, relative, and parallel
operations). At present they do not take correct pitch
spelling into account. For instance, the operations
`LPL' and `PLP' map C major to G$\sharp$ minor and
A$\flat$ minor respectively, but both will be
represented by pitch class 8.
 
; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(chord-pair2NRO-string '(3 2) '(4 0))
--> "LRL"
(chord-pair2NRO-string '(7 0) '(2 0))
--> "LR"
(chord-pair2NRO-string '(3 2) '(10 2))
--> "RL"
(chord-pair2NRO-string '(0 0) '(5 2))
--> "RLP"
\end{verbatim}

\noindent This function takes two chord pairs as
arguments. It returns the neo-Riemannian operation(s)
that transform the first chord into the second chord,
as a string. It will return the shortest such string
for this transformation, and if there are two of
equal length, it will return one arbitrarily. The
format of each input chord is a two-element list, with
first element for the MIDI note number modulo 12 of
the root of the chord, and the second element the
class of the chord (0, major triad; 2, minor
triad). |#

(defun chord-pair2NRO-string
       (a-pc-class-pair b-pc-class-pair &optional
        (pc-diff
         (mod
          (if (zerop (second a-pc-class-pair))
            (-
             (first b-pc-class-pair)
             (first a-pc-class-pair))
            (-
             (first a-pc-class-pair)
             (first b-pc-class-pair))) 12))
        (class-diff
         (mod
          (-
           (second b-pc-class-pair)
           (second a-pc-class-pair)) 4))
        (lookup
         '(((0 0) "") ((1 0) "LPRP") ((2 0) "LRLR")
           ((3 0) "PR") ((4 0) "LP") ((5 0) "RL")
           ((6 0) "RPRP") ((7 0) "LR") ((8 0) "PL")
           ((9 0) "RP") ((10 0) "LRPR")
           ((11 0) "LRLP") ((0 2) "P") ((1 2) "LPR")
           ((2 2) "RLR") ((3 2) "PRP") ((4 2) "L")
           ((5 2) "RLP") ((6 2) "RPR") ((7 2) "LRP")
           ((8 2) "LPL") ((9 2) "R") ((10 2) "RPRPL")
           ((11 2) "LRL"))))
  (second
   (assoc
    (list pc-diff class-diff) lookup :test 'equalp)))

#|
\noindent Example:
\begin{verbatim}
(chord-pairs2NRO-strings '((3 2) (4 0) (7 0) (2 0)))
--> ("LRL" "PR" "LR")
\end{verbatim}

\noindent This function takes a list of chord pairs as
its argument (first element for the MIDI note number
modulo 12 of the root of the chord, and the second
element the class of the chord: 0, major triad; 2,
minor triad). It returns a list of strings of length
$n - 1$, where $n$ is the length of the input list.
The strings are the neo-Riemannian operation(s)
that transform chord $i$ into chord $i + 1$. It will
return the shortest such string for each
transformation, and if there are two of equal length,
it will return one arbitrarily. The format of each
input chord is a two-element list, with first element
for the MIDI note number modulo 12 of the root of the
chord, and the second element the class of the chord
(0, major triad; 2, minor triad). |#

(defun chord-pairs2NRO-strings
       (chord-pairs)
  (loop for i from 0 to (- (length chord-pairs) 2)
    collect
    (chord-pair2NRO-string
     (nth i chord-pairs) (nth (+ i 1) chord-pairs))))

#|
\noindent Example:
\begin{verbatim}
(NRO-L '(3 2))
--> (11 0)
(NRO-L '(11 0))
--> (3 2)
\end{verbatim}

\noindent This function applies the neo-Riemannian
leading-note operation to an input chord pair,
represented as a two-element list, with first element
for the MIDI note number modulo 12 of the root of the
chord, and the second element the class of the chord
(0, major triad; 2, minor triad). |#

(defun NRO-L (pc-class-pair)
  (list
   (if (zerop (mod (second pc-class-pair) 4))
     (mod (+ (first pc-class-pair) 4) 12)
     (mod (- (first pc-class-pair) 4) 12))
   (mod (+ (second pc-class-pair) 2) 4)))

#|
\noindent Example:
\begin{verbatim}
(NRO-P '(3 2))
--> (3 0)
(NRO-P '(3 0))
--> (3 2)
\end{verbatim}

\noindent This function applies the neo-Riemannian
parallel operation to an input chord pair, represented
as a two-element list, with first element for the MIDI
note number modulo 12 of the root of the chord, and
the second element the class of the chord (0, major
triad; 2, minor triad). |#

(defun NRO-P (pc-class-pair)
  (list
   (first pc-class-pair)
   (mod (+ (second pc-class-pair) 2) 4)))

#|
\noindent Example:
\begin{verbatim}
(NRO-R '(3 2))
--> (6 0)
(NRO-R '(6 0))
--> (3 2)
\end{verbatim}

\noindent This function applies the neo-Riemannian
relative operation to an input chord pair, represented
as a two-element list, with first element for the MIDI
note number modulo 12 of the root of the chord, and
the second element the class of the chord (0, major
triad; 2, minor triad). |#

(defun NRO-R (pc-class-pair)
  (list
   (if (zerop (mod (second pc-class-pair) 4))
     (mod (- (first pc-class-pair) 3) 12)
     (mod (+ (first pc-class-pair) 3) 12))
   (mod (+ (second pc-class-pair) 2) 4)))

#|
\noindent Example:
\begin{verbatim}
(NRO-string2chord-pairs "PRP" '(0 0))
--> ((0 0) (0 2) (3 0) (3 2))
\end{verbatim}

\noindent This function applies the neo-Riemannian
operations specified as a string in the first argument
to an input chord pair (optional second argument),
represented as a two-element list, with first element
for the MIDI note number modulo 12 of the root of the
chord, and the second element the class of the chord
(0, major triad; 2, minor triad). The first element of
the string is applied first. The first, all
intermediate, and last chords are returned. |#

(defun NRO-string2chord-pairs
       (NRO-string &optional (chord-pair (list 0 0))
        (chord-pairs (list chord-pair)))
  (if (string= NRO-string "") (identity chord-pairs)
    (NRO-string2chord-pairs
     (subseq NRO-string 1) chord-pair
     (append
      chord-pairs
      (list
       (if (string= (subseq NRO-string 0 1) "L")
         (NRO-L (my-last chord-pairs))
         (if (string= (subseq NRO-string 0 1) "R")
           (NRO-R (my-last chord-pairs))
           (if (string= (subseq NRO-string 0 1) "P")
             (NRO-P (my-last chord-pairs))
             "error"))))))))

#|
\noindent Example:
\begin{verbatim}
(NRO-strings2chord-pairs '("PRP" "LR" "PRPL") '(1 0))
--> ((1 0) (4 2) (9 2) (10 2))
(NRO-strings2chord-pairs '("R" "L" "LRLR") '(0 0))
--> ((0 0) (9 2) (5 0) (7 0))
\end{verbatim}

\noindent This function applies the neo-Riemannian
operations specified as strings in the first argument
to an input chord pair (optional second argument),
represented as a two-element list, with first element
for the MIDI note number modulo 12 of the root of the
chord, and the second element the class of the chord
(0, major triad; 2, minor triad). The first element of
the string is applied first. There is a choice
(optional third argument) between returning all
intermediary chords, or just the final result of each
NRO (default). |#

(defun NRO-strings2chord-pairs
       (NRO-strings &optional (chord-pair (list 0 0))
        (verbose-out nil) (all-chord-pairs nil)
        (curr-chord-pairs
         (if NRO-strings
           (NRO-string2chord-pairs
            (first NRO-strings)
            (if all-chord-pairs
              (my-last (my-last all-chord-pairs))
              chord-pair)))))
  (if (null NRO-strings)
    (if verbose-out all-chord-pairs
      (append
       (list chord-pair)
       (mapcar
        #'(lambda (x) (my-last x)) all-chord-pairs)))
    (NRO-strings2chord-pairs
     (rest NRO-strings) chord-pair verbose-out
     (append
      all-chord-pairs (list curr-chord-pairs)))))
