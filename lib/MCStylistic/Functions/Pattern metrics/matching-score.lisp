#| Copyright 2008-2013 Tom Collins
   Wednesday 20 February 2013
   Incomplete

\noindent The functions below implement the
symbolic fingerprinting process described by
\cite{arzt2012}.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
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
   :name "sort-by"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "stats-sampling"
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
(setq X-tok '((4 0) (3 -1) (2 0)))
(setq
 Y-tok
 '((6 5) (2 2) (2 0) (2 1) (1 2) (3 -1) (2 0) (0 -1)
   (3 -1) (2 2) (2 0) (4 -1)))
(matching-lists-indices X-tok Y-tok)
--> ((1 1 2 2 2) (5 8 2 6 10))
\end{verbatim}

\noindent This function takes two lists $A$ and $B$ as
input. It returns two lists $a$ and $b$ of equal
length as output. The output lists contain all indices
such that $A(a) = B(b)$. There may be some lenience in
checking the last dimension of each sublist for
equality, depending on the variable flextime. |#

(defun matching-lists-indices
       (X-tok Y-tok &optional (flexitime nil)
        (X-idx nil) (Y-idx nil) (xn (length X-tok))
        (xi 0)
        (curr-pos
         (if (< xi xn)
           (if flexitime
             (positions-last-within-c
              (nth xi X-tok) Y-tok flexitime)
             (positions (nth xi X-tok) Y-tok)))))
  (if (>= xi xn) (list X-idx Y-idx)
    (matching-lists-indices
     X-tok Y-tok flexitime
     (if curr-pos
       (append
        X-idx (constant-vector xi (length curr-pos)))
       X-idx)
     (if curr-pos
       (append Y-idx curr-pos) Y-idx) xn (+ xi 1))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   X
   '((-1 81) (-3/4 76) (-1/2 85) (-1/4 81)
     (0 88) (1 57) (1 61) (1 64) (2 73)
     (9/4 69) (5/2 76) (11/4 73) (3 81) (4 45)
     (4 49) (4 52) (4 57) (5 61) (21/4 57)
     (11/2 64) (23/4 61) (6 57) (6 69) (7 54)
     (7 59) (7 63) (7 69) (8 51) (8 59) (8 66)
     (8 69) (9 52) (9 59) (9 66) (9 69)
     (10 40) (10 64) (10 68)))
  (setq
   Y-all
   (read-from-file
    (merge-pathnames
     (make-pathname
      :name "mutantBeethovenOp2No2Mvt3"
      :type "txt")
     *MCStylistic-MonthYear-example-files-data-path*)))
  (setq
   Y
   (orthogonal-projection-unique-equalp
    Y-all '(1 1 0 0 0)))
  (setq Y (firstn 250 Y))
  (setq
   X-fgp (symbolic-fingerprint X "query"))
  (setq
   Y-fgp (symbolic-fingerprint Y "mutant"))
  (matching-score-histogram X-fgp Y-fgp))
--> (0 6 715 7 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1
     0 6 660 7 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0
     0 8 158 4 1 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 1
     0 0 70)
\end{verbatim}

\noindent This function takes two lists $A$ and $B$ as
input. It returns two lists $a$ and $b$ of equal
length as output. The output lists contain all indices
such that $A(a) = B(b)$. There may be some lenience in
checking the last dimension of each sublist for
equality, depending on the variable flextime. |#

(defun matching-score-histogram
       (X-fgp Y-fgp &optional (res 1)
        (flexitime nil)
        (XY-idx
         (matching-lists-indices
          (nth-list-of-lists 0 X-fgp)
          (nth-list-of-lists 0 Y-fgp)
          flexitime))
        (X-idx (first XY-idx))
        (Y-idx (second XY-idx))
        ; Time stamps of matching tokens.
        (X-ts
         (nth-list-of-lists
          1
          (nth-list-of-lists
           1 (nth-list X-idx X-fgp))))
        (Y-ts
         (nth-list-of-lists
          1
          (nth-list-of-lists
           1 (nth-list Y-idx Y-fgp))))
        ; Time differences of matching tokens.
        (X-td
         (nth-list-of-lists
          2
          (nth-list-of-lists
           1 (nth-list X-idx X-fgp))))
        (Y-td
         (nth-list-of-lists
          2
          (nth-list-of-lists
           1 (nth-list Y-idx Y-fgp))))
        ; Apply affine transformation.
        (r
         (mapcar
          #'(lambda (x y) (/ y x))
          X-td Y-td))
        (s
         (mapcar
          #'(lambda (x y z) (- y (* x z)))
          X-ts Y-ts r))
        (edges
         (if s
           (loop for i from (min-item s) to
             (max-item s) by res collect i))))
  (if edges
    (if (equalp (length edges) 1)
      ; s is constant vector, so no need for hist.
      (list (length s))
      (histogram s edges))
    ; s is empty, there were no matches.
    (list 0)))

#|
\noindent Example:
\begin{verbatim}
(positions-last-within-c
 '(4 0) '((0 1) (3 2) (4 0.1) (2 2) (-4 4) (4 0)) .25)
--> (2 5)
\end{verbatim}

\noindent This is a very specific function. It checks
for instances of a query in a longer list. All but the
last elements are tested for equality, but the last
element is allowed to be within an amount $c$ of the
query. |#

(defun positions-last-within-c (a-query a-list c)
  (loop for element in a-list and position from 0
    when
    (and
     (equalp (butlast element) (butlast a-query))
     (<
      (abs
       (- (my-last element) (my-last a-query))) c))
     collect position))

#|
\noindent Example:
\begin{verbatim}
(setq
 dataset
 '((-1 81) (-3/4 76) (-1/2 85) (-1/4 81) (0 88)
   (1 57) (1 61) (1 64) (2 73) (9/4 69) (5/2 76)
   (11/4 73) (3 81) (4 45) (4 49) (4 52)))
(setq ID "beethovenOp2No2Mvt3")
(symbolic-fingerprint dataset ID)
--> (((81 76 85 1) ("beethovenOp2No2Mvt3" -1 1/4))
     ((81 76 81 2) ("beethovenOp2No2Mvt3" -1 1/4))
     ((81 76 88 3) ("beethovenOp2No2Mvt3" -1 1/4))
     ((81 76 57 7) ("beethovenOp2No2Mvt3" -1 1/4))
     ((81 76 61 7) ("beethovenOp2No2Mvt3" -1 1/4))
     ((81 85 81 1/2) ("beethovenOp2No2Mvt3" -1 1/2))
     ((81 85 88 1) ("beethovenOp2No2Mvt3" -1 1/2))
     ((81 85 57 3) ("beethovenOp2No2Mvt3" -1 1/2))
     ((81 85 61 3) ("beethovenOp2No2Mvt3" -1 1/2))
     ((81 85 64 3) ("beethovenOp2No2Mvt3" -1 1/2))
     ((81 81 88 1/3) ("beethovenOp2No2Mvt3" -1 3/4))
     ((81 81 57 5/3) ("beethovenOp2No2Mvt3" -1 3/4))
     ((81 81 61 5/3) ("beethovenOp2No2Mvt3" -1 3/4))
     ((81 81 64 5/3) ("beethovenOp2No2Mvt3" -1 3/4))
     ((81 81 73 3) ("beethovenOp2No2Mvt3" -1 3/4))
     ...
     ((76 73 52 5) ("beethovenOp2No2Mvt3" 5/2 1/4))
     ((76 81 45 2) ("beethovenOp2No2Mvt3" 5/2 1/2))
     ((76 81 49 2) ("beethovenOp2No2Mvt3" 5/2 1/2))
     ((76 81 52 2) ("beethovenOp2No2Mvt3" 5/2 1/2))
     ((73 81 45 4) ("beethovenOp2No2Mvt3" 11/4 1/4))
     ((73 81 49 4) ("beethovenOp2No2Mvt3" 11/4 1/4))
     ((73 81 52 4) ("beethovenOp2No2Mvt3" 11/4 1/4)))
\end{verbatim}

\noindent Given a two-dimensional dataset consisting
of ontimes and MIDI note numbers (or some other
numeric representation of pitch), this function
returns symbolic fingerprints as described by
\cite{arzt2012}. For transposition-variant
fingerprints, the format is
$$[m_1 : m_2 : m_3 : r] : \text{ID} : t : d_{1,2}$$
for locally constrained combinations (controlled by
n1, n2, and d) of successive MIDI notes, where $m_1$,
$m_2$, and $m_3$ are MIDI note numbers, $d_{i,j}$ is
the difference between the onsets of MIDI notes i and
j, r is the fraction $d_{2,3}/d_{1,2}$, and t is a
time stamp. For the transposition-invariant version,
replace $[m_1 : m_2 : m_3]$ by
$[m_2 - m_1 : m_3 - m_2]$. The tokens
$[m_1 : m_2 : m_3 : r]$ are stored as the first of a
pair and the rest of the fingerprint as the second of
a pair, in the list of lists that is returned. |#

(defun symbolic-fingerprint
       (dataset ID &optional (n1 5) (n2 5) (d .05)
        (trans-invar nil) (ontime-idx 0) (MNN-idx 1)
        (nD (length dataset))
        (i 0) ; Iterate over D.
        (i1 1) ; Iterate over the pair.
        (j1 0) ; Used if next MIDI event is too close.
        (i2 1) ; Iterate over the triple.
        (j2 0) ; Used if next MIDI event is too close.
        (idx
         (list
          i (+ i i1 j1) (+ i i1 j1 i2 j2)))
        (token
         (if (and
              (< i nD) (< (+ i i1 j1) nD) (<= i1 n1)
              (>=
               (-
                (nth
                 ontime-idx (nth (+ i i1 j1) dataset))
                (nth ontime-idx (nth i dataset))) d)
              (< (+ i i1 j1 i2 j2) nD) (<= i2 n2)
              (>=
               (-
                (nth
                 ontime-idx
                 (nth (+ i i1 j1 i2 j2) dataset))
                (nth
                 ontime-idx
                 (nth (+ i i1 j1) dataset))) d))
           (list
            (append
             (if trans-invar
               (list
                (-
                 (nth
                  MNN-idx (nth (second idx) dataset))
                 (nth
                  MNN-idx (nth (first idx) dataset)))
                (-
                 (nth
                  MNN-idx (nth (third idx) dataset))
                 (nth
                  MNN-idx
                  (nth (second idx) dataset))))
               (nth-list
                idx
                (nth-list-of-lists MNN-idx dataset)))
             (list
              (/
               (-
                (nth
                 ontime-idx (nth (third idx) dataset))
                (nth
                 ontime-idx
                 (nth (second idx) dataset)))
               (-
                (nth
                 ontime-idx
                 (nth (second idx) dataset))
                (nth
                 ontime-idx
                 (nth (first idx) dataset))))))
            (list
             ID
             (nth
              ontime-idx (nth (first idx) dataset))
             (-
              (nth
               ontime-idx (nth (second idx) dataset))
              (nth
               ontime-idx
               (nth (first idx) dataset))))))))
  (if (>= i nD) ()
    (if token
      (cons
       token
       (symbolic-fingerprint
        dataset ID n1 n2 d trans-invar ontime-idx
        MNN-idx nD i i1 j1 (+ i2 1) j2))
      (if (and
           (<= i1 n1) (<= i2 n2)
           (< (+ i i1 j1 i2 j2) nD)
           (<
            (-
             (nth
              ontime-idx
              (nth (+ i i1 j1 i2 j2) dataset))
             (nth
              ontime-idx
              (nth (+ i i1 j1) dataset))) d))
        ; Next MIDI event too close.
        (symbolic-fingerprint
         dataset ID n1 n2 d trans-invar ontime-idx
         MNN-idx nD i i1 j1 i2 (+ j2 1))
        (if (> i2 n2)
          (symbolic-fingerprint
           dataset ID n1 n2 d trans-invar ontime-idx
           MNN-idx nD i (+ i1 1) j1)
          (if (and
               (< (+ i i1 j1) nD) (<= i1 n1)
               (<
                (-
                 (nth
                  ontime-idx
                  (nth (+ i i1 j1) dataset))
                 (nth
                  ontime-idx (nth i dataset))) d))
            ; Next MIDI event too close.
            (symbolic-fingerprint
             dataset ID n1 n2 d trans-invar
             ontime-idx MNN-idx nD i i1 (+ j1 1))
            (if (> i1 n1)
              ; Time to increment i.
              (symbolic-fingerprint
               dataset ID n1 n2 d trans-invar
               ontime-idx MNN-idx nD (+ i 1))
              (if (>= (+ i i1 j1 i2 j2) nD)
                ; Time to increment i1.
                (symbolic-fingerprint
                 dataset ID n1 n2 d trans-invar
                 ontime-idx MNN-idx nD i (+ i1 1) j1)
                #| If we get to here, it must mean
                   (>= (+ i i1 j1) nD),
                in which case increment i. |#
                (symbolic-fingerprint
                 dataset ID n1 n2 d trans-invar
                 ontime-idx MNN-idx nD
                 (+ i 1))))))))))










        














