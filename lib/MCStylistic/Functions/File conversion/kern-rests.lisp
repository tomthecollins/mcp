#| Copyright 2008-2014 Tom Collins
   Friday 13 June 2014
   Incomplete

The functions below will parse a kern file
(http://kern.ccarh.org/) by column and convert the
rests therein to a point set. The main function is
\nameref{fun:kern-file2rest-set-by-col}.

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
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(kern-file2rest-set-by-col
  (merge-pathnames
   (make-pathname
    :name "C-6-1-small" :type "krn")
   *MCStylistic-MonthYear-example-files-data-path*))
--> ((-1 1 1) (3 1 0) (7/2 1/4 0) (4 1 0))
\end{verbatim}

\noindent This function is similar to the function
\nameref{fun:kern-file2dataset-by-col}. Rather than
converting written notes to points, it converts
written rests to points. The output is a point set,
where each point consists of an ontime, two `rest'
strings (placeholders for MIDI note and morphetic
pitch numbers), duration, and staff number. The
function was written for retrieving rests, which was
part of the requirements for the MediaEval 2014
C@merata task. |#
       
(defun kern-file2rest-set-by-col
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
        (loop for i from 0 to
          (- (length dataset) 1) when
          (and
           (stringp (second (nth i dataset)))
           (string=
            (second (nth i dataset)) "rest"))
          collect
          (list
           (first (nth i dataset))
           (fourth (nth i dataset))
           (fifth (nth i dataset))))))
      (sort-dataset-asc
       (loop for i from 0 to
         (- (length dataset) 1) when
         (and
          (stringp (second (nth i dataset)))
          (string= (second (nth i dataset)) "rest"))
         collect
         (list
          (first (nth i dataset))
          (fourth (nth i dataset))
          (fifth (nth i dataset))))))
    (kern-file2rest-set-by-col
     path&name nil nil kern-rows nil
     staves-variable anacrusis ontimes (+ i-stave 1)
     n-stave
     (append
      dataset
      (if (integerp
           (first (nth i-stave staves-variable)))
        (kern-col2rest-set
         (kern-rows2col
          kern-rows i-stave staves-variable)
         (first (nth i-stave staves-variable))
         ontimes))))))

#|
\noindent Example:
\begin{verbatim}
(rest-duration-time-intervals
 "sixteenth note rest"
 '((-1 1 1) (3 1 0) (7/2 1/4 0) (4 1 0))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((7/2 15/4))
(rest-duration-time-intervals
 "crotchet rest in the piano left hand"
 '((-1 1 1) (3 1 0) (7/2 1/4 0) (4 1 0))
 '(("piano left hand" "bass clef")
   ("piano right hand" "treble clef")))
--> ((-1 0))
\end{verbatim}

\noindent This function returns (ontime, offtime)
pairs of points (rests) that have the duration
specified by the first string argument. It can be in
the format `dotted minim rest' or `dotted half note
rest', for instance. The function does not look for
dotted rests in the case of the word dotted, but
adds one half of the value to the corresponding rest
type and looks for the numeric value. |#

(defun rest-duration-time-intervals
       (question-string point-set staff&clef-names
        &optional
        (ontime-index 0) (duration-index 1)
        (staff-idx 2)
        ; Convert the duration into numeric format.
        (rest-tf
         (search " rest" question-string))
        (val
         (if rest-tf
           (duration-string2numeric
            question-string)))
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
   