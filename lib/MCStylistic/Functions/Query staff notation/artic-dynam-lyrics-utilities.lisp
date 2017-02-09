#| Copyright 2008-2015 Tom Collins
   Tuesday 17 June 2014
   Incomplete

\noindent The functions here are designed to assist
with processing articulation, dynamics, and lyrics
information in kern files. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(setq question-string "fermata")
(setq
 artic-set
 '((3/2 59 59 1/2 2 NIL NIL NIL)
   (2 50 54 1 3 NIL NIL NIL) (2 57 58 1 2 NIL NIL NIL)
   (2 65 63 1 0 (";") NIL NIL)
   (2 66 63 1 1 NIL NIL NIL)))
(articulation-points question-string artic-set)
--> (("" "") ";" ((2 65 63 1 0 (";") NIL NIL))).
\end{verbatim}

\noindent This function looks for expressive markings
in the articulation dimension of an articulation point
set. It returns points (notes) to which the expressive
marking specified in the question string applies. |#

(defun articulation-points
       (question-string artic-set &optional
        (artic-idx 5)
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
            "fermata " question-string))
          ((search "fermata" question-string)
           (string-separated-string2list
            "fermata" question-string))
          ((search
            "phrase beginning on an " question-string)
           (string-separated-string2list
            "phrase beginning on an " question-string))
          ((search
            "phrase beginning on a " question-string)
           (string-separated-string2list
            "phrase beginning on a " question-string))
          ((search
            "phrase beginning " question-string)
           (string-separated-string2list
            "phrase beginning " question-string))
          ((search
            "phrase beginning" question-string)
           (string-separated-string2list
            "phrase beginning" question-string))
          ((search
            "phrase ending on an " question-string)
           (string-separated-string2list
            "phrase ending on an " question-string))
          ((search
            "phrase ending on a " question-string)
           (string-separated-string2list
            "phrase ending on a " question-string))
          ((search
            "phrase ending " question-string)
           (string-separated-string2list
            "phrase ending " question-string))
          ((search
            "phrase ending" question-string)
           (string-separated-string2list
            "phrase ending" question-string))
          ((search
            "slurred " question-string)
           (string-separated-string2list
            "slurred " question-string))))
        (probe-artic
         (cond
          ((search "staccato " question-string) "'")
          ((search "marcato " question-string) "^")
          ((search "fermata " question-string) ";")
          ((search "fermata" question-string) ";")
          ((search
            "phrase beginning " question-string) "(")
          ((search
            "phrase beginning" question-string) "(")
          ((search
            "phrase ending " question-string) ")")
          ((search
            "phrase ending" question-string) ")")
          ((search
            "slurred" question-string) "(")))
        (relevant-points
         (if probe-artic
           (remove-duplicates
            (loop for p in artic-set when
              (position
               probe-artic (nth artic-idx p)
               :test #'search) collect p)
            :test #'equalp))))
  (list question-split probe-artic relevant-points))
        
(defun dynamics-points
       (question-string artic-set &optional
        (dynam-idx 6)
        (question-split
         (cond
          ((search "sforzando " question-string)
           (string-separated-string2list
            "sforzando " question-string))
          ))
        (probe-artic
         (cond
          ((search
            "sforzando" question-string) "sfz")
          ))
        (relevant-points
         (if probe-artic
           (remove-duplicates
            (loop for p in artic-set when
              (position
               probe-artic (nth dynam-idx p)
               :test #'search) collect p)
            :test #'equalp))))
  (list question-split probe-artic relevant-points))
        

