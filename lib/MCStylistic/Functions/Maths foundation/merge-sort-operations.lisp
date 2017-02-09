#| Copyright 2008-2013 Tom Collins
   Thursday 30 September 2010
   Incomplete

\noindent These functions implement various merge
sorts and related operations. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(vector<vector-different-lengths '(0 -2) '(0 -2 3))
--> T
\end{verbatim}

\noindent If the vectors are of the same length,
the function vector<vector-t-or-nil is applied. If
the second vector $\mathbf{v}_2 = (v_{21},\ldots,
v_{2n})$ is longer than the first $\mathbf{v}_1 =
(v_{11},\ldots,v_{1m})$, we must allow for the
possibility of equality $\mathbf{v}_1 = \mathbf{v}_2$,
before applying the function vector<vector. When equal
as described, $\mathbf{v}_1 < \mathbf{v}_2$. |#

(defun vector<vector-different-lengths
       (vector1 vector2 &optional
        (length1 (length vector1))
        (length2 (length vector2)))
  (if (> length1 length2)
    (not
     (vector<vector-different-lengths
      vector2 vector1 length2 length1))
    (if (equalp length1 length2)
      (vector<vector-t-or-nil vector1 vector2)
      (let ((verdict
             (vector<vector
              vector1
              (subseq vector2 0 length1))))
        (if (stringp verdict) t verdict)))))
