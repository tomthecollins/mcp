#| Copyright 2008-2013 Tom Collins
   Friday 13 September 2013

Functions here are for finding indices of lists whose
members satisfy certain requirements.

Functions such as
\nameref{fun:index-1st-sublist-item<=} ought to be
moved here eventually as well. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(index-nth-sublist-item<
 0 3 '(14 14 14 11 0 0 -1 -2 -2))
--> 8
\end{verbatim}

\noindent This function takes three arguments: a real
number $x$, an integer counter $n$, and a list $L$ of
real numbers. It returns the index of the nth element
of $L$ which is less than $x$, where $n = 1$ refers to
the first element. |#

(defun index-nth-sublist-item<
       (item idx a-list &optional (i 0) (j 1))
  (if (null a-list) ()
    (if (and (< (first a-list) item) (>= j idx))
      (identity i)
      (index-nth-sublist-item<
       item idx (rest a-list) (+ i 1)
       (if (< (first a-list) item) (+ j 1) j)))))

#|
\noindent Example:
\begin{verbatim}
(index-nth-sublist-item>
 4 2 '(0 0 0 1 1 4 6 6 7 7 11 14 14 14))
--> 7
\end{verbatim}

\noindent This function takes three arguments: a real
number $x$, an integer counter $n$, and a list $L$ of
real numbers. It returns the index of the nth element
of $L$ which is greater than $x$, where $n = 1$ refers
to the first element. |#

(defun index-nth-sublist-item>
       (item idx a-list &optional (i 0) (j 1))
  (if (null a-list) ()
    (if (and (> (first a-list) item) (>= j idx))
      (identity i)
      (index-nth-sublist-item>
       item idx (rest a-list) (+ i 1)
       (if (> (first a-list) item) (+ j 1) j)))))

#|
\noindent Example:
\begin{verbatim}
(index-nth-sublist-item<=
 6 3 '(14 14 14 11 7 7 6 6 4 1 1 0 0))
--> 8
\end{verbatim}

\noindent This function takes three arguments: a real
number $x$, an integer counter $n$, and a list $L$ of
real numbers. It returns the index of the nth element
of $L$ which is less than or equal to $x$, where
$n = 1$ refers to the first element. |#

(defun index-nth-sublist-item<=
       (item idx a-list &optional (i 0) (j 1))
  (if (null a-list) ()
    (if (and (<= (first a-list) item) (>= j idx))
      (identity i)
      (index-nth-sublist-item<=
       item idx (rest a-list) (+ i 1)
       (if (<= (first a-list) item) (+ j 1) j)))))

#|
\noindent Example:
\begin{verbatim}
(index-nth-sublist-item>=
 4 2 '(0 0 0 1 1 4 6 6 7 7 11 14 14 14))
--> 6
\end{verbatim}

\noindent This function takes three arguments: a real
number $x$, an integer counter $n$, and a list $L$ of
real numbers. It returns the index of the nth element
of $L$ which is greater than or equal to $x$, where
$n = 1$ refers to the first element. |#

(defun index-nth-sublist-item>=
       (item idx a-list &optional (i 0) (j 1))
  (if (null a-list) ()
    (if (and (>= (first a-list) item) (>= j idx))
      (identity i)
      (index-nth-sublist-item>=
       item idx (rest a-list) (+ i 1)
       (if (>= (first a-list) item) (+ j 1) j)))))
