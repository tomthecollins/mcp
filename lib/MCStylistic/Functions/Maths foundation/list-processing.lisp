#| Copyright 2008-2013 Tom Collins
   Friday 15 January 2010
   Incomplete

\noindent The functions below are some of the most
straightforward and often-used extensions I have made
to the language. |#

; REQUIRED PACKAGES
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(add-to-list 5 '(1 2 3))
--> (6 7 8)
\end{verbatim}

\noindent This function adds a constant to each
element of a list. |#

(defun add-to-list (constant a-list)
  (if (null a-list) ()
    (cons (+ constant (first a-list))
          (add-to-list constant (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(add-to-nth 1 3 '(1 2 3 5 9))
--> (1 2 4 5 9)
\end{verbatim}

\noindent This function adds a constant to the nth
element of a list. |#

(defun add-to-nth (x n a-list)
  (append (firstn (- n 1) a-list)
          (list (+ x (nth (- n 1) a-list)))
          (lastn (- (length a-list) n) a-list)))

#|
\noindent Example:
\begin{verbatim}
(choose 9 5)
--> 126
\end{verbatim}

\noindent This function returns 'n choose r', that is
n!/(r!(n-r)!), where n and r are natural numbers or
zero. |#

(defun choose (n r)
  (/ (factorial-j n (- n (+ r 1)))
     (factorial (- n r))))

#|
\noindent Example:
\begin{verbatim}
(constant-vector 2.4 6)
--> (2.4 2.4 2.4 2.4 2.4 2.4)
\end{verbatim}

\noindent This function gives a constant vector of
prescribed length. |#

(defun constant-vector (c n &optional (i 0))
  (if (equal i n) ()
    (cons c (constant-vector c n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(cyclically-permute-list-by
 '(17.77 0.15 14.93 0.16 4.95) 2)
--> (14.93 0.16 4.95 17.77 0.15)
\end{verbatim}

\noindent This function moves the $i$th item of a list
to the first item in the output list, where $i - 1$ is
the second argument. The $i-1$th item is moved to the
last item in the output list, etc. |#

(defun cyclically-permute-list-by
       (a-list m &optional (n (length a-list))
        (m (mod m n)))
  (append
   (subseq a-list m)
   (subseq a-list 0 m)))

#|
\noindent Example:
\begin{verbatim}
(factorial 5)
--> 120
\end{verbatim}

\noindent This function returns n(n-1)(n-2)...3.2.1
where n is a natural number or zero. |#

(defun factorial (n &optional (m 1))
  (if (or (equal n 1) (equal n 0)) (identity m)
    (* n (factorial (- n 1)))))

#|
\noindent Example:
\begin{verbatim}
(factorial-j 9 3)
--> 3024
\end{verbatim}

\noindent The arguments of this function are $n > j$,
both natural numbers or zero. The answer
n(n-1)(n-2)...(n-j) is returned. Should $j \geq n$ or
$j < 0$, 1 is returned. This function makes the
function choose more efficient by avoiding direct
calculation of n!/r!. |#

(defun factorial-j (n j &optional (m n))
  (if (or (<= n j) (< j 0)) (identity 1)
    (if (equal j 0) (identity m)
       (* n (factorial-j (- n 1) (- j 1))))))

#|
\noindent Example:
\begin{verbatim}
(first-n-naturals 5)
--> (5 4 3 2 1)
\end{verbatim}

\noindent This function returns the first n natural
numbers as a list. |#

(defun first-n-naturals (n)
  (if (equal n 0) ()
    (cons n (first-n-naturals (- n 1)))))

#|
\noindent Example:
\begin{verbatim}
(firstn 3 '(3 4 (5 2) 2 0))
--> (3 4 (5 2))
\end{verbatim}

\noindent This function returns the first n items of a
list. |#

(defun firstn (n a-list)
  (if (or (null a-list) (<= n 0)) ()
    (cons (first a-list)
          (firstn (- n 1) (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(index-item-1st-occurs 2 '(1 0 0 2 4 2))
--> 3
\end{verbatim}

\noindent Taking an item and a list of items as its
arguments, this function returns the index at which
the given item first occurs, counting from zero. If
the item does not occur at all then the function
returns NIL. |#

(defun index-item-1st-occurs
       (item a-list &optional (index 0))
  (if (null a-list) ()
    (if (equalp (first a-list) item) (identity index)
      (index-item-1st-occurs
       item (rest a-list) (1+ index)))))

#|
\noindent Example:
\begin{verbatim}
(last-first 3 '(3 4 (5 2) 2 0))
--> (0 2 (5 2))
\end{verbatim}

\noindent This function returns the last n items of a
list, but in reverse order. NB (last a-list) behaves
rather weirdly, hence the code
(first (last a-list)). |#

(defun last-first (n a-list)
  (if (or (null a-list) (<= n 0)) ()
    (cons (first (last a-list))
          (last-first (- n 1) (butlast a-list)))))

#|
\noindent Example:
\begin{verbatim}
(lastn 3 '(3 4 (5 2) 2 0))
--> ((5 2) 2 0)
\end{verbatim}

\noindent This function returns the last n items of a
list. |#

(defun lastn (n a-list)
  (reverse (last-first n a-list)))

#|
\noindent Example:
\begin{verbatim}
(multiply-list-by-constant '(2 0) 5)
--> (10 0)
\end{verbatim}

\noindent Two arguments are supplied to this function:
a list and a constant. A list is returned, containing
the result of multiplying each element of the list by
the constant. |#

(defun multiply-list-by-constant
       (a-list a-constant)
  (if (null a-list) ()
    (cons
     (* a-constant (first a-list))
     (multiply-list-by-constant
      (rest a-list) a-constant))))

#|
\noindent Example:
\begin{verbatim}
(my-last '(1 3 6 7))
--> 7
\end{verbatim}

\noindent Returns the last element of a list as an
element, not as a list. |#

(defun my-last (a-list)
  (first (last a-list)))

#|
\noindent Example:
\begin{verbatim}
(nth-list '(1 3 0) '(6 -3 -88 0 4 44))
--> (-3 0 6)
\end{verbatim}

\noindent This function applies the function nth
recursively to the second list argument, according to
the items of the first list argument. |#

(defun nth-list (a-list b-list)
  (if (null a-list) ()
    (cons (nth (first a-list) b-list)
          (nth-list (rest a-list) b-list))))

#|
\noindent Example:
\begin{verbatim}
(nth-list-of-lists 0 '((48 2) (-50 0) (-5 5)))
--> (48 -50 5)
\end{verbatim}

\noindent This function takes two arguments; an item n
and a list of sub-lists. It returns the nth item of
each sub-list as a list. |#

(defun nth-list-of-lists (item a-list)
  (mapcar
   #'(lambda (x) (nth item x)) a-list))

#|
\noindent Example:
\begin{verbatim}
(positions
 '(4 0) '((0 1) (3 2) (4 0) (2 2) (-4 4) (4 0) (5 6)))
--> (2 5)
\end{verbatim}

\noindent This code returns the positions of a query
in a list. |#

(defun positions
       (a-query a-list &optional (test-fn #'equalp))
  (loop for element in a-list and position from 0
    when (funcall test-fn element a-query)
    collect position))

#|
\noindent Example:
\begin{verbatim}
(remove-nth 4 '(6 4 5 5 2 3 1))
--> (6 4 5 5 3 1)
\end{verbatim}

\noindent This code removes the nth item of a list,
counting from zero. |#

(defun remove-nth (n a-list)
  (if (or (< n 0) (>= n (length a-list))) ()
    (append
     (firstn n a-list)
     (lastn (1- (- (length a-list) n)) a-list))))

#|
\noindent Example:
\begin{verbatim}
(remove-nth-list '(3 5 0) '(1 2 3 4 5 6 7))
--> (2 3 5 7)
\end{verbatim}

\noindent The function remove-nth-list applies the
function remove-nth recursively to the second
argument, according to the indices in the first
argument, which do not have to be ordered but must be
distinct. |#

(defun remove-nth-list
       (a-list b-list &optional
        (c-list
	 (remove-duplicates
	  (sort a-list #'>) :test #'equalp)))
  (if (null c-list) (identity b-list)
    (remove-nth-list
     a-list (remove-nth (first c-list) b-list)
     (rest c-list))))

#|
\noindent Example:
\begin{verbatim}
(test-equalp-nth-to-x '(3 5 0) 1 5)
--> T
\end{verbatim}

\noindent The first argument to this function is a
list of numbers, the second argument is an index that
refers to one of these numbers. If this number is
equalp to the third argument, T is returned, and nil
otherwise. |#

(defun test-equalp-nth-to-x (a-list n x)
  (equalp (nth n a-list) x))

#|
\noindent Example:
\begin{verbatim}
(test-equalp-nth-to-xs '(3 5 0) 1 '(2 4 5 6))
--> T
\end{verbatim}

\noindent The first argument to this function is a
list of numbers, the second argument is an index that
refers to one of these numbers. This number is tested
for membership in the third argument, and the output
is the result of this test. Note it will not recognise
1.0 as 1. |#

(defun test-equalp-nth-to-xs (a-list n probes)
  (not
   (null
    (intersection (list (nth n a-list)) probes))))

