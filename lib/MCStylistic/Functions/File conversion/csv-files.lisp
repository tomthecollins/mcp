#| Copyright 2008-2013 Tom Collins
   Friday 30 July 2010
   Incomplete

\noindent These functions enable the conversion of csv
files into lists of lists, and vice versa. Despite
using terms like dataset below, neither representation
has to be balanced, that is, rows/lists can contain
different numbers of elements.

; REQUIRED PACKAGES:
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
(comma-positions "uh,ktr,3,4")
--> (2 6 8)
\end{verbatim}

\noindent This function applies the function read-
from-string recursively, once the string supplied as
an argument has had the function comma-separated-
string2list applied. |#

(defun comma-positions
       (string &optional (start 0)
        (local-result
         (position #\, (subseq string start)))
        (result
         (if local-result
           (+ start local-result))))
  (if (null result) ()
    (cons result
          (comma-positions string (+ result 1)))))

#|
\noindent Example:
\begin{verbatim}
(comma-separated-integers2list "1.00, 3.00")
--> (1 3)
\end{verbatim}

\noindent This function applies the function parse-
integer recursively, once the string supplied as an
argument has had the function comma-separated-
string2list applied. |#

(defun comma-separated-integers2list (a-string)
  (mapcar
    #'(lambda (x)
        (parse-integer x :junk-allowed t))
    (comma-separated-string2list a-string)))

#|
\noindent Example:
\begin{verbatim}
(comma-separated-reals2list "1.50, 3/4, -5.6")
--> (1.5 3/4 -5.6)
\end{verbatim}

\noindent This function applies the function read-
from-string recursively, once the string supplied as
an argument has had the function comma-separated-
string2list applied. |#

(defun comma-separated-reals2list (a-string)
  (mapcar
    #'(lambda (x)
        (read-from-string x))
    (comma-separated-string2list a-string)))

#|
\noindent Example:
\begin{verbatim}
(comma-separated-string2list "uh,ktr,3,4")
--> ("uh" "ktr" "3" "4")
\end{verbatim}

\noindent This function turns a comma-separated
string into a list, where formerly each item was
preceded or proceeded by a comma. |#

(defun comma-separated-string2list
       (comma-separated-string &optional
	(commas-positioned
	 (cons
          -1
          (comma-positions comma-separated-string)))
	(result nil))
  (if (equal (length commas-positioned) 1)
    (append
     result
     (list
      (subseq
       comma-separated-string
       (+ (first commas-positioned) 1))))
    (comma-separated-string2list
     comma-separated-string
     (rest commas-positioned)
     (append
      result
      (list
       (subseq
        comma-separated-string
        (+ (first commas-positioned) 1)
        (second commas-positioned)))))))

#|
\noindent Example:
\begin{verbatim}
(csv2dataset
 (merge-pathnames
  (make-pathname
   :name "short-list" :type "csv")
  *MCStylistic-MonthYear-example-files-data-path*))
--> ((1 3) (2 6.1) (5 2 2) (6 2))
\end{verbatim}

\noindent This function converts a file in comma-
separated-value (csv) format to a dataset. It will
handle reals, strings, and characters. |#

(defun csv2dataset (path&name)
  (mapcar
    #'(lambda (x)
        (comma-separated-reals2list x))
    (read-from-file-arbitrary path&name)))

#|
\noindent Example:
\begin{verbatim}
(dataset2csv
 (merge-pathnames
  (make-pathname
   :name "scarlatti-L10-bars1-19" :type "txt")
  *MCStylistic-MonthYear-example-files-data-path*)
 (merge-pathnames
  (make-pathname
   :name "scarlatti-L10-bars1-19" :type "csv")
  *MCStylistic-MonthYear-example-files-data-path*))
-->  0.00, 36.00, 46.00, 1.00, 1.00
     0.00, 48.00, 53.00, 1.00, 1.00
     0.50, 60.00, 60.00, 0.50, 0.00
    ...
    56.50, 74.00, 68.00, 0.50, 0.00
\end{verbatim}

\noindent This function converts a dataset (a list of
lists of equal length) to a csv file. The first
argument is either the path where the dataset resides
or the dataset itself. |#

(defun dataset2csv
       (data-path csv-path &optional
	(dataset
         (if (or
              (pathnamep data-path)
              (stringp data-path))
           (read-from-file data-path)
           (identity data-path)))
	(file
	 (open
	  csv-path
	  :direction :output :if-does-not-exist
	  :create :if-exists :overwrite)))
  (if (null dataset) (close file)
    (progn
      (format file "~{~10$~^, ~}~%" (first dataset))
      (dataset2csv
       data-path csv-path (rest dataset) file))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fpath&name
 (merge-pathnames
  (make-pathname
   :name "list2csv-test" :type "csv")
  *MCStylistic-MonthYear-example-files-results-path*))
(list-of-lists2csv
 '((4 2.0) (0 -2 1) ("A" "B") (4 3) ("W") (0 0 0)) 
 fpath&name)
--> T
\end{verbatim}

\noindent This function converts a (possibly
unbalanced) list of lists to a csv file. An unbalanced
list will also work. The first argument is either the
path where the dataset resides or the dataset
itself.

2/1/2015. Altered $\sim$a to $\sim$s in this function,
to aid lossless conversion when re-importing. |#

(defun list-of-lists2csv
       (data-path csv-path &optional
	(dataset
         (if (pathnamep data-path)
           (read-from-file data-path)
           (identity data-path)))
	(file
	 (open
	  csv-path
	  :direction :output :if-does-not-exist
	  :create :if-exists :overwrite))
        (first-dataset
         (mapcar
          #'(lambda (x)
              (if (and (numberp x) (not (integerp x)))
                (float x) x))
          (first dataset))))
  (if (null dataset) (close file)
    (progn
      (format file "~{~s~^,~}~%" first-dataset)
      (list-of-lists2csv
       data-path csv-path (rest dataset) file))))

#|
\noindent Example:
\begin{verbatim}
(string-positions "and" "yes and maybe no and May")
--> (4 17)
\end{verbatim}

\noindent This function returns the positions at
which the first specified substring occurs in the
second (longer) string. |#

(defun string-positions
       (substring string &optional (start 0)
        (local-result
         (search
          substring (subseq string start)
          :test #'string=))
        (result
         (if local-result
           (+ start local-result))))
  (if (null result) ()
    (cons result
          (string-positions
	   substring string (+ result 1)))))

#|
\noindent Example:
\begin{verbatim}
(string-separated-string2list
 "and" "yes and maybe no and May")
--> ("yes" "maybe no" "May")
\end{verbatim}

\noindent This function turns a tab-separated
string into a list, where formerly each item was
preceded or proceeded by a tab. |#

(defun string-separated-string2list
       (substring string &optional
	(strings-positioned
	 (cons
          (- 0 (length substring))
          (string-positions substring string)))
	(result nil))
  (if (equal (length strings-positioned) 1)
    (mapcar
     #'(lambda (x) (string-trim " " x))
     (append
      result
      (list
       (subseq
        string
        (+
         (first strings-positioned)
         (length substring))))))
    (string-separated-string2list
     substring string (rest strings-positioned)
     (append
      result
      (list
       (subseq
        string
        (+
         (first strings-positioned)
         (length substring))
        (second strings-positioned)))))))


#|
\noindent Example:
\begin{verbatim}
(tab-separated-reals2list "1.50	3/4	-5.6")
--> (1.5 3/4 -5.6)
\end{verbatim}

\noindent This function applies the function read-
from-string recursively, once the string supplied as
an argument has had the function tab-separated-
string2list applied. It did have problems parsing
elements consisting of a dot and no other
alpha-numeric characters. A fix was found using
string-trim, removing white space and new-line
commands before parsing. |#

(defun tab-separated-reals2list
       (a-string &optional
        (string-trims
         (mapcar
          #'(lambda (x)
              (string-trim
               '(#\Space #\return #\linefeed) x))
          (tab-separated-string2list a-string))))
  (mapcar
   #'(lambda (y)
       (if (not (or (string= y "") (string= y ".")))
         (read-from-string y)))
   string-trims))

#| Old version.
(defun tab-separated-reals2list (a-string)
  (mapcar
    #'(lambda (x)
        (progn
          (setq
           y
           (string-trim
            '(#\Space #\return #\linefeed) x))
          (if (not
               (or
                (string= y "")
                (string= y ".")))
            (read-from-string x))))
    (tab-separated-string2list a-string)))
|#

#|
\noindent Example:
\begin{verbatim}
(tab2dataset
 (merge-pathnames
  (make-pathname
   :name "short-tab-list" :type "txt")
  *MCStylistic-MonthYear-example-files-data-path*))
--> ((0.74 64) (-0.74 76) (1.44 -60))
\end{verbatim}

\noindent This function converts a file in tab-
separated format to a dataset. It will handle reals,
strings, and characters. It did have problems parsing
elements consisting of a dot and no other
alpha-numeric characters. A fix was found using
string-trim, removing white space and new-line
commands before parsing. |#

(defun tab2dataset (path&name)
  (mapcar
    #'(lambda (x)
        (tab-separated-reals2list x))
    (read-from-file-arbitrary path&name)))
