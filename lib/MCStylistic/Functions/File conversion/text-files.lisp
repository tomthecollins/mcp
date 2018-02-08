#| Copyright 2008-2013 Tom Collins
   Friday 15 January 2010
   Incomplete

\noindent The functions below will write a variable
to a text file, and read such text files back in as
lists.

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
   (merge-pathnames
    (make-pathname
   :directory '(:relative "Third party" "cl-fad")
   :name "fad2"
   :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
|#

#|
\noindent Example:
\begin{verbatim}
(frac2dec '(1 3.4 ("no" 4/3 "yeah")))
--> (1 3.4 ("no" 1.3333334 "yeah")).
\end{verbatim}

\noindent This function converts fractions occurring
in a list (of arbitrary depth) into floats. |#

(defun frac2dec
       (var &optional (var1 (first var))
        (var2
         (if (and var1 (listp var1))
           (frac2dec var1)
           (if (and
                (numberp var1) (null (integerp var1)))
             (float var1) var1))))
  (if (null var)
    () (cons var2 (frac2dec (rest var)))))

#|
\noindent Example:
\begin{verbatim}
(pathname-typesp
 #P"/Users/hello.txt" (list "csv" "txt"))
--> T
\end{verbatim}

\noindent This function checks whether the path
(including file name and type) supplied as the first
argument is of one of the types specified by the
second argument. |#

(defun pathname-typesp
       (a-path &optional
        (file-types (list "csv" "txt")))
  (if (null file-types) ()
    (if (string=
         (pathname-type a-path) (first file-types))
      T (pathname-typesp a-path (rest file-types)))))

#|
\noindent Example:
\begin{verbatim}
(positions-char #\_ "ascending _ _ _")
--> (10 12 14)
\end{verbatim}

\noindent This function returns the indices in a
string where instances of the character argument
occur. |#

(defun positions-char
       (a-char a-string)
  (loop for i from 0 to (- (length a-string) 1) when
    (equal (char a-string i) a-char) collect i))

#|
\noindent Example:
\begin{verbatim}
(read-from-file
 (concatenate
  'string
  "/Applications/CCL/Lisp documentation"
  "/Example files/a-list.txt"))
--> (((2 0 0) (0 60 2) (1 61 1))
     ((4 12 0) (0 60 2) (3 59 1))).
\end{verbatim}

\noindent This function returns the contents of a file
specified by the variable path\&name. It returns each
row of the file as a list, in a list of lists. |#

(defun read-from-file (path&name)
  (if (null path&name)
    ()
    ;;(print "Empty path and file provided.")
    (with-open-file (stream path&name)
      (loop for row = (read-line stream nil)
        while row do (setf
                      row
                      (read-from-string row nil))
        when row collect row into results
        finally (return results)))))

#|
\noindent Example:
\begin{verbatim}
(read-from-file-arbitrary
 (concatenate
  'string
  "/Applications/CCL/Lisp documentation"
  "/Example files/a-file.txt"))
--> ("first line consisting of anything"
     "second line consisting of &^%$").
\end{verbatim}

\noindent This function is similar to the function
read-from-file. The difference is that read-from-file-
arbitrary will parse any file, converting each line to
a string for further processing. |#

(defun read-from-file-arbitrary (path&name)
  (if (null path&name)
    ()
    ;;(print "Empty path and file provided.")
    (with-open-file (stream path&name)
      (loop for row = (read-line stream nil)
        while row do (setf row row)
        when row collect row into results
        finally (return results)))))

#|
\noindent Example:
\begin{verbatim}
(replace-all
 "all the occurences of the part" "the" "THE")
--> "all THE occurences of THE part"
\end{verbatim}

\noindent This function, from the Common Lisp
Cookbook, returns a new string in which all the
occurences of the part is replaced with
replacement. |#

(defun replace-all
       (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences
  of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
      for old-pos = 0 then (+ pos part-length)
      for pos = (search part string
                        :start2 old-pos
                        :test test)
      do (write-string string out
                       :start old-pos
                       :end (or pos (length string)))
      when pos do (write-string replacement out)
      while pos)))

#|
\noindent Example:
\begin{verbatim}
(replace-once "16 16th notes" "16" "17")
--> "17 16th notes"
(replace-once "16 16th notes" "16" "")
--> "16th notes"
\end{verbatim}

\noindent This function replaces the first instance
(from the left) of its second argument in the first
argument by the third argument. |#

(defun replace-once
       (string part replacement &optional
        (pos (search part string))
        (string-without-part
         (if pos
           (concatenate
            'string
            (subseq string 0 pos)
            (subseq string (+ pos (length part))))))
        (string-with-replacement
         (if string-without-part
           (string-trim
            " "
            (concatenate
             'string
             (subseq string 0 pos)
             replacement
             (subseq
              string (+ pos (length part))))))))
  (if string-with-replacement
    string-with-replacement string))

#|
\noindent Example:
\begin{verbatim}
(update-written-file
 (merge-pathnames
  (make-pathname
   :name "list-to-update" :type "txt")
  *MCStylistic-MonthYear-example-files-data-path*)
 1 '(6 60) '((2 2) . ((2 72))))
--> (((0 7) . ((0 60) (2 63)))
     ((2 2) . ((2 72) (6 60)))
     ((3 -1) . ((0 60) (3 67)))
     ((6 0) . ((3 67) (5 66))))
whereas originally file read
    (((0 7) . ((0 60) (2 63)))
     ((2 2) . ((2 72)))
     ((3 -1) . ((0 60) (3 67)))
     ((6 0) . ((3 67) (5 66))))
\end{verbatim}

\noindent This function updates the contents of a
specifed file by removing the row associated with the
variable updatee, and replacing it with updater
appended within updatee. It should overwrite the
existing file. The position of the row is
preserved. |#

(defun update-written-file
       (path&name filename-counter updater updatee
        &optional
        (list-to-update
	 (read-from-file
          (merge-pathnames
           (make-pathname
            :name
            (concatenate
             'string
             (pathname-name path&name) " "
             (write-to-string filename-counter))
            :type (pathname-type path&name))
           (pathname-directory-pathname
             path&name)))))
  (progn
    (rplacd (assoc (car updatee) list-to-update
                   :test #'equalp)
            (append
	     (cdr updatee) (list updater)))
    (write-to-file
     list-to-update
     (merge-pathnames
      (make-pathname
       :name
       (concatenate
        'string
        (pathname-name path&name) " "
        (write-to-string filename-counter))
       :type (pathname-type path&name))
      (pathname-directory-pathname
       path&name)))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file
 '(5 7 8 "hello" 9) "/Users/tec69/Desktop/test.txt")
--> have a look at the desktop.
\end{verbatim}

\noindent This function writes the data provided in
the first list to a file with the path and name
provided in the second list. The s in the format
argument is essential for retaining strings as they
appear in the data.

2/1/2015. Added an optional argument to prevent
closing the file after the end of writing. |#

(defun write-to-file
       (variable path&name
        &optional
        (file (open path&name
                    :direction
                    :output
                    :if-does-not-exist
                    :create
                    :if-exists
                    :overwrite))
        (close-file-at-finish t))
  (if (null variable)
    (if close-file-at-finish (close file))
    (progn (format file
                   "~s~%" (first variable))
      (write-to-file (rest variable)
                     path&name
                     file close-file-at-finish))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-append
 '(10 "goodbye") "/Users/tec69/Desktop/test.txt")
--> have a look at the desktop.
\end{verbatim}

\noindent The only difference between this and the
function write-to-file is that an existing file will
be opened and new data appended, rather than
overwritten. |#

(defun write-to-file-append
       (variable path&name &optional
	(file
	 (open
	  path&name :direction :output
	  :if-does-not-exist :create
	  :if-exists :append)))
  (if (null variable) (close file)
    (progn (format file "~s~%" (first variable))
      (write-to-file-append
       (rest variable) path&name file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-frac2dec
 '((5/2 7 "h") 9 1/3 (4 5 6/2))
 "/Users/tomthecollins/Desktop/test.txt")
--> have a look at the desktop.
\end{verbatim}

\noindent This function writes the data provided in
the first list to a file with the path and name
provided in the second list. Any fractions appearing
in the list are converted to decimals. |#

(defun write-to-file-frac2dec
       (variable path&name &optional
        (var (frac2dec variable))
        (file
         (open
          path&name :direction :output
          :if-does-not-exist
          :create :if-exists :overwrite)))
  (if (null var) (close file)
    (progn
      (format file "~s~%" (first var))
      (write-to-file-frac2dec
       variable path&name (rest var) file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-supersede
 '(5 7 8 "hello" 9) "/Users/tec69/Desktop/test.txt")
--> have a look at the desktop.
\end{verbatim}

\noindent The only difference between this and the
function write-to-file is that an existing file will
be superseded, rather than overwritten. |#

(defun write-to-file-supersede
       (variable path&name &optional
	(file
	 (open
	  path&name :direction :output
	  :if-does-not-exist :create
	  :if-exists :supersede)))
  (if (null variable) (close file)
    (progn (format file
                   "~s~%" (first variable))
      (write-to-file (rest variable)
                     path&name
                     file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-with-open-file
 "hello"
 "/Users/tec69/Desktop/Some new folder/test.txt")
--> have a look at the desktop.
\end{verbatim}

\noindent There was a problem with the function write-
to-file in Emacs, because it would not create a
directory that did not already exist. This was
remedied using the functions with-open-file and
ensure-directories-exist. However, this function
only works with a single (i.e. non-list) variable.
Once you have used it to create the directory, use
the function write-to-file as per usual. |#

(defun write-to-file-with-open-file
       (variable path&name)
  (with-open-file
      (out
       (ensure-directories-exist path&name)
       :direction :output)
    (format out "~s~%" variable)))
