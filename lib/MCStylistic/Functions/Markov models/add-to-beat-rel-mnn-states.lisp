
(setq midi-notes '(84 60 67 72 41))
(setq tonic-midi-note-closest 55)
(squash-range midi-notes tonic-midi-note-closest)

(defun squash-range
       (a-list center &optional
        (dedupe&sort-tf t)
        (squashed-list
         (mapcar
          #'(lambda (x)
              (loop while
                (> x (+ center 12)) do
                (setq x (- x 12)))
              (loop while
                (< x (- center 12)) do
                (setq x (+ x 12)))
              x)
          a-list)))
  (if dedupe&sort-tf
    (remove-duplicates
     (sort squashed-list #'<) :test #'equalp)
    squashed-list))


(defun beat-rel-sq-MNN-states
       (dataset &optional
        (catalogue-information "no information")
	(beats-in-bar 4) (MNN-index 1) (MPN-index 2)
	(dur-index 3)
        (fifth-steps-mode
         (fifth-steps-mode
          dataset *Aarden-key-profiles* 0 1
          dur-index))
        (trans-pair&c-dataset
         (centre-dataset
          fifth-steps-mode dataset MNN-index
          MPN-index))
        (trans-pair (first trans-pair&c-dataset))
        (c-dataset (second trans-pair&c-dataset))
        (c-segments
         (butlast
          (segments-strict
           c-dataset MNN-index dur-index)))
        (segments
         (butlast
          (segments-strict
           dataset MNN-index dur-index)))
        (c-segment (first c-segments))
        (segment (first segments)))
  (if (null c-segment) ()
    (cons
     (list
      (list
       (+ (mod (first c-segment) beats-in-bar) 1)
       (squash-range
        (nth-list-of-lists MNN-index (second c-segment))
        (first trans-pair)))
      (list
       catalogue-information
       (mapcar
        #'(lambda (x) (subseq x 0 5))
        (second segment))
       trans-pair fifth-steps-mode))
     (beat-rel-sq-MNN-states
      dataset catalogue-information beats-in-bar
      MNN-index MPN-index dur-index fifth-steps-mode
      trans-pair&c-dataset trans-pair c-dataset
      (rest c-segments) (rest segments)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 file-pathname
 (merge-pathnames
  (make-pathname
   :name "scarlatti-L10-bars1-19" :type "txt")
  *MCStylistic-MonthYear-example-files-data-path*))
(point-set2phrase-boundary-states
 file-pathname '("ending" 4) "beat-rel-sq-MNN-states")
--> (((4 (9))
      ("C-6-1-small" ((-1 66 63 5/3 0)) (57 58) (6 5)))
     ((19/4 (-15 9))
      ("C-6-1-small" ((3 42 49 1 1) (15/4 66 63 1/4 0))
       (57 58) (6 5)))).
\end{verbatim}

\noindent This function imports a kern file and searches
for notes that have articulation associated with phrase
beginnings or endings, with the aim of returning states
that will be appropriate for using as initial or final
internal states. The first optional argument phrase-str
can be called with `phrase beginning', `phrase ending',
`fermata beginning', or `fermata ending'. Phrase
beginnings are indicated by \texttt{(} in a kern file,
endings by \texttt{)}, and fermata by \texttt{;}.
Fermata indicate the end of a phrase, and therefore the
next state will be the beginning of the next phrase. So
if this function is called with phrase-str equal to
`fermata beginning', the next state(s) following fermata
will be returned.

Sometimes fermata signs appear in close succession (for
instance imagine the tenor sings A3 held over from the
fourth beat of the bar to the first beat of the next
bar, resolving to G$\sharp$3 on beat two, whilst the
bass, alto, and soprano sing E3, E4, B4 resepctively on
beat one, and suppose further that fermata are written
on these three notes and the resolving G$\sharp$3. Then
there will be three fermata at ontime $x$ and one at
ontime $x + 1$. The end of the phrase is not at ontime
$x$ but at ontime $x + 1$. This function will remove
fermata ontimes that are too close together---in our
example removing the fermata ontime $x$. This
functionality is controlled by the optional argument
too-close. |#

(defun point-set2phrase-boundary-states
       (file-pathname &optional
        (phrase-str (list "beginning" 1))
        (state-fn "beat-rel-MNN-states")
        (beats-in-bar 4)
        (catalogue-information
         (pathname-name file-pathname))
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4)
        ; Unpack the phrase-str into phrase and ontimes.
        (duration-or-ontimes
         (if (equalp (length (rest phrase-str)) 1)
           (second phrase-str)
           (rest phrase-str)))
        (phrase-str (first phrase-str))
        (states (read-from-file file-pathname))
        )
  (if (string= phrase-str "ending")
    #| If we're looking for phrase endings (via the
    ontime method), we should collect whatever state
    precedes the beginning of the next phrase. |#
    (loop for i from 1 to (- (length states) 1)
      when (equalp
            (first
             (first (nth i states)))
            duration-or-ontimes)
      collect (nth (- i 1) states))
    (loop for i from 0 to (- (length states) 2)
      when (equalp
            (first
             (first (nth i states)))
            duration-or-ontimes)
      collect (nth i states))))

#|
(defun point-set2phrase-boundary-states
       (file-pathname &optional
        (phrase-str (list "beginning" 4))
        (state-fn "beat-rel-MNN-states")
        (beats-in-bar 4)
        (catalogue-information
         (pathname-name file-pathname))
        (ontime-idx 0) (MNN-idx 1) (MPN-idx 2)
        (duration-idx 3) (staff-idx 4)
        ; Unpack the phrase-str into phrase and ontimes.
        (duration-or-ontimes
         (if (equalp (length (rest phrase-str)) 1)
           (second phrase-str)
           (rest phrase-str)))
        (phrase-str (first phrase-str))
        (point-set
         (sort-dataset-asc
          (mapcar
           #'(lambda (x)
               (list
                (nth ontime-idx x) (nth MNN-idx x)
                (nth MPN-idx x) (nth duration-idx x)
                (nth staff-idx x)))
           (read-from-file file-pathname))))
        (relevant-times
         (if (listp duration-or-ontimes)
           duration-or-ontimes
           (loop for i
             from 0
             to (nth ontime-idx (my-last point-set))
             by duration-or-ontimes
             collect i)))
        (seg-ons
         (nth-list-of-lists
          ontime-idx
          (segments-strict
           point-set MNN-idx duration-idx)))
        (relevant-idx
         (loop for time in relevant-times
           when (position time seg-ons)
           collect (position time seg-ons)))
        (states
         (cond
          ((string= state-fn "beat-rel-MNN-states")
           (beat-rel-MNN-states
            point-set catalogue-information
            beats-in-bar MNN-idx MPN-idx
            duration-idx))
          ((string= state-fn "beat-rel-sq-MNN-states")
           (beat-rel-sq-MNN-states
            point-set catalogue-information
            beats-in-bar MNN-idx MPN-idx
            duration-idx))
          )))
  (if (string= phrase-str "ending")
    #| If we're looking for phrase endings (via the
    ontime method), we should collect whatever state
    precedes the beginning of the next phrase. |#
    (loop for i in relevant-idx
      when (and relevant-idx (> i 0))
      collect (nth (- i 1) states))
    (loop for i in relevant-idx
      when relevant-idx
      collect (nth i states))))

(setq phrase-str '("ending" 4))
(setq state-fn "beat-rel-sq-MNN-states")
(setq beats-in-bar 4)
(setq catalogue-information
      (pathname-name file-pathname))
(setq ontime-idx 0) (setq MNN-idx 1) (setq MPN-idx 2)
(setq duration-idx 3) (setq staff-idx 4)
; Unpack the phrase-str into phrase and ontimes.
(setq duration-or-ontimes
      (if (equalp (length (rest phrase-str)) 1)
        (second phrase-str)
        (rest phrase-str)))
(setq phrase-str (first phrase-str))
(setq point-set
      (sort-dataset-asc
       (mapcar
        #'(lambda (x)
            (list
             (nth ontime-idx x) (nth MNN-idx x)
             (nth MPN-idx x) (nth duration-idx x)
             (nth staff-idx x)))
        (read-from-file file-pathname))))
(setq relevant-times
      (if (listp duration-or-ontimes)
        duration-or-ontimes
        (loop for i
          from 0
          to (nth ontime-idx (my-last point-set))
          by duration-or-ontimes
          collect i)))
(setq seg-ons
      (nth-list-of-lists
       ontime-idx
       (segments-strict
        point-set MNN-idx duration-idx)))
(setq relevant-idx
      (loop for time in relevant-times
        when (position time seg-ons)
        collect (position time seg-ons)))
(setq states
      (cond
       ((string= state-fn "beat-rel-MNN-states")
        (beat-rel-MNN-states
         point-set catalogue-information
         beats-in-bar MNN-idx MPN-idx
         duration-idx))
       ((string= state-fn "beat-rel-sq-MNN-states")
        (beat-rel-sq-MNN-states
         point-set catalogue-information
         beats-in-bar MNN-idx MPN-idx
         duration-idx))
       ))

|#



