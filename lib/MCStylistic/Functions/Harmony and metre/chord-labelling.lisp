#| Copyright 2008-2013 Tom Collins
   Tuesday 19 April 2011
   Incomplete

\noindent An implementation of the HarmAn algorithm
as described by \cite{pardo2002}, as well as an
extension of this algorithm to provide
functional-harmonic analysis.

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
   (merge-pathnames
    (make-pathname
     :directory '(:relative "Markov models")
     :name "beat-rel-MNN-states"
     :type "lisp")
    *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "interpolation"
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
   :directory '(:relative "Maths foundation")
   :name "list-processing"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Harmony and metre")
   :name "ontimes-signatures"
   :type "lisp")
  *MCStylistic-MonthYear-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Markov models")
   :name "segmentation"
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
   :directory '(:relative "Pattern discovery")
   :name "structural-induction-mod"
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

(defvar
 *chord-templates-pardo&birmingham*
 '(#| Major triad |#
   (0 4 7) (1 5 8) (2 6 9) (3 7 10) (4 8 11) (5 9 0)
   (6 10 1) (7 11 2) (8 0 3) (9 1 4) (10 2 5) (11 3 6)
   #| Dom7 |#
   (0 4 7 10) (1 5 8 11) (2 6 9 0) (3 7 10 1)
   (4 8 11 2) (5 9 0 3) (6 10 1 4) (7 11 2 5)
   (8 0 3 6) (9 1 4 7) (10 2 5 8) (11 3 6 9)
   #| Minor triad |#
   (0 3 7) (1 4 8) (2 5 9) (3 6 10) (4 7 11) (5 8 0)
   (6 9 1) (7 10 2) (8 11 3) (9 0 4) (10 1 5) (11 2 6)
   #| Fully diminished 7th |#
   (0 3 6 9) (1 4 7 10) (2 5 8 11)
   #| Half diminished 7th |#
   (0 3 6 10) (1 4 7 11) (2 5 8 0) (3 6 9 1)
   (4 7 10 2) (5 8 11 3) (6 9 0 4) (7 10 1 5)
   (8 11 2 6) (9 0 3 7) (10 1 4 8) (11 2 5 9)
   #| Diminished triad |#
   (0 3 6) (1 4 7) (2 5 8) (3 6 9) (4 7 10) (5 8 11)
   (6 9 0) (7 10 1) (8 11 2) (9 0 3) (10 1 4)
   (11 2 5)))

(defvar
 *chord-names-pardo&birmingham*
  '(#| Major triad |#
    ("C major") ("C# major" "Db major") ("D major")
    ("D# major" "Eb major") ("E major") ("F major")
    ("F# major" "Gb major") ("G major")
    ("G# major" "Ab major") ("A major")
    ("A# major" "Bb major") ("B major")
    #| Dom7 |#
    ("C7") ("C#7" "Db7") ("D7") ("D#7" "Eb7") ("E7")
    ("F7") ("F#7" "Gb7") ("G7") ("G#7" "Ab7") ("A7")
    ("A#7" "Bb7") ("B7")
    #| Minor triad |#
    ("C minor") ("C# minor" "Db minor") ("D minor")
    ("D# minor" "Eb minor") ("E minor") ("F minor")
    ("F# minor" "Gb minor") ("G minor")
    ("G# minor" "Ab minor") ("A minor")
    ("A# minor" "Bb minor") ("B minor")
    #| Fully diminished 7th |#
    ("C dim 7" "D# dim 7" "Eb dim 7" "F# dim 7"
     "Gb dim 7" "A dim 7")
    ("C# dim 7" "Db dim 7" "E dim 7" "G dim 7"
     "A# dim 7" "Bb dim 7")
    ("D dim 7" "F dim 7" "G# dim 7" "Ab dim 7"
     "B dim 7")
    #| Half diminished 7th |#
    ("C half dim 7") ("C# half dim 7" "Db half dim 7")
    ("D half dim 7") ("D# half dim 7" "Eb half dim 7")
    ("E half dim 7") ("F half dim 7")
    ("F# half dim 7" "Gb half dim 7") ("G half dim 7")
    ("G# half dim 7" "Ab half dim 7") ("A half dim 7")
    ("A# half dim 7" "Bb half dim 7") ("B7")
    #| Diminished triad |#
    ("C dim") ("C# dim" "Db dim") ("D dim")
    ("D# dim" "Eb dim") ("E dim") ("F dim")
    ("F# dim" "Gb dim") ("G dim")
    ("G# dim" "Ab dim") ("A dim")
    ("A# dim" "Bb dim") ("B dim")))

(defvar
 *chord-name-template-assoc-pardo&birmingham*
  (loop for i from 0
    to
    (-
     (length *chord-templates-pardo&birmingham*) 1)
    append
    (loop for j from 0 to
      (-
       (length
        (nth i *chord-names-pardo&birmingham*)) 1)
      collect
      (list
       (nth j (nth i *chord-names-pardo&birmingham*))
       (nth i *chord-templates-pardo&birmingham*)))))
    
(defvar
 *chord-templates-p&b&min7ths*
 '(#| Major triad |#
   (0 4 7) (1 5 8) (2 6 9) (3 7 10) (4 8 11) (5 9 0)
   (6 10 1) (7 11 2) (8 0 3) (9 1 4) (10 2 5) (11 3 6)
   #| Dom7 |#
   (0 4 7 10) (1 5 8 11) (2 6 9 0) (3 7 10 1)
   (4 8 11 2) (5 9 0 3) (6 10 1 4) (7 11 2 5)
   (8 0 3 6) (9 1 4 7) (10 2 5 8) (11 3 6 9)
   #| Minor triad |#
   (0 3 7) (1 4 8) (2 5 9) (3 6 10) (4 7 11) (5 8 0)
   (6 9 1) (7 10 2) (8 11 3) (9 0 4) (10 1 5) (11 2 6)
   #| Fully diminished 7th |#
   (0 3 6 9) (1 4 7 10) (2 5 8 11)
   #| Half diminished 7th |#
   (0 3 6 10) (1 4 7 11) (2 5 8 0) (3 6 9 1)
   (4 7 10 2) (5 8 11 3) (6 9 0 4) (7 10 1 5)
   (8 11 2 6) (9 0 3 7) (10 1 4 8) (11 2 5 9)
   #| Diminished triad |#
   (0 3 6) (1 4 7) (2 5 8) (3 6 9) (4 7 10) (5 8 11)
   (6 9 0) (7 10 1) (8 11 2) (9 0 3) (10 1 4)
   (11 2 5)
   #| Minor 7th |#
   (0 3 7 10) (1 4 8 11) (2 5 9 0) (3 6 10 1)
   (4 7 11 2) (5 8 0 3) (6 9 1 4) (7 10 2 5)
   (8 11 3 6) (9 0 4 7) (10 1 5 8) (11 2 6 9)))

(defvar
 *chord-names-p&b&min7ths*
  (append
   *chord-names-pardo&birmingham*
   '(#| Minor 7th |#
     ("C minor 7") ("C# minor 7th" "Db minor 7th")
     ("D minor 7th") ("D# minor 7th" "Eb minor 7th")
     ("E minor 7th") ("F minor 7th")
     ("F# minor 7th" "Gb minor 7th") ("G minor 7th")
     ("G# minor 7th" "Ab minor 7th") ("A minor 7th")
     ("A# minor 7th" "Bb minor 7th") ("B minor 7th")
    )))

(defvar
 *chord-name-template-assoc-p&b&min7ths*
  (loop for i from 0
    to
    (-
     (length *chord-templates-p&b&min7ths*) 1)
    append
    (loop for j from 0 to
      (-
       (length
        (nth i *chord-names-p&b&min7ths*)) 1)
      collect
      (list
       (nth j (nth i *chord-names-p&b&min7ths*))
       (nth i *chord-templates-p&b&min7ths*)))))

(defvar
 *chord-templates-assa*
 '(#| Major triad |#
   (0 4 7) (1 5 8) (2 6 9) (3 7 10) (4 8 11) (5 9 0)
   (6 10 1) (7 11 2) (8 0 3) (9 1 4) (10 2 5) (11 3 6)
   #| Minor triad |#
   (0 3 7) (1 4 8) (2 5 9) (3 6 10) (4 7 11) (5 8 0)
   (6 9 1) (7 10 2) (8 11 3) (9 0 4) (10 1 5)
   (11 2 6)))

(defvar
 *roman-major-templates-p&b*
 '(#| Major triad |#
   "I" "bII" "II" "bIII" "III" "IV"
   "#VI" "V" "bVI" "VI" "bVII" "VII"
   #| Dom7 |#
   "I7" "bII7" "II7" "bIII7" "III7" "IV7"
   "#VI7" "V7" "bVI7" "VI7" "bVII7" "VII7"
   #| Minor triad |#
   "i" "#i" "ii" "biii" "iii" "iv"
   "#vi" "v" "#vi" "vi" "bvii" "vii"
   #| Fully diminished 7th |#
   "viio7" "viio7" "viio7"
   #| Half diminished 7th |#
   "ih7" "#ih7" "iih7" "biiih7" "iiih7" "ivh7"
   "#vih7" "vh7" "#vih7" "vih7" "bviih7" "viih7"
   #| Diminished triad |#
   "io" "#io" "iio" "biiio" "iiio" "ivo"
   "#vio" "vo" "#vio" "vio" "bviio" "viio"
   #| Minor 7th |#
   "i7" "#i7" "ii7" "biii7" "iii7" "iv7"
   "#vi7" "v7" "#vi7" "vi7" "bvii7" "vii7"))

(defvar
 *roman-minor-templates-p&b*
 '(#| Major triad |#
   "III" "#III" "IV" "#IV" "V" "VI"
   "#VI" "VII" "#VII" "I" "bII" "II"
   #| Dom7 |#
   "III7" "#III7" "IV7" "#IV7" "V7" "VI7"
   "#VI7" "VII7" "#VII7" "I7" "bII7" "II7"
   #| Minor triad |#
   "iii" "#iii" "iv" "bv" "v" "vi"
   "#vi" "vii" "#vii" "i" "bii" "ii"
   #| Fully diminished 7th |#
   "viio7" "viio7" "viio7"
   #| Half diminished 7th |#
   "iiih7" "#iiih7" "ivh7" "bvh7" "vh7" "vih7"
   "#vih7" "viih7" "#viih7" "ih7" "biih7" "iih7"
   #| Diminished triad |#
   "iiio" "#iiio" "ivo" "bvo" "vo" "vio"
   "#vio" "viio" "#viio" "io" "biio" "iio"
   #| Minor 7th |#
   "iii7" "#iii7" "iv7" "bv7" "v7" "vi7"
   "#vi7" "vii7" "#vii7" "i7" "bii7" "ii7"))

(defvar
 *major-key-triad-assoc*
  '(("tonic triad" "I")
    ("tonic triad in root position" "I")
    ("I" "I")
    ("Ia" "I")
    ("tonic triad in first inversion" "Ib")
    ("Ib" "Ib")
    ("tonic triad in second inversion" "Ic")
    ("Ic" "Ic")
    
    ("supertonic triad" "ii")
    ("supertonic triad in root position" "ii")
    ("ii" "ii")
    ("iia" "ii")
    ("supertonic triad in first inversion" "iib")
    ("iib" "iib")
    ("supertonic triad in second inversion" "iic")
    ("iic"  "iic")
    
    ("mediant triad" "iii")
    ("mediant triad in root position" "iii")
    ("iii" "iii")
    ("iiia" "iii")
    ("mediant triad in first inversion" "iiib")
    ("iiib" "iiib")
    ("mediant triad in second inversion" "iiic")
    ("iiic" "iiic")
    
    ("subdominant triad" "IV")
    ("subdominant triad in root position" "IV")
    ("IV" "IV")
    ("IVa" "IV")
    ("subdominant triad in first inversion" "IVb")
    ("IVb" "IVb")
    ("subdominant triad in second inversion" "IVc")
    ("IVc" "IVc")
    
    ("dominant triad" "V")
    ("dominant triad in root position" "V")
    ("V" "V")
    ("Va" "V")
    ("dominant triad in first inversion" "Vb")
    ("Vb" "Vb")
    ("dominant triad in second inversion" "Vc")
    ("Vc" "Vc")
    
    ("submediant triad" "vi")
    ("submediant triad in root position" "vi")
    ("relative minor triad" "vi")
    ("relative minor triad in root position" "vi")
    ("vi" "vi")
    ("via" "vi")
    ("vi" "vi")
    ("submediant triad in first inversion" "vib")
    ("relative minor triad in first inversion" "vib")
    ("vib" "vib")
    ("submediant triad in second inversion" "vib")
    ("relative minor triad in second inversion" "vic")
    ("vic" "vic")
    
    ("diminished triad on the leading tone in root position" "viio")
    ("diminished triad on the leading tone" "viio")
    ("leading tone diminished triad" "viio")
    ("leading tone triad" "viio")
    ("vii" "viio")
    ("viia" "viio")
    ("viio" "viio")
    ("viioa" "viio")
    ("diminished triad on the leading tone in first inversion" "viiob")
    ("leading tone diminished triad in first inversion" "viiob")
    ("leading tone triad in first inversion" "viiob")
    ("viib" "viiob")
    ("viiob" "viiob")
    ("diminished triad on the leading tone in second inversion" "viioc")
    ("leading tone diminished triad in second inversion" "viioc")
    ("leading tone triad in second inversion" "viioc")
    ("viic" "viioc") ("viioc" "viioc")))

(defvar
 *minor-key-triad-assoc*
  '(("tonic triad" "i")
    ("tonic triad in root position" "i")
    ("i" "i")
    ("ia" "i")
    ("tonic triad in first inversion" "ib")
    ("ib" "ib")
    ("tonic triad in second inversion" "ic")
    ("ic" "ic")
    
    ("supertonic triad" "iio")
    ("supertonic triad in root position" "iio")
    ("ii" "iio")
    ("iio" "iio")
    ("iia" "iio")
    ("iioa" "iio")
    ("supertonic triad in first inversion" "iiob")
    ("iib" "iiob")
    ("iiob" "iiob")
    ("supertonic triad in second inversion" "iioc")
    ("iic" "iioc")
    ("iioc" "iioc")
    
    ("mediant triad" "III")
    ("mediant triad in root position" "III")
    ("relative major triad" "III")
    ("relative major triad in root position" "III")
    ("III" "III")
    ("IIIa" "III")
    ("mediant triad in first inversion" "IIIb")
    ("relative major triad in first inversion" "IIIb")
    ("IIIb" "IIIb")
    ("mediant triad in second inversion" "IIIc")
    ("relative major triad in second inversion" "IIIc")
    ("IIIc" "IIIc")
    
    ("subdominant triad" "iv")
    ("subdominant triad in root position" "iv")
    ("iv" "iv")
    ("iva" "iv")
    ("subdominant triad in first inversion" "ivb")
    ("ivb" "ivb")
    ("subdominant triad in second inversion" "ivc")
    ("ivc" "ivc")
    
    ("dominant triad" "V")
    ("dominant triad in root position" "V")
    ("V" "V")
    ("Va" "V")
    ("dominant triad in first inversion" "Vb")
    ("Vb" "Vb")
    ("Vc" "dominant triad in second inversion")
    ("Vc" "Vc")
    
    ("submediant triad" "VI")
    ("submediant triad in root position" "VI")
    ("VIa" "VI")
    ("VI" "VI")
    ("submediant triad in first inversion" "VIb")
    ("VIb" "VIb")
    ("submediant triad in second inversion" "VIc")
    ("VIc" "VIc")
    
    ("diminished triad on the leading tone in root position" "viio")
    ("diminished triad on the leading tone" "viio")
    ("leading tone diminished triad" "viio")
    ("leading tone triad" "viio")
    ("vii" "viio")
    ("viia" "viio")
    ("viio" "viio")
    ("viioa" "viio")
    ("viio" "viio")
    ("diminished triad on the leading tone in first inversion" "viiob")
    ("leading tone diminished triad in first inversion" "viiob")
    ("leading tone triad in first inversion" "viiob")
    ("viib" "viiob")
    ("viiob" "viiob")
    ("diminished triad on the leading tone in second inversion" "viioc")
    ("leading tone diminished triad in second inversion" "viioc")
    ("leading tone triad in second inversion" "viioc")
    ("viic" "viioc")
    ("viioc" "viioc")))

#|
\noindent Example:
\begin{verbatim}
(a-list-in-b-list '(0 48 3 5 2 3) '(5 3 48))
--> 4
\end{verbatim}

\noindent This function takes two lists as arguments.
It returns the number of elements in the first list
that are contained in the second list. |#

(defun a-list-in-b-list
       (a-list b-list &optional (n 0))
  (if (null a-list) n
    (if (find (first a-list) b-list :test #'equalp)
      (a-list-in-b-list (rest a-list) b-list (+ n 1))
      (a-list-in-b-list (rest a-list) b-list n))))

#|
\noindent Example:
\begin{verbatim}
(setq
 path&name
 (merge-pathnames
  (make-pathname
   :directory '(:relative "C@merata2014" "misc")
   :name "dowland_denmark_galliard" :type "krn")
  *MCStylistic-MonthYear-data-path*))
(setq question-string "perfect cadence")
(setq
 point-set (kern-file2dataset-by-col path&name))
(setq
 ontimes-signatures
 (kern-file2ontimes-signatures path&name))
(cadence-time-intervals
 question-string point-set ontimes-signatures)
--> ((8 12))
\end{verbatim}

\noindent This function uses the function
\nameref{fun:HarmAn->roman} to create a Roman
numeral labelling for an input point set. Bigrams in
this labelling are used to identify certain types of
cadence queries, which can be specified as the first
argument. Time windows in which the cadences occur
are returned. |#

(defun cadence-time-intervals
       (question-string point-set &optional
        (ontimes-signatures '((1 4 4)))
        ; Define the different cadence possibilities.
        (cadence-types
         (if (search "cadence" question-string)
           '(("perfect" ("V" "I"))
             ("authentic" ("V" "I"))
             ("perfect" ("V" "i"))
             ("authentic" ("V" "i"))
             ("perfect" ("V7" "I"))
             ("authentic" ("V7" "I"))
             ("perfect" ("V7" "i"))
             ("authentic" ("V7" "i"))
             ("imperfect" ("I" "V"))
             ("half" ("I" "V"))
             ("imperfect" ("i" "V"))
             ("half" ("i" "V"))
             ("interrupted" ("V" "vi"))
             ("deceptive" ("V" "vi"))
             ("interrupted" ("V" "VI"))
             ("deceptive" ("V" "VI"))
             ("interrupted" ("V7" "vi"))
             ("deceptive" ("V7" "vi"))
             ("interrupted" ("V7" "VI"))
             ("deceptive" ("V7" "VI"))
             ("plagal" ("IV" "I"))
             ("plagal" ("iv" "i")))))
        (chord-templates
         *chord-templates-pardo&birmingham*)
        (major-templates *roman-major-templates-p&b*)
        (minor-templates *roman-minor-templates-p&b*)
        (key-profiles *Aarden-key-profiles*)
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3)
        ; Include ontimes in the ontime-signatures.
        (ontimes-signatures
         (if cadence-types
           (append-ontimes-to-time-signatures
            ontimes-signatures)))
        ; Remove word cadence from question string.
        (question-string
         (replace-all
          question-string " cadence" ""
          :test #'string=))
        ; Define type of cadence we are looking for.
        (target-cadences
         (if cadence-types
           (nth-list-of-lists
            1
            (nth-list
             (positions
              question-string
              (nth-list-of-lists 0 cadence-types)
              #'string=)
             cadence-types))))
        ; Do some chord labelling.
        (roman
         (if target-cadences
           (HarmAn->roman
            point-set chord-templates major-templates
            minor-templates key-profiles ontime-index
            MNN-index MPN-index duration-index)))
        ; Create bigrams from the labels.
        (roman-bigrams
         (if target-cadences
           (firstn-list
            2 (nth-list-of-lists 0 roman))))
        #| Return the indices of each target cadence
        in the bigrams. |#
        (candidate-idxs
         (if target-cadences
           (sort
            (append-list
             (loop for element in target-cadences
               collect
               (positions
                element roman-bigrams #'equal)))
            #'<)))
        #| Find the time signature in operation for
        the ontime of the second chord in each
        candidate cadence. If it is 4-4, we will only
        allow the second chord to begin on beats 1 or
        3. It would be possible to introduce 
        similar rules for other time signatures as
        well. |#
        (sigs-in-operation
         (if candidate-idxs
           (mapcar
            #'(lambda (x)
                (row-of-max-ontime<=ontime-arg
                 (first (second (nth (+ x 1) roman)))
                 ontimes-signatures))
            candidate-idxs)))
        #| Restrict candidate-idxs to candidates
        where the onbeat of the second chord conforms
        to the rules. |#
        (candidate-idxs
         (if candidate-idxs
           (append-list
            (loop for i from 0
              to (- (length candidate-idxs) 1)
              collect
              (if (equalp
                   (subseq
                    (nth i sigs-in-operation) 1 3)
                   '(4 4))
                ; Checking 4-4 instance.
                (if (find
                     (second
                      (bar&beat-number-of-ontime
                       (first
                        (second
                         (nth
                          (+
                           (nth i candidate-idxs) 1)
                          roman)))
                       ontimes-signatures))
                     ; List of permissible beats. 
                     '(1 3))
                  (list (nth i candidate-idxs)))
                ; No rules for other time sigs yet:
                (list (nth i candidate-idxs))))))))
  ; Get ontime/offtimes for any remaining candidates.
  (mapcar
   #'(lambda (x)
       (list
        (first (second (nth x roman)))
        (second (second (nth (+ x 1) roman)))))
   candidate-idxs))

#|
\noindent Example:
\begin{verbatim}
(chord-index2MNN-mod12&class 37)
--> (1 3)
\end{verbatim}

\noindent This function takes an index in the variable
*chord-templates-p&b&min7ths* as argument. It can be
seen from this variable that there are six different
classes of chord template (0, major triad; 1, dom7;
2, minor triad; 3, fully diminished 7th; 4, half
diminished 7th; 5, diminished triad; 6, minor 7th).
All classes but one have unambiguous roots. For
example, if the index is 4, we know that the 5th (5 =
4 + 1) element of the list is (5 9 0), and that this
is a major triad with root F. This function converts
the index into a pair consisting of root (MIDI note
number modulo 12) and class (as listed above). The
ambiguity of a fully diminished 7th chord can be
resloved by context, using another function. |#

(defun chord-index2MNN-mod12&class (x)
  (if (and (>= x 0) (<= x 11)) (list x 0)
    (if (and (>= x 12) (<= x 23)) (list (- x 12) 1)
      (if (and (>= x 24) (<= x 35)) (list (- x 24) 2)
        (if (and (>= x 36) (<= x 38))
          (list (- x 36) 3)
          (if (and (>= x 39) (<= x 50))
            (list (- x 39) 4)
            (if (and (>= x 51) (<= x 62))
              (list (- x 51) 5)
              (if (and (>= x 63) (<= x 74))
                (list (- x 63) 6)))))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 point-set
 '((0 50 54 1/2 3) (1/2 57 58 1/2 3) (1 53 56 1/2 3)
   (3/2 57 58 1/2 3) (2 50 54 1 3) (2 69 65 1 2)
   (5/2 53 56 1/2 3) (3 55 57 1/2 3) (3 70 66 3/2 2)
   (7/2 58 59 1/2 3) (4 52 55 1/2 3) (9/2 55 57 1/2 3)
   (9/2 69 65 1/2 2) (5 49 53 1/2 3) (5 67 64 1/4 2)
   (21/4 70 66 1/4 2) (11/2 52 55 1/2 1)
   (11/2 69 65 1/2 1) (6 49 53 1/2 3) (6 67 64 1 2)))
(setq question-string "chord of D minor")
(chord-time-intervals question-string point-set)
--> ((5/2 3))
(setq question-string "chord C#3, G4")
(chord-time-intervals question-string point-set)
--> ((5 21/4) (6 13/2))
(setq question-string "chord E, A")
(chord-time-intervals question-string point-set)
--> ((11/2 6))
(setq question-string "quaver note chord")
(chord-time-intervals question-string point-set)
--> ((2 5/2) (9/2 5) (11/2 6))
(setq
 question-string "quaver note chord in the left hand")
(chord-time-intervals question-string point-set)
--> ((11/2 6))

(setq
 point-set
 '((15 60 60 1/2 1) (15 64 62 1/2 1) (15 72 67 1/2 0)
   (31/2 60 60 1/2 1) (31/2 64 62 1/2 1)
   (31/2 76 69 1/4 0) (63/4 74 68 1/4 0)
   (16 60 60 1/2 1) (16 64 62 1/2 1) (16 76 69 1/2 0)
   (33/2 60 60 1/2 1) (33/2 64 62 1/2 1)
   (33/2 79 71 1/4 0) (67/4 77 70 1/4 0)
   (17 60 60 1/2 1) (17 64 62 1/2 1) (17 79 71 1/2 0)
   (35/2 60 60 1/2 1) (35/2 64 62 1/2 1)
   (35/2 82 73 1/4 0) (71/4 81 72 1/4 0)
   (18 53 56 1/2 1) (18 60 60 1/2 1) (18 64 62 1/2 1)
   (18 81 72 2 0) (37/2 53 56 1/2 1) (37/2 60 60 1/2 1)
   (37/2 64 62 1/2 1) (19 53 56 1/2 1)))
(setq
 question-string "sixteenth note chord Bb, C, E")
(chord-time-intervals question-string point-set)
--> ((35/2 71/4))
\end{verbatim}

\noindent This function takes a natural language query
as its first argument and a point-set representation of
a music excerpt as its second argument. The third
(optional) argument consists of staff and clef names.
The function parses the query for mention of a chord
(e.g., `C major chord', `chord of D minor',
`sixteenth note chord Bb, C, E') and then extracts
instances of this chord from the point set, returning
the time intervals at which they occur. Chord names 
(e.g., C minor) are mapped to chord templates (e.g., C,
E$\flat$, G) using the variable
*chord-name-template-assoc-p\&b\&min7ths*.

The function can handle pitches and pitch classes, as
well as restricting extracted chords to particular
durations or searching on duration alone. If the
excerpt contains pitches/pitch classes specified in the
query as well as some extras, then such chords will
still be returned. If a chord occurs over several
segments (because other notes come and go), then
several adjoining time windows will be returned. |#

(defun chord-time-intervals
       (question-string point-set &optional
        (staff&clef-names
         '(("left hand" "bass clef")
           ("right hand" "treble clef")))
        (chord-name-template-assoc
         *chord-name-template-assoc-p&b&min7ths*)
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3) (staff-index 4)
        (chordp (search "chord" question-string))
        (duration-qualifier
         (if chordp
           (duration-string2numeric question-string)))
        (question-string
         (if duration-qualifier
           (edit-out-duration-of-question-string
            question-string) question-string))
        (chord-probe
         (if chordp
           (replace-all
            (replace-all
             question-string "chord of " "")
            " chord" "")))
        (chord-template
         (if chordp
           (second
            (assoc
             chord-probe
             chord-name-template-assoc
             :test #'equalp))))
        (chord-pitch&octaves-orig
         (if (and chordp (not chord-template))
           (string-separated-string2list
            (if (search ", " question-string)
              ", " " ")
            (replace-all
             question-string "chord" ""))))
        (chord-pitch&octaves
         (if chord-pitch&octaves-orig
           (loop for str in chord-pitch&octaves-orig
             when
             (pitch&octave2MIDI-morphetic-pair str)
             collect
             (pitch&octave2MIDI-morphetic-pair str))))
        (chord-pitch-classes
         (if chord-pitch&octaves-orig
           (loop for str in chord-pitch&octaves-orig
             when
             (assoc
              str *pitch-class-MNN-MPN-mod-assoc*
              :test #'string=)
             collect
             (second
              (assoc
               str *pitch-class-MNN-MPN-mod-assoc*
               :test #'string=)))))
             
        #| Identify restrictions to any particular
        staves or voices, identify the numerical
        index of the relevant staff and edit the
        question string appropriately. |#
        (question-string&staff-idx
         (if chordp
           (modify-question-by-staff-restriction
            question-string staff&clef-names)))
        (staff-restriction
         (second question-string&staff-idx))
        (question-string
         (if staff-restriction
           (first question-string&staff-idx)
           question-string))
        (point-set
         (if staff-restriction
           (restrict-dataset-in-nth-to-xs
            point-set staff-index staff-restriction)
           point-set))
        (segments
         (if chordp
           (segments-strict
            point-set MNN-index duration-index)))
        #| Include the ending time for the segments,
        and get them in the best shape for testing for
        the presence of pitch and octaves or pitch
        classes. |#
        (segments
         (if segments
           (loop for i from 0 to (- (length segments) 2)
             collect
             (list
              (list
               (first (nth i segments))
               (first (nth (+ i 1) segments)))
              (cond
               ((> (length chord-template) 0)
                (loop for point in
                  (second (nth i segments)) collect
                  (mod (nth MNN-index point) 12)))
               ((> (length chord-pitch&octaves) 0)
                (loop for point in
                  (second (nth i segments)) collect
                  (list
                   (nth MNN-index point)
                   (nth MPN-index point))))
               ((> (length chord-pitch-classes) 0)
                (loop for point in
                  (second (nth i segments)) collect
                  (list
                   (mod (nth MNN-index point) 12)
                   (mod
                    (+
                     (nth MPN-index point) 3) 7))))
               ; Test for quaver (eg) chords in general
               ((numberp chordp)
                (loop for point in
                  (second (nth i segments)) collect
                  (list
                   (nth ontime-index point)
                   (nth duration-index point))))
               
               )
              ))))
        (relevant-time-intervals
         (cond
          ((> (length chord-template) 0)
           (loop for seg in segments when
             (test-all-true
              (loop for mnn-m12 in chord-template
                collect
                (if (find
                     mnn-m12 (second seg)) t nil)))
             collect (first seg)))
          ((> (length chord-pitch&octaves) 0)
           (loop for seg in segments when
             (test-all-true
              (loop for p&o in chord-pitch&octaves
                collect
                (if (find
                     p&o (second seg) :test #'equalp)
                  t nil)))
             collect (first seg)))
          ((> (length chord-pitch-classes) 0)
           (loop for seg in segments when
             (test-all-true
              (loop for pc in chord-pitch-classes
                collect
                (if (find
                     pc (second seg) :test #'equalp)
                  t nil)))
             collect (first seg)))
          ((numberp chordp)
           (loop for seg in segments
             when
             (and
              (> (length (second seg)) 1)
              (equalp
               (length
                (remove-duplicates
                 (second seg) :test #'equalp)) 1))
             collect (first seg)))
          
          ))
        )
  (progn
    question-string ; Just to avoid warning msg.
    (if duration-qualifier
        (loop for int in relevant-time-intervals
          when
          ; Was equalp but needs rethinking.
          (<=
           (- (second int) (first int)) duration-qualifier)
          collect int)
        relevant-time-intervals)))

#|
\noindent Example:
\begin{verbatim}
(HarmAn->
 '((15 54 56 1 3) (15 62 61 1/2 2) (15 69 65 1 1)
   (15 74 68 1 0) (31/2 60 60 1/2 2) (16 55 57 1 3)
   (16 59 59 1 2) (16 67 64 1 1) (16 74 68 1 0)
   (17 57 58 1 3) (17 60 60 1 2) (17 66 63 1 1)
   (17 74 68 1 0) (18 59 59 1 3) (18 62 61 1 2)
   (18 67 64 1 1) (18 74 68 1 0) (19 52 55 1 3)
   (19 64 62 1 2) (19 67 64 1 1) (19 71 66 1/2 0)
   (39/2 72 67 1/2 0) (20 47 52 1 3) (20 62 61 1 2)
   (20 67 64 1 1) (20 74 68 1 0) (21 48 53 1 3)
   (21 64 62 1 2) (21 69 65 1/2 1) (21 72 67 1/2 0)
   (43/2 67 64 1/2 1) (43/2 71 66 1/2 0)
   (22 50 54 1 3) (22 57 58 1 2) (22 66 63 1 1)
   (22 69 65 1 0)))
--> ((15 2 1 1 8) (16 7 0 1 4) (17 2 1 1 4)
     (18 7 0 1 4) (19 4 2 1/2 4) (39/2 0 0 1/2 4)
     (20 7 0 1 4) (21 9 2 1/2 4) (43/2 0 0 1/2 2)
     (22 2 0 1 4))
\end{verbatim}

\noindent This function is an implementation of the
forwards-running HarmAn algorithm described by
\cite{pardo2002}. The format of the output is a chord
dataset, where the first dimension is ontime, the
second dimension is the MIDI note number modulo 12 of
the root of the chord, the third dimension is the
class of the chord (0, major triad; 1, dom7; 2, minor
triad; 3, fully diminished 7th; 4, half diminished
7th; 5, diminished triad; 6, minor 7th), the fourth
dimension is the duration of the chord, and the
fifth dimension contains the score as assigned by the
HarmAn algorithm (a large weight suggests that the
chord was labelled unambiguously---a small weight
suggests otherwise). |#

(defun HarmAn->
       (dataset &optional (MNN-index 1)
        (duration-index 3)
        (chord-templates
         *chord-templates-pardo&birmingham*)
        (segments
         (segments-strict
          dataset MNN-index duration-index)))
  (resolve-dim7s
   (labelled-listed-segments2datapoints
    (HarmAn->labelling segments chord-templates))))

#|
\noindent Example:
\begin{verbatim}
(HarmAn->labelling
 '((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)))
   (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)))
   (3/2 ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2)))
   (2 ((2 59 59 1) (2 65 63 1) (2 79 71 1)))
   (3 ((3 60 60 1) (3 64 62 1) (3 79 71 1)))))
--> ((((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)))
       (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)))
       (3/2
        ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2))))
      (6 0))
     (((2 ((2 59 59 1) (2 65 63 1) (2 79 71 1))))
      (2 19))
     (((3 ((3 60 60 1) (3 64 62 1) (3 79 71 1))))
      (3 0)))
\end{verbatim}

\noindent This function is a partial implementation of
the forwards-running HarmAn algorithm described by
\cite{pardo2002}. It is partial in the sense that
further functions are required to produce chord
datapoints rather than labelled segments (see the
function labelled-listed-segments2datapoints) and
resolve the ambiguity of diminished 7ths (see the
function resolve-dim-7s). |#

(defun HarmAn->labelling
       (segments &optional
        (chord-templates
         *chord-templates-pardo&birmingham*)
        (listed-segs-max-argmax-pairs nil)
        (current-segment-max-argmax
         (if segments
           (max-argmax-of-segment-scores
            (mod-list
             (nth-list-of-lists
              1 (second (first segments))) 12)
            chord-templates)))
        (last-segments-max-argmax
         (if listed-segs-max-argmax-pairs
           (second
            (my-last listed-segs-max-argmax-pairs))
           (list 0)))
        (combined-segments-max-argmax
         (if segments
           (max-argmax-of-segments-score
            (append
             (first
              (my-last listed-segs-max-argmax-pairs))
             (list (first segments)))
            chord-templates) (list 0))))
  (if (null segments) listed-segs-max-argmax-pairs
    (if (<
         (first combined-segments-max-argmax)
         (+
          (first last-segments-max-argmax)
          (first current-segment-max-argmax)))
      #| Label separately. |#
      (HarmAn->labelling
       (rest segments) chord-templates
       (append
        listed-segs-max-argmax-pairs
        (list
         (list
          (list (first segments))
          current-segment-max-argmax))))
      #| Label combined. |#
      (HarmAn->labelling
       (rest segments) chord-templates
       (append
        (butlast listed-segs-max-argmax-pairs)
        (list
         (list
          (append
           (first
            (my-last listed-segs-max-argmax-pairs))
           (list (first segments)))
          combined-segments-max-argmax)))))))

#|
\noindent Example:
\begin{verbatim}
(HarmAn->roman
 '((0 51 55 1/2 3) (0 58 59 1 2) (0 63 62 1 1)
   (0 67 64 1 0) (1/2 50 54 1/2 3) (1 48 53 1/2 3)
   (1 60 60 1 2) (1 63 62 1 1) (1 68 65 1 0)
   (3/2 51 55 1/2 3) (2 50 54 1/2 3) (2 53 56 1 2)
   (2 65 63 1/2 1) (2 70 66 1 0) (5/2 48 53 1/2 3)
   (5/2 63 62 1/2 1) (3 46 52 1/2 3) (3 55 57 1 2)
   (3 62 61 1 1) (3 70 66 1 0) (7/2 50 54 1/2 3)
   (4 48 53 1/2 3) (4 55 57 1 2) (4 63 62 1 0)
   (4 63 62 1 1) (9/2 46 52 1/2 3) (5 44 51 1/2 3)
   (5 60 60 1/2 2) (5 63 62 1/2 1) (5 65 63 1 0)
   (11/2 46 52 1/2 3) (11/2 58 59 1/2 2)
   (11/2 62 61 1/2 1) (6 39 48 2 3) (6 58 59 2 2)
   (6 63 62 2 1) (6 67 64 2 0))
 *chord-templates-p&b&min7ths*)
--> (("I" (0 1)) ("IVb" (1 2)) ("Vb" (2 5/2))
     ("II7c" (5/2 3)) ("iiib" (3 4)) ("vi7d" (4 5))
     ("ii7b" (5 11/2)) ("V" (11/2 6)) ("I" (6 8))).
\end{verbatim}

\noindent This function segments and labels chords in
some input piece of music. The algorithm is based on
an implementation of HarmAn by \cite{pardo2002}.
HarmAn compares input triples of ontimes, MIDI note
numbers, and durations to predefined chord templates,
and performs segmentation and segment labelling on
this basis. The labels are absolute, for instance
(15 2 1 1 8) means that a chord begins on ontime 15,
has root 2 modulo 12 (i.e., D), is of type 1 (dom7
chord), lasts 1 beat, and was assigned to this chord
template with strength 8.

While useful, this output does not provide a
functional-harmonic analysis. I programmed some extra
steps to estimate the overall key of the input piece,
using the Krumhansl-Schmuckler key-finding algorithm
\cite{krumhansl1990}, and then to caluclate relative
(or functional) harmonic labels by combining the
estimate of overall key with the absolute labels
output by HarmAn-$>$. For instance, if the overall
key is G major, and HarmAn-$>$ output the label D
dom7, then my code would convert this to V7. I have
taken care to make sure the labelling of diminished
7th chords is correct. The overall program is
referred to as HarmAn-$>$roman. It does not handle
secondary keys, but might be adapted to do so using
a slice through a keyscape \cite{sapp2005}. |#

(defun HarmAn->roman
       (point-set &optional
        (chord-templates
         *chord-templates-p&b&min7ths*)
        (major-templates *roman-major-templates-p&b*)
        (minor-templates *roman-minor-templates-p&b*)
        (key-profiles *Aarden-key-profiles*)
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3)
        ; Estimate overall key and centre point set.
        (fifth-steps-mode
         (fifth-steps-mode
          point-set key-profiles
          ontime-index MNN-index duration-index))
        (point-set-centred
         (centre-dataset
          fifth-steps-mode point-set MNN-index
          MPN-index))
        #| Get segments and apply nearly all of the
        HarmAn-> algorithm. |# 
        (segments
         (segments-strict
          (second point-set-centred)
          MNN-index duration-index))
        (pre-roman
         (HarmAn->labelling
          segments chord-templates))
        #| Unique-equalp the combined segments, so
        that notes are not double counted. In this
        list, we have (combined) segment ontime,
        unique points in combined segment, and
        (strength, label index) pair. |#
        (pre-roman2
         (mapcar
          #'(lambda (x)
              (list
               (first (first (first x)))
               (orthogonal-projection-unique-equalp
                (append-list
                 (nth-list-of-lists 1 (first x)))
                (constant-vector 1 4))
               (second x)))
          pre-roman))
        #| Find lowest note in the segment that is
        also a chord tone. If this lowest note occurs
        at the beginning of the segment, or lasts for
        at least 50% of the segment, it is used to
        calculate the inversion. |#
        (lowest-note-candidates
         (mapcar
          #'(lambda (x)
              (restrict-point-set-to-MNN-mod12
               (second x)
               (nth
                (second (third x))
                chord-templates) MNN-index))
          pre-roman2))
        ; Apply lowest (min) operation.
        (candidate-idxs
         (mapcar
          #'(lambda (x)
              (second
               (min-argmin 
                (nth-list-of-lists MNN-index x))))
          lowest-note-candidates))
        ; Get the (combined) segment on/offtimes.
        (segment-on-offs
         (firstn-list
          2
          (append
           (nth-list-of-lists 0 pre-roman2)
           (list (first (my-last segments))))))
        #| Determine the MNN mod 12 of each lowest
        chord tone, to work out whether the chord is
        in root position, first, second, or third
        inversion. In the next operation, these are
        positioned in the chord-templates to
        determine whether "b", "c", or "d" strings
        should be appended to indicate inversions. |#
        (MNNs-mod12-of-lowest-notes
         (mapcar
          #'(lambda (x y z)
              (if (or
                   ; Either begins at/before segment.
                   (<=
                    (nth ontime-index (nth y x))
                    (first z))
                   ; Or dur over half of segment dur.
                   (>=
                    (nth duration-index (nth y x))
                    (/ (- (second z) (first z)) 2)))
                (mod (nth MNN-index (nth y x)) 12)))
          lowest-note-candidates candidate-idxs
          segment-on-offs))
        (inversion-label-idxs
         (mapcar
          #'(lambda (x y)
              (position x (nth y chord-templates)))
          MNNs-mod12-of-lowest-notes
          (nth-list-of-lists
           1 (nth-list-of-lists 2 pre-roman2))))
        #| Join Roman numerals with inversion
        indicators. |#
        (roman&inversion
         (mapcar
          #'(lambda (x y z)
              (list
               (concatenate
                'string
                (if (equalp
                     (second fifth-steps-mode) 0)
                  (nth
                   (nth 1 (nth 2 x)) major-templates)
                  (nth
                   (nth 1 (nth 2 x)) minor-templates))
                (if (and (numberp y) (> y 0))
                  (nth y '("" "b" "c" "d"))))
               (list (first z) (second z))))
          pre-roman2 inversion-label-idxs
          segment-on-offs)))
  #| Refine diminshed-7th chord labels before final
  output. |#
  (mapcar
   #'(lambda (x y z)
       (list
        (if (numberp
             (search
              "viio7" (first x) :test #'string=))
          (resolve-dim7s-roman
           (second y)
           (nth (second (third y)) chord-templates)
           z fifth-steps-mode)
          (identity (first x)))
        (second x)))
   roman&inversion pre-roman2
   MNNs-mod12-of-lowest-notes))

#|
\noindent Example:
\begin{verbatim}
(labelled-listed-segments2datapoints
 '((((22
      ((22 50 54 1 3 23 97) (22 57 58 1 2 23 98)
       (22 66 63 1 1 23 99) (22 69 65 1 0 23 100)))
     (23
      ((23 47 52 1 3 24 101) (23 59 59 1 2 24 102)
       (23 66 63 1 1 24 103) (23 74 68 1 0 24 104))))
    (8 74))
   (((24
      ((24 52 55 1/2 3 49/2 105)
       (24 59 59 1/2 2 49/2 106)
       (24 67 64 1/2 1 49/2 107)
       (24 72 67 1 0 25 108))))
    (2 0))
   (((49/2
      ((49/2 54 56 1/2 3 25 109)
       (49/2 57 58 1/2 2 25 110)
       (49/2 69 65 1 1 51/2 111)
       (24 72 67 1 0 25 108))))
    (4 57))
   (((25
      ((25 55 57 1 3 26 112) (25 59 59 1/2 2 51/2 113)
       (49/2 69 65 1 1 51/2 111)
       (25 71 66 1 0 26 114)))
     (51/2
      ((25 55 57 1 3 26 112) (51/2 60 60 1/2 2 26 115)
       (51/2 67 64 1 1 53/2 116)
       (25 71 66 1 0 26 114)))
     (26
      ((26 50 54 1 3 27 117) (26 62 61 1 2 27 118)
       (51/2 67 64 1 1 53/2 116)
       (26 69 65 1 0 27 119)))
     (53/2
      ((26 50 54 1 3 27 117) (26 62 61 1 2 27 118)
       (53/2 64 62 1/2 1 27 120)
       (26 69 65 1 0 27 119))))
    (8 67))
   (((27
      ((27 51 54 1 3 28 121) (27 60 60 1 2 28 122)
       (27 66 63 1 1 28 123) (27 69 65 1 0 28 124))))
    (4 36))
   (((28
      ((28 52 55 1/2 3 57/2 125) (28 59 59 1 2 29 126)
       (28 67 64 2 1 30 128)
       (28 67 64 1/2 0 57/2 127))))
    (4 28))
   (((57/2
      ((57/2 54 56 1/2 3 29 129) (28 59 59 1 2 29 126)
       (28 67 64 2 1 30 128)
       (57/2 69 65 1/2 0 29 130))))
    (1 23))
   (((29
      ((29 55 57 1/2 3 59/2 131) (29 64 62 1 2 30 132)
       (28 67 64 2 1 30 128)
       (29 71 66 1/2 0 59/2 133))))
    (4 28))
   (((59/2
      ((59/2 57 58 1/2 3 30 134) (29 64 62 1 2 30 132)
       (28 67 64 2 1 30 128)
       (59/2 72 67 1/2 0 30 135))))
    (4 72))
   (((30
      ((30 59 59 1 3 31 136) (30 62 61 1 2 31 137)
       (30 66 63 1 1 31 138) (30 74 68 1 0 31 139)))
     (31 NIL))
    (4 35))))
--> ((22 11 6 2 8) (24 0 0 1/2 2) (49/2 6 5 1/2 4)
     (25 4 6 2 8) (27 0 3 1 4) (28 4 2 1/2 4)
     (57/2 11 1 1/2 1) (29 4 2 1/2 4) (59/2 9 6 1/2 4)
     (30 11 2 1 4))
\end{verbatim}

\noindent This function takes labelled listed
segments as an argument. It converts these to
datapoints where the first dimension is ontime, the
second dimension is the MIDI note number modulo 12 of
the root of the chord, the third dimension is the
class of the chord (0, major triad; 1, dom7; 2, minor
triad; 3, fully diminished 7th; 4, half diminished
7th; 5, diminished triad; 6, minor 7th), the fourth
dimension is the duration of the chord, and the
fifth dimension contains the score as assigned by the
HarmAn algorithm (a large weight suggests that the
chord was labelled unambiguously---a small weight
suggests otherwise). |#

(defun labelled-listed-segments2datapoints
       (a-list &optional (b-list nil)
        (labelled-segment-list (first a-list))
        (MNN-mod12&class
         (if labelled-segment-list
           (chord-index2MNN-mod12&class
            (second
             (second labelled-segment-list))))))
  (if (null (second a-list))
    (append
     b-list
     (list
      (list
       #| Ontime. |#
       (first (first (first labelled-segment-list)))
       #| MIDI note number modulo 12 of the root of
       the chord. |#
       (first MNN-mod12&class)
       #| Class of the chord. |#
       (second MNN-mod12&class)
       #| Duration of the chord. |#
       (-
        (first
         (my-last (first labelled-segment-list)))
        (first (first (first labelled-segment-list))))
       #| Score as assigned by the HarmAn
       algorithm. |#
       (first (second labelled-segment-list)))))
    (labelled-listed-segments2datapoints
     (rest a-list)
     (append
      b-list
      (list
       (list
        #| Ontime. |#
        (first (first (first labelled-segment-list)))
        #| MIDI note number modulo 12 of the root of
        the chord. |#
        (first MNN-mod12&class)
        #| Class of the chord. |#
        (second MNN-mod12&class)
        #| Duration of the chord. |#
        (-
         (first (first (first (second a-list))))
         (first
          (first (first labelled-segment-list))))
        #| Score as assigned by the HarmAn
        algorithm. |#
        (first (second labelled-segment-list))))))))

#|
\noindent Example:
\begin{verbatim}
(max-argmax-of-segment-scores '(0 3 5 7 5))
--> (2 17)
\end{verbatim}

\noindent This function takes a list of MIDI note
numbers modulo 12 as its only argument. It scores this
list using the function score-segment-against-
template, for each chord template in the variable
*chord-template*, and returns the index of the chord
that produces the maximum score. If there is a tie,
the index of the first such chord is returned. |#

(defun max-argmax-of-segment-scores
       (MNNs-mod12 &optional
        (chord-templates
         *chord-templates-pardo&birmingham*)
        (segment-scores
         (mapcar
          #'(lambda (x)
              (score-segment-against-template
               MNNs-mod12 x)) chord-templates)))
  (max-argmax segment-scores))

#|
\noindent Example:
\begin{verbatim}
(max-argmax-of-segments-score
 '((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)))
   (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)))
   (3/2 ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2)))))
--> (6 0)
\end{verbatim}

\noindent This function a list of segments its only
argument. The segments are appended for scoring
purposes. A score is given according to the function
score-segment-against-template, for each chord
template in the variable chord-templates, and the
index of the chord that produces the maximum score is
returned, as well as the maximum score. If there is a
tie, the index of the first such chord is returned. |#

(defun max-argmax-of-segments-score
       (segments &optional
        (chord-templates
         *chord-templates-pardo&birmingham*)
        (MNNs-mod12 (segments2MNNs-mod12 segments))
        (segment-scores
         (mapcar
          #'(lambda (x)
              (score-segment-against-template
               MNNs-mod12 x)) chord-templates)))
  (max-argmax segment-scores))

#|
\noindent Example:
\begin{verbatim}
(minimal-segment-scores
 '((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)))
   (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)))
   (3/2 ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2)))
   (2 ((2 59 59 1) (2 65 63 1) (2 79 71 1)))
   (3 ((3 60 60 1) (3 64 62 1) (3 79 71 1)))))
--> ((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)) (2 0))
     (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)) (0 0))
     (3/2
      ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2)) (2 0))
     (2 ((2 59 59 1) (2 65 63 1) (2 79 71 1)) (2 19))
     (3 ((3 60 60 1) (3 64 62 1) (3 79 71 1)) (3 0)))
\end{verbatim}

\noindent This function takes a list of MIDI note
numbers modulo 12 as its only argument. It scores this
list using the function
score-segment-against-template, for each chord
template in the variable *chord-template*, and returns
the index of the chord that produces the maximum
score. If there is a tie, the index of the first such
chord is returned. |#

(defun minimal-segment-scores (minimal-segments)
  (mapcar
   #'(lambda (x)
       (append
        x
        (list
         (max-argmax-of-segment-scores
          (mod-list
           (nth-list-of-lists 1 (second x)) 12)))))
   minimal-segments))

#|
\noindent Example:
\begin{verbatim}
(MNN-mod12&class2chord-index '(10 4))
--> 49
(MNN-mod12&class2chord-index '(11 6))
--> 74
\end{verbatim}

\noindent This function takes a pair consisting of
root (MIDI note number modulo 12) and chord class
(listed above) as argument. It converts this pair into
the index in the variable
*chord-templates-p&b&min7ths*. The situation is
complicated slightly by the fourth category (of
fully-diminished 7th chords), which contains only 3
chords. |#

(defun MNN-mod12&class2chord-index
       (MNN-mod12&class &optional
        (pc (first MNN-mod12&class))
        (class (second MNN-mod12&class)))
  (if (<= class 2) (+ (* 12 class) pc)
    (if (equalp class 3) (+ (* 12 class) (mod pc 3))
      (- (+ (* 12 class) pc) 9))))

#|
\noindent Example:
\begin{verbatim}
(resolve-dim7s
 '((25 4 6 2 8) (27 0 3 1 4) (28 4 2 1/2 4)))
--> ((25 4 6 2 8) (27 3 3 1 4) (28 4 2 1/2 4))
\end{verbatim}

\noindent This function takes a chord dataset as an
argument, where usually the first dimension is ontime,
the second dimension is the MIDI note number modulo 12
of the root of the chord, the third dimension is the
class of the chord (0, major triad; 1, dom7; 2, minor
triad; 3, fully diminished 7th; 4, half diminished
7th; 5, diminished triad; 6, minor 7th), the fourth
dimension is the duration of the chord, and the
fifth dimension contains the score as assigned by the
HarmAn algorithm (a large weight suggests that the
chord was labelled unambiguously---a small weight
suggests otherwise). The function searches for any
chord datapoints of class 3 (fully diminished). If it
finds such a chord datapoint, it looks at the MIDI
note number modulo 12 of the subsequent chord, $y$. If
$y - 1 \mod 12$ is a member of the previous chord,
then $y - 1 \mod 12$ becomes its root. Otherwise the
root is unchanged. Thus, this function resolves the
spelling of ambiguous fully diminished 7th chords. |#

(defun resolve-dim7s
       (chord-dataset &optional (MNN-mod12-index 1)
        (class-index 2) (growing-list nil)
        (first-chord (first chord-dataset))
        (current-MNN-mod12
         (nth MNN-mod12-index first-chord))
        (candidate-MNN-mod12
         (if (second chord-dataset)
           (if (equalp
                (nth class-index first-chord) 3)
             (mod
              (-
               (nth
                MNN-mod12-index
                (second chord-dataset)) 1) 12))))
        (new-MNN-mod12
         (if candidate-MNN-mod12
           (if (find
                candidate-MNN-mod12
                (list
                 current-MNN-mod12
                 (mod (+ current-MNN-mod12 3) 12)
                 (mod (+ current-MNN-mod12 6) 12)
                 (mod (+ current-MNN-mod12 9) 12)))
             candidate-MNN-mod12))))
  (if (null (second chord-dataset))
    (append growing-list (last chord-dataset))
    (resolve-dim7s
     (rest chord-dataset) MNN-mod12-index class-index
     (append
      growing-list
      (list
       (append
        (subseq first-chord 0 MNN-mod12-index)
        (if new-MNN-mod12
          (list new-MNN-mod12)
          (list current-MNN-mod12))
        (subseq
         first-chord
         (+ MNN-mod12-index 1) class-index)
        (subseq first-chord class-index)))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 point-set2
 '((16 -3 -2 2) (17 3 2 1) (17 6 3 1) (17 12 7 4/3)))
(setq template-MNNs-mod12 '(0 3 6 9))
(setq MNN-mod12-of-lowest-note 9)
(setq fifth-steps-mode '(6 5))
(resolve-dim7s-roman
 point-set2 template-MNNs-mod12
 MNN-mod12-of-lowest-note fifth-steps-mode)
--> "#vio7b".

(setq
 point-set2
 '((19 -5 -3 2) (20 1 1 1) (20 4 2 1) (20 10 6 4/3)))
(setq template-MNNs-mod12 '(1 4 7 10))
(setq MNN-mod12-of-lowest-note 7)
(setq fifth-steps-mode '(6 5))
(resolve-dim7s-roman
 point-set2 template-MNNs-mod12
 MNN-mod12-of-lowest-note
 fifth-steps-mode)
--> "vo7b".
\end{verbatim}

\noindent This function returns a label for spelling
a diminished 7th chord. The morphetic pitch numbers
passed to the function (in the third column of each
list in the input point set) are central to this
spelling process. In the above examples they are
expressed relative to a tonic that has MIDI note
number 0 and morphetic pitch number 0. |#

(defun resolve-dim7s-roman
       (point-set template-MNNs-mod12
        MNN-mod12-of-lowest-note fifth-steps-mode
        &optional (MNN-index 1)
        (MPN-index 2)
        #| Use template-MNNs-mod12 to get the
        candidate MPNs. |#
        (MPN-mod7-candidates
         (second
          (assoc
           template-MNNs-mod12
           '(
            ((0 3 6 9)
             ((0 2 4 6) (1 3 5 0) (3 5 0 2)
              (5 0 2 4)))
            ((1 4 7 10)
             ((0 2 4 6) (2 4 6 1) (4 6 1 3)
              (5 0 2 4)))
            ((2 5 8 11)
             ((1 3 5 0) (2 4 6 1) (4 6 1 3)
              (6 1 3 5))))
           :test #'equalp)))
        #| Restrict point set to chord tones and
        determine unique MPNs mod 7 of those chord
        tones. |#
        (point-set-r
         (restrict-point-set-to-MNN-mod12
          point-set template-MNNs-mod12 MNN-index))
        (unq-MPNs-mod7
         (remove-duplicates
          (mapcar
           #'(lambda (x)
               #| No plus 3 here (replaced with 0),
               because the MPNs have already been
               shifted so that C = 0). |#
               (mod (+ (nth MPN-index x) 0) 7))
           point-set-r)
          :test #'equalp))
        #| Use intersections over the candidates to
        find the best match, and convert the root of
        the diminished-7th chord to an MNN-MPN
        pair. |#
        (soln-idx
         (second
          (max-argmax
           (mapcar
            #'(lambda (x)
                (length
                 (intersection x unq-MPNs-mod7)))
            MPN-mod7-candidates))))
        (soln-MNN-MPN-pair
         (list
          (nth soln-idx template-MNNs-mod12)
          (first
           (nth soln-idx MPN-mod7-candidates))))
        #| Convert MNN-MPN-pair to a Roman numeral
        label with this lightweight call to assoc! |#
        (roman-label
         (if (equalp (second fifth-steps-mode) 0)
           (second
            (assoc
             soln-MNN-MPN-pair
             '(; C.
               ((10 0) "bbio7") ((11 0) "bio7")
               ((0 0) "io7") ((1 0) "#io7")
               ((2 0) "##io7")
               ; D.
               ((0 1) "bbiio7") ((1 1) "biio7")
               ((2 1) "iio7") ((3 1) "#iio7")
               ((4 1) "##iio7")
               ; E.
               ((2 2) "bbiiio7") ((3 2) "biiio7")
               ((4 2) "iiio7") ((5 2) "#iiio7")
               ((6 2) "##iiio7")
               ; F.
               ((3 3) "bbivo7") ((4 3) "bivo7")
               ((5 3) "ivo7") ((6 3) "#ivo7")
               ((7 3) "##ivo7")
               ; G.
               ((5 4) "bbvo7") ((6 4) "bvo7")
               ((7 4) "vo7") ((8 4) "#vo7")
               ((9 4) "##vo7")
               ; A.
               ((7 5) "bbvio7") ((8 5) "bvio7")
               ((9 5) "vio7") ((10 5) "#vio7")
               ((11 5) "##vio7")
               ; B.
               ((9 6) "bbviio7") ((10 6) "bviio7")
               ((11 6) "viio7") ((0 6) "#viio7")
               ((1 6) "##viio7")) :test #'equalp))
           (second
            (assoc
             soln-MNN-MPN-pair
             '(; C.
               ((10 0) "bbiiio7") ((11 0) "biiio7")
               ((0 0) "iiio7") ((1 0) "#iiio7")
               ((2 0) "##iiio7")
               ; D.
               ((0 1) "bbivo7") ((1 1) "bivo7")
               ((2 1) "ivo7") ((3 1) "#ivo7")
               ((4 1) "##ivo7")
               ; E.
               ((2 2) "bbvo7") ((3 2) "bvo7")
               ((4 2) "vo7") ((5 2) "#vo7")
               ((6 2) "##vo7")
               ; F.
               ((3 3) "bbvio7") ((4 3) "bvio7")
               ((5 3) "vio7") ((6 3) "#vio7")
               ((7 3) "##vio7")
               ; G.
               ((5 4) "bbviio7") ((6 4) "bviio7")
               ((7 4) "viio7") ((8 4) "#viio7")
               ((9 4) "##viio7")
               ; A.
               ((7 5) "bbio7") ((8 5) "bio7")
               ((9 5) "io7") ((10 5) "#io7")
               ((11 5) "##vio7")
               ; B.
               ((9 6) "bbiio7") ((10 6) "biio7")
               ((11 6) "iio7") ((0 6) "#iio7")
               ((1 6) "##iio7")) :test #'equalp))))
        #| Correct the inversion label according to
        how the chord has moved to through the
        possible MPN combinations. |#
        (inversion-label
         (if MNN-mod12-of-lowest-note
           (nth
            (mod
             (-
              (position
               MNN-mod12-of-lowest-note
               template-MNNs-mod12 :test #'equalp)
              soln-idx) 4)
            '("" "b" "c" "d")) "")))
  (concatenate 'string roman-label inversion-label))

#|
\noindent Example:
\begin{verbatim}
(restrict-point-set-to-MNN-mod12
 '((1 -8 -5 1) (1 -1 -1 1) (1 2 1 1) (1 8 4 1/2)
   (3/2 9 5 1/2) (2 -8 -5 1) (2 -4 -3 1) (2 2 1 1)
   (2 11 6 3/4) (11/4 5 3 1/4)) '(4 8 11 2))
--> ((1 -8 -5 1) (1 -1 -1 1) (1 2 1 1) (1 8 4 1/2)
     (2 -8 -5 1) (2 -4 -3 1) (2 2 1 1) (2 11 6 3/4)).
\end{verbatim}

\noindent This function returns only the points from
the input point set whose MIDI note numbers belong
to the second argument (a list of MIDI note numbers
modulo 12). |#

(defun restrict-point-set-to-MNN-mod12
       (point-set MNNs-mod12 &optional
        (MNN-index 1))
  (if (null point-set) ()
    (append
     (if (numberp
          (position
           (mod (nth MNN-index (first point-set)) 12)
           MNNs-mod12))
       (list (first point-set)))
     (restrict-point-set-to-MNN-mod12
      (rest point-set) MNNs-mod12 MNN-index))))
 
#|
\noindent Example:
\begin{verbatim}
(score-segment-against-template '(0 1 5 7 5) '(5 9 0))
--> 0
\end{verbatim}

\noindent This function takes two lists as arguments.
The first is a list of MIDI note numbers modulo 12,
and the second is a chord template. Three quantities
are calcuated: $P$, the number of MNNs that are
members of the chord template; $N$, the number of MNNs
that are not members of the chord template; $M$, the
number of elements of the chord template that are not
members of the list of MNNs. The value of
$P - (M + N)$ is returned. |#

(defun score-segment-against-template
       (MNNs-mod12 chord-template &optional
        (P
         (a-list-in-b-list MNNs-mod12 chord-template))
        (N (- (length MNNs-mod12) P))
        (M
         (-
          (length chord-template)
          (a-list-in-b-list
           chord-template MNNs-mod12))))
  (- P (+ M N)))

#|
\noindent Example:
\begin{verbatim}
(segments2MNNs-mod12
 '((0 ((0 60 60 2) (0 64 62 2) (0 72 67 1)))
   (1 ((0 60 60 2) (0 64 62 2) (1 74 68 1/2)))
   (3/2 ((0 60 60 2) (0 64 62 2) (3/2 76 69 1/2)))))
--> (0 4 0 0 4 2 0 4 4)
\end{verbatim}

\noindent This function takes a list of segments its
only argument. The MIDI note numbers of each segment
are mapped to modulo 12 and appended into one list. |#

(defun segments2MNNs-mod12 (segments)
  (if (null segments) ()
    (append
     (mod-list
      (nth-list-of-lists
       1 (second (first segments))) 12)
     (segments2MNNs-mod12 (rest segments)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "subdominant triad in first inversion")
(setq
 point-set
 '((0 51 55 1/2 3) (0 58 59 1 2) (0 63 62 1 1)
   (0 67 64 1 0) (1/2 50 54 1/2 3) (1 48 53 1/2 3)
   (1 60 60 1 2) (1 63 62 1 1) (1 68 65 1 0)
   (3/2 51 55 1/2 3) (2 50 54 1/2 3) (2 53 56 1 2)
   (2 65 63 1/2 1) (2 70 66 1 0) (5/2 48 53 1/2 3)
   (5/2 63 62 1/2 1) (3 46 52 1/2 3) (3 55 57 1 2)
   (3 62 61 1 1) (3 70 66 1 0) (7/2 50 54 1/2 3)
   (4 48 53 1/2 3) (4 55 57 1 2) (4 63 62 1 0)
   (4 63 62 1 1) (9/2 46 52 1/2 3) (5 44 51 1/2 3)
   (5 60 60 1/2 2) (5 63 62 1/2 1) (5 65 63 1 0)
   (11/2 46 52 1/2 3) (11/2 58 59 1/2 2)
   (11/2 62 61 1/2 1) (6 39 48 2 3) (6 58 59 2 2)
   (6 63 62 2 1) (6 67 64 2 0)))
(triad-time-intervals question-string point-set)
--> ((1 2))
\end{verbatim}

\noindent This function takes a string and a point
set as its compulsory arguments, where the string
may refer to a triad. It returns the time intervals in
the point set where the triad occurs. |#

(defun triad-time-intervals
       (question-string point-set &optional
        (major-key-triad-assoc
         *major-key-triad-assoc*)
        (minor-key-triad-assoc
         *minor-key-triad-assoc*)
        (chord-templates
         *chord-templates-p&b&min7ths*)
        (major-templates *roman-major-templates-p&b*)
        (minor-templates *roman-minor-templates-p&b*)
        (key-profiles *Aarden-key-profiles*)
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3)
        #| First of all, see if the question string
        contains any mention of triads. |#
        (probe-roman-numeral
         (second
          (assoc
           question-string
           (append
            major-key-triad-assoc
            minor-key-triad-assoc)
           :test #'string=)))
        #| Estimate overall key. This is useful for
        refining the probe Roman numeral defined
        above. For instance, if the question string
        contains "subdominant", then in a major
        piece/excerpt the probe ought to be "IV",
        whereas in a minor piece it ought to be
        "iv". |#
        (fifth-steps-mode
         (if probe-roman-numeral
           (fifth-steps-mode
            point-set key-profiles
            ontime-index MNN-index duration-index)))
        #| Determine if a triad is mentioned in the
        question string. |#
        (probe-roman-numeral
         (if probe-roman-numeral
           (second
            (assoc
             question-string
             (if (zerop (second fifth-steps-mode))
               major-key-triad-assoc
               minor-key-triad-assoc)
             :test #'string=))))
        ; Label the point set with Roman numerals.
        (point-set-labelled
         (if probe-roman-numeral
           (HarmAn->roman
            point-set chord-templates major-templates
            minor-templates key-profiles ontime-index
            MNN-index MPN-index duration-index))))
  (loop for i from 0 to
    (- (length point-set-labelled) 1) when
    (string=
     (first (nth i point-set-labelled))
     probe-roman-numeral)
    collect (second (nth i point-set-labelled))))

#|
\noindent Example:
\begin{verbatim}
(setq
 question-string
 "triad in first inversion")
(setq
 point-set
 '((0 51 55 1/2 3) (0 58 59 1 2) (0 63 62 1 1)
   (0 67 64 1 0) (1/2 50 54 1/2 3) (1 48 53 1/2 3)
   (1 60 60 1 2) (1 63 62 1 1) (1 68 65 1 0)
   (3/2 51 55 1/2 3) (2 50 54 1/2 3) (2 53 56 1 2)
   (2 65 63 1/2 1) (2 70 66 1 0) (5/2 48 53 1/2 3)
   (5/2 63 62 1/2 1) (3 46 52 1/2 3) (3 55 57 1 2)
   (3 62 61 1 1) (3 70 66 1 0) (7/2 50 54 1/2 3)
   (4 48 53 1/2 3) (4 55 57 1 2) (4 63 62 1 0)
   (4 63 62 1 1) (9/2 46 52 1/2 3) (5 44 51 1/2 3)
   (5 60 60 1/2 2) (5 63 62 1/2 1) (5 65 63 1 0)
   (11/2 46 52 1/2 3) (11/2 58 59 1/2 2)
   (11/2 62 61 1/2 1) (6 39 48 2 3) (6 58 59 2 2)
   (6 63 62 2 1) (6 67 64 2 0)))
(triad-inversion-time-intervals
 question-string point-set)
--> ((1 2) (2 5/2) (3 4) (5 11/2))
\end{verbatim}

\noindent This function takes a string and a point
set as its compulsory arguments, where the string
may refer to a type of triad inversion. It returns
the time intervals in the point set where the type of
triad inversion occurs. |#

(defun triad-inversion-time-intervals
       (question-string point-set &optional
        (triad-inversion-assoc
         '(("triad in root position" "a")
           ("root position triad" "a")
           ("triad in first inversion" "b")
           ("first inversion triad" "b")
           ("triad in second inversion" "c")
           ("second inversion triad" "c")
           ("triad in third inversion" "d")
           ("third inversion triad" "d")))
        (chord-templates
         *chord-templates-p&b&min7ths*)
        (major-templates *roman-major-templates-p&b*)
        (minor-templates *roman-minor-templates-p&b*)
        (key-profiles *Aarden-key-profiles*)
        (ontime-index 0) (MNN-index 1) (MPN-index 2)
        (duration-index 3)
        #| Determine if a triad is mentioned in the
        question string. |#
        (probe-triad-inversion
         (second
          (assoc
           question-string triad-inversion-assoc
           :test #'string=)))
        ; Label the point set with Roman numerals.
        (point-set-labelled
         (if probe-triad-inversion
           (mapcar
            #'(lambda (x)
                (list
                 (if (string=
                      (my-last-string (first x)) "b")
                   "b"
                   (if (string=
                        (my-last-string
                         (first x)) "c")
                     "c"
                     (if (string=
                          (my-last-string
                           (first x)) "d")
                       "d" "a")))
                 (second x)))
            (HarmAn->roman
             point-set chord-templates major-templates
             minor-templates key-profiles ontime-index
             MNN-index MPN-index duration-index)))))
  (loop for i from 0 to
    (- (length point-set-labelled) 1) when
    (string=
     (first (nth i point-set-labelled))
     probe-triad-inversion)
    collect (second (nth i point-set-labelled))))



