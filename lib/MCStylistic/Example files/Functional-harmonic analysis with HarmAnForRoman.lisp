#| Copyright 2008-2014 Tom Collins
   Tuesday 6 May 2014

Functional-harmonic analysis with HarmAn-$>$roman

This code demonstrates the use of an algorithm called
HarmAn-$>$roman for segmenting and labelling chords
in some input piece of music. The algorithm is based
on an implementation of HarmAn by \cite{pardo2002}.
HarmAn compares input triples of ontimes, MIDI note
numbers, and durations to predefined chord templates,
and performs segmentation and segment labelling on
this basis. The labels are absolute, for instance
(15 2 1 1 8) means that a chord begins on ontime 15,
has root 2 modulo 12 (i.e., D), is of type 1 (dom7
chord), lasts 1 beat, and was assigned to this chord
template with strength 8.

While useful, this output does not provide a
functional-harmonic analysis. I programmed some
extra steps to estimate the overall key of the input
piece, using the Krumhansl-Schmuckler key-finding
algorithm \citep{krumhansl1990}, and then to
caluclate relative (or functional) harmonic labels
by combining the estimate of overall key with the
absolute labels output by HarmAn-$>$. For instance,
if the overall key is G major, and HarmAn-$>$ output
the label D dom7, then my code would convert this to
V7. I have taken care to make sure the labelling of
diminished 7th chords is correct. The overall
program is referred to as HarmAn-$>$roman. It does
not handle secondary keys, but might be adapted to
do so using a slice through a keyscape
\citep{sapp2005}.

The input piece should be in kern or point-set
format. If your piece is in MusicXML format instead,
then it can be converted according to the description
in Sec.~\ref{sec:musicxml}. Before proceeding, please
note: it is assumed that the package MCStylistic has
been loaded, and that the variable
*MCStylistic-MonthYear-data-path* has been defined
appropriately. Files are imported from the location
specified by this variable.

References

Tom Collins. Improved methods for pattern discovery
in music, with applications in automated stylistic
composition. PhD thesis, Faculty of Mathematics,
Computing and Technology, The Open University,
2011. |#


#| Step 1 - Load a piece by Chopin. |#
(setq *piece-name* "C-6-1-ed")
(setq
 *path&name*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "Dataset")
   :name *piece-name* :type "txt")
  *MCStylistic-MonthYear-data-path*))
(progn
  (setq
   point-set
   (restrict-dataset-in-nth-to-tests
    (read-from-file *path&name*) 0 (list #'<)
    (list 47)))
  "Piece loaded.")

#| Step 2 - Run HarmAn->roman. To see these results
and compare with a ground truth, please see
Fig. 3.2. |#
(HarmAn->roman
 point-set *chord-templates-p&b&min7ths*)
--> (("Ic" (-1 1/3)) ("V" (1/3 2/3)) ("Ic" (2/3 1))
     ("V7" (1 3)) ("i7" (3 7)) ("VII7" (7 39/4))
     ("III" (39/4 12)) ("#vih7c" (12 13))
     ("II7" (13 14)) ("V7c" (14 15))
     ("vh7c" (15 16)) ("I7" (16 17))
     ("#vio7b" (17 18)) ("ivh7c" (18 19))
     ("VII7" (19 20)) ("vo7b" (20 21))
     ("iiih7c" (21 22)) ("VI7" (22 23))
     ("iih7c" (23 24)) ("Ic" (24 73/3))
     ("V" (73/3 74/3)) ("Ic" (74/3 25))
     ("V7" (25 27)) ("i7" (27 31))
     ("VII7" (31 135/4)) ("III" (135/4 36))
     ("ivh7b" (36 38)) ("III" (38 39))
     ("ivh7b" (39 41)) ("i7b" (41 85/2))
     ("VI" (85/2 43)) ("iih7c" (43 44))
     ("V" (44 45)) ("ivc" (45 136/3))
     ("i" (136/3 137/3)) ("iih7d" (137/3 46))
     ("VIb" (46 47)))

#| Step 3 - Load another piece, this time by Bach,
chorale harmonisation BWV115.6/R38. |#
(progn
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
     (6 63 62 2 1) (6 67 64 2 0) (8 44 51 1/2 3)
     (8 56 58 1 2) (8 63 62 1 1) (8 72 67 1 0)
     (17/2 43 50 1/2 3) (9 41 49 1/2 3) (9 56 58 1 2)
     (9 65 63 1 1) (9 74 68 1 0) (19/2 44 51 1/2 3)
     (10 43 50 1/2 3) (10 58 59 1 2) (10 63 62 1/2 1)
     (10 75 69 1 0) (21/2 41 49 1/2 3)
     (21/2 62 61 1/2 1) (11 39 48 1/2 3)
     (11 58 59 1 2) (11 63 62 1 1) (11 67 64 1 0)
     (23/2 43 50 1/2 3) (12 44 51 1/2 3)
     (12 60 60 1 2) (12 63 62 1 1) (12 65 63 2 0)
     (25/2 41 49 1/2 3) (13 46 52 1 3)
     (13 58 59 1/2 2) (13 62 61 1 1)
     (27/2 56 58 1/2 2) (14 39 48 2 3) (14 55 57 2 2)
     (14 58 59 2 1) (14 63 62 2 0)))
  "Excerpt loaded.")

#| Step 4 - Again, run HarmAn->roman. To see these
results and compare with a ground truth, please see
the pdf for the score in the folder Example files ->
Example results. |#
(HarmAn->roman
 point-set *chord-templates-p&b&min7ths*)
--> (("I" (0 1)) ("IVb" (1 2)) ("Vb" (2 5/2))
     ("II7c" (5/2 3)) ("iiib" (3 4)) ("vi7d" (4 5))
     ("ii7b" (5 11/2)) ("V" (11/2 6)) ("I" (6 8))
     ("IV" (8 9)) ("viiob" (9 10)) ("Ib" (10 21/2))
     ("Vc" (21/2 11)) ("I" (11 12)) ("ii7" (12 13))
     ("V7" (13 14)) ("I" (14 16)))

#| The results are decent. One issue arises with
inversion labelling: for example, if we have the
chord ii7b, with the bass falling from the third of
the chord to the root of the chord, then
HarmAn->roman will label it as a root position chord
because the lowest note appearing in the section is
the root and not the third. A rule change could
address this issue. A second issue pertains to what
might be called backwards labelling. If in quick
succession, V turns into V7, then a music analyst
might just use one label, V7, to describe both
chords, even though V7 does not begin until the
second chord in the sequence. Backwards labelling
can be taken to extremes by HarmAn-> (and therefore
HarmAn->roman). Suppose V begins and lasts a whole
bar, and then V7 begins. Still, HarmAn-> would
combine the two bars into one and label it V7. |#
