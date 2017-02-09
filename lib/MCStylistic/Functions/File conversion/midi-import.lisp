#| Copyright 2008-2013 Tom Collins, based midi-load by
   Peter Elsea
   Monday 26 January 2009

The main change to PQE's version is that rather than
converting beat-numbers-subdivisions into
milliseconds, they are converted into beats beginning
from zero, with crotchet equal to 1.

Specific changes:
1) third element of *sequence-tempo-map* changed from
500000 to 1;
2) 1000 changed to 1 in the function add-tempo. To
some extent this change is immaterial, since the
function add-tempo is no longer invoked (see point 4);
3) 1000 changed to 1 in the function ticks-ms, and a
float wrap has been removed from the final line;
4) after #X51 (=81), the function parse-metadata used
to read (add-tempo (cdr the-data));
5) the function setup reiterates some of the earlier
defVar's, so 500000 is changed to 1 again here;
6) the last line of the function load-midi-file used
to read
(loop
  for notes
  across *sequence-notes*
  collect (mapcar #' round notes)).
The rounding is unnecessary for the type of data I
wish to handle, so I have changed this to
(coerce *sequence-notes*). |#

;;;MIDI-LOAD
;; This is the simple version.
;; All it gives is a list of note events
;; Tempo events are collected and used to calculate times
;; Text Meta events are colleccted into *sequence-strings*
;; other events are discarded
;; the functiion load-midifile requires a valid path as a string.
;; this version uses special variables, a forthcoming version will be self contained.
;; pqe 6-04-08


(defVar *CHUNK-TYPE* ())             ; only two types are defined so far
(defVar *CHUNK-LENGTH* 0)            ; number of bytes in chunk
(defVar *MIDI-FILE-FORMAT* 0)        ; type 0 is single track, type 1 is multitrack, type 2 is indepentent loops
(defVar *MIDI-FILE-NTRKS* 0)         ; number of tracks in file
(defVar *MIDI-FILE-GRANULARITY* 24)  ; number of ticks per quarter note
(defVar *TRACK-TIME* 0)              ; unconverted track time, in ticks
(defVar *RUNNING-STATUS* 0)          ; running status is used
(defVar *TRACK-END* t)               ; flag for finding ends of tracks (rather than byte coounting) EOT sets this nil


;; A place to put metadata -- later version can be more elegant
(defVar *SEQUENCE-STRINGS* (make-array 1 :initial-contents #("sequence-strings") :fill-pointer t :adjustable t ))

;; a place to put tempos. all tracks must refer to this when converting from ticks to time in ms
;; format of each entry is (time-in-ticks time-in-ms usec/qn)
(defVar *SEQUENCE-TEMPO-MAP* (make-array 1 :element-type 'list :initial-element '(0 0 1) :fill-pointer t :adjustable t ))


;; a place to put note data
;; *sequence-notes* format is (time-ms  note-number duration channel velocity)
;; This is an array to simplify setting durations when note off is detected.
(defVar *SEQUENCE-NOTES* (make-array 0 :element-type 'list :initial-element '(0 0 0 0 0) :fill-pointer t :adjustable t ))

; helper for header reading
(defun GET-TYPE (input-stream)
    (let ((type-string (make-string 4)))
      (loop for i from 0 to 3
            do (setf (char type-string i) (code-char(read-byte input-stream)))
            )
      type-string))

; general 32 bit retreiver
(defun GET-WORD (input-stream)
    (let ((value 0))
      (loop for i from 0 to 3
            do (setq value (+ (* value 256) (read-byte input-stream)))
            )
      value))

; general 16 bit retriever
(defun GET-SHORT (input-stream)
    (+ (* (read-byte input-stream) 256) (read-byte input-stream)))

; division is weird- this is a try at making sense out of it
; granularity is ticks per beat (quarter note)
(defun CONVERT-GRANULARITY (division)
  (let ((high-byte (ash division -8))(low-byte (logand #XFF)))
    (case high-byte
      (#XE2 (* 30 low-byte))
      (#XE3 (* 30 low-byte))
      (#XE7 (* 25 low-byte))
      (#XE8 (* 24 low-byte))
      (t division))))

; read the file header
(defun GET-HEADER (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream))
    (setq *midi-file-format* (get-short input-stream))
    (setq *midi-file-ntrks* (get-short input-stream))
    (setq *midi-file-granularity* (convert-granularity (get-short input-stream))))

; read a track header
(defun GET-TRACK-HEADER (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream)))

; time is listed as ticks in variable length quantities
(defun CONVERT-VLQ (arg-list &optional (accum 0))
      (if (> (first arg-list) 127)
        (convert-vlq (rest arg-list) (+ (- (first arg-list) 128) (* accum 128)))
        (+ (first arg-list) (* accum 128))))

; all events are seperated by a delta time 
(defun GET-VLQ (input-stream)
  (let ((new-byte (read-byte input-stream)))
    (if (< new-byte 128) (list new-byte)
        (cons new-byte (get-vlq input-stream)))))

; times are between events, so *track-time* must be accumulated across each track
(defun SET-TRACK-TIME (input-stream)
  (incf *track-time* (convert-vlq (get-vlq input-stream))))

; read arbitrary bytes into a list
(defun GATHER-BYTES (input-stream how-many)
  (if (zerop how-many) ()
      (cons (read-byte input-stream) (gather-bytes input-stream (1- how-many)))))

; reads a length, then gathers that many
(defun GET-METADATA (input-stream)
  (gather-bytes input-stream (read-byte input-stream)))

; test function for tempo searches
(defun FIRST>= ( data alist)
  (>= data (first alist) ))

#| The number 81 signifies a change in tempo. When
this number is parsed, this function used to be
invoked, with the variable the-data being the
three numbers following 81, for example
(setq the-data '(3 15 66))
is the value parsed by the example file cited
below. This is us-qn (whatever that means!) and
this particular us-qn is 200514, which just
doesn't make any sense to me! |#
;; Stuff the tempo map. format of each entry is (time-in-ticks time-in-ms usec/qn)
;; tempo and granualrity are need to convert ticks to ms
;; storing the time of the tempo change in both formats simplifies the calculations
(defun ADD-TEMPO (the-data)
  (let* ((us-qn (+ (ash (first the-data) 16)(ash (second the-data) 8) (third the-data)))
	(last-tempo-entry (elt *sequence-tempo-map*  (- (length *sequence-tempo-map* )1)))
        (last-tempo-time (second last-tempo-entry))
        (last-tempo (third last-tempo-entry))
        (ticks (- *track-time* (first last-tempo-entry))))
    (vector-push-extend (list *track-time* 
			      (+ last-tempo-time
                                 (/(* ticks last-tempo )(* *midi-file-granularity* 1)))
                                 us-qn)
                        *sequence-tempo-map*)))

;; the time conversion function
;; search the tempo map from the end to find tempo in effect at the time
(defun TICKS-MS (ticks)
  (let* ((current-tempo-entry (find ticks *sequence-tempo-map* :test #'first>= :from-end t))
	 (current-tempo-time (second current-tempo-entry))
	 (current-tempo (third current-tempo-entry))
	 (delta-ticks (- ticks (first current-tempo-entry))))
    (+ current-tempo-time (/(* delta-ticks current-tempo)(* *midi-file-granularity* 1)))))

;; most meta-data is text
(defun LIST-TO-STRING (ascii)
  (if (null ascii) #\. 
      (format nil "~A~A" (code-char (car ascii)) (list-to-string (cdr ascii)))))

;; meta data is mostly in the way, but tempos and end of trak are vital
(defun PARSE-METADATA (the-data)
  (case (car the-data)
    (0 ()) ; sequence number
    ((1 2 3 4 5 6 7 8 9 10)  (vector-push-extend (list-to-string (cdr the-data)) *sequence-strings* )); text 
    (#X20 ()) ; MIDI Channel prefix
    (#X2F (setq *track-end* nil)) ; End of track
    (#X51 ()) ;  Set tempo usec/qn in *sequence-tempo-map*
    (#X54 ()) ;  SMPTE offset H:M:S:F:F/100
    (#X58 ()) ;  Time Signature nnn dd  cc bb
    (#X59 ()) ;  Key Signature
    (#X7F ()) ;  Program specific
    (t ())))  ; unknown

;; Other events to parse
;; note ons are keepers
(defun HANDLE-NOTE (status nn vel)
  (vector-push-extend (list (ticks-ms *track-time*) nn 0 (+ (logand status #X0F) 1) vel ) *sequence-notes* ))

; test function for not off, which must search for matching note on
(defun MATCH-NOTE (status-nn target)
  (and (= (second status-nn) (second target))(= (first status-nn) (fourth target))(zerop (third target))))

;; search for note on this belongs to and set duration
;; this doesn't handle overlapping notes of the same pitch well but whatcha gonna do?
;; note number is &rest because we don't get a velocity with running status
;; note off velocity is discarded anyhow
(defun HANDLE-OFF (status &rest nn  )
  (let* ((channel (+ (logand status #X0F) 1)) 
	 (where (position (list channel (first nn)) *sequence-notes* :test #'match-note :from-end t))
	 (the-note)
	 (duration))
    (if (null where) () ; no matchng note on
      (progn
	(setf the-note (elt *sequence-notes* where))
	(setf duration (- (ticks-ms *track-time*) (first the-note)))
	(setf (third (elt *sequence-notes* where)) duration)))))

;; these just discard the data- ther are listed to prevent compiler warnings
;; one day I'll do something intelligent with these
(defun HANDLE-TOUCH (status nn pressure)
  (list status nn pressure))

(defun HANDLE-CONTROL (status cn value)
  (list status cn value))

(defun HANDLE-PROGRAM (status pn)
  (list status pn))

(defun HANDLE-PRESSURE (status pressure)
  (list status pressure))

(defun HANDLE-BEND (status lsb msb)
  (list status lsb msb))

(defun STRIP-SYSEX (input-stream)
  "just delete sysex for now"
  (if (= (read-byte input-stream) #XF7) ()
      (strip-sysex input-stream)))

;;; this is the grand track data handler
(defun PARSE-EVENTS (status-byte data-byte input-stream)
  (let ((vel))
   (cond 
    ((< status-byte #X90) (handle-off status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XA0) (if (zerop (setq vel (read-byte input-stream)))
                             (handle-off status-byte data-byte )
                                (handle-note status-byte data-byte vel)))
    ((< status-byte #XB0) (handle-touch status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XC0) (handle-control status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XD0) (handle-program status-byte data-byte ))
    ((< status-byte #XE0) (handle-pressure status-byte data-byte ))
    ((< status-byte #XF0) (handle-bend status-byte data-byte (read-byte input-stream)))
    ((= status-byte #XF0) (strip-sysex input-stream))
    ((= status-byte #XFF) (parse-metadata (cons data-byte (get-metadata input-stream))))
    (t ()))))

;;; this layer deals with running status
(defun READ-AND-PARSE-EVENT (input-stream)
  (let ((first-byte (read-byte input-stream)))
    (if (>= first-byte #X80) (parse-events (setf *running-status* first-byte) (read-byte input-stream) input-stream)
      (parse-events *running-status* first-byte input-stream))))

;;;; call this once per track
(defun READ-TRACK (input-stream)
  (get-track-header input-stream)
  (if (zerop *chunk-length*) ()
    (if (not (equal *chunk-type* "MTrk")) (gather-bytes input-stream *chunk-length*) ; discard alien chunks
      (do ((*track-end* t)(*track-time* 0)(*running-status* 0))
	  ((null *track-end*)())
	(set-track-time input-stream)
	(read-and-parse-event input-stream)))))

;;;; initialize all those specials
(defun SETUP ()
  (setf *sequence-strings* (make-array 1 :initial-contents #("sequence-strings") :fill-pointer t :adjustable t ))
  (setq *sequence-tempo-map* (make-array 1 :element-type 'list :initial-element '(0 0 1) :fill-pointer t :adjustable t ))
  (setq *sequence-notes* (make-array 0 :element-type 'list :initial-element '(0 0 0 0 0) :fill-pointer t :adjustable t ))) 

;; test function for sorting by time
(defun EARLIER (alist blist)
  (< (first alist) (first blist)))

;;;;;;;; Ta-Da ;;;;;;;;;;;;;;;
(defun LOAD-MIDI-FILE (fstring)
  (with-open-file (input-stream fstring :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (setup)
    (get-header input-stream)
    (do ((track-index 0 (+ track-index 1)))
	((>= track-index *midi-file-ntrks*) ())
      (read-track input-stream))
    (setq *sequence-notes* (sort *sequence-notes* #'earlier))
    (coerce *sequence-notes* 'list)))

; test with a short file
; this is the path format for mac
; if you put your stuff directly in documents, your path may be
; "/Users/WACM/Documents/myfile.mid"
; (setq *test* (load-midi-file "/Users/tomcollins/Open/Music/MIDI/Clean Mazurkas/test.mid"))

(defun RE-TIME (to-be-added-to-events events)
  "For placing cope-event groups one after another in time sequence."
  (let* ((last-event (my-last events))
         (offset-time (if last-event (+ (first last-event)(third last-event)) 0)))
    (append events (loop for event in to-be-added-to-events
                         collect (cons (+ (first event) offset-time)(rest event))))))

#| Example:
(my-last '(1 3 6 7))
gives
7.

Returns the last element of a list as an element,
not as a list. |#

(defun my-last (a-list)
  (first (last a-list)))