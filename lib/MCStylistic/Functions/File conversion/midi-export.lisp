#| Copyright 2008-2013 Tom Collins, based midi-save by
   Paul Pelton, Soren Goodman, and Dave Cope
   Monday 26 January 2009

;;;MIDI-SAVE

Written by Paul Pelton with additions and changes by Soren Goodman and Dave Cope.

The input format that the program uses is one used by David Cope, called "Cope
Events".  Each event is a list in the format (ontime duration pitch channel
volume).  All times are in milliseconds, with pitch as 60 = middle C,
channel 1-16, and volume in the range 0-127.  
The input should be a list of events in the variable *events*, so for example:

((0 60 1500 1 90)(500 64 1000 1 90)(1000 67 500 1 90))

would be an arpeggiated major chord (midi pitches 60 64 67) such that the notes
enter at different times and will overlap and end at the same time.

The format:
(save-as-midi (make-pathname :name "dave") '((0 60 1000 1 127)))
will put a quarter note middle C in a midi file called "dave" in your top-level disk drive
(save-as-midi (make-pathname :directory "books" :name "dave") '((0 60 1000 1 127)))
will put the above in a midi file in a folder called "books" in your top-level disk drive 
and so on.
|#

(defConstant KMTHDHEADERBYTES '(#x4D #x54 #x68 #x64))
(defConstant KMTHDHEADERLENBYTES '(0 0 0 6))
(defConstant KMTHDHEADERFORMATBYTES '(0 1))
(defConstant KMTRKHEADERBYTES '(#x4D #x54 #x72 #x6B))
(defConstant KMTRKENDBYTES '(#x00 #xFF #x2F #x00))
(defConstant KPPQNVALUE #x30)


(defVar *WORK-DIR* "/Users/tec69/Webapps/")
(defVar *EVENTS* nil)
(defVar *CHANNEL-NO-1* 1)
(defVar *CHANNEL-NO-2* 2)
(defVar *CHANNEL-NO-3* 3)
(defVar *CHANNEL-NO-4* 4)
(defVar *CHANNEL-NO-5* 5)
(defVar *CHANNEL-NO-6* 6)
(defVar *CHANNEL-NO-7* 7)
(defVar *CHANNEL-NO-8* 8)
(defVar *CHANNEL-NO-9* 9)
(defVar *CHANNEL-NO-10* 10)
(defVar *CHANNEL-NO-11* 11)
(defVar *CHANNEL-NO-12* 12)
(defVar *CHANNEL-NO-13* 13)
(defVar *CHANNEL-NO-14* 14)
(defVar *CHANNEL-NO-15* 15)
(defVar *CHANNEL-NO-16* 16)
(defvar *overwrite-if-exists* t) 

#|
Calling (SAVE-AS-MIDI #P"mj" ((0 60 1000 1 127))) 
SAVE-AS-MIDI returned #P"mj"
|#

(defun SAVE-AS-MIDI (outfilename events &optional (pathname *work-dir*))
  (when pathname (setq outfilename (concatenate 'string pathname outfilename))) 
  (let ((tracks (create-midi-tracks (insert-program-changes events))))
      (with-open-stream (file (create-midi-file outfilename))
        (push (create-MThd (length tracks)) tracks)
        (write-to-midi-file file tracks))
     outfilename)) 
   
#|
Calling (INSERT-PROGRAM-CHANGES ((0 60 1000 1 127))) 
INSERT-PROGRAM-CHANGES returned ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) 
   (0 6 0 6 255) (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) 
   (0 13 0 13 255) (0 14 0 14 255) (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))
|#

(defun INSERT-PROGRAM-CHANGES (events)
  (append
   (list
    (list 0 *channel-no-1* 0 1 255)
    (list 0 *channel-no-2* 0 2 255)
    (list 0 *channel-no-3* 0 3 255)
    (list 0 *channel-no-4* 0 4 255)
    (list 0 *channel-no-5* 0 5 255)
    (list 0 *channel-no-6* 0 6 255)
    (list 0 *channel-no-7* 0 7 255)
    (list 0 *channel-no-8* 0 8 255)
    (list 0 *channel-no-9* 0 9 255)
    (list 0 *channel-no-10* 0 10 255)
    (list 0 *channel-no-11* 0 11 255)
    (list 0 *channel-no-12* 0 12 255)
    (list 0 *channel-no-13* 0 13 255)
    (list 0 *channel-no-14* 0 14 255)
    (list 0 *channel-no-15* 0 15 255)
    (list 0 *channel-no-16* 0 16 255))
   events))
   
#|
Calling (CREATE-MIDI-FILE #P"mj") 
CREATE-MIDI-FILE returned #<BASIC-FILE-BINARY-OUTPUT-STREAM ("mj"/12 ISO-8859-1) #x8B5E79E>
|#

(defun CREATE-MIDI-FILE (outfilename)
  (open outfilename :direction :output :if-exists (if *overwrite-if-exists* :overwrite :error)
        :if-does-not-exist :create :element-type 'unsigned-byte))
   
#|
Calling (WRITE-TO-MIDI-FILE #<BASIC-FILE-BINARY-OUTPUT-STREAM ("mj"/12 ISO-8859-1) #x8B5E79E> ((77 84 104 100 0 0 0 6 0 1 0 17 0 48) 
    (77 84 114 107 0 0 0 4 0 255 47 0) (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0) 
    (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0) (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0) . . . .
WRITE-TO-MIDI-FILE returned NIL
|#

(defun WRITE-TO-MIDI-FILE (file listOfChunks)
   (if (null listOfChunks) ()
      (progn
       (dolist (byte (first listOfChunks))
         (write-byte byte file))
       (write-to-midi-file file (rest listOfChunks)))))
   
#|
0> Calling (CREATE-MIDI-TRACKS ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
   (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) (0 13 0 13 255) (0 14 0 14 255) 
   (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))) 
CREATE-MIDI-TRACKS returned ((77 84 114 107 0 0 0 4 0 255 47 0) (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0) 
   (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0) (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0) (77 84 114 107 0 0 0 7 0 195 3 0 255 47 0) . . . 
|#

(defun CREATE-MIDI-TRACKS (copeEvents)
  (let ((tracks (create-tempo-track)))
    (dotimes (channel 16 (reverse tracks))
      (let ((channelEvents (get-channel-events (1+ channel) copeEvents)))
        (if channelEvents
          (push (create-MTrk channelEvents) tracks))))))
   
#|
Calling (CREATE-TEMPO-TRACK) 
CREATE-TEMPO-TRACK returned ((77 84 114 107 0 0 0 4 0 255 47 0))
|#

(defun CREATE-TEMPO-TRACK ()
   (list (append kMTrkHeaderBytes (split-bytes (length kMTrkEndBytes) 4) kMTrkEndBytes)))
   
#|
Calling (GET-CHANNEL-EVENTS 1 ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
    (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) (0 13 0 13 255) (0 14 0 14 255) 
    (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))) 
GET-CHANNEL-EVENTS returned ((0 1 0 1 255) (0 60 1000 1 127))
|#

(defun GET-CHANNEL-EVENTS (channel events)
   (cond
    ((null events) ())
    ((= channel (fourth (first events)))
     (cons (first events) (get-channel-events channel (rest events))))
    (t (get-channel-events channel (rest events)))))
   
#|
Calling (MAKE-VAR-LEN 0) 
MAKE-VAR-LEN returned (0)

this is adapted to Lisp from the C code at
www.borg.com/~jglatt/tech/midifile.htm
|#

; Try to avoid stack overflow using collect not push.
; #|
(defun MAKE-VAR-LEN (value)
  (reverse
   (cons
    (logand #x7F value)
    (loop while
      (not (zerop (setq value (ash value -7))))
      collect
      (logior (logior #x80 (logand #x7F value)))))))
; |#

; Original version.
#|
(defun MAKE-VAR-LEN (value)
   (let ((buffer (list (logand #x7F value))))
      (loop while
        (not (zerop (setq value (ash value -7))))
         do
        (push
         (logior (logior #x80 (logand #x7F value)))
         buffer))
      buffer))
|#

#|
Calling (CREATE-MTHD 17) 
CREATE-MTHD returned (77 84 104 100 0 0 0 6 0 1 0 17 0 48)


assume that 1 <= numtracks <= 16
|#

(defun CREATE-MTHD (numtracks)
   (append
    kMThdHeaderBytes
    kMThdHeaderLenBytes
    kMThdHeaderFormatBytes
    (list 0 numtracks 0 kPPQNValue)))
   
#|
Calling (CREATE-MTRK ((0 1 0 1 255) (0 60 1000 1 127))) 
CREATE-MTRK returned (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0)


assume 2 bytes for length of track data
|#

(defun CREATE-MTRK (events)
   (if (null events) ()
      (let* ((mtrkData
              (append
               (create-midi-track-data
                (fix-deltatime 0 (sort-by-deltatime (create-midi-events events))))
               kMTrkEndBytes)))
         (append kMTrkHeaderBytes (split-bytes (length mtrkData) 4) mtrkData))))
   
#| 
Calling (SPLIT-BYTES 7 4) 
SPLIT-BYTES returned (0 0 0 7)

splits a long integer into its high byte and low byte
|#

(defun SPLIT-BYTES (num count)
   (let ((bytes ()))
      (dotimes (i count bytes)
         (push (get-byte i num) bytes))))
   
#|
Calling (GET-BYTE 3 7) 
GET-BYTE returned 0

byteIndex starts at 0 (rightmost) and goes to whatever (leftmost)
|#

(defun GET-BYTE (byteIndex num)
   (ldb (byte 8 (* 8 byteIndex)) num))
   
#|
Calling
(CREATE-MIDI-TRACK-DATA '((0 (193 1)) (4 (192 3))))
CREATE-MIDI-TRACK-DATA returned (0 193 1)

midiEvents are ((deltaTime (byte3 byte2 byte1))(deltaTime (byte3 byte2 byte1))...),
are sorted in the order they should be in the file and their deltaTimes are
relative to each other (each relative to the previous).
|#

; New loop version.
; #|
(defun CREATE-MIDI-TRACK-DATA (midiEvents)
  (loop for i from 0 to (- (length midiEvents) 1)
    append
    (append
     (make-var-len (first (nth i midiEvents)))
     (second (nth i midiEvents)))))
; |#

; Original version.
#|
(defun CREATE-MIDI-TRACK-DATA (midiEvents)
   (if (null midiEvents) ()
      (let ((midiEvt (first midiEvents)))
        (append
         (make-var-len (first midiEvt))
         (second midiEvt)
         (create-midi-track-data (rest midiEvents))))))
|#

#|
Calling
(CREATE-MIDI-EVENTS '((0 3 0 3 60) (3 2 0 3 255)))
returned ((0 (146 3 60)) (0 (130 3 60)) (0 (194 1)))
|#

; New loop version.
; #|
(defun CREATE-MIDI-EVENTS (copeEvents)
  (loop for i from 0 to (- (length copeEvents) 1)
    append
    (if (= (fifth (nth i copeEvents)) 255)
      (list
       (list
        (convert-ontime-to-deltatime
         (first (nth i copeEvents)))
        (make-midi-pc-msg (nth i copeEvents))))
      (list
       (list
        (convert-ontime-to-deltatime
         (first (nth i copeEvents)))
        (make-midi-note-msg
         (nth i copeEvents) #x90))
       (list
        (convert-ontime-to-deltatime
         (+
          (first (nth i copeEvents))
          (third (nth i copeEvents))))
        (make-midi-note-msg
         (nth i copeEvents) #x80))))))
; |#

; Original version.
#|
(defun CREATE-MIDI-EVENTS (copeEvents)
   (if (null copeEvents) ()
      (let* ((event (first copeEvents))
             (ontime (first event)))
         (append
          (cond ((= (fifth event) 255)
                 (list (list (convert-ontime-to-deltatime ontime) (make-midi-pc-msg event))))
                (t
                 (list
                  (list
                   (convert-ontime-to-deltatime ontime) (make-midi-note-msg event #x90))
                  (list
                   (convert-ontime-to-deltatime (+ ontime (third event))) (make-midi-note-msg event #x80)))))
          (create-midi-events (rest copeEvents))))))
|#

; Old mapcar version, incorrect output format.
#|
(defun CREATE-MIDI-EVENTS (copeEvents)
  (mapcar
   #'(lambda (x)
       (if (= (fifth x) 255)
         (list
          (convert-ontime-to-deltatime (first x))
          (make-midi-pc-msg x))
         (list
          (list
           (convert-ontime-to-deltatime (first x))
           (make-midi-note-msg x #x90))
          (list
           (convert-ontime-to-deltatime
            (+ (first x) (third x)))
           (make-midi-note-msg x #x80)))))
   copeEvents))
|#
   
#|
Calling (MAKE-MIDI-NOTE-MSG (0 60 1000 1 127) 144) 
MAKE-MIDI-NOTE-MSG returned (144 60 127)

#x90 = note-on
#x80 = note-off
|#

(defun MAKE-MIDI-NOTE-MSG (copeEvent flag)
   (list (logior (1- (fourth copeEvent)) flag) (second copeEvent) (fifth copeEvent)))
   
#|
Calling (MAKE-MIDI-PC-MSG (0 16 0 16 255)) 
MAKE-MIDI-PC-MSG returned (207 15)

#xC0 = program change
|#

(defun MAKE-MIDI-PC-MSG (copeEvent)
   (list (logior (1- (fourth copeEvent)) #xC0) (1- (second copeEvent))))
                 
#|
Calling (SORT-BY-DELTATIME ((0 (207 15)))) 
SORT-BY-DELTATIME returned ((0 (207 15)))
|#
                                 
(defun SORT-BY-DELTATIME (midiEvents)
   (sort midiEvents #'< :key #'car))
   
#|
Calling
(FIX-DELTATIME
 5 '((2 (207 15)) (4 (208 13)) (11 (202 10))))
FIX-DELTATIME returned ((0 (207 15)))
|#

; New mapcar version.
; #|
(defun FIX-DELTATIME (lasttime midiEvents)
  (cons
   (list
    (-
     (first (first midiEvents)) lasttime)
    (second (first midiEvents)))
   (mapcar
    #'(lambda (x y)
        (list
         (- (first x) (first y))
         (second x)))
    (rest midiEvents) midiEvents)))
; |#

; Original version.
#|
(defun FIX-DELTATIME (lasttime midiEvents)
   (if (null midiEvents) ()
      (let* ((midiEvt (first midiEvents))
            (newLastTime (first midiEvt)))
         (cons
          (list (- newLastTime lasttime) (second midiEvt))
          (fix-deltatime newLastTime (rest midiEvents))))))
|#
   
#|
Calling (CONVERT-ONTIME-TO-DELTATIME 0) 
CONVERT-ONTIME-TO-DELTATIME returned 2 values :
      0
      0
|#

(defun CONVERT-ONTIME-TO-DELTATIME (copeOntime)
   (round (* (/ kPPQNValue 1000) copeOnTime)))

;; This is added to stop clipping at end of file
(defun saveit
       (outfilename events &optional
        (last-event (first (last events)))
        (new-time
         (if last-event
           (+
            (first last-event) (third last-event)
            500) (identity 500))))
  (let ((tracks
         (create-midi-tracks
          (insert-program-changes
           (if last-event
             (append
              events
              (list (list new-time 1 500 1 1)))
             events)))))
    (if (probe-file outfilename)
      (delete-file outfilename))
    (with-open-stream
        (file (create-midi-file outfilename))
      (push (create-MThd (length tracks)) tracks)
      (write-to-midi-file file tracks))
    outfilename))

#| Old version, deprecated 9 March 2016 so that a user
can choose whether or not to add a final event to stop
clipping at the end of the QuickTime MIDI file
playback.
(defun saveit
       (outfilename events &optional
        (last-event (first (last events)))
        (new-time
         (if last-event
           (+
            (first last-event) (third last-event)
            500) (identity 500))))
  (let ((tracks
         (create-midi-tracks
          (insert-program-changes
           (append
            events
            (list (list new-time 1 500 1 1)))))))
    (if (probe-file outfilename)
      (delete-file outfilename))
    (with-open-stream
        (file (create-midi-file outfilename))
      (push (create-MThd (length tracks)) tracks)
      (write-to-midi-file file tracks))
    outfilename))
|#
#| Old version
(defun saveit
       (&optional (file-name "example.mid")
                  (events *events*)
                  (path&name
                   (concatenate 'string
                                *work-dir*
                                file-name)))
  (let* ((last-event (first (last events)))
         (new-time (+
                    (first last-event)
                    (third last-event)
                    500)))
    (if (probe-file path&name)
      (delete-file path&name))
    (save-as-midi file-name (append
                              events
                              (list (list new-time 1 500 1 1))))))
|#



