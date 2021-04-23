(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

#|
a) you need to use insert on the REPL to do command, recall and execution. It is part of VIM and have two modes

b) Part 3 is not really understandable as the debug etc. is not easy to be followed.

c) sometimes need to use (in-package :morse) in the real

d) you need to (ql:quickload :split-sequence) as even (use ... :split-sequence) does not auto load it

e) a lot of commands needed in vim and REPL switching

- ctr-w w between the two
- ,d ,F ,L
- vim -
  edit i/A/esc/:wq/:q!/:w filename.lisp/... 
  line: $/0 
  word:b/e/w
  screen: H/M/L
  ctrl- for screen up and down
  file: gg/G


|#


(defpackage :morse
  (:use :common-lisp :split-sequence)

  )

(in-package :morse)



(defun add2 (x) (+ 2 x))

(defparameter *morse-mapping*
  '((#\A ".-"
     )
    (#\B "-..."
         )
    
    
  (#\C "-.-.")
  (#\, "--..--"))

)

(defun character-to-morse (character)
  (second ( assoc character *morse-mapping* :test #'char-equal))
  )

(defun morse-to-character (morse-string)
  (first (find morse-string *morse-mapping* :test #'string= :key #'second)))

(defun string-to-morse (string)
  (with-output-to-string (morse)
    (write-string (character-to-morse (aref string 0)) morse)
    (loop
      for char across (subseq string 1)
      do (write-char #\Space morse)
      do (write-string (character-to-morse char) morse
                       ))))

(defun morse-to-strng (string)
  (with-output-to-string (character-stream)
    (loop
      for morse-char in (split-sequence:split-sequence #\Space string)
      do (write-char (morse-to-character morse-char) character-stream
                     ))))

