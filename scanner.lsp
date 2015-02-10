; scanner.lsp            Gordon S. Novak Jr.            ; 29 Nov 06

; Auxiliary functions for reading characters from files using Lisp
; See also the file tokendefs.lsp .

; Copyright (c) 2006 Gordon S. Novak Jr. and
; The University of Texas at Austin.

; 03 May 01

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License (file gpl.text) for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(in-package :user)

; To use:
; First call (read-file "<file-name>"),
; e.g. (read-file "/projects/cs375/pastst.pas")
;   For small tests, you can set *lines* to one line instead:
;   (setq *lines* '("123"))
; (test-scanner) will run a complete test.

; Your program should call (getchar) repeatedly to get characters until
; it returns NIL.

; while macro:  (while <condition> ...)
(defmacro while (test &rest forms) `(loop (unless ,test (return)) ,@forms) )

(defvar *lines*      nil)  ; List of all lines in file
(defvar *line*       nil)  ; pointer to current line
(defvar *lineptr*    nil)  ; pointer to remaining lines
(defvar *linelength* nil)  ; length of current line
(defvar *linenumber* nil)  ; number of current line
(defvar *charindex*  nil)  ; index of current character
(defvar *char*       nil)  ; current character
(defvar *charclass*  (make-array 256 :initial-element 0))
(defvar *alpha*      1)    ; alpha   char code
(defvar *numeric*    2)    ; numeric char code
(defvar *special*    3)    ; special char code

; Read in the whole input file, save in *lines*.
(defun read-file (filename)
  (let (line)
    (setq *lines* nil)
    (with-open-file (instream filename :direction :input)
      (loop (if (setq line (read-line instream nil nil))
		(push line *lines*)
		(return))))
    (setq *lines* (nreverse *lines*))
    (format t "File read.~%")
    (init-file) ))

; Initialize pointers to start of file.
(defun init-file ()
  (setq *lineptr* *lines*)                       ; pointer to remaining lines
  (setq *linenumber* 0)
  (init-charclass)
  (initialize-line-pointers) )

(defun initialize-line-pointers ()
  (setq *line* (first *lineptr*))
  (setq *linelength* (length *line*))
  (setq *charindex* 0))

(defun next-line ()
  (pop *lineptr*)
  (when *lineptr*
        (initialize-line-pointers)
	(incf *linenumber*)) )

; Get next character.  Returns one space at end of line, NIL at End of File.
(defun getchar ()
  (if *lineptr*
      (if (< *charindex* *linelength*)
	  (prog1 (char *line* *charindex*)
	         (incf *charindex*))
	  (progn (next-line)
		 (if *lineptr* #\Space)))) )

; Peek at the next character without moving the character pointer
(defun peekchar ()
  (if *lineptr*
      (if (< *charindex* *linelength*)
	  (char *line* *charindex*)
	  (if (rest *lineptr*) #\Space))) )

; Peek at the character after the next character without moving the pointer
(defun peek2char ()
  (if *lineptr*
      (if (< (1+ *charindex*) *linelength*)
	  (char *line* (1+ *charindex*))
	  #\Space)) )

; Get the character class of a character.
; Test with = or eql against   *alpha*   *numeric*   *special*
(defun charclass (char)
  (if char (aref *charclass* (char-code char))))

; Initialize the character class array.
(defun init-charclass ()
  (dotimes (i 26) (setf (aref *charclass* (+ i (char-code #\a))) *alpha*)
                  (setf (aref *charclass* (+ i (char-code #\A))) *alpha*))
  (dotimes (i 10) (setf (aref *charclass* (+ i (char-code #\0))) *numeric*))
  (dolist (c '(#\+ #\- #\* #\/ #\: #\= #\< #\> #\^ #\.
               #\, #\; #\( #\) #\[ #\] #\{ #\}) )
    (setf (aref *charclass* (char-code c)) *special*))   )

; 20 Jan 95
; Get one token
(defun gettoken ()
  (let (c cclass)
    (skip-blanks)
    (when (setq c (peekchar))
          (setq cclass (charclass c))
	  (if (eql cclass *alpha*)
	      (parse-alpha)
	      (if (eql cclass *numeric*)
		  (parse-number)
		  (if (char= c #\')
		      (parse-string)
		      (parse-special))))) ))

; Fill this in to skip comments too...
(defun skip-blanks ()
  (let (c)
    (while 
      (and (setq c (peekchar))
           (or (char= c #\Space) 
               (char= c #\Tab)))
      (getchar))
    (if 
      (and (setq c (peekchar))
           (char= c #\{))
      (progn
        (getchar)
        (while 
          (not 
            (and (setq c (peekchar)) 
                 (char= c #\})))
          (getchar))
        (getchar)))
    (if 
        (and (setq c (peekchar))
             (setq d (peek2char))
             (char= c #\() 
             (char= d #\*))
        (progn
            (getchar)
            (getchar)
            (while (not (and (setq c (peekchar))
                             (setq d (peek2char))
                             (char= c #\*)
                             (char= d #\))))
                    (getchar))
            (getchar)
            (getchar)))

    (if (and (setq c (peekchar)) 
             (or 
               (char= c #\Space) 
               (char= c #\Tab) 
               (char= c #\{)))
      (skip-blanks))
    )
  )

(defun parse-string()
    (getchar)
    (setq s "")
    (while
        (or 
          (not (char= (peekchar) #\')) 
          (and (char= (peekchar) #\') 
               (char= (peek2char) #\')))
        (progn
            (setq escapequote 
                  (and 
                    (char= (peekchar) #\') 
                    (char= (peek2char) #\'))) 
            (setq s 
                  (concatenate 'string s 
                               (list (getchar))))
            (if escapequote
                (getchar)))) 
    (getchar)
    (if 
      (>= (length s) 15)
      (list 'stringtok (subseq s 0 15))
    (list 'stringtok s))
)

(defun parse-special()
    (setq s (string (getchar)))
    (setq symbolly (list "+" "-" "*" "/" ":" "=" "<" ">" "^" "." "(" ")" "," ";" ":" "[" "]"))
    (setq operators (list "+" "-" "*" "/" ":=" "=" "<>" "<" "<=" ">=" ">" "^" "."))
    (setq delimiters (list "," ";" ":" "(" ")" "[" "]" ".."))
    (while
        (member (string (peekchar)) 
                symbolly 
                :test #'string-equal)
        (setq s
            (concatenate 'string s 
                         (list (getchar)))))
    (if (member s operators :test #'string-equal) 
        (list 'operator (+ 1 (position s operators :test #'string-equal)))
        (if (member s delimiters :test #'string-equal)
            (list 'delimiter (+ 1 (position s delimiters :test #'string-equal)))
            (format t "Error")))
)

(defun parse-alpha()
    ;If first char isn't a letter, error
    (setq s (string (getchar)))
    ;Not efficient storage
    (setq reserved (list "array" "begin" "case" "const" "do" "downto" "else" "end" "file" "for" "function" "goto" "if" "label" "nil" "of" "packed" "procedure" "program" "record" "repeat" "set" "then" "to" "type" "until" "var" "while" "with"))
    (while 
        (or 
            (eql (charclass (peekchar)) *alpha*) 
            (eql (charclass (peekchar)) *numeric*))
        (setq s 
            (concatenate 'string s (list (getchar)))))
    (setq whichval (position s reserved :test #'string-equal))
    (if whichval (list 'reserved (+ whichval 1)) (list 'identifiertok s))
)

(defun parse-number()
    (setq intpart 0)
    (setq decimalpart 0)
    (setq exponent 0)
    (setq exponentdirection 1)
    (setq numtype 'integer)
    ;If this while never fires, error
    (while (eql (charclass (peekchar)) 
                          *numeric*) 
           (setq intpart 
                 (+ (* intpart 10) 
                    (digit-char-p (getchar)))))
    ;If the next character isn't a dot or e, error
    (if (and (eql (peekchar) #\.)
             (not (eql (peek2char) #\.)))
        (progn
            (setq numtype 'real)
            (getchar)
            (setq accum 10)
            (while (eql (charclass (peekchar))
                                   *numeric*)
                   (progn (setq decimalpart 
                                (+ decimalpart 
                                   (/ (digit-char-p (getchar)) accum)))
                          (setq accum (* accum 10))))))
    (if (eql (peekchar) #\e)
        (progn
            (setq numtype 'real)
            (getchar)
            (case (peekchar)
                (#\+ (setq exponentdirection 1))
                (#\- (setq exponentdirection -1))
                (t   (setq exponent (digit-char-p (peekchar)))))
            (getchar)
            (while (eql (charclass (peekchar)) *numeric*) 
                (setq exponent (+ (* exponent 10) (digit-char-p (getchar)))))))

    ;(format t "~D ~D ~D ~D ~%" intpart decimalpart exponent exponentdirection)
    (setq n (+ intpart decimalpart))
    (if (> exponent 1000)
        (setq numtype 'error)
        (setq n (* n 
                   (expt 10 
                         (* exponentdirection exponent)))))
    (if (or 
          (and (eql numtype 'integer)
               (> n 2147483647))
          (and (eql numtype 'real)
               (or
                 (> n 3.402823E+38)
                 (< n 1.175495E-38)))
          (eql numtype 'error))
        (progn 
            (format t "~A number out of range ~%" (string numtype))
            (list 'numbertok 0 'error))
        (list 'numbertok n numtype)))
    

; Initialize, then call gettoken repeatedly until it returns NIL.
(defun test-scanner ()
  (let (token)
    (init-file)
    (while (setq token (gettoken)) (printtoken token))))
