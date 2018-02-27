#lang racket

(provide
 current-bitcask
 current-keydir
 TOMBSTONE
 ACTIVE-FILE-EXTENSION
 CLOSED-FILE-EXTENSION
 MAX-FILE-SIZE-BEFORE-CLOSE

 fp-file-id
 fp-value-size
 fp-value-posn
 fp-timestamp)
3
; (current-bitcask) gives a reference to the bitcask to use
(define current-bitcask (make-parameter #f))

; (current-keydir) is the current keydir
(define current-keydir (make-parameter (make-hash)))

; The value saying that a value was deleted, JSON has no #<void> so this is unique
(define TOMBSTONE (void))

; the file extension for the write-file
(define ACTIVE-FILE-EXTENSION ".bcactive")

; the file extension for all closed files
(define CLOSED-FILE-EXTENSION ".bcclosed")

; the size cap for files before closed
(define MAX-FILE-SIZE-BEFORE-CLOSE 500)


; convenience accessors for a FilePointer:
(define fp-file-id    first)
(define fp-value-size second)
(define fp-value-posn third)
(define fp-timestamp  fourth)
