#lang racket

(provide
 current-bitcask
 ACTIVE-FILE-EXTENSION
 CLOSED-FILE-EXTENSION
 MERGED-FILE-EXTENSION
 KEYDIR-FILE-EXTENSION
 MAX-FILE-SIZE)

(require json
         racket/serialize)

; String -> String
; ensures that whatever value the current bitcask holds
; is a string representing a real path, builds the
; directory with a empty keydir and file
(define (bitcask-guard new-bc)
  (when (not (directory-exists? new-bc))
    (make-directory new-bc))

  (define key-file (string-append new-bc "/" KEYDIR-FILE-EXTENSION))
  (when (not (file-exists? key-file))
    (with-output-to-file key-file
        (thunk (write (serialize (hasheq))))))
  
  new-bc)
  
(define current-bitcask
  (make-parameter #f bitcask-guard))

  

(define ACTIVE-FILE-EXTENSION ".bcactive")
(define CLOSED-FILE-EXTENSION ".bcclosed")
(define MERGED-FILE-EXTENSION ".bcmerged")
(define KEYDIR-FILE-EXTENSION "keydir.bck")
(define MAX-FILE-SIZE 50000)