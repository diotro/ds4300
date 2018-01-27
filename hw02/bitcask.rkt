#lang racket/base
; This file provides the interface for interacting with bitcask handles,
; and two functions that produce them.

(provide
 ; A BitCaskHandle is a bitcask<%>
 bitcask<%>
 
 ; String -> BitCaskHandle
 ; returns the bitcask handle in charge of the given directory, with write permissions
 open/read-write)



(require rackunit
         racket/class
         racket/string
         json)


; A BitCaskHandle is a bitcask<%>
(define bitcask<%>
  (interface () get list-keys fold put! delete! merge!))

; A BitCaskFilePointer is a (bcfp String N N TimeStamp)
(define-struct bcfp [file-id value-size value-position timestamp] #:transparent)
; file-id is where to find the value
; value-size is the size (in bytes) of the value
; value-position is the offset from the start of the file where the value begins
; timestamp is when this key was last updated


; String -> BitCaskHandle
; returns the bitcask handle in charge of the given directory, with write permissions
(define (open/read-write dir)
  (new bitcask% [directory dir]))


(define bitcask%
  (class*
      object%
    (bitcask<%>)
    ; directory : String
    (init-field directory)
    ; key-hash : [Hash String BitCaskFilePointer]
    (field [key-hash (read-key-hash directory)])
    (super-new)
    
    (public
      get
      put!
      delete!
      list-keys
      fold
      merge!)

    ; Symbol -> JSON or Void
    (define (get key)
      (if (hash-has-key? key-hash key)
          (do-lookup directory (hash-ref key-hash key))
          (void)))

    ; Symbol JSON -> Boolean
    ; puts the given value under the given key,
    ; and updates the key-hash
    (define (put! key value)
      (define file-name (append-file-name directory))
      (define file (append-file directory))
      (do-append file key value)
      (define size-of-value (value-size value))
      (define start-loc (- (file-size file-name) size-of-value 1))
      (hash-set! key-hash key
                 (bcfp "test1"
                       size-of-value
                       start-loc
                       (current-seconds))))


    ; Symbol -> Boolean
    ; deletes the value under the given key,
    ; and returns whether the key exists
    (define (delete! key)
      #f)
      
    (define (list-keys) (hash-keys key-hash))

    (define (fold f acc0) acc0)

    (define (merge!) (void))
    
    ))


; read-key-hash : String -> [Hash Symbol BitCaskFilePointer]
; a pointer from a key to where to look
(define (read-key-hash directory)
  (define key-file (string-append directory "/keys.json"))
  (if (file-exists? key-file)
      (make-bcfp-hash (read-json (open-input-file key-file)))
      (hash)))

; make-bcfp-hash : [Hash Symbol List] -> [Hash Symbol BitCaskFilePointer]
(define (make-bcfp-hash json)
  (make-hash
   (hash-map json (λ (key value) (cons key (apply bcfp value))))))

; do-lookup : Symbol BitCaskFilePointer -> JSON
; in the given directory, reads the specified value
(define (do-lookup directory bcfp)
  (define file (open-input-file (string-append directory "/" (bcfp-file-id bcfp))))
  (file-position file (bcfp-value-position bcfp)) ; throw away the offset
  (begin0
    (read-bytes (bcfp-value-size bcfp) file)
    (close-input-port file)))


; do-append : OutputFileHandle Symbol JSON -> Boolean
; appends the given key/value to the file, and returns
; #t if successful and #f otherwise
(define (do-append file key value)
  (write-string (key+value->string key value) file))


; key+value->string : Symbol JSON -> String
; produces the string encoding for a key and value
(define (key+value->string key value)
  (string-join
   (list "crc"
         (number->string (current-seconds))
         (number->string (key-size key))
         (number->string (value-size value))
         (symbol->string key)
         (jsexpr->string value)
         "\n")))


; key-size : Symbol -> Number
(define (key-size key)
  (string-length (symbol->string key)))

; value-size : Symbol -> Number
(define (value-size value)
  (string-length (jsexpr->string value)))


; append-file : String -> OutputFileHandle
; returns the file to append bitcask information to
(define (append-file dir)
  (open-output-file (append-file-name dir) #:exists 'append))


; append-file-name : String -> String
; given a directory, what is the name of the current append file
(define (append-file-name dir)
  (string-append dir "/" "test1"))





(module+ test
  (define writer (open/read-write "test"))
  (send writer put! 'b  "asdf")
  (send writer get 'b)
  (send writer delete! 'a)

  (send writer put! 'asdf 1)
  (send writer get 'asdf))
  
      
