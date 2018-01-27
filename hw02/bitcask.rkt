#lang racket/base
; This file provides the interface for interacting with bitcask handles,
; and two functions that produce them.

(provide
 ; A BitCaskHandle is a bitcask<%>
 bitcask<%>
 
 ; String -> BitCaskHandle
 ; returns the bitcask handle for the given directory
 open/read-only

 
 ; String -> BitCaskHandle
 ; returns the bitcask handle in charge of the given directory, with write permissions
 open/read-write)



(require rackunit
         racket/class
         json)


; A BitCaskHandle is a bitcask<%>
(define bitcask<%>
  (interface () get list-keys fold put! delete! merge!))


; String -> BitCaskHandle
; returns the bitcask handle for the given directory
(define (open/read-only dir)
  (new bitcask% [directory dir]))


; String -> BitCaskHandle
; returns the bitcask handle in charge of the given directory, with write permissions
(define (open/read-write dir)
  (new bitcask-writer% [directory dir]))


(define bitcask%
  (class
      object%
    ; directory : String
    (init-field directory)
    ; key-hash : [Hash String JSON]
    (field [key-hash (read-key-hash directory)])
    (super-new)
    
    (public
      get
      put!
      delete!
      list-keys
      fold
      merge!)

    ; String -> JSON or Void
    (define (get key) (void))

    ; String JSON -> Boolean
    ; puts the given value under the given key,
    ; and returns whether the operation was successful
    (define (put! key value) (error "can't 'put!' with a read-only bitcask"))

    ; String -> Boolean
    ; deletes the value under the given key,
    ; and returns whether the key exists
    (define (delete! key) (error "can't 'delete!' with a read-only bitcask"))

    (define (list-keys) '())

    (define (fold f acc0) acc0)

    (define (merge!) (void))
    
    ))

; read-key-hash : String -> [Hash String BitCaskFilePointer]
; a pointer from a key to where to look
(define (read-key-hash directory)
  (define key-file (string-append directory "/keys.json"))
  (if (file-exists? key-file)
      (read-json (open-input-file))
      (hash)))

; Extends bitcask% to allow for writing to file.
(define bitcask-writer%
  (class 
      bitcask%
    (super-new)
    (override put!
              delete!)
    (define (put! key value)
      #f)

    (define (delete! key)
      #f)
    ))




(module+ test
  (define writer (open/read-write "test")))
  
      
  