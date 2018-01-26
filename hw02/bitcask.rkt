#lang racket/base
(provide
 ; String -> BitCaskHandle
 ; returns the bitcask handle for the given directory
 open/read-only

 
 ; String -> BitCaskHandle
 ; returns the bitcask handle in charge of the given directory, with write permissions
 open/read-write

 bitcask%
 )


(require racket/class)


; String -> BitCaskHandle
; returns the bitcask handle for the given directory
(define (open/read-only directory)
  (new bitcask% [directory directory]))


; String -> BitCaskHandle
; returns the bitcask handle in charge of the given directory, with write permissions
(define (open/read-write directory)
  (new bitcask% [write #t] [directory directory]))


(define bitcask%
  (class
      object%
    (init-field directory)
    (init-field [write #f])
    
    (public
      get
      put!
      delete!
      list-keys
      fold
      merge)

    (define (get key) #f)

    (define (put! key value) (void))

    (define (delete! key) (void))

    (define (list-keys) '())

    (define (fold f acc0) acc0)

    (define (merge) (void))
    
    ))
      
