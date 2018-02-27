#lang racket

(provide (all-defined-out))
(require (only-in "get.rkt" get)
         (only-in "put.rkt" put!)
         "common.rkt"
         #;(only-in "fold.rkt" fold))

(parameterize ([current-bitcask "test"]
               [current-keydir (make-hash)])
  (put! 'a 3)
  (get 'a))
