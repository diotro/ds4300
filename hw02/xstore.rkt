#! /usr/bin/env racket
#lang racket


; Called with a single argument, specifying the file to be used to store data
; Reads a single json dictionary from stdin, adding each pair

(require
  (only-in "store.rkt" store-values!)
  (only-in json read-json))


(define db-filename (vector-ref (current-command-line-arguments) 0))
(define db-file (open-output-file db-filename #:exists 'append))
(store-values! db-file (read-json))
  