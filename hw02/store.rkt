#lang racket/base
(provide

 store-values!)

; [Hash String JSON] -> [Hash Key KeyDir]
; stores all the values in the hash under their corresponding key
(define (store-values! db-file hash)
  
  ; String JSON -> Void
  ; stores the given value under the given key
  (define (store-value! key value)
    (store-value-in-file db-file value)
    (store-key-pointer-location! key))
  
  (hash-for-each hash store-value!))