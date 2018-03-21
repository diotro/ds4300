#lang racket
(require bitcask)


(delete-directory/files "bitcasko-testing")
(current-bitcask "bitcasko-testing")

(define my-array `(1 #f "strings" ,(hasheq 'dict #t) null ("nested array")))
(void
  (set! 'num 3)
  (set! 'array my-array)
  
  (for ([i (in-range 1 100)])
    (set! (string->symbol (make-string i #\a)) my-array))
  (set! 'a "not array anymore"))



(equal? (get 'num) 3)
(equal? (get 'array) my-array)
(equal? (get 'a) "not array anymore")
(equal? (get 'aa) my-array)

(bitcask-merge)

(equal? (get 'num) 3)
(equal? (get 'array) my-array)
(equal? (get 'a) "not array anymore")
(equal? (get 'aa) my-array)

(length (list-keys))
(fold (λ (key value count) (add1 count)) 0)

(delete! 'a)
(equal? (get 'a) (void))
(length (list-keys))