#lang racket
(provide
 ; [List-of SQLQuery] -> Void
 perform-sql/sequential
 
 ; [List-of SQLQuery] -> Void
 perform-sql/parallel)



(require
  db
  (only-in "create-database.rkt" TWEETY))


; [List-of SQLQuery] -> Void
(define (perform-sql/sequential queries)
  (for-each (λ (query) (query-exec (TWEETY) query)) queries))


; [List-of SQLQuery] -> Void
(define (perform-sql/parallel queries)
  (for/async ([query (in-slice (ceiling (/ (length queries) 16)) queries)])
    (for-each (λ (query) (query-exec (TWEETY) query)) queries)))
