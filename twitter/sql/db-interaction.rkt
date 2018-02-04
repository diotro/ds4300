#lang racket
(provide
 ; [List-of SQLQuery] -> Void
 perform-sql/sequential
 
 ; [List-of SQLQuery] -> Void
 perform-sql/parallel

 query-value/tweety)



(require
  db
  (only-in "create-database.rkt" TWEETY))


; [List-of SQLQuery] -> Void
(define (perform-sql/sequential queries)
  (for-each (λ (query) (query-exec (TWEETY) query)) queries))


; [List-of SQLQuery] -> Void
(define (perform-sql/parallel queries)
  (when (cons? queries) ; empty lists break in-slice
    (for/async ([query-set (in-slice (ceiling (/ (length queries) 16)) queries)])
      ; each thread should perform queries sequentially
      (perform-sql/sequential query-set))))


(define (query-value/tweety q)
  (query-value (TWEETY) q))