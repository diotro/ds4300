#lang racket
(require
  benchmark
  plot/pict

db
  (only-in "setup/create-database.rkt" db-setup! TWEETY)
  (only-in "setup/setup-tweets.rkt" add-n-tweets!)
  (only-in "setup/setup-followers.rkt" add-number-followers! clear-all-followers!))


(define NUM-USERS-IN-DB 10000)


(define (benchmark-follow-results with-indexes)
  (define (benchmark-followers-setup _1 n)
    (db-setup! #:with-indexes with-indexes)
    (add-n-tweets! NUM-USERS-IN-DB)) ; ensure there are users

  (run-benchmarks
   ; how to insert followers
   '(sequential parallel)

   '((1000 2000)) ; number of followers

   insert-followers

   #:build benchmark-followers-setup
   #:clean benchmark-followers-setup
   #:num-trials 1
   #:extract-time 'delta-time
   ))

; SQLConnectionMethod N _ -> Voidz
; inserts n tweets into the database with the given method
(define (insert-followers sql-processing-type n)
  (add-number-followers! n sql-processing-type))


(define (plot-follow-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "timeline retrieval speed"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([added (first opts)])
                        (format "added ~a followers" added)))))))


(define follow-results/no-index (benchmark-follow-results #f))
(plot-follow-results follow-results/no-index)

(define follow-results/index (benchmark-follow-results #t))
(plot-follow-results follow-results/index)
