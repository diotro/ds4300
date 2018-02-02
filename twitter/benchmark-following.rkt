#lang racket
(require
  benchmark
  plot/pict

  db
  (only-in "setup/create-database.rkt" db-setup! TWEETY)
  (only-in "setup/setup-tweets.rkt" add-n-tweets!)
  "setup/setup-followers.rkt")



(define (benchmark-follow-results with-indexes
                                  db-setup-func
                                  tweet-adding-func
                                  follower-query-producer)
  (define (benchmark-followers-setup _1 n-followers n-users)
    (db-setup-func #:with-indexes with-indexes)
    (tweet-adding-func n-users))

  
  (define (insert-followers sql-processing-type n-followers n-users)
    (define queries (make-queries n-followers))
    (time ((run-sql sql-processing-type) queries)))
  
  (run-benchmarks
   ; how to insert followers
   '(sequential parallel)

   '((1000 2000)
     (1000 1000000))

   insert-followers

   #:build benchmark-followers-setup
   #:num-trials 15
   ))



(define (plot-follow-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "adding followers"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([amt-added (first opts)]
                            [n-users (second opts)])
                        (format "added ~a followers, db has ~a users"
                                amt-added n-users)))))))


(define follow-results/no-index (benchmark-follow-results #f))
(plot-follow-results follow-results/no-index)

(define follow-results/index (benchmark-follow-results #t))
(plot-follow-results follow-results/index)
