#lang racket


(provide benchmark-follow-results)

(require
  benchmark
  plot/pict)



(define (benchmark-follow-results with-indexes
                                  db-setup-func
                                  tweet-adding-func
                                  add-number-followers!)
  (define (benchmark-followers-setup _1 n-followers n-users)
    (db-setup-func #:with-indexes with-indexes)
    (tweet-adding-func n-users))

  
  (define (insert-followers sql-processing-type n-followers n-users)
    (time (add-number-followers! n-followers)))
  
  (define results (run-benchmarks
   ; how to insert followers
   '(sequential parallel)

   '((1000 2000)
     (1000 1000000))

   insert-followers

   #:build benchmark-followers-setup
   #:num-trials 15
   ))

  (plot-follow-results results))



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
