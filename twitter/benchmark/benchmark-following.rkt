#lang racket


(provide benchmark-follow-results)

(require
  benchmark
  plot/pict
  "../tweet-generator.rkt")



(define (benchmark-follow-results db port reader)
  
  (define tweets (port->list reader port))
  
  (define (benchmark-followers-setup _1 n-followers n-users)
    (send db setup-db!)
    (send db add-tweets tweets))

  (define (insert-followers _1 n-followers n-users)
    (time (send db add-followers n-followers)))
  
  (define results
    (run-benchmarks
     ; how to insert followers
     '(add)

     '((10 100)
       (10000 100000))

     insert-followers

     #:build benchmark-followers-setup
     #:num-trials 5
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
