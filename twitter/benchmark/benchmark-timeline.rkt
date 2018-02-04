#lang racket
(provide benchmark-timeline-results)


(require
  benchmark
  plot/pict
  racket/serialize)




(define (benchmark-timeline-results db port)
  (define tweets (deserialize (read port)))

  (define (setup n-follows n-retrieved n-tweets)
    (send db setup-db!)
    (send db add-tweets tweets)
    (send db add-followers (match n-follows
                             ['1-followers 1]
                             ['10-followers 10]))
  (collect-garbage 'major))

(define (timeline-request n-follows n-retrieved n-tweets)
  (send db timeline-request (random 100) n-tweets))
  
(plot-results
 (run-benchmarks
  '(1-followers 10-followers) ; number of followers
  '((100 1000) ; number of tweets to retrieve
    (10000 100000)) ; number of tweets in db
  timeline-request
  #:build setup
  #:num-trials 30
  #:extract-time 'delta-time
  )))


(define (plot-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "timeline retrieval speed (100 requests)"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([added (first opts)]
                            [db-num-follows (second opts)])
                        (format "retrieved ~a, db had ~a tweets"
                                added db-num-follows)))))))

