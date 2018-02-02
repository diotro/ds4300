#lang racket
(provide benchmark-timeline-results)


(require
  benchmark
  plot/pict)




(define (benchmark-timeline-results db-setup! add-n-tweets! add-number-followers! timeline-func)
  (define (setup n-follows n-retrieved n-tweets)
    (db-setup!)
    (add-n-tweets! n-tweets)
    (add-number-followers!
     (* n-tweets (match n-follows
                   ['1-followers 1]
                   ['10-followers 10])))
    (collect-garbage 'major))

  (define (timeline-request n-follows n-retrieved n-tweets)
  (timeline-func n-tweets))
  
  (plot-results (run-benchmarks
   '(1-followers 10-followers) ; number of followers
   '((100 1000) ; number of tweets to retrieve
     (10000 100000)) ; number of tweets in db
   timeline-request
   #:build setup
   #:num-trials 5
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

