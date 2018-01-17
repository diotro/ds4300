#lang racket
(require
  benchmark
  plot/pict

  "setup/tweet-generator.rkt"
  (only-in "setup/create-database.rkt" db-setup!)
  (only-in "setup/setup-tweets.rkt" add-n-tweets!)
  (only-in "setup/setup-followers.rkt" add-uniform-number-followers!)
  (only-in "setup/user-recent-tweets.rkt" n-recent-tweets))


;---------------------------------------------------------------------------------------------------
; TIMELINE RESULTS
;---------------------------------------------------------------------------------------------------


(define (benchmark-timeline-results with-indexes)

  (define (benchmark-followers-setup n-follows n-retrieved n-tweets)
    (db-setup! #:with-indexes with-indexes)
    (add-n-tweets! n-tweets)
    (add-uniform-number-followers!
     (match n-follows ['10-followers 10]
       ['20-followers 20])))
  
  (define (timeline-request n-follows n-retrieved n-tweets)
    (n-recent-tweets n-tweets))

  
  (run-benchmarks
   '(10-followers 20-followers) ; number of followers
   '((1 100) ; number of tweets to retrieve
     (10000)) ; number of tweets in db
   timeline-request
   #:build benchmark-followers-setup
   #:clean (λ (n-follows n-retrieved n-tweets) (db-setup! #:with-indexes with-indexes))
   #:num-trials 1
   #:extract-time 'delta-time
   ))


(define (plot-timeline-results results)
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
                      (let ([added (first opts)]
                            [db-num-follows (second opts)])
                        (format "retrieved ~a, db had ~a tweets"
                                added db-num-follows)))))))


(define timeline-results/index (benchmark-timeline-results #t))
(plot-timeline-results timeline-results/index)


(define timeline-results/no-index (benchmark-timeline-results #f))
(plot-timeline-results timeline-results/no-index)
