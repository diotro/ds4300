#lang racket
(require
  benchmark
  plot/pict

  "setup/tweet-generator.rkt"
  (only-in "setup/create-database.rkt" db-setup!)
  (only-in "setup/setup-tweets.rkt" add-n-tweets!)
  (only-in "setup/setup-followers.rkt" add-number-followers!)
  (only-in "setup/user-recent-tweets.rkt" n-recent-tweets))



(define (benchmark-timeline-results with-indexes)

  (define (setup n-follows n-retrieved n-tweets)
    (db-setup! #:with-indexes with-indexes)
    (add-n-tweets! n-tweets)
    (add-number-followers!
     (* n-tweets (match n-follows
                   ['10-followers 10]
                   ['100-followers 100])))
    (collect-garbage 'major)
  
  (run-benchmarks
   '(10-followers 100-followers) ; number of followers
   '((100 1000) ; number of tweets to retrieve
     (100000 1000000)) ; number of tweets in db
   timeline-request
   #:build setup
   #:num-trials 30
   #:extract-time 'delta-time
   ))


(define (timeline-request n-follows n-retrieved n-tweets)
  (build-list 100 (λ (_) (n-recent-tweets n-tweets))))

(define (plot-timeline-results results)
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
                                (* added 100) db-num-follows)))))))


(define timeline-results/index (benchmark-timeline-results #t))
(plot-timeline-results timeline-results/index)


(define timeline-results/no-index (benchmark-timeline-results #f))
(plot-timeline-results timeline-results/no-index)
