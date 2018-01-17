#lang racket
(require
  benchmark
  plot/pict

  "setup/tweet-generator.rkt"
  (only-in "setup/create-database.rkt" db-setup!)
  (only-in "setup/setup-tweets.rkt"
           add-tweets!
           add-n-tweets!
           clear-all-tweets!
           num-tweets)
  (only-in "setup/user-recent-tweets.rkt" n-recent-tweets))



(define (tweet-insertion-results with-indexes)  
  ; Any... -> Void
  ; collects garbage, resets the database to the proper state, and adds a million tweets
  (define (reset-db . args)
    (collect-garbage 'major)
    (db-setup! #:with-indexes with-indexes)
    ;(add-n-tweets! 1000000 'parallel))
  
  ; SQLConnectionMethod N _ -> Void
  ; inserts n tweets into the database with the given method
  (define (insert-tweets sql-processing-method n)
    (add-tweets! (take TWEETS n) sql-processing-method))
  
  (define TWEETS (generate-n-tweets 20000))

  (reset-db)
  
  (run-benchmarks
   ; how to insert tweets
   '(sequential parallel)

   '(; number of tweets to add
     (5000 10000))

   ; run the benchmark
   insert-tweets

   #:num-trials 1
   #:clean reset-db
   #:extract-time 'delta-time
   ))


(define (plot-tweet-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "tweet insertion speed, into a db with a million tweets"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([added (first opts)])
                        (format "~a tweets" added)))))))


(define tweet-results/no-index (tweet-insertion-results #f))
(plot-tweet-results tweet-results/no-index)

(define tweet-results/index (tweet-insertion-results #t))
(plot-tweet-results tweet-results/index)