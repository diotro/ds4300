#lang racket

(provide tweet-insertion-results)

(require
  benchmark
  plot/pict)



(define (tweet-insertion-results db-setup! add-tweets! add-number-followers! timeline-func tweets)  
  ; Any... -> Void
  ; collects garbage, resets the database to the proper state, and adds a million tweets
  (define (reset-db . args)
    (collect-garbage 'major)
    (db-setup!)
    (add-tweets! 1000000 'parallel))
  
  ; SQLConnectionMethod N _ -> Void
  ; inserts n tweets into the database with the given method
  (define (insert-tweets sql-processing-method n)
    (add-tweets! (take tweets n) sql-processing-method))
  

  (reset-db)
  
  (define results (run-benchmarks
   '(sequential parallel)
   '((5000 10000 20000))
   insert-tweets

   #:num-trials 10
   #:clean reset-db
   #:extract-time 'delta-time
   ))
  (plot-tweet-results results))


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
