#lang racket

(provide tweet-insertion-results)

(require
  benchmark
  plot/pict
  (only-in "../tweet-generator.rkt" tweet))




(define (tweet-insertion-results db port reader)
  (define tweets (port->list reader port))
  ; Any... -> Void
  ; collects garbage, resets the database to the proper state, and adds a million tweets
  (define (reset-db . args)
    (collect-garbage 'major)
    (send db setup-db!)
    (send db add-tweets tweets))
  
  ; SQLConnectionMethod N _ -> Void
  ; inserts n tweets into the database with the given method
  (define (insert-tweets _1 n)
    (send db add-tweets (take tweets n)))
  
  (define results (run-benchmarks
   '(add)
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
