#lang racket

(require
  (only-in "tweet-generator.rkt" generate-n-tweets) 

  (only-in "sql/sql-impl.rkt" sql-index% sql-no-index%)
  (only-in "redis/redis-impl.rkt" redis-no-broadcast%)
  
  ; Individual benchmarks
  (only-in "benchmark/benchmark-timeline.rkt" benchmark-timeline-results)
  (only-in "benchmark/benchmark-tweet-insertion.rkt" tweet-insertion-results)
  (only-in "benchmark/benchmark-following.rkt" benchmark-follow-results)
  (only-in "tweet-generator.rkt" tweet)
  pict)




;; tweety-db<%> -> Image
;; runs benchmarks against the given database
(define (run-benchmark tweety)
  ; the reader that converts the port into a list of tweets
  (define (port-reader port)
    (define next-val (read port))
    (cond [(eof-object? next-val) next-val]
          [else (apply tweet (rest (vector->list (read port))))]))

  ; runs the benchmark with the tweets file and a reader that can handle it
  (define (run-benchmark-func benchmark-function)
    (call-with-input-file "tweets.txt"
      (λ (port) (benchmark-function tweety port port-reader))))
  
  
  (define timeline (run-benchmark-func benchmark-timeline-results))
  (displayln "timeline")
  (define tweets (run-benchmark-func tweet-insertion-results))
  (displayln "tweet")
  (define follow (run-benchmark-func benchmark-follow-results))
  (displayln "follow")

  (vl-append
   timeline
   tweets
   follow))


;;---------------------------------------------------------------------------------------------------
;; Run the actual benchmarks
(define (save-benchmark pict name)
  (define bitmap (pict->bitmap pict))
  (send bitmap save-file name 'png))


#;(save-benchmark (run-benchmark (new sql-index%)) "img/sql-index.png")

#;(save-benchmark (run-benchmark (new sql-no-index%)) "img/sql-no-index.png")

(save-benchmark (run-benchmark (new redis-no-broadcast%)) "img/redis-no-broadcast")









