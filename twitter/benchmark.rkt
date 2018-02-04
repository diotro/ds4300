#lang racket

(require
  racket/serialize
  (only-in "tweet-generator.rkt" generate-n-tweets) 

  (only-in "sql/sql-impl.rkt" sql-index% sql-no-index%)
  ; Individual benchmarks
  (only-in "benchmark/benchmark-timeline.rkt" benchmark-timeline-results)
  (only-in "benchmark/benchmark-tweet-insertion.rkt" tweet-insertion-results)
  (only-in "benchmark/benchmark-following.rkt" benchmark-follow-results)
  pict)

;; [Instanceof tweety-db<%>] -> Image
;; runs benchmarks against the given database
(define (run-benchmark tweety)
  (define (run-benchmark-func  benchmark-function)
    (call-with-input-file "tweets.txt"
      (λ (port) (benchmark-function tweety port))))
  
  (vl-append
   (run-benchmark-func benchmark-timeline-results)
   (run-benchmark-func tweet-insertion-results)
   (run-benchmark-func benchmark-follow-results)))


(run-benchmark (new sql-index%))
(run-benchmark (new sql-no-index%))
