#lang racket

(require

  (only-in "tweet-generator.rkt" generate-n-tweets) 

  (only-in "sql/sql-impl.rkt" sql-index% sql-no-index%)
  ; Individual benchmarks
  (only-in "benchmark/benchmark-timeline.rkt" benchmark-timeline-results)
  (only-in "benchmark/benchmark-tweet-insertion.rkt" tweet-insertion-results)
  (only-in "benchmark/benchmark-following.rkt" benchmark-follow-results)
  pict)


; [Instanceof tweety-db<%>] -> Image
; runs benchmarks against the given database
(define (run-benchmark tweety)
  (vl-append (benchmark-timeline-results tweety TWEETS)
             (tweet-insertion-results    tweety TWEETS)
             (benchmark-follow-results   tweety TWEETS)))

(define TWEETS (generate-n-tweets 1000000))


(apply run-benchmark (new sql-index%))
(apply run-benchmark (new sql/no-index%))
