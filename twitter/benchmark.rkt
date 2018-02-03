#lang racket

(require

  (only-in "tweet-generator.rkt" generate-n-tweets) 

  ; SQL
  (rename-in "sql/create-database" [db-setup! db-setup!/sql])
  (rename-in "sql/setup-tweets.rkt" [add-n-tweets! add-n-tweets!/sql])
  (rename-in "sql/setup-followers.rkt" [add-number-followers! add-number-followers!/sql])
  (rename-in "sql/user-recent-tweets.rkt" [n-recent-tweets n-recent-tweets/sql])


  ; Individual benchmarks
  (only-in "benchmark/benchmark-timeline.rkt" benchmark-timeline-results)
  (only-in "benchmark/benchmark-tweet-insertion.rkt" tweet-insertion-results)
  (only-in "benchmark/benchmark-following.rkt" benchmark-follow-results)
  pict)


; [ -> Void] [N -> Void] [N -> Void] [N -> Void] -> Image
; uses db-setup! to set up adata base.
; add-tweets to add tweets to it,
; add-followers to add followers to it
; timeline-request to make a request to the timeline
; and times how long they take
(define (run-benchmark db-setup! add-tweets add-followers timeline-request)
  (vl-append (benchmark-timeline-results db-setup! add-tweets add-followers timeline-request TWEETS)
             (tweet-insertion-results    db-setup! add-tweets add-followers timeline-request TWEETS)
             (benchmark-follow-results   db-setup! add-tweets add-followers timeline-request TWEETS)))

(define TWEETS (generate-n-tweets 1000000))

(define SQL-NO-INDEX
  (list (thunk (db-setup!/sql #:with-indexes #f))
        add-n-tweets!/sql
        add-number-followers!
        n-recent-tweets/sql))

(define SQL-WITH-INDEX
  (list (thunk (db-setup!/sql #:with-indexes #t))
        add-n-tweets!/sql
        add-number-followers!/sql
        n-recent-tweets/sql))

(apply run-benchmark SQL-NO-INDEX)
(apply run-benchmark SQL-WITH-INDEX)
