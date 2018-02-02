#lang racket

(require "setup/tweet-generator.rkt"
         (only-in "setup/create-database.rkt" db-setup!)
         (only-in "setup/setup-tweets.rkt" add-n-tweets!)
         (only-in "setup/setup-followers.rkt" add-number-followers!)
         (only-in "setup/user-recent-tweets.rkt" n-recent-tweets)
         (only-in "benchmark-timeline.rkt" benchmark-timeline-results))


; [ -> Void] [N -> Void] [N -> Void] [N -> Void] -> Image
; uses db-setup! to set up a data base.
; add-tweets to add tweets to it,
; add-followers to add followers to it
; timeline-request to make a request to the timeline
; and then times how long they take
(define (run-benchmark db-setup! add-tweets add-followers timeline-request)
  (benchmark-timeline-results db-setup! add-tweets add-followers timeline-request))


(define SQL-NO-INDEX
  (list (thunk (db-setup! #:with-indexes #f))
        add-n-tweets!
        add-number-followers!
        n-recent-tweets))

(define SQL-WITH-INDEX
  (list (thunk (db-setup! #:with-indexes #t))
        add-n-tweets!
        add-number-followers!
        n-recent-tweets))

(apply run-benchmark SQL-NO-INDEX)
(apply run-benchmark SQL-WITH-INDEX)
