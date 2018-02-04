#lang racket
; The interface used in benchmarks.

(provide
 tweety-db<%>)

(define tweety-db<%>
  (interface ()
    ; -> Void
    ; Builds the database, leaving it empty
    setup-db!

    ; [List-of Tweet] -> Void
    ; adds the tweets
    add-tweets

    ; N -> Void
    ; adds n followers to each user
    add-followers

    ; N N -> Void
    ; returns the given number of tweets from followers of the given user
    timeline-request))
