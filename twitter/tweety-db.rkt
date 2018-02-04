#lang racket
; The interface used in benchmarks.

(provide
 tweety-db<%>)

(define tweety-db<%>
  (interface ()
    ; -> Void
    ; Builds the database, with a million tweets, with 100 tweets per user
    setup-db!

    ; [List-of Tweet] -> Void
    ; adds the tweets
    add-tweets

    ; [List-of [Pair N N]] -> Void
    ; adds each of the given follower pairs
    add-followers

    ; N N -> Void
    ; returns the given number of tweets from followers of the given user
    timeline-request))
