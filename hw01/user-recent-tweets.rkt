#lang racket

(provide
 ; n-recent-tweets : N -> [Listof String]
 ; Picks a random user, and returns their follower's most recent n tweets
 n-recent-tweets)

(require
  db
  (only-in "create-database.rkt" TWEETY))

; n-recent-tweets : N -> [Listof String]
; Picks a random user, and returns their follower's most recent n tweets
(define (n-recent-tweets n)
  (follower-recent-tweets n (pick-user)))

; pick-user : -> N
; returns the ID of a random user
(define (pick-user)
  1)

; follower-recent-tweets : N N -> [Listof String]
; returns the most recent n tweets from the given user
(define (follower-recent-tweets n user)
  (recent-tweets/list (user-followers user) n))

; recent-tweets/list : N [Listof N] -> [Listof String]
; returns the most recent tweets from the users in the given list
(define (recent-tweets/list n users)
    (cond [(empty? users) '()]
          [else (query-list TWEETY
              (bind-prepared-statement
               (prepare TWEETY "SELECT tweet_text FROM tweets WHERE user_id IN (?)")
                 (foldr (λ (user str-so-far) (string-append user ", " str-so-far))
                        (first users)
                        (rest users))))]))

; user-followers : N -> [List-of N]
; returns all users that follow the given user
(define (user-followers user)
  (query-list TWEETY
              (bind-prepared-statement
               (prepare TWEETY "SELECT follows_id FROM followers WHERE user_id = ?")
               user)))