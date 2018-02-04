#lang racket

(provide
 ; n-recent-tweets : N -> [Listof String]
 ; Picks a random user, and returns their follower's most recent n tweets
 n-recent-tweets)


(require
  db
  (only-in "create-database.rkt" TWEETY))


; n-recent-tweets : N N -> [Listof String]
; Produces the given user's follower's most recent n tweets
(define (n-recent-tweets user n)
  (follower-recent-tweets n user))


; pick-user : -> N
; returns the ID of a random user
(define (pick-user)
  (query-value (TWEETY) "SELECT DISTINCT (user_id)
                       FROM tweets ORDER BY RAND() LIMIT 1"))


; follower-recent-tweets : N N -> [Listof RowResult]
; returns the most recent n tweets from the given user
(define (follower-recent-tweets n user)
  (define sql (recent-tweets/list n (user-followers user)))
  (when sql (query-rows (TWEETY) sql)))




; recent-tweets/list : N [Listof N] -> [Maybe String]
; returns the most recent tweets from the users in the given list
(define (recent-tweets/list n users)
  (cond [(empty? users) #f]
        [else (string-append "SELECT *
                         FROM tweets
                         WHERE user_id
                         IN (" (foldr (λ (user str-so-far)
                                        (string-append (number->string user) ", " str-so-far))
                                      (number->string (first users))
                                      (rest users)) ")
                         ORDER BY tweet_ts DESC
                         LIMIT " (number->string n))]))


; user-followers : N -> [List-of N]
; returns all users that follow the given user
(define (user-followers user)
  (define conn (TWEETY))
  (query-list conn
              (bind-prepared-statement
               (prepare conn "SELECT follows_id FROM followers WHERE user_id = ?")
               (list user))))