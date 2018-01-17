#lang racket
(require
  db
  racket/math
  (only-in "create-database.rkt" TWEETY)
  (only-in "tweet-generator.rkt" add-n-tweets))

(provide
 ; N N -> Void
 ; creates the given number of tweets from the given number of users
 set-number-tweets-in-db!)
 
; N -> Void
; sets the number of tweets in the database
(define (set-number-tweets-in-db! number)
  (clear-all-tweets!)
  (add-tweets 0 (- number (query-value TWEETY "SELECT COUNT(*) FROM tweets"))))
  
(define (clear-all-tweets!)
  (query-exec TWEETY "DELETE FROM tweets WHERE 1 = 1")
)

; Adding followers

(define USER-IDS (query-list TWEETY "SELECT DISTINCT user_id FROM tweets;"))

; -> Void
; Each user follows some number of other users
(define (everyone-follows-someone)
  (define users-already-following (query-list TWEETY "SELECT DISTINCT user_id FROM followers"))
  (define users-not-following-yet
    (filter-not (λ (user) (member user users-already-following)) USER-IDS))

  (define (follow-and-print users)
    (cond [(empty? users) (println "Done creating follow-ships.")]
          [else 
           (define num-users (min (length users) 100))
           (define this-round (take users num-users))
           (define remaining-users (drop users num-users))
           (for-each follow-some-people this-round)
           (println (format "~a people left" (length remaining-users)))
           (follow-and-print remaining-users)]))
  (follow-and-print users-not-following-yet))


; N -> Void
; the given user follows some people
(define (follow-some-people user-id)
  ; N -> Void
  ; the user follows the given number of people
  (define (follow-n-people n)
    (cond [(zero? n) (void n)]
          [else
           (follow-person user-id)
           (follow-n-people (sub1 n))]))

  (define (follow-person user-id)
    (define to-be-followed (list-ref USER-IDS (random (length USER-IDS))))
    (with-handlers ([exn:fail? (λ (exn) #f)])
      (query-exec TWEETY
                  (bind-prepared-statement
                   (prepare TWEETY "INSERT INTO followers VALUES (?, ?)")
                   (list user-id to-be-followed)))))
  
  (follow-n-people (random 0 200)))

; -> [List-of N]
; returns a list of all users in the database
(define (get-users)
  (query-list TWEETY "SELECT DISTINCT user_id FROM tweets;"))
