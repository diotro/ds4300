#lang racket
(require
  db
  (only-in "create-database.rkt" TWEETY)
  (only-in "tweet-generator.rkt" add-n-tweets))

(define (create-a-million-tweets)
  (add-n-tweets-gradually 1000000))

(define (add-n-tweets-gradually number)
  (define (add-tweets so-far to-do)
    (cond [(zero? to-do) (print "Done.")]
          [else
           (define tweets-per-run 1000)
           (add-n-tweets (min to-do tweets-per-run) TWEETY)
           (println (format "~a tweets left." to-do))
           (add-tweets (+ so-far tweets-per-run) (- to-do tweets-per-run))]))
  
  (add-tweets 0 (- number (query-value TWEETY "SELECT COUNT(*) FROM tweets"))))
  



; Adding followers
(define (get-user-ids)
  (query-list TWEETY "SELECT DISTINCT user_id FROM tweets;"))

(define USER-IDS (get-user-ids))

(define (add-followers-to-everone)
  (define users-already-following (query-exec TWEETY "SELECT DISTINCT user_id FROM followers"))
  (for-each follow-some-people
            (filter (λ (user) (member user users-already-following)) USER-IDS)))


(define (follow-some-people user-id)
  
  (define (follow-n-people n)
    (cond [(zero? n) #t]
          [else
           (follow-person user-id)
           (follow-n-people (sub1 n))]))

  (define (follow-person user-id)
    (define to-be-followed (list-ref USER-IDS (random (length USER-IDS))))
    (query-exec TWEETY
                (bind-prepared-statement
                 (prepare TWEETY "INSERT INTO followers VALUES (?, ?)")
                 (list user-id to-be-followed))))
  
  (follow-n-people (random 0 500)))


(create-a-million-tweets)
(add-followers-to-everone)
