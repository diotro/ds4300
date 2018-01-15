#lang racket
(provide
 ; N -> Void
 ; sets the number of followers of each user to a fixed amount
 add-uniform-number-followers!
 ; -> Void
 clear-all-followers!)



(require db
         (only-in "create-database.rkt" TWEETY)
         (only-in "db-interaction.rkt" perform-sql/parallel))

(define (clear-all-followers!)
  (perform-sql/parallel '("DELETE FROM followers WHERE 1 = 1")))

; sets the number of followers of each user to be n
(define (add-uniform-number-followers! n)
  (set-number-followers! (λ () n)))


; [ -> Number] -> Void
; sets the number of followers of each user to the result of the given generator
(define (set-number-followers! follower-count-generator)
  (define users (get-users))
  (for-each (λ (user-id) (add-followers! user-id (follower-count-generator) users)) users))


; N N [List-of N] -> Void
; adds to the given user the given number of followers, from the givne list of ids
(define (add-followers! user-id n possible-users)
  (define followers (choose n possible-users))
  (define query-arg (follower-string followers user-id))
  (when query-arg
    (perform-sql/parallel `(,query-arg))))


; [List-of N] N -> [Maybe String]
(define (follower-string followers user-id)
  (make-following-string (not-in-db (map (λ (f) (list f user-id)) followers))))

(define (make-following-string followers)
  (define (make-following-pair follower)
    (format "(~a, ~a)" (first follower) (second follower)))
  
  (cond [(empty? followers) #f]
        [else  (string-append
                "INSERT INTO followers VALUES "
                (foldr
                 (λ (follower query-string)
                   (string-append
                    (make-following-pair follower)
                    ", "
                    query-string))
                 (make-following-pair (first followers))
                 (rest followers)))]))

;[List [Pair N N]] -> [List [Pair N N]]
(define (not-in-db followers)
  (define in-db (map vector->list (query-rows (TWEETY) "SELECT * FROM followers")))
  (filter-not (λ (follower) (member follower in-db)) (set->list (list->set followers))))
  


; N [List-of N] -> [List-of N]
; chooses n items from the lst, with replacement
(define (choose n lst)
  (build-list n (λ (_) (list-ref lst (random (length lst))))))


; -> [List-of N]
; returns a list of all users in the database
(define (get-users)
  (query-list (TWEETY) "SELECT DISTINCT user_id FROM tweets;"))

(define (num-users)
  (query-value (TWEETY) "SELECT COUNT(DISTINCT(user_id)) from tweets"))