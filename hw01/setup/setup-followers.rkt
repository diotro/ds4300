#lang racket
(provide
 ; N -> Void
 ; sets the number of followers in the database to n
 add-number-followers!
 ; -> Void
 clear-all-followers!

 run-sql
 make-queries)



(require db
         (only-in "create-database.rkt" TWEETY)
         (only-in "db-interaction.rkt" perform-sql/sequential perform-sql/parallel))


(define (clear-all-followers!)
  (perform-sql/parallel '("DELETE FROM followers WHERE 1 = 1")))

(define (add-number-followers! n [sql-processing-type 'sequential])
  ((run-sql sql-processing-type) (make-queries n)))


(define (run-sql type)
  (match type
    ['sequential perform-sql/sequential]
    ['parallel perform-sql/parallel]))

; make-queries :  N -> [List-of SQLQuery]
; makes queries to add n followers to users in the database
(define (make-queries num-followers)
  (define user-ids (get-users))
  (define maybe-followers (choose-followers user-ids num-followers))
  (define in-db (get-followers))
  
  (define to-follow
    (foldr
     ; (list N N) [Hash N [List-of N]] -> [Hash N [List-of N]]
     ; removes the given pair from the hash
     (λ (follower-pair follower-hash)
       (let ([follower (first follower-pair)]
             [followed (second follower-pair)])
       (if (hash-has-key? follower-hash follower)
           (hash-set follower-hash
                     follower
                     (remove followed (hash-ref follower-hash follower)))
           follower-hash)))
     maybe-followers
     in-db))

  (filter identity (hash-map to-follow follower->sql-query)))


; [List-of N] N -> [Hash N [List-of N]]
; returns a list of the same length as the given list containing a total of
; num-followers elements from the given list
(define (choose-followers user-ids num-followers)
  (for/fold ([followers (hash)])
            ([user user-ids])
    #:break (>= (length (apply append (hash-values followers))) num-followers)
    (define new-followers (choose user-ids (ceiling (/ num-followers (length user-ids)))))
    (define (good-ones new-ones)
      (remove user (remove-duplicates new-ones)))
    ; set the user's followers in the hash to be the good ones of the new followers
    (hash-set followers user (good-ones new-followers))))


; [List-of N] N -> [Maybe String]
(define (follower->sql-query user-id follows)
  (define (make-following-pair follower)
    (format "(~a, ~a)" user-id follower))
  
  (cond [(empty? follows) #f]
        [else  (string-append
                "INSERT INTO followers VALUES "
                (foldr
                 (λ (follower query-string)
                   (string-append
                    (make-following-pair follower)
                    ", "
                    query-string))
                 (make-following-pair (first follows))
                 (rest follows)))]))



; [List-of N] N -> [List-of N]
; chooses n items from the lst, with replacement
(define (choose lst n)
  (build-list n (λ (_) (list-ref lst (random (length lst))))))


; -> [List-of N]
; returns a list of all users in the database
(define (get-users)
  (query-list (TWEETY) "SELECT DISTINCT user_id FROM tweets;"))

; -> [List-of (cons N N)]
; returns a list of all followers in the database
(define (get-followers)
  (define follower-rows (query-rows (TWEETY) "SELECT * FROM followers;"))
  (map vector->list follower-rows))

(define (num-users)
  (query-value (TWEETY) "SELECT COUNT(DISTINCT(user_id)) from tweets"))

(define (num-followers)
  (query-value (TWEETY) "SELECT COUNT(*) FROM followers"))