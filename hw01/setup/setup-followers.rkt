#lang racket
(provide
 ; N -> Void
 ; sets the number of followers in the database to n
 add-number-followers!
 ; -> Void
 clear-all-followers!)



(require db
         (only-in "create-database.rkt" TWEETY)
         (only-in "db-interaction.rkt" perform-sql/sequential perform-sql/parallel))


(define (clear-all-followers!)
  (perform-sql/parallel '("DELETE FROM followers WHERE 1 = 1")))


(define (add-number-followers! n [sql-processing-type 'sequential])
  (define sql (match sql-processing-type
                ['sequential perform-sql/sequential]
                ['parallel perform-sql/parallel]))
  (define queries (make-queries (get-users) n))
  (displayln queries)
  (sql queries))


; make-queries : [List-of N] N -> [List-of SQLQuery]
; makes queries to add n followers to each user
(define (make-queries user-ids num-followers)
  (define to-follow
    (for/fold ([followers '()])
              ([user user-ids])
      #:break (>= (length (apply append followers)) num-followers)
      (define new-followers (choose user-ids (ceiling (/ num-followers (length user-ids)))))
      (define (good-ones new-ones)
        (set->list (list->set new-ones)))
      (cons (good-ones new-followers) followers)))
  (filter identity (for/list ([follower-list to-follow]
                              [user user-ids])
                     (follower-string follower-list user))))


; [List-of N] N -> [Maybe String]
(define (follower-string followers user-id)
  (define (make-following-pair follower)
    (format "(~a, ~a)" follower user-id))
  
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



; [List-of N] N -> [List-of N]
; chooses n items from the lst, with replacement
(define (choose lst n)
  (build-list n (λ (_) (list-ref lst (random (length lst))))))


; -> [List-of N]
; returns a list of all users in the database
(define (get-users)
  (query-list (TWEETY) "SELECT DISTINCT user_id FROM tweets;"))

(define (num-users)
  (query-value (TWEETY) "SELECT COUNT(DISTINCT(user_id)) from tweets"))

(define (num-followers)
  (query-value (TWEETY) "SELECT COUNT(*) FROM followers"))