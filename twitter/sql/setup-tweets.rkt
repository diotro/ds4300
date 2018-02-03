#lang racket
(require
  db
  "db-interaction.rkt"
  "tweet-generator.rkt")

(provide
 ; -> Void
 ; deletes all the tweets from the database
 clear-all-tweets!

 ; [Listof String] SQLConnectionMethod -> Void
 ; adds the given tweets to the database, from the given number of users,
 ; with a method chosen by the SQLConnectionMethod
 add-tweets!
 num-tweets
 )


; -> Void
; clears all the tweets.
(define (clear-all-tweets!)
  (perform-sql/sequential '("DELETE FROM tweets WHERE 1 = 1")))

; A SQLConnectionMethod is one of:
; - 'sequential
; - 'parallel

; [List-of Tweet] SQLConnectionMethod -> Void
; adds the given tweets to the database
(define (add-tweets! tweets [method 'sequential])
  (define perform-sql (match method
                        ['sequential  perform-sql/sequential]
                        ['parallel    perform-sql/parallel]))
  (perform-sql (generate-sql tweets)))


; [List-of Tweet] -> [List-of SQLQuery]
(define (generate-sql tweets)
  (cond [(empty? tweets) '()]
        [else
         (define prefix "INSERT INTO tweets VALUES ")

         (list (string-append
                (string-append*
                 prefix
                 (map (λ (t) (string-append (tweet->sql t) ",")) (rest tweets)))
                (tweet->sql (first tweets))))]))


(define (tweet->sql t)
  (format "(DEFAULT, ~a, FROM_UNIXTIME(~a), '~a')"
          (tweet-user-id t)
          (tweet-timestamp t)
          (tweet-text t)))

(define (num-tweets)
  (query-value/tweety "SELECT COUNT(*) FROM tweets"))
