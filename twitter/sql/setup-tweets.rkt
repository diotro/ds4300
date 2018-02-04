#lang racket
(require
  db
  (only-in "sql-db.rkt" TWEETY)
  "../tweet-generator.rkt")

(provide
 ; -> Void
 ; deletes all the tweets from the database
 clear-all-tweets!

 ; [Listof String] -> Void
 ; adds the given tweets to the database
 add-tweets!
 )


; -> Void
; clears all the tweets.
(define (clear-all-tweets!)
  (query-exec TWEETY '("DELETE FROM tweets WHERE 1 = 1")))


(define (add-n-tweets! n)
  (add-tweets! (generate-n-tweets n)))

; [List-of Tweet] -> Void
; adds the given tweets to the database
(define (add-tweets! tweets)
  (define sql (generate-sql tweets))
  (when sql
    (query-exec TWEETY sql)))


; [List-of Tweet] -> [List-of SQLQuery]
(define (generate-sql tweets)
  (cond [(empty? tweets) #f]
        [else
         (define prefix "INSERT INTO tweets VALUES ")

         (string-append
                (string-append*
                 prefix
                 (map (λ (t) (string-append (tweet->sql t) ",")) (rest tweets)))
                (tweet->sql (first tweets)))]))


(define (tweet->sql t)
  (format "(DEFAULT, ~a, FROM_UNIXTIME(~a), '~a')"
          (tweet-user-id t)
          (tweet-timestamp t)
          (tweet-text t)))

