#lang racket
(require db)
(provide
 ; Connection
 ; the connection to the tweety database
 TWEETY)

(define LOCAL-MYSQL-USER "root")
(define LOCAL-MYSQL-PASS "mysql")
(define DATABASE-NAME    "tweety")

(define DB-CONNECTIONS '())
(define DB-CONNECTION-INDEX 0)

(define (TWEETY)
  (cond [(< (length DB-CONNECTIONS) 8) (new-connection!)]
        [else (next-conn!)]))

(define (new-connection!)
  (define conn
    (mysql-connect #:user     LOCAL-MYSQL-USER
                   #:password LOCAL-MYSQL-PASS
                   #:database DATABASE-NAME))
  (set! DB-CONNECTIONS (cons conn DB-CONNECTIONS))
  conn)

(define (next-conn!)
  (define conn (list-ref DB-CONNECTIONS DB-CONNECTION-INDEX))
  (set! DB-CONNECTION-INDEX (remainder (add1 DB-CONNECTION-INDEX) (length DB-CONNECTIONS)))
  conn)

(define (db-reset!)
  (query-exec (TWEETY) "DROP TABLE IF EXISTS tweets")
  (query-exec (TWEETY) "CREATE TABLE IF NOT EXISTS tweets (
    tweet_id   BIGINT NOT NULL AUTO_INCREMENT,
    user_id    BIGINT,
    tweet_ts   DATETIME,
    tweet_text VARCHAR(140),
    CONSTRAINT tweet_id_pk PRIMARY KEY (tweet_id)
)")
  (query-exec (TWEETY) "ALTER TABLE tweets ADD INDEX `user_id` (`user_id`)")


  (query-exec (TWEETY) "DROP TABLE IF EXISTS followers")
  (query-exec (TWEETY) "CREATE TABLE IF NOT EXISTS followers (
    user_id    BIGINT,
    follows_id BIGINT,
    CONSTRAINT both_id_pk PRIMARY KEY (user_id, follows_id))")
  )