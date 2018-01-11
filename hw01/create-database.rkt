#lang racket
(require db)
(provide
 ; connection?
 ; the connection to the tweety database
 TWEETY)

(define LOCAL-MYSQL-USER "root")
(define LOCAL-MYSQL-PASS "mysql")
(define DATABASE-NAME    "tweety")


(define mysql
  (mysql-connect #:user LOCAL-MYSQL-USER
                 #:password LOCAL-MYSQL-PASS))

(query-exec mysql "CREATE DATABASE IF NOT EXISTS tweety")


(define TWEETY
  (mysql-connect #:user LOCAL-MYSQL-USER
                 #:password LOCAL-MYSQL-PASS
                 #:database DATABASE-NAME))

(query-exec TWEETY "DROP TABLE IF EXISTS tweets")
(query-exec TWEETY "CREATE TABLE IF NOT EXISTS tweets (
    tweet_id   BIGINT,
    user_id    BIGINT,
    tweet_ts   DATETIME,
    tweet_text VARCHAR(140),
    CONSTRAINT tweet_id_pk PRIMARY KEY (tweet_id)
)")

(query-exec TWEETY "DROP TABLE IF EXISTS followers")
(query-exec TWEETY "CREATE TABLE IF NOT EXISTS followers (
    user_id    BIGINT,
    follows_id BIGINT
)")

(module+ test
  (require rackunit)
  (check-true (table-exists? TWEETY "tweets"))
  (check-true (table-exists? TWEETY "followers"))
  )