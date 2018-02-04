#lang racket

(provide
 sql-index%
 sql-no-index%)


(require
  "../tweety-db.rkt"
  "sql-db.rkt"
  db
  "setup-followers.rkt"
  "setup-tweets.rkt"
  "user-recent-tweets.rkt")



(define abstract-sql%
  (class* object% (tweety-db<%>)
    (super-new)

    (abstract setup-db!)

    ; [List-of Tweet] -> Void
    ; adds the tweets
    (define/public (add-tweets tweets)
      (add-tweets! tweets))

    ; N -> Void
    ; adds n followers to each user
    (define/public (add-followers n)
      (add-number-followers! n))

    ; N N -> Void
    ; returns the given number of tweets from followers of the given user
    (define/public (timeline-request user n-tweets)
      (n-recent-tweets user n-tweets))
    ))


(define sql-index%
  (class abstract-sql%
    (super-new)
    (override setup-db!)
    (define (setup-db!)
      (db-setup! #:with-indexes #true))))


(define sql-no-index%
  (class abstract-sql%
    (super-new)
    (override setup-db!)
    (define (setup-db!)
      (db-setup! #:with-indexes #false))))


(define (db-setup! #:with-indexes with-indexes)
  (query-exec TWEETY "DROP TABLE IF EXISTS tweets")
  (query-exec TWEETY "CREATE TABLE IF NOT EXISTS tweets (
    tweet_id   BIGINT NOT NULL AUTO_INCREMENT,
    user_id    BIGINT,
    tweet_ts   DATETIME,
    tweet_text VARCHAR(140),
    CONSTRAINT tweet_id_pk PRIMARY KEY (`tweet_id`)
    )")


  (query-exec TWEETY "DROP TABLE IF EXISTS followers")
  (query-exec TWEETY "CREATE TABLE IF NOT EXISTS followers (
    user_id    BIGINT,
    follows_id BIGINT,
    CONSTRAINT both_id_pk  PRIMARY KEY (`user_id`, `follows_id`)
    )")

  (when with-indexes
    (query-exec TWEETY "ALTER TABLE tweets    ADD INDEX      `user_id`  (`user_id`)")
    (query-exec TWEETY "ALTER TABLE tweets    ADD INDEX      `tweet_ts` (`tweet_ts`)")
    (query-exec TWEETY "ALTER TABLE followers ADD INDEX      `user_id`  (`user_id`)")))