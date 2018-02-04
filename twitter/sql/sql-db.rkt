#lang racket

(provide TWEETY)


(require db)

; Implementation for sql connection, db-setup.
(define LOCAL-MYSQL-USER "root")
(define LOCAL-MYSQL-PASS "mysql")
(define DATABASE-NAME    "tweety")


(define (db-connection)
  (mysql-connect #:user     LOCAL-MYSQL-USER
                 #:password LOCAL-MYSQL-PASS
                 #:database DATABASE-NAME))

(define TWEETY (db-connection))