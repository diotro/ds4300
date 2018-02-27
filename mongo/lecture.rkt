#lang racket
(require db/mongodb)


(current-mongo-db (mongo-db (create-mongo) "twitter"))

(void
 (mongo-collection-drop!
  (make-mongo-collection (current-mongo-db) "posts")))

(define-mongo-struct post "posts"
  ([user #:required]
   [text #:required]
   [views #:inc]
   [comments #:push]))


(define (print-posts-by-me)
  (define posts (make-mongo-collection (current-mongo-db) "posts"))
  (define my-posts (mongo-collection-find posts (hasheq 'user "me")))
  (println (sequence->list my-posts)))


(define p (make-post #:user "me" #:text "bleh"))
(print-posts-by-me)

(inc-post-views! p)
(print-posts-by-me)

(push-post-comments! p "hello")
(print-posts-by-me)