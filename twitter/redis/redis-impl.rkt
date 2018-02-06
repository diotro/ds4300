#lang racket
(provide
 redis-no-broadcast%)



(require redis
         "../tweety-db.rkt"
         "../tweet-generator.rkt")

(define USER-LIST "user-list")
(define USER-PREFIX "user:")
(define TWEET-PREFIX "user-tweets:")

(define (redis-exec command-strings)
  (void
   (with-output-to-string
       (thunk (system
               (string-append
                "(printf \"" (string-join command-strings "\r\n") "\")"
                " | nc localhost 6379"))))))

(define abstract-redis%
  (class* object% (tweety-db<%>)
    (super-new)

    ; -> Void
    ; Builds the database, leaving it empty
    (define/public (setup-db!)
      (FLUSHDB)
      ; add 1000 users
      (for-each (λ (id) (SADD USER-LIST (string-append USER-PREFIX (number->string id))))
                (build-list 1000 add1)))

    ; [List-of Tweet] -> String
    ; creates the command to add tweets
    (define/public (add-tweets tweets)
      ; Tweet -> Void
      ; adds the given tweet
      (define (add-tweet tweet)
        (string-append
         "SADD "
         ; key
         TWEET-PREFIX  (number->string (tweet-user-id tweet))

         " "
         ; value, "-"-delimited
         "\\\""
         (number->string (tweet-user-id tweet))
         "-"
         (number->string (tweet-timestamp tweet))
         "-"
         (tweet-text tweet)
         "\\\" "))

      (for/async ([redis-set (in-slice 500 (map (λ (tweet) (add-tweet tweet)) tweets))])
        (redis-exec redis-set)))

    

    ; -> Number
    ; returns the most recent unused id for a tweet
    #;(define (next-tweet-id)
        (string-append TWEET-PREFIX (number->string (INCR "current_tweet_id"))))

    ; N -> Void
    ; adds n followers to each user
    (define/public (add-followers n)
      (define users (SMEMBERS USER-LIST))
      (define user-numbers
        (map (λ (str) (second (string-split (bytes->string/utf-8 str) ":"))) users))
      (define (add-followers user)
        (define (add-followers/acc user n-left)
          (cond [(= n-left 0) ""]
                [else (string-append
                       "SADD "
                       (bytes->string/utf-8 user) " "
                       (list-ref user-numbers (random (length user-numbers))) " "
                       "\r\n"
                       (add-followers/acc user (sub1 n-left)))]))
        (add-followers/acc user n))
      (redis-exec (map add-followers users)))
      

    ; N N -> Void
    ; returns the given number of tweets from followers of a random user
    (abstract timeline-request)

    (define (get-random-user)
      (define users (GET USER-LIST))
      (list-ref users (random (length users))))
    ))

(define redis-no-broadcast%
  (class abstract-redis%
    (super-new)

    (define/override (timeline-request user amount)
      (define followers (SMEMBERS (string-append USER-PREFIX (number->string user))))

      (define (get-tweets user)
        (SMEMBERS (string-append TWEET-PREFIX (bytes->string/utf-8 user))))
      (define tweets (map to-tweet (apply append (map get-tweets followers))))
      (take (sort tweets (λ (t1 t2) (< (tweet-timestamp t1) (tweet-timestamp t2))))
            (min (length tweets) amount)))
      
    ))

(define (to-tweet string)
  (define values (string-split (bytes->string/utf-8 string) "-"))
  (tweet (string->number (first values)) (string->number (second values)) (third values)))

