#lang racket
(provide
 redis-no-broadcast%
 redis-broadcast%)



(require redis
         "../tweety-db.rkt"
         "../tweet-generator.rkt")

(define USER-LIST "user-list")
(define USER-PREFIX "user:")
(define TWEET-PREFIX "user-tweets:")


;---------------------------------------------------------------------------------------------------
; UTILITY FUNCTIONS

(define-syntax-rule (MULTI/create-conn commands ...)
  (void (parameterize ([current-redis-connection (connect)])
          (do-MULTI commands ...))))

; -> Number
; returns a random user
(define (get-random-user)
  (define users (GET USER-LIST))
  (list-ref users (random (length users))))

; String -> Tweet
; separates the given tweet by "-" and uses the fields as the components of a Tweet
(define (redis-tweet-string->tweet string)
  (define values (string-split (bytes->string/utf-8 string) "-"))
  (tweet (string->number (first values)) (string->number (second values)) (third values)))

    
; -> Number
; returns the most recent unused id for a tweet
(define (next-tweet-id)
  (string-append TWEET-PREFIX (number->string (INCR "current_tweet_id"))))

; Tweet -> String
; puts the given tweet into a string that can be stored
(define (tweet->redis-tweet-string tweet)
  (string-append
   (number->string (tweet-user-id tweet))
   "-"
   (number->string (tweet-timestamp tweet))
   "-"
   (tweet-text tweet)))

; Integer -> [List-of Tweet]
; produces the list of tweets the given user has made
(define (get-tweets user)
  (define string-tweets (SMEMBERS (string-append TWEET-PREFIX (bytes->string/utf-8 user))))
  (map redis-tweet-string->tweet string-tweets))

; Integer -> [List-of String]
; returns the keys for all of the followers of this user
(define (followers user)
  (SMEMBERS (string-append USER-PREFIX (number->string user))))

;---------------------------------------------------------------------------------------------------
; ABSTRACT CLASS
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

    
    ; N N -> Void
    ; returns the given number of tweets from followers of a random user
    (abstract timeline-request)

    ; [List-of Tweet] -> String
    (abstract add-tweets)

    ; N -> Void
    ; adds n followers to each user
    (define/public (add-followers n)
      (define conn (current-redis-connection))
      (define users (SMEMBERS USER-LIST))
      (define user-numbers
        (map (λ (str) (second (string-split (bytes->string/utf-8 str) ":"))) users))
      
      (define (add-followers user)
        (define (add-followers/acc user n-left)
          (cond [(= n-left 0) ""]
                [else (SADD #:rconn conn
                            (bytes->string/utf-8 user)
                            (list-ref user-numbers (random (length user-numbers))))
                      (add-followers/acc user (sub1 n-left))]))
        (add-followers/acc user n))

      (MULTI/create-conn (for-each add-followers users)))
    ))


;---------------------------------------------------------------------------------------------------
; NO-BROADCAST IMPLEMENTATION
; each user's tweets are stored with them, and dynamically queried
; whenever you make a timeline request
(define redis-no-broadcast%
  (class abstract-redis%
    (super-new)

    (define/override (add-tweets tweets)
      ; Tweet -> Void
      ; adds the given tweet
      (define (add-tweet tweet)
        (SADD
         (string-append TWEET-PREFIX  (number->string (tweet-user-id tweet)))
         (tweet->redis-tweet-string tweet)))
      
      (MULTI/create-conn (for-each add-tweet tweets)))

    
    (define/override (timeline-request user amount)
      (define followers (SMEMBERS (string-append USER-PREFIX (number->string user))))
      (define tweets (append-map get-tweets followers))

      (take (sort tweets (λ (t1 t2) (< (tweet-timestamp t1) (tweet-timestamp t2))))
            (min (length tweets) amount)))
    ))


;---------------------------------------------------------------------------------------------------
; BROADCAST IMPLEMENTATION
; each user's timeline is created when you insert a 

(define TIMELINE-PREFIX "timeline:")

(define redis-broadcast%
  (class abstract-redis%
    (super-new)

    (define/override (add-tweets tweets)
      (define (add-tweet tweet followers)
        (add-tweet-to-user tweet)
        (add-tweet-to-timelines tweet followers))

      (define (add-tweet-to-user tweet)
        (SADD
         (string-append TWEET-PREFIX  (number->string (tweet-user-id tweet)))
         (tweet->redis-tweet-string tweet)))

      (define (add-tweet-to-timelines tweet followers)
        (define (add-to-timeline user)
          (ZADD (string-append TIMELINE-PREFIX (bytes->string/utf-8 user))
                (tweet-timestamp tweet)
                (tweet->redis-tweet-string tweet)))
        (for-each add-to-timeline followers))

      (define tweeter-followers (map followers (map tweet-user-id tweets)))
      (MULTI/create-conn (for-each add-tweet tweets tweeter-followers)))

    
    (define/override (timeline-request user amount)
      (define tweets (ZREVRANGE (string-append TIMELINE-PREFIX (number->string user)) 0 amount))
      (or tweets '()))
    ))
