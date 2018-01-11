#lang racket
(provide
 ; add-n-tweets : N Connection String -> Void
 ; Adds n tweets to the given database, using the given string as the table to add the tweets to
 add-n-tweets)

(require
  db
  (only-in "create-database.rkt" TWEETY))


(module+ test
  (require rackunit))


(define WORDS (sequence->list (in-lines (open-input-file "/usr/share/dict/words"))))



; A Tweet is a (tweet N N DateTime String)
(struct tweet [id user-id timestamp text] #:transparent)

; add-n-tweets : N Connection String -> Void
; Adds n tweets to the given database.
(define (add-n-tweets n db table)
  (for (['i (build-list n identity)])
    (add-tweet-to-db (generate-tweet i) TWEETY "tweets")))
    
; add-tweet-to-db : Tweet Connection String -> Void
; adds the given tweet to the given database
(define (add-tweet-to-db tweet db table)
  (define (tweet->sql-string tweet)
    (format "(~a, ~a, ~a, ~a)"
            (tweet-id tweet)
            (tweet-user-id tweet)
            (tweet-timestamp tweet)
            (tweet-text tweet)))
  (query-exec db "INSERT INTO $1 VALUES $2" table (tweet->sql-string tweet)))

; generate-tweet : N -> Tweet
; produces a random tweet, including some hashtagged words, with the given id
(define (generate-tweet tweet-id)
  ; -> N
  ; generates a number in a log distribution
  (define (generate-tweet-user-id)
    (ceiling (log (random 1 22000) 1.001)))
  (tweet tweet-id
         (generate-tweet-user-id)
         (current-seconds)
         (generate-tweet-text)))

; generate-tweet-text : -> String
; generates a tweet out of words mixed with hashtags
(define (generate-tweet-text)
  (collapse-tweet (build-tweet)))

; build-tweet :  -> [Listof String]
; adds words at random to a tweet, until it's large enough
(define (build-tweet)
  (define (add-words tweet-so-far)
    (define tweet-plus-one-word (add-word tweet-so-far))
    (if (too-long? tweet-plus-one-word)
        (if (has-hashtag? (collapse-tweet tweet-so-far))
            tweet-so-far
            (add-words '()))
        (add-words tweet-plus-one-word)))
  (add-words '()))

; collapse-tweet : [List-of String] -> String
; collapses a tweet in progress into its text
(define (collapse-tweet word-list)
  (cond [(empty? word-list) ""]
        [else (foldr (λ (s1 s2) (string-append s1 " " s2))
                     (first word-list)
                     (rest word-list))]))

; too-long? : String -> Boolean
; is the given list of words over 140 characters total?
(define (too-long? tweet-text)
  (> (string-length (collapse-tweet tweet-text))
     (tweet-goal-length)))


; has-hashtag? : String -> Boolean
; does the given string contain a hashtag?
(define (has-hashtag? text)
  (regexp-match? #rx"(^|\\s)#\\S" text))
  

; -> Number
; what length should a generated tweet be?
(define (tweet-goal-length)
  (+ 40 (random 0 100)))
     

; add-word : [List-of String] -> [List-of String]
; 
(define (add-word tweet-in-progress)
  (cons (next-word) tweet-in-progress))

; next-word : -> String
; returns a random word to use in a tweet
(define (next-word)
  (define (maybe-add-hashtag word)
    (if (< (random) .4) (string-append "#" word) word))
  (maybe-add-hashtag (list-ref WORDS (random (length WORDS)))))

(module+ test
  ; tweets have the right id
  (check-equal? (tweet-id (generate-tweet 2)) 2)

  
  (define test-tweets (build-list 100 generate-tweet))

  (define (check-tweet cur-tweet)
    ; max 10,000 users
    (check-true (< 0 (tweet-user-id cur-tweet) 10001))
    ; between 1 and 140 chars
    (define chars-in-tweet (string-length (tweet-text cur-tweet)))
    (check-true (<= 1 chars-in-tweet 140))
    ; each tweet must have a hashtag
    (check-pred has-hashtag? (tweet-text cur-tweet)))

  (void (map check-tweet test-tweets))

  ; No duplicates in 100 tweets
  (define tweet-texts (map tweet-text test-tweets))
  (check equal? (set-count (list->set tweet-texts)) (length tweet-texts))
  )

