#lang racket
(provide
 ; generate-tweet : N  -> Tweet
 ; generates a tweet for the user with the given id
 generate-tweet
 ; N -> Tweet
 ; generates n tweets, from unique users
 generate-n-tweets
 (struct-out tweet))

(require
  db
  (only-in "create-database.rkt" TWEETY))


(module+ test
  (require rackunit))


(define WORDS
  (filter (λ (x) (< (random) .05))
          (sequence->list
           (in-lines
            (open-input-file "/usr/share/dict/words")))))



; A Tweet is a (tweet N N String)
(struct tweet [user-id timestamp text] #:transparent)

; generate-n-tweets : N -> Tweet
; generates n tweets, from unique users
(define (generate-n-tweets n)
  (build-list n generate-tweet))

; generate-tweet : N -> Tweet
; produces a random tweet, including some hashtagged words, from the given user
(define (generate-tweet user-id)
  (tweet user-id
         (random)
         (generate-tweet-text)))

; generate-tweet-text : -> String
; generates a tweet out of words mixed with hashtags
(define (generate-tweet-text)
  (collapse-tweet (build-tweet)))

; build-tweet :  -> [Listof String]
; adds words at random to a tweet, until it's large enough
(define (build-tweet)
  (define (build-tweet/acc tweet-so-far)
    (define tweet-plus-one-word (add-word tweet-so-far))
    (if (too-long? tweet-plus-one-word)
        (if (has-hashtag? (collapse-tweet tweet-so-far))
            tweet-so-far
            (build-tweet/acc '()))
        (build-tweet/acc tweet-plus-one-word)))
  (build-tweet/acc '()))

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
  (regexp-match? #px"(^#|\\s#)\\S" text))

(module+ test
  (check-pred has-hashtag? "#hello")
  (check-pred has-hashtag? "#hello #twohashtags")
  (check-pred has-hashtag? "hello #whoa")
  (check-pred has-hashtag? "sweer #asdf")
  (check-false (has-hashtag? "hello"))
  (check-false (has-hashtag? "hel#lo"))
  (check-false (has-hashtag? "hello #"))
  (check-false (has-hashtag? "hello # "))
  (check-false (has-hashtag? "# "))
  )
  

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
    (if (< (random) .3) (string-append "#" word) word))
  (maybe-add-hashtag (list-ref WORDS (random (length WORDS)))))

(module+ test

  (define test-tweets (build-list 100 generate-tweet))

  (define (check-tweet cur-tweet)
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

