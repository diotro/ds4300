#lang racket
(require
  benchmark
  plot/pict

  "tweet-generator.rkt"

  (only-in "setup-tweets.rkt"
           add-tweets!
           clear-all-tweets!)
  (only-in "setup-followers.rkt"
           add-uniform-number-followers!
           clear-all-followers!)
  (only-in "user-recent-tweets.rkt" n-recent-tweets))


;---------------------------------------------------------------------------------------------------
; TWEET INSERTION RESULTS
;---------------------------------------------------------------------------------------------------

(define (tweet-insertion-results)
  (run-benchmarks
   ; how to insert tweets
   '(sequential parallel)

   '(; number of tweets to add
     (5000 10000 20000)
     ; number of tweets already in the database
     (0 1000000))

   ; run the benchmark
   insert-tweets

   #:build benchmark-tweets-setup
   #:clean (λ (_1 _2 _3) clear-all-tweets!)  ; remove all tweets
   #:num-trials 3
   #:extract-time 'delta-time
   ))


; _ _ N -> Void
; adds n the tweets to the database
(define (benchmark-tweets-setup _1 _2 n)
  (define (add-tweets-gradually! n)
    (cond [(< n NUM-TWEETS) (add-tweets! (get-tweets n))]
          [else (add-tweets! (get-tweets NUM-TWEETS))
                
                (add-tweets-gradually! (- n NUM-TWEETS))]))
  (add-tweets-gradually! n))

; SQLConnectionMethod N _ -> Void
; inserts n tweets into the database with the given method
(define (insert-tweets sql-processing-type n _)
  (add-tweets! (get-tweets n) sql-processing-type))

(define NUM-TWEETS 1000)
(define TWEETS (generate-n-tweets NUM-TWEETS))

(define (get-tweets n)
  (cond [(< n NUM-TWEETS) (take TWEETS n)]
        [else (append (take TWEETS NUM-TWEETS) (get-tweets (- n NUM-TWEETS)))]))


;---------------------------------------------------------------------------------------------------
; TIMELINE RESULTS
;---------------------------------------------------------------------------------------------------


(define (benchmark-timeline-results)
  
  (run-benchmarks
   ; how to retrieve timelines
   '(retrieve)

   '(; number of tweets to retrieve per use
     (100 1000)

     ; number of followers per user
     (10 100)

     ; number of tweets (and users)
     (100000 1000000)
     )

   ; run the benchmark
   timeline-request

   #:build benchmark-followers-setup
   #:clean (λ (_1 _2 _3 _4) clear-all-followers!)  ; remove all tweets
   #:num-trials 1
   #:extract-time 'delta-time
   ))


(define (benchmark-followers-setup _1 _2 n-follows n-tweets)
  (clear-all-followers!)
  (clear-all-tweets!)
  (benchmark-tweets-setup _1 _2 n-tweets)
  (add-uniform-number-followers! n-follows))


(define (timeline-request how n _1 _2)
  (n-recent-tweets n))

;---------------------------------------------------------------------------------------------------
; RENDERING
;---------------------------------------------------------------------------------------------------

(define (plot-tweet-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "tweet insertion speed"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([added (first opts)]
                            [n-tweets (second opts)])
                        (format "inserted ~a tweets, db had ~a tweets"
                                added n-tweets)))))))

(define (plot-timeline-results results)
  (parameterize ([plot-x-ticks no-ticks])
    (plot
     #:title "timeline retrieval speed"
     #:x-label #f
     #:y-label "time (ms)"
     (render-benchmark-alts
      (vector-ref (struct->vector (first results)) 2)
      #:normalize? #f
      results
      #:format-opts (lambda (opts)
                      (let ([added (first opts)]
                            [db-num-follows (second opts)]
                            [db-num-tweets (third opts)])
                        (format "retrieved ~a, db had ~a followers per user and ~a tweets"
                                added db-num-follows db-num-tweets)))))))



(define tweet-results (tweet-insertion-results))
(define timeline-results (benchmark-timeline-results))


(plot-tweet-results tweet-results)
(plot-timeline-results timeline-results)
 