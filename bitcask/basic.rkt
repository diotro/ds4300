#lang racket

(provide
 (contract-out [get (-> symbol? (or/c jsexpr? void?))])
 (contract-out [set! (-> symbol? jsexpr? boolean?)]))

(require "common.rkt"
         "test.rkt"
         json
         rackunit)

; Symbol -> [Or JSExpr Void]
; returns the value under the given key in the current bitcask
(define (get key)
  (define bcfp (hash-ref (read-keydir) key #f))
  (if bcfp (read-bcfp bcfp) (void)))

; BCFP -> JSExpr
; reads the json value at the given BCFP
(define (read-bcfp bcfp)
  (call-with-input-file (bcfp-file-id bcfp)
    (λ (port)
      (read-string (bcfp-value-posn bcfp) port)
      (string->jsexpr (read-string (bcfp-value-size bcfp) port)))))


; Symbol JSExpr -> Boolean
; sets the key to the given value, and returns whether there
; was already a value there
(define (set! key value)
  (define return-value (not (void? (get key))))
  (define new-pointer (append-data key value))
  (write-keydir! (hash-set (read-keydir) key new-pointer))
  (when (> (bcfp-value-posn new-pointer) MAX-FILE-SIZE)
    (new-active-file!))
  return-value)

(module+ test
  (setup-test-bitcask)
  (void (set! 'a 3))
  (check-equal? (get 'a) 3)

  (for ([i (in-range 100)])
    (void (set! 'a '(1 2 3))))
  (check-equal? (get 'a) '(1 2 3))
  )

(define (new-active-file!)
  (define file (findf (λ (s) (string-suffix? s ACTIVE-FILE-EXTENSION))
                      (map path->string (directory-list (current-bitcask)))))
  (when file
    (new-file ACTIVE-FILE-EXTENSION)
    #;(rename-file-or-directory
     (in-bitcask file)
     (string-replace (in-bitcask file)
                     ACTIVE-FILE-EXTENSION
                     CLOSED-FILE-EXTENSION))))


































