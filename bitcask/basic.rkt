#lang racket
(require racket/contract/parametric)
(provide
 (contract-out [get (-> symbol? (or/c jsexpr? void?))])
 (contract-out [set! (-> symbol? jsexpr? boolean?)])
 (contract-out [delete! (-> symbol? void?)])
 (contract-out [list-keys (-> (listof symbol?))])
 (contract-out [fold (parametric->/c [X] (-> (-> symbol? jsexpr? X X) X X))]))

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
      (define val (read-string (bcfp-value-size bcfp) port))
      (if (void? val)
          val
          (string->jsexpr val)))))


; Symbol JSExpr -> Boolean
; sets the key to the given value, and returns whether there
; was already a value there
(define (set! key value)
  (define return-value (not (void? (get key))))
  (define new-pointer (append-data key value ))
  (define new-keydir (hash-set (read-keydir) key new-pointer))
  (if (> (bcfp-value-posn new-pointer) MAX-FILE-SIZE)
      (write-keydir! (new-active-file! new-keydir))
      (write-keydir! new-keydir))
  return-value)

(module+ test
  (setup-test-bitcask)
  (void (set! 'a 3))
  (check-equal? (get 'a) 3)

  (for ([i (in-range 100)])
    (void (set! 'a '(1 2 3))))
  (check-equal? (get 'a) '(1 2 3))
  )

; KeyDir -> KeyDir
(define (new-active-file! keydir)
  (define file (findf (λ (s) (string-suffix? s ACTIVE-FILE-EXTENSION))
                      (map path->string (directory-list (current-bitcask)))))
  (cond
    [file
     (new-file ACTIVE-FILE-EXTENSION)
     (rename-file-or-directory
      (in-bitcask file)
      (string-replace (in-bitcask file)
                      ACTIVE-FILE-EXTENSION
                      CLOSED-FILE-EXTENSION))
     (for/fold ([out-keydir (hasheq)])
               ([(key value) (in-hash keydir)])
       (hash-set
        out-keydir
        key
        (if (string-suffix? (bcfp-file-id value) (string-append "/" file))
            (struct-copy bcfp value
                         [file-id (string-replace (bcfp-file-id value)
                                                  ACTIVE-FILE-EXTENSION
                                                  CLOSED-FILE-EXTENSION)])
            value)))]
    [else keydir]))

; delete : Symbol -> Void
(define (delete! key)
  (set! key (void))
  (write-keydir! (hash-remove (read-keydir) key)))

(module+ test
  (set! 'a 3)
  (check-equal? (get 'a) 3)
  (delete! 'a)
  (check-equal? (get 'a) (void)))
  

; list-keys : -> [Listof Symbol]
(define (list-keys)
  (hash-keys (read-keydir)))

; fold [X] : [Symbol JSExpr X -> X] X -> X
(define (fold func base)
  (for/fold ([result base])
            ([key (in-list (list-keys))])
    (func key (get key) result)))
































