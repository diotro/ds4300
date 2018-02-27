#lang racket
(provide/contract
 [get (-> symbol? jsexpr?)])

(require json
         "common.rkt")


(define (get key)
  (if (hash-has-key? (current-keydir) key)
          (read-value (current-bitcask) (hash-ref (current-keydir) key))
          (void)))


; read-value : FilePointer -> JSON
; from the given directory, read the value the pointer points to
(define (read-value file-pointer)
  (call-with-input-file (input-file-from-id (current-bitcask) (fp-file-id file-pointer))
    (λ (port)
      (file-position port (fp-value-posn file-pointer))
      (define result (read-string (fp-value-size file-pointer) port))
      (if (string=? result "#<void>")
          (void)
          (string->jsexpr result)))))


; input-file-from-id : String -> String
(define (input-file-from-id dir id)
  (define file-if-closed (string-append dir "/" id CLOSED-FILE-EXTENSION))
  (define file-if-open   (string-append dir "/" id ACTIVE-FILE-EXTENSION))
  (cond
    [(file-exists? file-if-closed) file-if-closed]
    [else file-if-open]))
