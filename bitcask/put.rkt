#lang racket
(provide put!)

(require "common.rkt"
         json)

(require rackunit)

; String JSON -> Void
; puts the given value under the given key
(define (put! key value)
  (define ptr (write-value! (current-bitcask) (crc key value) key value))
  (hash-set! (current-keydir) key ptr)
  (write-key-hash! (current-bitcask) (current-keydir))
  (maybe-new-file!))


; write-value! : String String String JSON -> FilePointer
; writes the given value in the given directory, returning where it was written
(define (write-value! directory crc key value)
  (define write-file (current-write-file directory))
  (call-with-output-file write-file
    #:exists 'append
    (λ (port)
      (write-string crc port)
      (write-string (number->string (key-size key)) port)
      (write-string " " port)
      (write-string (number->string (value-size value)) port)
      (write-string " " port)
      (write-string (symbol->string key) port)
      (write-string " " port)
      (if (void? value)
          (write-string "#<void>" port)
          (write-json value port))
      (write-string "\n" port)
      (flush-output port)
      (list (number->string (highest-num-file (directory-list directory)))
            (value-size value)
            (- (file-size write-file) (value-size value) 1)
            (current-seconds)))))


; Stub, to be implemented, but keeps 8 byte width
(define (crc . args)
  "8 bytes.")


; write-key-hash! : String KeyHash -> Void
(define (write-key-hash! directory keys)
  (call-with-output-file (string-append directory "/keys.bitcask")
    #:exists 'replace
    (λ (port) (write-json keys port))))

; -> Void
(define (maybe-new-file!)
  (define directory (current-bitcask))
  (define old-write-file (current-write-file directory))
  (define active-file-size (file-size old-write-file))
      
  (when (> active-file-size MAX-FILE-SIZE-BEFORE-CLOSE)
    (rename-file-or-directory
     (current-write-file directory)
     (closed-name-for old-write-file))
    (with-output-to-file
        (current-write-file directory)
      ; write zero bytes
      (thunk (void)))))


(define (current-write-file directory)
  (string-append directory "/" (current-write-file-name directory)))

(define (current-write-file-name dir)
  (define dir-contents (directory-list dir)) 
  (define file (findf (λ (path) (string-suffix? (path->string path) ACTIVE-FILE-EXTENSION))
                      dir-contents))
 
  (if file
      (path->string file)
      (string-append (number->string
                      (add1 (highest-num-file dir-contents)))
                     ACTIVE-FILE-EXTENSION)))

(define (highest-num-file dir-contents)
  (define (get-path-num path)
    (string->number (first (string-split (path->string path) "."))))
  (define path-nums (filter-map get-path-num dir-contents))
  (apply max path-nums))

(define (closed-name-for file)
      (string-append (first (string-split file ".")) CLOSED-FILE-EXTENSION))

; String -> Number
(define (key-size key)
  (string-length (symbol->string key)))


; JSExpr -> Number
(define (value-size value)
  (if (void? value)
      7 ; length of "#<void>"
      (string-length (jsexpr->string value))))

