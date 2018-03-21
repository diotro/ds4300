#lang racket
(provide
 (all-from-out "parameters.rkt")
 keydir?
 (contract-out [find-keydir (-> keydir?)])
 (contract-out [read-keydir (-> keydir?)])
 (contract-out [write-keydir! (-> keydir? void?)])
 (struct-out bcfp)
 (contract-out [active-bitcask-file (-> port?)])
 (contract-out [active-bitcask-file-name (-> string? string?)])
 new-file
 (contract-out [in-bitcask (-> string? string?)])

 append-data)

(require "parameters.rkt"
         "test.rkt"
         json
         racket/serialize
         racket/hash)
(module+ test
  (require rackunit))

; A BFCP is a (make-bcfp String Integer Integer Integer)
; the file-id is the relative path to the file from the current bitcask
; the value-size is the size of the value (in bytes)
; the value-pos is the offset of the value (in bytes)
; the timestamp is when this keystamp was last updated
(define-serializable-struct bcfp
  [file-id value-size value-posn timestamp]
  #:transparent)

(define (read-keydir)
  (call-with-input-file (keydir-location)
    (λ (port) (deserialize (read port)))))

(define (write-keydir! hash)
  (call-with-output-file (keydir-location)
    #:exists 'replace
    (λ (port) (write (serialize hash) port))))
(define keydir? (hash/c symbol? bcfp?))

(module+ test
  ; test that write-keydir! is the opposite of read-keydir
  (setup-test-bitcask)
  (check-equal? (read-keydir) (hasheq))

  (define BCFP1 (bcfp "1.bcactive" 8 8 8))
  (define BCFP2 (bcfp "1.bcactive" 8 9 10))
  (define BCFP3 (bcfp "1.bcactive" 1 2 3))
  (define KEYDIR (hasheq 'a BCFP1 'b BCFP2 'c BCFP3))
  (write-keydir! KEYDIR)
  (check-equal? (read-keydir) KEYDIR))


(define (keydir-location)
  (string-append (current-bitcask) "/" KEYDIR-FILE-EXTENSION))

; -> Port
(define (active-bitcask-file)
  (open-input-file
   (active-bitcask-file-name)))

; -> String
(define (active-bitcask-file-name extension)
  (define highest-num (highest-num-extension extension))
  (string-append  (current-bitcask)
                  "/"
                  (string-append (number->string highest-num) extension)))


; Symbol JSExpr [#:timestamp Integer] [#:extension String] -> BCFP
; writes the given value to the current bitcask, returning its location
; optionally, uses the given timestamp or file extension instead of
; the defaults (now and .bcactive, respectively)
(define (append-data key value
                     #:timestamp [timestamp (current-milliseconds)]
                     #:extension [extension ACTIVE-FILE-EXTENSION])
  (define active-file-name (active-bitcask-file-name extension))
  (define active-file (open-output-file active-file-name #:exists 'append))
  (define offset
    (call-with-input-file active-file-name
      (λ (port) (string-length (port->string port)))))
  (define ksize (key-size key))
  (define vsize (value-size value))

  (define row (bc-row timestamp ksize vsize key value))
  (write-string row active-file)

  (close-output-port active-file)
  
  (bcfp active-file-name
        vsize
        (sub1 (- (+ offset (string-length row)) vsize))
        (current-milliseconds)))

; Int Int Int Symbol JSExpr -> String
; turns the values into the row to write in the bitcask
(define (bc-row timestamp key-size value-size key value)
  (string-join 
   `(
     "crc8byte"
     ,(number->string timestamp)
     ,(number->string key-size)
     ,(number->string value-size)
     ,(symbol->string key)
     ,(value->string value)
     )
   " " #:after-last "\n"))

(define (key-size key)
  (string-length (symbol->string key)))

(define (value->string val)
  (if (void? val) "" (jsexpr->string val)))

(define (value-size value)
  (string-length (value->string value)))

; -> Num
(define (highest-num-extension extension)
  (define names (map path->string (directory-list (current-bitcask))))
  (define active-names
    (filter (λ (name) (string-suffix? name extension)) names))
  (define file-nums
    (map (λ (name) (string->number (first (string-split name "."))))
         active-names))
  (if (empty? file-nums)
      1
      (apply max file-nums)))


(define (new-file extension)
  (define num (add1 (highest-num-extension extension)))
  (with-output-to-file (in-bitcask (string-append (number->string num) extension))
    #:exists 'append
    (thunk (display ""))))

(define (in-bitcask path)
  (string-append (current-bitcask) "/" path))


; find-keydir : -> KeyDir
; calculates the keydir for the current bitcask by looking at the data
(define (find-keydir #:extensions [extensions (list ACTIVE-FILE-EXTENSION
                                                    CLOSED-FILE-EXTENSION)])
  (define files
    (filter (λ (str) (ormap (λ (suffix) (string-suffix? str suffix)) extensions))
            (map (λ (p) (in-bitcask (path->string p)))
                 (directory-list (current-bitcask)))))
  (define ports (map open-input-file files))

  (define (find-keydir/file port file-name)
    (define-values [keydir offset]
      (for/fold ([keydir (hasheq)]
                 [offset 0])
                ([crc (in-port read port)]
                 [timestamp (in-port read port)]
                 [key-size (in-port read port)]
                 [value-size (in-port read port)]
                 [key (in-port read port)]
                 [value (in-port read-json port)])
        (define row (bc-row timestamp key-size value-size key value))
        (values (hash-set keydir key (bcfp file-name
                                           value-size
                                           (sub1 (- (+ offset (string-length row)) value-size))
                                           timestamp))
                (+ offset (string-length row)))))
    keydir)

  
  (begin0
    (for/fold ([keydir (hasheq)])
              ([port ports]
               [file files])
      (hash-union keydir
                  (find-keydir/file port file)
                  #:combine/key
                  (λ (key val1 val2)
                    (argmax bcfp-timestamp (list val1 val2)))))
    
    (for-each close-input-port ports)))
                
  


