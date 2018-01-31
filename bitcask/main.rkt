#lang racket

(provide bitcask%)



(require json)



(define bitcask%
  (class
      object%
    (super-new)

    (public
      get
      put!
      delete!
      list-keys
      fold)


    
    (init-field directory)
    (field [key-map (read-key-hash directory)])
    
    (define (get key)
      (if (hash-has-key? key-map key)
          (read-value directory (hash-ref key-map key))
          (void)))

    (define (put! key value)
      (define ptr (write-value! directory (crc key value) key value))
      (set! key-map (hash-set key-map key ptr)))

    (define (delete! key)
      (put! key TOMBSTONE))

    (define (list-keys)
      (hash-keys key-map))

    (define (fold func acc)
      (define keys (send this list-keys))
      (foldr
       func
       acc
       (send this list-keys)
       (map (λ (key) (send this get key)) keys)))
    ))

(define TOMBSTONE (void))
  
; A KeyHash is a [HashEq String FilePointer]

; A FilePointer is a (list String N N N)
; containing the id of the file, the size of the value,
; the position of the value, and the timestamp of the last write
; convenience accessors:
(define fp-file-id    first)
(define fp-value-size second)
(define fp-value-posn third)
(define fp-timestamp  fourth)

; read-key-hash : String -> KeyHash
(define (read-key-hash directory)
  (if (file-exists? (key-file-name directory))
      (call-with-input-file 
          (λ (port) (read-json port)))
      (hasheq)))

; write-key-hash! : String KeyHash -> Void
(define (write-key-hash! directory keys)
  (call-with-output-file (string-append directory "/keys.bitcask")
    (λ (port) (write-json keys))))

; read-value : String FilePointer -> JSON
; from the given directory, read the value the pointer points to
(define (read-value directory file-pointer)
  (call-with-input-file (string-append directory "/" (fp-file-id file-pointer))
    (λ (port)
      (file-position port (fp-value-posn file-pointer))
      (define result (read-string (fp-value-size file-pointer) port))
      (if (and (string? result) (string=? result "#<void>"))
          (void)
          (string->jsexpr result)))))

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
      (write-string key port)
      (write-string " " port)
      (if (void? value)
          (write-string "#<void>" port)
          (write-json value port))
      (write-string "\n" port)
      (flush-output port)
      (list (current-write-file-name directory)
            (value-size value)
            (- (file-size write-file) (value-size value) 1)
            (current-seconds)))))


(define (key-size key)
  (string-length key))

(define (value-size value)
  (if (void? value)
      7
      (string-length (jsexpr->string value))))

; Stub, to be implemented, but keeps 8 byte width
(define (crc . args)
  "8 bytes.")
  
(define (key-file-name directory)
  (string-append directory "/keys.bitcask"))

(define (current-write-file directory)
  (string-append directory "/" (current-write-file-name directory)))

(define (current-write-file-name dir)
  "file1")



(define bc (new bitcask% [directory "test"]))

; put, get, delete!
(send bc put! "a" "asdfasdf")
(send bc get "a")
(send bc put! "g" (hasheq 'blah 2))
(send bc delete! "g")
(send bc get "g")

; Sample fold over all keys, to count the number of elements
(send bc put! "g" (hasheq 'blah 2))
(send bc fold (λ (k v acc) (add1 acc)) 0)
