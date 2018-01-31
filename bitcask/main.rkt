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
      delete!)


    
    (init-field directory)
    (field [key-map (read-key-hash directory)])
    
    (define (get key)
      (if (hash-has-key? key-map key)
          (read-value directory (hash-ref key-map key))
          (void)))

    (define (put! key value)
      (define ptr (write-value! directory (crc key value) key value))
      (set-field! key-map (hash-set key-map key ptr)))

    (define (delete! key value)
      #f)
    ))

; A KeyHash is a [HashEq String FilePointer]

; A FilePointer is a (list String N N N)
; containing the id of the file, the size of the value,
; the position of the value, and the timestamp of the last write
; convenience accessors:
(define file-id    first)
(define value-size second)
(define value-posn third)
(define timestamp  fourth)

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
  (call-with-input-file (string-append directory "/" (file-id file-pointer))
    (λ (port)
      (file-position port (value-posn file-pointer))
      (read-bytes (value-size file-pointer) port))))

; write-value! : String String String JSON -> FilePointer
; writes the given value in the given directory, returning where it was written
(define (write-value! directory crc key value)
  (call-with-output-file (current-write-file directory)
    #:exists 'append
    (λ (port)
      (write-json value port))))


; Stub, to be implemented, but keeps 8 byte width
(define (crc . args)
  "8 bytes.")
  
(define (key-file-name directory)
  (string-append directory "/keys.bitcask"))

(define (current-write-file directory)
  (string-append directory "/file1"))




(define bc (new bitcask% [directory "test"]))
(send bc put! "a" "asdfasdf")
(send bc get "a")
