#lang racket


; Things that are still hacky:
; - Keys are consumed as strings but must be symbols for json-lib
; - Merging data files neccesary
; - missing test coverage
; - this is all one file

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
      fold
      merge)

    
    (init-field directory)
    (when (not (directory-exists? directory))
      (make-directory directory)
      (with-output-to-file (current-write-file directory)
        (thunk (void))))
    (field [key-map (read-key-hash directory)])

    ; String -> JSON or Void
    ; returns the value related to the key
    (define (get key)
      (if (hash-has-key? key-map (string->symbol key))
          (read-value directory (hash-ref key-map (string->symbol key)))
          (void)))

    ; String JSON -> Void
    ; puts the given value under the given key
    (define (put! key value)
      (define ptr (write-value! directory (crc key value) key value))
      (hash-set! key-map (string->symbol key) ptr)
      (write-key-hash! directory key-map)
      (maybe-new-file!))

    (define/private (maybe-new-file!)
      (define old-write-file (current-write-file directory))
      (define active-file-size (file-size old-write-file))
      
      (when (> active-file-size MAX-FILE-SIZE-BEFORE-CLOSE)
        (rename-file-or-directory
         (current-write-file directory)
         (closed-name-for old-write-file))
        (with-output-to-file
            (current-write-file directory)
          (thunk (void)))))

    (define (closed-name-for file)
      (string-append (first (string-split file ".")) CLOSED-FILE-EXTENSION))
    
    ; String -> Void
    ; removes the value corresponding to the given key
    (define (delete! key)
      (put! key TOMBSTONE)
      (hash-remove! key-map key))

    ; -> [List-of String]
    ; returns the list of all keys in this bitcask
    (define (list-keys)
      (map symbol->string (hash-keys key-map)))

    ; fold : [X] [String JSON X] X -> X
    ; folds the given function over the keys in this bitcask, starting from acc0
    (define (fold func acc0)
      (define keys (send this list-keys))
      (foldr func acc0 keys
             (map (λ (key) (send this get key)) keys)))

    ; merge : -> Void
    ; merges all inactive data files
    (define (merge)
      (define new-map (do-log-sequential-merge directory))
      (write-key-hash! new-map)
      (set! key-map new-map))
    ))

; String -> KeyHash
; performs log sequential merge on the items in the given directory,
; returning the new key hash
(define (do-log-sequential-merge directory)
  ; this part's hard
  
  (write-key-hash! directory)
  (read-key-hash directory))


; The value saying that a value was deleted, JSON has no #<void>
(define TOMBSTONE (void))

; the file extension for the write-file
(define ACTIVE-FILE-EXTENSION ".bcactive")

; the file extension for all closed files
(define CLOSED-FILE-EXTENSION ".bcclosed")

; the size cap for files before closed
(define MAX-FILE-SIZE-BEFORE-CLOSE 500)

  
; A KeyHash is a [HashEq Symbol FilePointer]

; A FilePointer is a (list String N N N)
; containing the id of the file, the size of the value,
; the position of the value, and the timestamp of the last write

; convenience accessors for a FilePointer:
(define fp-file-id    first)
(define fp-value-size second)
(define fp-value-posn third)
(define fp-timestamp  fourth)

; read-key-hash : String -> KeyHash
(define (read-key-hash directory)
  (if (file-exists? (key-file-name directory))
      (call-with-input-file (key-file-name directory)
        (λ (port) (make-hasheq (hash->list (read-json port)))))
      (make-hasheq)))

; write-key-hash! : String KeyHash -> Void
(define (write-key-hash! directory keys)
  (call-with-output-file (string-append directory "/keys.bitcask")
    #:exists 'replace
    (λ (port) (write-json keys port))))

; read-value : String FilePointer -> JSON
; from the given directory, read the value the pointer points to
(define (read-value directory file-pointer)
  (call-with-input-file (input-file-from-id directory (fp-file-id file-pointer))
    (λ (port)
      (file-position port (fp-value-posn file-pointer))
      (define result (read-string (fp-value-size file-pointer) port))
      (if (and (string? result) (string=? result "#<void>"))
          (void)
          (string->jsexpr result)))))

; input-file-from-id : String -> String
(define (input-file-from-id dir id)
  (define file-if-closed (string-append dir "/" id CLOSED-FILE-EXTENSION))
  (define file-if-open   (string-append dir "/" id ACTIVE-FILE-EXTENSION))
  (cond
    [(file-exists? file-if-closed) file-if-closed]
    [else file-if-open]))

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
      (list (number->string (highest-num-file (directory-list directory)))
            (value-size value)
            (- (file-size write-file) (value-size value) 1)
            (current-seconds)))))


(define (key-size key)
  (string-length key))

(define (value-size value)
  (if (void? value)
      7 ; length of "#<void>"
      (string-length (jsexpr->string value))))

; Stub, to be implemented, but keeps 8 byte width
(define (crc . args)
  "8 bytes.")
  
(define (key-file-name directory)
  (string-append directory "/keys.bitcask"))

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
  (foldr
   max
   0
   (filter identity
           (map (λ (path)
                  (string->number (first (string-split (path->string path) "."))))
                dir-contents))))


(define TEST-DIR "test")

(with-handlers ([exn:fail? (λ (e) (displayln "error deleting TESTDIR"))])
  (delete-directory/files TEST-DIR))
(define bc (new bitcask% [directory TEST-DIR]))


; put, get, delete!
(send bc put! "a" "asdfasdf")
(send bc get "a")
(send bc put! "g" (hasheq 'blah 2))

(send bc get "g")

(for ([i (build-list 100 identity)])
  (send bc put! (number->string i) i))

; Sample fold over all keys, to count the number of elements
(send bc put! "g" (hasheq 'blah 2))
(send bc fold (λ (k v acc) (add1 acc)) 0)

