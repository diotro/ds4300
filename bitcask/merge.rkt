#lang racket
(provide
 (contract-out [bitcask-merge (-> void?)]))


(require "common.rkt"
         "basic.rkt"
         "test.rkt"
         racket/hash
         json)

(module+ test
  (require rackunit))

; A BitcaskValue is a (list JSExpr Integer)
; where the jsexpr is the value
; and the integer is the timestamp in milliseconds,
; when the data was last updated


; bitcask-merge : -> KeyDir
; merges the data files for the current bitcask,
; then produces the new keydir
(define (bitcask-merge)
  (define bc (current-bitcask))
  (define unmerged-closed-files (find-unmerged-closed-files))
  (define new-keydir (write-new-data! (merge-data unmerged-closed-files)))

  (current-bitcask bc)
  (delete-closed-data-files! unmerged-closed-files)
  (define keydir
    (hash-union new-keydir
                (find-keydir)
                #:combine/key (λ (k val1 val2)
                                val2 #;(argmax bcfp-timestamp (list val1 val2)))))
  (write-keydir! keydir))

(module+ test
  (setup-test-bitcask "merge-test")
  (for ([i (in-range 100)])
    (set! 'a '(1 2 3 #f #t null (1 2))))
  (set! 'a "did it!")
  (bitcask-merge)
  (for ([i (in-range 100)])
    (set! 'a '(1 2 3 #f #t null (1 2))))
  (bitcask-merge)
  (bitcask-merge))

; find-unmerged-closed-files : -> [Listof String]
; produces the name of each unmerged closed file for the current bitcask
(define (find-unmerged-closed-files)
  (define (files-to-merge)
    (append (map path->string
                 (directory-list (current-bitcask)))
            (if (directory-exists? (string-append (current-bitcask) "/merged"))
                (map path->string (directory-list
                                   (string-append (current-bitcask) "/merged")))
                '())))
  
  (sort (filter (λ (file)
                  (string-suffix? file CLOSED-FILE-EXTENSION))
                (files-to-merge))
        <
        #:key (λ (file) (string->number (first (string-split file "."))))))

; merge-data : [Listof String] -> [Hash Symbol BitcaskValue]
; merges the files, keeping the most recently timestamped piece of data
(define (merge-data files)

  ; String -> [Hash Symbol BitcaskValue]
  (define (get-data file)
    (define port (open-input-file (in-bitcask file)))
    (for/fold ([data (hasheq)])
              ([crc (in-port read port)]
               [timestamp (in-port read port)]
               [key-size (in-port read port)]
               [value-size (in-port read port)]
               [key (in-port read port)]
               [value (in-port read-json port)])
      (hash-set data key (list value timestamp))))

  ; BitcaskValue BitcaskValue -> BitcaskValue
  (define (keep-newest key v1 v2)
    (argmax second (list v1 v2)))
        
  (for/fold ([data (hasheq)])
            ([file files])
    (hash-union data (get-data file) #:combine/key keep-newest)))


; write-new-data! : [Hash Symbol BitcaskValue] -> KeyDir
; writes the new data to merged files, returning a keydir with the
; location of the new data
(define (write-new-data! data-map)
  (define bc (current-bitcask))
  (current-bitcask (string-append bc "/" "merged"))
  (for/fold ([keydir (hasheq)])
            ([(key value) (in-hash data-map)])
    (hash-set keydir
              key
              (append-data key (first value)
                           #:timestamp (second value)
                           #:extension MERGED-FILE-EXTENSION))))

; delete-closed-data-files! : [Listof String] -> Void
; deletes all the given files, in the current bitcask directory
(define (delete-closed-data-files! files)
  (for-each (λ (file) (delete-file (in-bitcask file)))
            files))









