#lang racket
(provide
 (contract-out [bitcask-merge (-> keydir?)]))


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
    (hash-union (read-keydir)
                new-keydir
                #:combine/key (λ (k v1 v2) v2)))
  (write-keydir! keydir)
  keydir)

(module+ test
  (setup-test-bitcask "merge-test")
  (for ([i (in-range 100)])
    (set! 'a '(1 2 3 #f #t null (1 2))))
  (set! 'a "did it!")
  (bitcask-merge)
  (directory-list "merge-test/merged")

  )

; find-unmerged-closed-files : -> [Listof String]
; produces the name of each unmerged closed file for the current bitcask
(define (find-unmerged-closed-files)
  (sort (filter (λ (file)
                  (string-suffix? file ACTIVE-FILE-EXTENSION))
                (map path->string (directory-list (current-bitcask))))
        <
        #:key (λ (file) (string->number (first (string-split file "."))))))

; merge-data : [Listof String] -> [Hash Symbol BitcaskValue]
; merges the files, keeping the most recently timestamped piece of data
(define (merge-data files)
  
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
             
  (define file-hashes (map get-data files))
  (if (empty? file-hashes)
      (hasheq)
      (apply hash-union file-hashes #:combine/key (λ (k v1 v2) v2))))


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
              (append-data key (first value) (second value)))))

; delete-closed-data-files! : [Listof String] -> Void
; deletes all the given files, in the current bitcask directory
(define (delete-closed-data-files! files)
  (for-each (λ (file) (delete-file (in-bitcask file)))
            files))









