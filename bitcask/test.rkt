#lang racket

(require "parameters.rkt")
(provide setup-test-bitcask)

(define (setup-test-bitcask [dir "default-test-dir"])
  (when (directory-exists? dir)
    (delete-directory/files dir))
  (current-bitcask dir))