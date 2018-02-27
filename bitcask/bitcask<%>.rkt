#lang racket
; Produces the interface specified by the bitcask protocol
(provide bitcask<%>)

(require json)

(define bitcask<%>
  (interface ()
    ; gets the value under the given key
    [get (-> symbol? jsexpr?)]

    ; lists the keys in the bitcask
    [list-keys (-> [listof symbol?])]

    ; folds the given function over each key and value
    [fold (parametric->/c [X] (-> (-> symbol? jsexpr? X) X))] 

    ; puts the given value under the given key, returning #true if something was already there
    [put! (-> symbol? jsexpr? boolean?)]

    ; deletes the value under the given key, returning #true if something was already there
    [delete! (-> symbol? boolean?)]

    ; compacts the data, returning #true if the data changed on disk
    [merge! (-> boolean?)]))
