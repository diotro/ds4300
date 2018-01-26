#lang racket

; A KeyDir is a [Hash Key KeyDirEntry]

; A KeyDirEntry is a (make-keydir String N N Time)
(struct keydir [file-name value-size value-position timestamp])
; where file-name is the name of the file where the value can be found
; value-size is the size of the value
; value-position is the offset of the value from the start of the file
; timestamp is when the key was last updated

; A Entry is a (make-entry N Time N N String JSON)
(struct entry [crc timestamp key-size value-size key value])
; where crc is some checksum thing
; timestamp is the time the value was posted
; key-size is the size of the key (in bytes)
; value-size is the size of the value (in bytes)
; key is the key
; value is the value