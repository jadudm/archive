#lang racket

(require "code-execution.rkt"
         "seq.rkt")

(define source       (make-parameter false))
(define destination  (make-parameter false))
(define tag          (make-parameter "archive"))

;; Size in megabytes
(define block-size   (make-parameter 500))

(define (make-archive)
  (define p (new process%))
  
  (seq p
    [(initial? 'ERROR-STARTUP)
     (andmap directory-exists? 
             (list (source) (destination)))]
    [(true? 'ERROR-SRC-DEST)
     (debug 'TAR "Making tarfile: ~a")
     (
  )

(define archive
  (command-line 
   #:program "archive"
   #:once-each
   [("-b" "--block-size") bs
                          "Size of archive blocks (in MB)"
                          (block-size (string->number bs))]
   [("-t" "--tag") t
                   "Identifier tag for archive."
                   (tag t)]
   #:args (src dst)
   (source src)
   (destination dst)
   (make-archive)
   ))
                