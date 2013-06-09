#lang racket

(require "code-execution.rkt"
         "seq.rkt"
         "util.rkt"
         "debug.rkt"
         racket/date)

(define (pad n)
  (if (< n 10)
      (format "0~a" n)
      (format "~a" n)))

(define now          (current-date))
(define now-year     (date-year now))
(define now-month    (pad (date-month now)))
(define now-day      (pad (date-day now)))
(define datestring   (format "~a~a~a" now-year now-month now-day))

(define source       (make-parameter false))
(define destination  (make-parameter false))
(define tag          (make-parameter "archive"))
(define year         (make-parameter false))
(define redundancy   (make-parameter 10))

(define target       (make-parameter false))
(define tarfile      (make-parameter false))

;; Size in bytes
(define block-size   (make-parameter 5000000))

(define (tar-cmd)
  (system-call 
   'tar
   `(cvf 
     ,(->string (tarfile))
     ,(path->string (source)))))

(define (split-cmd)
  (system-call
   'split
   `(-b ,(block-size)
        ,(build-path (tarfile))
        ,(build-path 
          (build-path (destination) (target))
          (->string (format "~a-split." (target)))))))

(define (par2-cmd)
  (system-call
   'par2 
   `(create 
     -v 
     ,(format "-r~a" (redundancy))
     ,(format "~a.par2" (target))
     ,(format "~a.tar" (target)))))



(define (make-archive)
  (define p (new process%))
  
  (seq p
    [(initial? 'ERROR-STARTUP)
     (andmap directory-exists? 
             (list (source) (destination)))]
    ;; TARBALL CREATION
    [(true? 'ERROR-MAKEDIR)
     (debug 'TAR "TAR CMD: ~a" (tar-cmd))
     (exe (tar-cmd))]
    ;; SPLIT EVERYTHING UP
    [(zero? 'ERROR-TAR)
     (debug 'SPLIT "Tarfile size [~a]" (file-size (tarfile)))
     (when (> (file-size (tarfile)) (block-size))
       (debug 'SPLIT "SPLIT CMD: ~a" (split-cmd))
       (exe (split-cmd)))]
    ;; CREATE PAR2 DATA
    [(zero? 'ERROR-SPLIT)
     (debug 'PAR2 "Creating [~a%] redundant archive files." (redundancy))
     ;; We can just create a reundancy file for the original archive.
     ;; The split files, when recombined, can be saved by the PAR2 file
     ;; created from the (intact) original.
     (debug 'PAR2 "CMD: ~a" (par2-cmd))
     (parameterize ([current-directory 
                     (build-path (destination) (target))])
       (exe (par2-cmd)))]
    ;; EMIT RECOVERY SCRIPT
    [(pass 'ERROR-DELETE)
     (when (> (file-size (tarfile)) (block-size))
       (let* ([script (build-path (destination)
                                  (target)
                                  (format "rebuild-~a.sh" (target)))]
              [op (open-output-file script
                                    #:exists 'replace)])
         (fprintf op "#!/bin/bash~n")
         (fprintf op "echo Concatenating ~a.tar~n" (target))
         (fprintf op "~a~n"
                  (system-call 'cat 
                               `("*-split*" ">" 
                                            ,(format "~a.tar" (target)))))
         (fprintf op "echo We always attempt a repair---this is not a cause for alarm.~n")
         (fprintf op "echo Repairing integrity of ~a.tar~n" (target))
         (fprintf op "~a~n"
                  (system-call 'par2 
                               `(r ,(format "~a.par2" (target)))))
         (fprintf op "echo Done.~n")
         (close-output-port op)
         (exe (system-call 'chmod `(755 ,script)))
         ))]
    ;; MAKE THE MANIFEST
    [(pass 'ERROR-RECOVERY-SCRIPT)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (exe
        (system-call 'tar `(tvf ,(format "~a.tar" (target))
                                ">"
                                ,(format "~a.manifest" (target))))))]
     
    ;; REMOVE THE TARBALL
    [(pass 'ERROR-PAR2)
     (debug 'TAR "Removing the tarfile at [~a]" (tarfile))
     (delete-file (build-path (tarfile)))]
    ;; DONE
    [(pass 'ERROR-DONE)
     (printf "Done.")]
           
     
  ))

(define archive
  (command-line 
   #:program "archive"
   #:once-each
   [("-b" "--block-size") bs
                          "Size of archive blocks (in bytes)"
                          (block-size (string->number bs))]
   [("-t" "--tag") t
                   "Identifier tag for archive."
                   (tag t)]
   [("-y" "--year") y
                    "Year data was generated"
                    (year y)]
   [("-r" "--redundancy") r
                          "Percent redundancy in PAR2 files."
                          (redundancy r)]
   #:args (src dst)
   
   
   (when (not (and (tag) (year)))
     (printf "Please provide a tag and year.")
     (exit))
   
   ;; SETUP
   (enable-debug! 'ALL)
   
   (source (string->path src))
   (destination (string->path dst))
   (target (format "~a-~a" (year) (tag)))
   (tarfile (build-path (destination) (target) (format "~a.tar" (target))))
   
   (unless (directory-exists? (destination))
     (make-directory (destination)))
   
   (unless (directory-exists? (build-path (destination) (target)))
     (make-directory (build-path (destination) (target))))
   
   (make-archive)
   ))
                