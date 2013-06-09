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
(define redundancy   (make-parameter 15))

(define target       (make-parameter false))
(define tarfile      (make-parameter false))
(define archive   (make-parameter false))
(define compress?    (make-parameter false))
(define extension    (make-parameter "tar"))
(define threshold    (make-parameter 0.75))

;; Size in bytes
(define block-size   (make-parameter 50000000))

(define (tar-cmd)
  (system-call 
   'tar
   `(cvf 
     ,(->string (tarfile))
     ,(extract-filename (source)))))

(define (split-cmd)
  (system-call
   'split
   `(-b ,(block-size)
        ,(build-path (archive))
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
     ,(archive))))

(define (compress-cmd)
  (system-call
   'bzip2
   `(--best 
     ,(tarfile))))


(define incompressible 
  '(;; Audio
    mp3 mp4 
        ;; Archives
        zip gz tgz gz bz2 bzip2 7z
        ;; Video
        mov avi flbv mpeg mp4 m4v wmv
        ;; Images
        png jpg jpeg gif cr2
        ;; Documents
        pdf
        ))

(define (incompressible? path)
  (member (->sym (->lower (file-extension path))) incompressible))

(define (tc path)
  (tree-contents (map (λ (o)
                        (build-path path o))
                      (directory-list path))))
                      
(define (->path o)
  (cond
    [(string? o)
     (string->path o)]
    [else o]))

(define (tree-contents ls)
  ;(printf "LS ~a~n" ls)
  (cond
    [(empty? ls) (list 0 0)]
    [else 
     (let ([p (->path (first ls))])
       (cond
         [(file-exists? p) 
          ;(printf "FILE ~a~n" p)
          (let ([res (tree-contents (rest ls))])
            (cond
              [(incompressible? p)
               (list (first res)
                     (+ (file-size p)
                        (second res)))]
              [else
               (list (+ (file-size p)
                        (first res))
                     (second res))]))]
         [(directory-exists? p)
          ;(printf "DIRECTORY ~a~n" p)
          (let ([res-dep
                 (tree-contents 
                  (map (λ (o)
                         (build-path p o))
                       (directory-list p)))]
                [res-rest (tree-contents (rest ls))])
            (list (+ (first res-dep)
                     (first res-rest))
                  (+ (second res-dep)
                     (second res-rest))))]
         [else ls]))]
    ))

(define (compressible? p)
  (let ([sizes (tc p)])
    (> 
     (/ (first sizes)
        (+ (first sizes) (second sizes)))
     (threshold)
     )))

(define (make-archive)
  (define p (new process%))
  
  (seq p
    [(initial? 'ERROR-STARTUP)
     (andmap directory-exists? 
             (list (source) (destination)))]
    ;; TARBALL CREATION
    [(true? 'ERROR-MAKEDIR)
     (debug 'TAR "TAR CMD: ~a" (tar-cmd))
     (parameterize ([current-directory
                     (build-path (source) 'up)])
       (exe (tar-cmd)))]
;; MAKE THE MANIFEST
    [(pass 'ERROR-TAR)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (exe
        (system-call 'tar `(tvf ,(format "~a.tar" (target))
                                ">"
                                ,(format "~a.manifest" (target))))))]
    [(pass 'ERROR-COMPRESS)
     (when (compress?)
       (debug 'COMPRESS "CMD: ~a" (compress-cmd))
       (exe (compress-cmd)))]
    ;; SPLIT EVERYTHING UP
    [(pass 'ERROR-TAR)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
     (let ([size (file-size (archive))])
       (debug 'SPLIT "Archive size [~a]" size)
       (when (> size (block-size))
         (debug 'SPLIT "SPLIT CMD: ~a" (split-cmd))
         (exe (split-cmd)))))]
    ;; CREATE PAR2 DATA
    [(pass 'ERROR-SPLIT)
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
     (when (> (file-size (build-path (destination) (target) (archive))) (block-size))
       (let* ([script (build-path (destination)
                                  (target)
                                  (format "rebuild-~a.sh" (target)))]
              [op (open-output-file script
                                    #:exists 'replace)])
         (fprintf op "#!/bin/bash~n")
         (fprintf op "echo Concatenating ~a~n" (archive))
         (fprintf op "~a~n"
                  (system-call 'cat 
                               `("*-split*" ">" 
                                            ,(archive))))
         (fprintf op "echo We always attempt a repair---this is not a cause for alarm.~n")
         (fprintf op "echo Repairing integrity of ~a~n" (archive))
         (fprintf op "~a~n"
                  (system-call 'par2 
                               `(r ,(format "~a.par2" (target)))))
         ;; Decompress
         
         (when (compress?)
           (fprintf op "echo Decompressing.~n")
           (fprintf op "if [ -f ~a ]; then~n" (archive))
           (fprintf op "  bunzip2 ~a~n" (archive))
           (fprintf op "fi~n"))
         
         (fprintf op "echo Done.~n")
         (close-output-port op)
         (exe (system-call 'chmod `(755 ,script)))
         ))]
    
    
    ;; REMOVE THE TARBALL
    [(pass 'ERROR-PAR2)
     (debug 'TAR "Removing the archive at [~a]" (archive))
     (parameterize ([current-directory (build-path (destination) (target))])
       (delete-file (archive)))]
    ;; DONE
    [(pass 'ERROR-DONE)
     (printf "Done.")]
    
    
    ))

(define main
  (command-line 
   #:program "archive"
   
   #:once-any
   [("-b" "--block-size") bs
                          "Size of archive blocks (in bytes)"
                          (block-size (string->number bs))]
   [("-m" "--megablock-size") mbs
                              "Size of archive blocks (in megabytes)"
                              (block-size (* 1000000 (string->number mbs)))]
   
   #:once-each
   [("-t" "--tag") t
                   "Identifier tag for archive."
                   (tag t)]
   [("-y" "--year") y
                    "Year data was generated"
                    (year y)]
   [("-r" "--redundancy") r
                          "Percent redundancy in PAR2 files."
                          (redundancy r)]
   [("--threshold") th
                    "Percent compressible in a tree before we bzip. 0.75 is the default." 
                    (threshold (string->number th))]
   
   [("-c" "--compress") 
    "Compress with BZIP2."
    (compress? true)]
   #:args (src dst)
   
   (when (not (and (tag) (year)))
     (printf "Please provide a tag and year.")
     (exit))
   
   ;; SETUP
   (enable-debug! 'ALL)
   
   (source      (string->path src))
   (destination (string->path dst))
   
   ;; Check compressibility
   (cond
     [(compressible? (source))
     (debug 'COMPRESS "Looks like the source is compressible.")
     (compress? true)]
     [else (debug 'COMPRESS "Too many files that won't compress. Skipping BZ2.")])
   
   ;; Set the archive extension
   (cond
     [(compress?)
      (extension "tar.bz2")]
     [else (extension "tar")])
   
   (target (format "~a-~a" (year) (tag)))
   (tarfile (build-path (destination) (target) (format "~a.tar" (target))))
   (archive (format "~a.~a" (target) (extension)))
   
   (unless (directory-exists? (destination))
     (make-directory (destination)))
   
   (unless (directory-exists? (build-path (destination) (target)))
     (make-directory (build-path (destination) (target))))
   
   (make-archive)
   ))
