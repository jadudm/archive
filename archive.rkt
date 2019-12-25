#lang racket

(require "code-execution.rkt"
         "seq.rkt"
         "util.rkt"
         "debug.rkt"
         racket/date
         file/md5
         )

(define (pad n)
  (if (< n 10)
      (format "0~a" n)
      (format "~a" n)))

(define MD5CMD
  (cond
    [(equal? (system-type 'os) 'macos)
     "md5"]
    [(equal? (system-type 'os) 'unix)
     "md5sum"]
    [else
     (error "Not supported on Windows.")]))

(define MD5FLAGS
  (cond
    [(equal? (system-type 'os) 'macos)
     "-q"]
    [(equal? (system-type 'os) 'unix)
     ""]
    [else
     (error "Not supported on Windows.")]))



(define now          (current-date))
(define now-year     (date-year now))
(define now-month    (pad (date-month now)))
(define now-day      (pad (date-day now)))
(define datestring   (format "~a~a~a" now-year now-month now-day))

(define source       (make-parameter false))
(define destination  (make-parameter false))
(define tag          (make-parameter false))
(define year         (make-parameter false))
(define redundancy   (make-parameter 15))

(define target       (make-parameter false))
(define tarfile      (make-parameter false))
(define archive      (make-parameter false))
(define compress?    (make-parameter false))
(define extension    (make-parameter "tar"))
(define threshold    (make-parameter 0.65))

;; Adding disk images
(define disk-image   (make-parameter false))

;; Size in bytes
(define block-size   (make-parameter 50000000))

(define (tar-cmd)
  (system-call 
   'tar
   `(cf ;cvf
     ,(->string (tarfile))
     ,(extract-filename (source)))))

(define (split-cmd)
  (system-call
   'split
   `(-b ,(block-size)
        ,(archive)
        ,(->string (format "~a-split." (archive))))))

(define (split-disk-image-cmd)
  (let ([basename (target)])
    (system-call
     'split
     `(-b ,(block-size)
          ,(source)
          ,(->string (format "~a-split." basename))))
    ))

(define (par2-cmd)
  (system-call
   'par2 
   `(create 
     ;-v 
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
        mov avi flv mpeg mp4 m4v wmv 3gp
        ;; Images
        png jpg jpeg gif cr2
        ;; Documents
        
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
         [(link-exists? p) (list 0 0)]
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

(define (round n pl)
  (exact->inexact 
   (/ (floor (* n (expt 10 pl))) (expt 10 pl))))

(define (compressible? p)
  (let* ([sizes (tc p)]
         [ratio (/ (first sizes)
                   (+ (first sizes) (second sizes)))])
    (debug 'COMPRESSIBLE-RATIO "~a" sizes)
    (debug 'COMPRESSIBLE-RATIO "~a" (round (exact->inexact ratio) 2))
    
    (> ratio (threshold))))

(define (sys-md5 f)
  (regexp-replace* 
   "\n"
   (with-output-to-string (lambda () (system (format "~a ~a ~a" MD5CMD MD5FLAGS f))))
   ""))

(define (generate-md5s ls)
  (for/list ([f ls])
            (list f (sys-md5 (path->string (build-path (destination) (target) f))))))

(define (archive-directory)
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
    [(pass 'ERROR-SPLIT)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (let ([size (file-size (archive))])
         (debug 'SPLIT "Archive size [~a]" size)
         (when (> size (block-size))
           (debug 'SPLIT "SPLIT CMD: ~a" (split-cmd))
           (exe (split-cmd)))))]
    ;; CREATE PAR2 DATA
    [(pass 'ERROR-PAR2)
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
     (debug 'MD5 "Generating MD5s for files.")
     (let* ([check-script (build-path (destination) (target) 
                                      (format "check-md5s-~a.sh" (target)))]
            [op (open-output-file check-script #:exists 'replace)]
            [md5s (generate-md5s (directory-list 
                                  (build-path
                                   (destination) (target))))])
       ; (debug 'MD5 "MD5s:~n~a~n" md5s)
       (fprintf op "#!/bin/bash~n")
       (fprintf op "echo Checking MD5s~n")
       (fprintf op "ERROR=0~n")
       (newline op)
       (for ([f (map first md5s)]
             ;; 20191225 MCJ
             ;; I only want the MD5
             [md5 (map (λ (s) (first (regexp-split " " s)))
                       (map second md5s))])
            ;; We are about to delete the archive itself.
            (cond
              [(equal? (path->string f) (archive))
               (fprintf op "# The archive '~a' should check as '~a'~n"
                        f md5)]
              [(equal? (path->string f) (format "check-md5s-~a.sh" (target)))
               ;; Skip the check script; we change the permissions in a moment.
               'DoNothing]
              [else
               (fprintf op "echo Checking ~a~n" f)
               (fprintf op "SUM=`~a ~a \"~a\"`~n" MD5CMD MD5FLAGS f)             
               (fprintf op "if [[ ${SUM} != *\"~a\"* ]]~nthen~n" md5)
               (fprintf op "ERROR=1~n")
               (fprintf op "  echo MD5 error in \"~a\"~n" f)
               (fprintf op "fi~n")])
            (newline op))
       (fprintf op "if [ $ERROR == 0 ]~n")
       (fprintf op "then~n")
       (fprintf op "  echo NO MD5 ERRORS FOUND~n")
       (fprintf op "fi~n")
       (close-output-port op)
       (file-or-directory-permissions check-script #o744)
       )]
    
    [(pass 'REBUILD-SCRIPT)
     
     (when (> (file-size (build-path (destination) (target) (archive))) (block-size))
       (debug 'REBUILD "Generating the rebuild script.")
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
    
    
    ;; REMOVE THE TARBALL IF WE SPLIT IT
    [(pass 'ERROR-PAR2)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (let ([size (file-size (archive))])
         (debug 'SPLIT "Archive size [~a]" size)
         (when (> size (block-size))
           (debug 'TAR "Removing the archive at [~a]" (archive))
           (delete-file (archive)))))]
    ;; DONE
    [(pass 'ERROR-DONE)
     (printf "Done.~n")]
    
    
    ))

(define (archive-disk-image)
  (define p (new process%))
  
  (seq p
    [(initial? 'ERROR-STARTUP)
     (and (directory-exists? (destination)) 
          (file-exists? (source)))]
    ;; SPLIT EVERYTHING UP
    [(pass 'ERROR-SPLIT)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (let ([size (file-size (source))])
         (debug 'SPLIT "Disk image size [~a]" size)
         (when (> size (block-size))
           (debug 'SPLIT "SPLIT CMD: ~a" (split-disk-image-cmd))
           (exe (split-disk-image-cmd)))))]
    ;; CREATE PAR2 DATA
    [(pass 'ERROR-PAR2)
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
     (debug 'MD5 "Generating MD5s for files.")
     (let* ([check-script (build-path (destination) (target) 
                                      (format "check-md5s-~a.sh" (target)))]
            [op (open-output-file check-script #:exists 'replace)]
            [md5s (generate-md5s (directory-list 
                                  (build-path
                                   (destination) (target))))])
       ; (debug 'MD5 "MD5s:~n~a~n" md5s)
       (fprintf op "#!/bin/bash~n")
       (fprintf op "echo Checking MD5s~n")
       (fprintf op "ERROR=0~n")
       (newline op)
       (for ([f (map first md5s)]
             [md5 (map second md5s)])
            ;; We are about to delete the archive itself.
            (cond
              [(equal? (path->string f) (archive))
               (fprintf op "# The archive '~a' should check as '~a'~n"
                        f md5)]
              [(equal? (path->string f) (format "check-md5s-~a.sh" (target)))
               ;; Skip the check script; we change the permissions in a moment.
               'DoNothing]
              [else
               (fprintf op "echo Checking ~a~n" f)
               (fprintf op "SUM=`md5 -q \"~a\"`~n" f)             
               (fprintf op "if [ ${SUM} != \"~a\" ]~nthen~n" md5)
               (fprintf op "ERROR=1~n")
               (fprintf op "  echo MD5 error in \"~a\"~n" f)
               (fprintf op "fi~n")])
            (newline op))
       (fprintf op "if [ $ERROR == 0 ]~n")
       (fprintf op "then~n")
       (fprintf op "  echo NO MD5 ERRORS FOUND~n")
       (fprintf op "fi~n")
       (close-output-port op)
       (file-or-directory-permissions check-script #o744)
       )]
    
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
    
    
    ;; REMOVE THE TARBALL IF WE SPLIT IT
    [(pass 'ERROR-PAR2)
     (parameterize ([current-directory
                     (build-path (destination) (target))])
       (let ([size (file-size (archive))])
         (debug 'SPLIT "Archive size [~a]" size)
         (when (> size (block-size))
           (debug 'TAR "Removing the archive at [~a]" (archive))
           (delete-file (archive)))))]
    ;; DONE
    [(pass 'ERROR-DONE)
     (printf "Done.~n")]
    
    
    ))

(define main
  (command-line 
   #:program "archive"
   
   #:once-any
   [("-b" "--block-size-bytes") bs
                                "Size of archive blocks (in bytes)"
                                (block-size (string->number bs))]
   [("-m" "--block-size-mb") mbs
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
                    "Percent compressible in a tree before we bzip. 0.65 is the default." 
                    (threshold (string->number th))]
   
   [("-c" "--compress") 
    "Compress with BZIP2."
    (compress? true)]
   #:args (src dst)
   
   (when (not (year))
     (printf "Please provide a tag and year.~n")
     (exit))
   
   ;; SETUP
   (enable-debug! 'ALL)
   
   (source      (string->path src))
   (destination (string->path dst))
   
   (cond
     ;; If we are archiving a directory
     [(directory-exists? (source))
      ;; Check compressibility
      (cond
        [(compressible? (source))
         (debug 'COMPRESS "Looks like the source is compressible.")
         (compress? true)]
        [else
         (debug 'COMPRESS
                "Too many files that won't compress. Skipping BZ2.")])
   
      ;; Set the archive extension
      (cond
        [(compress?)
         (extension "tar.bz2")]
        [else (extension "tar")])

      (if (tag)
          (target (format "~a-~a-~a" (year) (tag)
                          (extract-filename (source))))
          (target (format "~a-~a" (year) 
                          (extract-filename (source)))))
      
      (tarfile (build-path (destination) (target) (format "~a.tar" (target))))
      (archive (format "~a.~a" (target) (extension)))
   
      (unless (directory-exists? (destination))
        (make-directory (destination)))
   
      (unless (directory-exists? (build-path (destination) (target)))
        (make-directory (build-path (destination) (target))))

      (archive-directory)]
     ;; If it is a disk image
     [(file-exists? (source))
      (if (tag)
          (target (format "~a-~a-~a" (year) (tag)
                          (regexp-replace
                           (format ".~a$" (file-extension (source)))
                           (extract-filename (source))
                           "")))
          (target (format "~a-~a" (year)
                          (regexp-replace
                           (format ".~a$" (file-extension (source)))
                           (extract-filename (source))
                           ""))))
 
      (unless (directory-exists? (destination))
        (make-directory (destination)))
   
      (unless (directory-exists? (build-path (destination) (target)))
        (make-directory (build-path (destination) (target))))
      
      (archive-disk-image)]
     [else
      (error 'SOURCE "Source is neither a directory or disk image.")])
   

   ))
