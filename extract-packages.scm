#!/usr/bin/env -S guile -s
!#

(define (maybe-skip-comment char)
  (if (eq? char #\;)
      (let loop ((char (read-char)))
        (if (or (eq? char #\newline)
                (eof-object? char))
            #t
            (loop (read-char))))
      #f))

(define (maybe-skip-string char last)
  (if (and (eq? char #\")
           (not (eq? last #\\)))
      (let loop ((char (read-char))
                 (last last))
        (if (or (eof-object? char)
                (and (eq? char #\"))
                     (not (eq? last #\\)))
            #t
            (loop (read-char) char)))
      #f))

(define* (peek-prefix? prefix #:optional #:key read-match)
  (let loop ((buffer (list (read-char)))
             (tail (string->list prefix)))
    (cond ((null? tail)
           (unless read-match
             (for-each (lambda (c) (unread-char c)) buffer))
           #t)
          ((not (eq? (car buffer)
                     (car tail)))
           (for-each (lambda (c) (unread-char c)) buffer)
           #f)
          (else (loop (cons (read-char) buffer) (cdr tail))))))

(define (main)
  (let loop ((char (read-char))
             (last #\ ))
    (cond ((eof-object? char) #t)
          ;; Skip comments and strings
          ((or (maybe-skip-comment char)
               (maybe-skip-string char last))
           (loop (read-char) char))
          ;; Loop until we hit a `:'
          ((not (eq? char #\:))
           (loop (read-char) char))
          ;; Check for `:guix'
          ((and (char-set-contains?
                  (string->char-set "(" char-set:whitespace)
                  last)
                (peek-prefix? "guix" #:read-match #t))
           ;; Consume any whitespace
           (while (char-set-contains? char-set:whitespace
                                      (peek-char))
             (read-char))
           ;; Emit leading space
           (display #\ )
           ;; Emit each package name until we hit a comment, paren, or
           ;; EOF, collapsing whitespace as we go.
           (let hit ((char (read-char))
                     (last #\ )
                     (nested? #f))
             (cond ((maybe-skip-comment char)
                    (hit (read-char) char nested?))
                   ((eq? char #\()
                    (if nested?
                        (error "depth > 1")
                        (hit (read-char) char #t)))
                   ((eq? char #\))
                    (if (not nested?)
                        ;; exit at end of sexp
                        (loop (read-char) char)
                        (hit (read-char) char #f)))
                   ;; or at next keyword
                   ((or (and (eq? char #\:)
                             (char-set-contains?
                               (string->char-set "(" char-set:whitespace)
                               last))
                        (eof-object? char))
                    (loop (read-char) char))
                   (else
                    (cond ((not (char-set-contains? char-set:whitespace char))
                           (display char))
                          ((not (char-set-contains? char-set:whitespace last))
                           (display #\ )))
                    (hit (read-char) char nested?)))))
          (else (loop (read-char) char)))))

(main)
