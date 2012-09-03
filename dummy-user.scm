;;; dummy-user.scm -- Simulate user input for testing interactive programs
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>. This is open source
;;;   software released under a BSD-style license; see LICENSE file for details.


(module dummy-user
        (make-dummy-user
         with-dummy-user)

        (import scheme)
        (import chicken)
        (import data-structures)
        (import ports)


;;; ====================================================================
;;; --  INTERNAL DEFINITIONS  ------------------------------------------

(define (print-errors . msgs)
  (for-each
    (lambda (m)
      (display m (current-error-port))
      (display " " (current-error-port)))
    msgs)
  (newline (current-error-port))
  (flush-output (current-error-port)))

(define (make-output logq)
  (let* ((ext-writer
           (lambda (s)
             (queue-add! logq (list program: s))))
         (ext-close-out
           (lambda () #f)))
    (make-output-port ext-writer ext-close-out)))

(define (make-input outq logq)
  (let* ((chars (make-queue))
         (done?
           (lambda ()
             ;(print-errors "done?")
             (and (queue-empty? chars)
                  (queue-empty? outq))))
         (next-string
           (lambda ()
             ;(print-errors "next-string")
             (if (queue-empty? outq)
               #f
               (let ((next (queue-remove! outq)))
                 (queue-add! logq (list user: next))
                 next))))
         (set-chars
           (lambda (s)
             ;(print-errors "set-chars")
             (when s
               (for-each
                 (lambda (c) (queue-add! chars c))
                 (string->list s)))
             (queue-add! chars #\newline)))   ; should it be newline or EOF?
             ;(queue-add! chars #!eof)))
         (ext-char-reader
           (lambda ()
             ;(print-errors "ext-char-reader")
             (if (done?)
               #!eof
               (begin
                 ;(print-errors "not done")
                 (when (queue-empty? chars)
                   ;(print-errors "char q empty")
                   (set-chars (next-string)))
                 (queue-remove! chars)))))
         (ext-in-ready?
           (lambda ()
             ;(print-errors "ext-in-ready?")
             (and chars
                  (not (queue-empty? chars))
                  (not (queue-empty? outq)))))
         (ext-close-in
           (lambda () (set! chars #f))))
    (make-input-port ext-char-reader ext-in-ready? ext-close-in)))

;;; ====================================================================



;;; ====================================================================
;;; --  PUBLIC INTERFACE  ----------------------------------------------

(define (make-dummy-user strings)
  (let* ((my-outq #f)
         (logq #f)
         (ext-input #f)
         (ext-output #f)
         (setup
           (lambda ()
             (set! my-outq (list->queue strings))
             (set! logq (make-queue))
             (set! ext-input (make-input my-outq logq))
             (set! ext-output (make-output logq))
             '())))
    (setup)
    (lambda (cmd)
      (case cmd
        ((input) ext-input)
        ((output) ext-output)
        ((reset) (setup))
        ((dump) (queue->list logq))))))
 

(define (with-dummy-user user thunk #!optional (return-result #f))
  (let ((return-result
          (or return-result
              (lambda () (user 'dump))))
        (stdin (current-input-port))
        (stdout (current-output-port)))
    (current-input-port (user 'input))
    (current-output-port (user 'output))
    (thunk)
    (current-input-port stdin)
    (current-output-port stdout)
    (return-result)))

;;; ====================================================================

)
