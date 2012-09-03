(use dummy-user)
(use test)


(define user (make-dummy-user '("123" "456")))

(define (test-app)
  (let ((inputs (make-queue)))
    (display "abc ")
    (queue-add! inputs (read-line))
    (display "def ")
    (queue-add! inputs (read-line))
    (display "Thanks. Bye.")
    (queue->list inputs)))

(test '((program: "abc ") (user: "123") (program: "def ") (user: "456") (program: "Thanks. Bye."))
      (with-dummy-user user test-app))

(test-exit)
