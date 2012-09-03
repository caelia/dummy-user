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

(test "test 1" '((program: "abc ") (user: "123") (program: "def ") (user: "456") (program: "Thanks. Bye."))
      (with-dummy-user user test-app))

(user 'reset)

(test "test 2" '((program: "abc ") (user: "123") (program: "def ") (user: "456") (program: "Thanks. Bye."))
      (with-dummy-user user test-app))

(user 'reset)

(define test-result #f)
(test "test 3" '("123" "456")
      (with-dummy-user
        user
        (lambda () (set! test-result (test-app)))
        (lambda () test-result)))

(test-exit)
