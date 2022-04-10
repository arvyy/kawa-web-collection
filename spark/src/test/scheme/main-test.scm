(define-library
  (main-test)
  (import (scheme base)
          (mapping-test)
          (request-test)
          (response-test)
          (session-test)
          (srfi 64))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "Kawa-Spark test")

      (test-group 
        "Mapping test"
        (do-mapping-test))

      (test-group
        "Request methods test"
        (do-request-test))

      (test-group
        "Response methods test"
        (do-response-test))

      (test-group
        "Session methods test"
        (do-session-test))
      (test-end))))
