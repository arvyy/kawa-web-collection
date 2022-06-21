(define-library
  (main-test)
  (import (scheme base)
          (srfi 64))
  
  (export run-tests)
  
  (begin
    (define (run-tests)
      (test-begin "Tests")
      (test-assert #t) ;;TODO
      (test-end))))
