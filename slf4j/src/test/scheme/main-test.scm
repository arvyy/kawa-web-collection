(define-library
  (main-test)

  (import (scheme base)
          (srfi 64)
          (arvyy slf4j))
  (export run-tests)

  (begin
    (define (run-tests)
      (test-begin "Slf4j")

      ((java.lang.System:getProperties):setProperty "org.slf4j.simpleLogger.defaultLogLevel" "trace")

      (define l (get-logger "test-logger"))
      (test-equal "test-logger" (logger-name l))

      (test-assert (trace-enabled? l))
      (log-trace l "TRACE {}" 'arg)

      (test-assert (debug-enabled? l))
      (log-debug l "DEBUG {}" 'arg)

      (test-assert (warn-enabled? l))
      (log-warn l "WARN {}" 'arg)

      (test-assert (info-enabled? l))
      (log-info l "INFO {}" 'arg)

      (test-assert (error-enabled? l))
      (log-error l "ERROR {}" 'arg)

      (test-end))))

