(define-library
  (arvyy dbutils)
  (import
    (scheme base)
    (scheme write)
    (srfi 1)
    (class java.sql Connection)
    (class java.sql ResultSet)
    (class org.apache.commons.dbutils QueryRunner ResultSetHandler)
    (class org.apache.commons.dbcp2 BasicDataSource)
    (kawa lib reflection))
  
  (export
    dbutils-error?
    dbutils-error-message
    dbutils-error-statement
    handler/scalar
    handler/single-of
    handler/list-of
    handler/generator-of
    row-handler/rec
    row-handler/vector
    row-handler/alist
    make-query-runner
    query
    update
    insert
    call-in-transaction
    in-transaction?
    commit
    rollback
    )
  
  (begin
    
    (define-record-type 
      <dbutils-error>
      (make-dbutils-error message statement)
      dbutils-error?
      (message dbutils-error-message)
      (statement dbutils-error-statement))
    
    (define (exception-handler/attach-statement statement)
      (lambda (err)
        (define new-err 
          (if (dbutils-error? err)
              (make-dbutils-error (dbutils-error-message err) statement)
              (make-dbutils-error ((->java.lang.Throwable err):getMessage) statement)))
        (raise new-err)))
    
    (define (f->null el)
      (if el el #!null))
    
    (define (null->f el)
      (if (equal? el #!null) #f el))
    
    ;; active connection when in transaction
    (define connection (make-parameter #!null))
    
    (define (in-transaction?)
      (if (connection) #t #f))
    
    (define (call-in-transaction query-runner use-existing? thunk)
      (define curr-connection (connection))
      (define qr (->QueryRunner query-runner))
      (define (thunk*)
        (with-exception-handler
          (lambda (k)
            (rollback))
          thunk))
      (cond 
        ((and use-existing? curr-connection)
         (dynamic-wind
           (lambda () #t)
           thunk*
           (lambda () #t)))
        (else 
          (let ((conn ((qr:getDataSource):getConnection)))
           (parameterize 
             ((connection conn))
             (dynamic-wind
               (lambda () (conn:setAutoCommit #f))
               thunk*
               (lambda () (conn:close))))))))
    
    (define (commit)
      (define conn (connection))
      (unless conn (error "Cannot commit -- not in transaction."))
      ((->Connection conn):commit))
    
    (define (rollback)
      (define conn (connection))
      (unless conn (error "Cannot rollback -- not in transaction."))
      ((->Connection conn):rollback))
    
    (define (make-data-source driver ::String  url ::String  name ::String password ::String) ::BasicDataSource
      (define ds (BasicDataSource))
      (ds:setDriverClassName driver)
      (ds:setUrl url)
      (ds:setUsername name)
      (ds:setPassword password)
      ds)
    
    (define (make-query-runner driver ::String  url ::String  name ::String password ::String) ::QueryRunner
      (QueryRunner (make-data-source driver url name password)))
    
    (define query
      (let ((query-method-with-conn 
              (*:getDeclaredMethod QueryRunner
                                   "query"
                                   Connection:class
                                   java.lang.String:class
                                   ResultSetHandler:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;")))
            (query-method-no-conn 
              (*:getDeclaredMethod QueryRunner
                                   "query"
                                   java.lang.String:class
                                   ResultSetHandler:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;"))))
        (lambda (query-runner sql handler . args)
          (with-exception-handler
            (exception-handler/attach-statement sql)
            (lambda ()
              (define h ::ResultSetHandler handler)
              (define qr ::QueryRunner query-runner)
              (define args-java-lst (java.util.ArrayList))
              (for-each
                (lambda (arg)
                  (args-java-lst:add (f->null arg)))
                args)
              (define args-java-array (args-java-lst:toArray))
              (if (in-transaction?)
                  (let ((conn ::Connection (connection)))
                   (query-method-with-conn:invoke qr conn (->String sql) h args-java-array))
                  (query-method-no-conn:invoke qr (->String sql) h args-java-array)))))))
    
    (define update
      (let ((update-method-with-conn 
              (*:getDeclaredMethod QueryRunner
                                   "update"
                                   Connection:class
                                   java.lang.String:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;")))
            (update-method-no-conn 
              (*:getDeclaredMethod QueryRunner
                                   "update"
                                   java.lang.String:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;"))))
        (lambda (query-runner sql . args)
          (with-exception-handler
            (exception-handler/attach-statement sql)
            (lambda ()
              (define qr ::QueryRunner query-runner)
              (define args-java-lst (java.util.ArrayList))
              (for-each
                (lambda (arg)
                  (args-java-lst:add (f->null arg)))
                args)
              (define args-java-array (args-java-lst:toArray))
              (if (in-transaction?)
                  (let ((conn ::Connection (connection)))
                   (update-method-with-conn:invoke qr conn (->String sql) args-java-array) )
                  (update-method-no-conn:invoke qr (->String sql) args-java-array)))))))
    
    (define insert
      (let ((insert-method-with-conn 
              (*:getDeclaredMethod QueryRunner
                                   "insert"
                                   Connection:class
                                   java.lang.String:class
                                   ResultSetHandler:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;")))
            (insert-method-no-conn 
              (*:getDeclaredMethod QueryRunner
                                   "insert"
                                   java.lang.String:class
                                   ResultSetHandler:class
                                   (java.lang.Class:forName "[Ljava.lang.Object;"))))
        (lambda (query-runner sql . args)
          (with-exception-handler
            (exception-handler/attach-statement sql)
            (lambda ()
              (define qr ::QueryRunner query-runner)
              (define handler ::ResultSetHandler (handler/scalar))
              (define args-java-lst (java.util.ArrayList))
              (for-each
                (lambda (arg)
                  (args-java-lst:add (f->null arg)))
                args)
              (define args-java-array (args-java-lst:toArray))
              (if (in-transaction?)
                  (let ((conn ::Connection (connection)))
                   (insert-method-with-conn:invoke qr conn (->String sql) handler args-java-array) )
                  (insert-method-no-conn:invoke qr (->String sql) handler args-java-array)))))))
    
    (define (handler/scalar) ::ResultSetHandler
      (lambda (rs ::ResultSet)
        (if (not (rs:next))
            (raise (make-dbutils-error "Query returned no results" #f))
            (let ()
              (define rez (null->f (rs:getObject 1)))
              (when (rs:next)
                (raise (make-dbutils-error "Query returned more than one result" #f)))
              rez))))
    
    (define (handler/single-of row-handler) ::ResultSetHandler
      (lambda (rs ::ResultSet)
        (if (rs:next)
            (let ((value (row-handler rs)))
             (when (rs:next)
               (raise (make-dbutils-error "Query returned more than one result" #f)))
             value)
            (raise (make-dbutils-error "Query returned no results" #f)))))
    
    (define (handler/generator-of row-handler consumer) ::ResultSetHandler
      (lambda (rs ::ResultSet)
        (define empty #f)
        (define (generator)
          (cond
            (empty (eof-object))
            ((rs:next) (row-handler rs))
            (else (begin
                    (set! empty #t)
                    (eof-object)))))
        (consumer generator)))

    (define (handler/list-of row-handler) ::ResultSetHandler
      (lambda (rs ::ResultSet)
        (define lst '())
        (define last #f)
        (let loop ((has-next? (rs:next)))
         (if has-next?
             (let ((new-entry (row-handler rs)))
              (if (not last)
                  (begin
                    (set! last (cons new-entry '()))
                    (set! lst last))
                  ;;else has last
                  (let ((new-last (cons new-entry '())))
                   (set-cdr! last new-last)
                   (set! last new-last)))
              (loop (rs:next)))
             ;;else doesn't have next
             lst))))
    
    ;; in kawa, rtd is java.lang.Class 
    (define (row-handler/rec rtd ::java.lang.Class)
      (define fields (record-type-field-names rtd))
      (define constructor (record-constructor rtd))
      (lambda (rs ::ResultSet)
        (define vals
          (map
            (lambda (field-name)
              (null->f (rs:getObject (symbol->string field-name))))
            fields))
        (apply constructor vals)))

    (define (row-handler/alist)
      (lambda (rs ::ResultSet)
        (define meta (rs:getMetaData))
        (define col-count (meta:getColumnCount))
        (let loop ((alist '())
                   (i 1)) ;;java's resultset uses 1-based indexing
          (if (> i col-count)
              alist
              (let* ((col-name (meta:getColumnName i))
                     (key (string->symbol col-name))
                     (value (null->f (rs:getObject i)))
                     (entry (cons key value)))
               (loop (cons entry alist)
                     (+ i 1)))))))
    
    (define (row-handler/vector)
      (lambda (rs ::ResultSet)
        (define meta (rs:getMetaData))
        (define col-count (meta:getColumnCount))
        (define vec (make-vector col-count))
        (let loop ((i 1))
         (if (> i col-count)
             vec
             (begin
               (vector-set! vec (- i 1) (null->f (rs:getObject i)))
               (loop (+ 1 i)))))))))
