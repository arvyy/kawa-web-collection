(define-library
  (main-test)
  (import (scheme base)
          (scheme write)
          (arvyy dbutils)
          (srfi 1)
          (srfi 64))
  (export run-tests)

  (begin
    (define (run-tests)

      (define qr (make-query-runner "org.sqlite.JDBC" "jdbc:sqlite:target/testdb" "" ""))

      (define-syntax reset-db!
        (syntax-rules ()
          ((_)
           (define _ 
             (let ()
              (update qr "drop table if exists TestTable")
              (update qr "create table TestTable(id integer primary key, val text)")
              (update qr "insert into TestTable(id, val) values (1, 'Foo'), (2, 'Bar')"))))))

      (define-record-type <TestRec>
                          (make-test-rec id val)
                          test-rec?
                          (id test-rec-id)
                          (val test-rec-val))

      (test-begin "Kawa-Dbutils test")

      (test-group
        "Test select scalar"
        (reset-db!)
        (define rez (query qr "select id from TestTable where id = 1" (handler/scalar)))
        (test-equal 1 rez))

      (test-group
        "Test select scalar when not exists"
        (reset-db!)
        (test-assert
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (err)
                  (when (dbutils-error? err)
                    (k #t))
                  (k #f))
                (lambda ()
                  (query qr "select * from TestTable where id = ?" (handler/scalar) 4)
                  #f))))))

      (test-group
        "Test select scalar when more than one exists"
        (reset-db!)
        (test-assert
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (err)
                  (when (dbutils-error? err)
                    (k #t))
                  (k #f))
                (lambda ()
                  (query qr "select * from TestTable" (handler/scalar))
                  #f))))))

      (test-group
        "Test select list-of"
        (reset-db!)
        (define rez (query qr "select id, val from TestTable" (handler/list-of (row-handler/vector))))
        (test-equal '(#(1 "Foo") #(2 "Bar")) rez))

      (test-group
        "Test select generator-of"
        (reset-db!)
        (define (consumer gen)
          (test-equal #(1 "Foo") (gen))
          (test-equal #(2 "Bar") (gen))
          (test-assert (eof-object? (gen)))
          #t)
        (define rez (query qr "select id, val from TestTable" (handler/generator-of (row-handler/vector) consumer)))
        (test-equal #t rez))

      (test-group
        "Test select single-of"
        (reset-db!)
        (define rez (query qr "select id, val from TestTable where id = 1" (handler/single-of (row-handler/vector))))
        (test-equal #(1 "Foo") rez))

      (test-group
        "Test select single-of when not exists"
        (reset-db!)
        (test-assert
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (err)
                  (when (dbutils-error? err)
                    (k #t))
                  (k #f))
                (lambda ()
                  (query qr "select * from TestTable where id = ?" (handler/single-of (row-handler/alist)) 4)
                  #f))))))

      (test-group
        "Test select single-of when more than one exists"
        (reset-db!)
        (test-assert
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (err)
                  (when (dbutils-error? err)
                    (k #t))
                  (k #f))
                (lambda ()
                  (query qr "select * from TestTable" (handler/single-of (row-handler/alist)))
                  #f))))))

      (test-group
        "Test select row-handler/vector"
        (reset-db!)
        (define rez (query qr "select id, val from TestTable where id = 1" (handler/single-of (row-handler/vector))))
        (test-equal #(1 "Foo") rez))

      (test-group
        "Test select row-handler/alist"
        (reset-db!)
        (define rez (query qr "select id, val from TestTable where id = 1" (handler/single-of (row-handler/alist))))
        (test-assert
          (or (equal? '((id . 1) (val . "Foo")) rez)
              (equal? '((val . "Foo") (id . 1)) rez))))

      #| TODO fix
      (test-group
        "Test select row-handler/rec"
        (reset-db!)
        (define rez (query qr "select id, val from TestTable where id = 1" (handler/single-of (row-handler/rec <TestRec>))))
        (test-assert (test-rec? rez))
        (test-equal 1 (test-rec-id rez))
        (test-equal "Foo" (test-rec-val rez)))
      |#

      (test-group
        "Test select with params"
        (reset-db!)

        (define rez (query qr "select id, val from TestTable where id = ? and val = ?" (handler/list-of (row-handler/vector)) 
                           1 "Foo"))

        (test-equal '(#(1 "Foo")) rez))

      (test-group
        "Test insert"
        (reset-db!)
        (define count (update qr "insert into TestTable(id, val) values (3, 'Baz')"))
        (test-equal 1 count)
        (let ((rez (query qr "select * from TestTable where id = 3" (handler/single-of (row-handler/alist)))))
         (test-equal 
           '(id . 3)
           (assoc 'id rez))))

      (test-group
        "Test insert null"
        (reset-db!)
        (define count (update qr "insert into TestTable(id, val) values (?, ?)" 3 #f))
        (test-equal 1 count)
        (let ((rez (query qr "select * from TestTable where val is null" (handler/single-of (row-handler/alist)))))
         (test-equal 
           '(id . 3)
           (assoc 'id rez))
         (test-equal
           '(val . #f)
           (assoc 'val rez))))

      (test-group
        "Test insert and get autoincrement id"
        (update qr "drop table if exists TestTable")
        (update qr "create table TestTable(id integer primary key autoincrement, val text)")
        (define rez (insert qr "insert into TestTable(val) values('Foo')"))
        (test-equal
          1
          rez))

      (test-group
        "Test update"
        (reset-db!)
        (define count (update qr "update TestTable set val = 'Baz' where id = 1"))
        (test-equal 1 count)
        (let ((rez (query qr "select * from TestTable where id = 1" (handler/single-of (row-handler/alist)))))
         (test-equal 
           '(val . "Baz")
           (assoc 'val rez))))

      (test-group
        "Test delete"
        (reset-db!)
        (define count (update qr "delete from TestTable where id = 1"))
        (test-equal 1 count)
        (let ((rez (query qr "select * from TestTable where id = 1" (handler/list-of (row-handler/alist)))))
         (test-equal 
           '()
           rez)))

      (test-group
        "Test transaction commit"
        (reset-db!)
        (call-in-transaction 
          qr
          #f
          (lambda ()
            (update qr "delete from TestTable")
            (test-equal
              '()
              (query qr "select * from TestTable" (handler/list-of (row-handler/alist))))
            (commit)))
        (test-equal
          0
          (length (query qr "select * from TestTable" (handler/list-of (row-handler/alist))))))

      (test-group
        "Test transaction rollback"
        (reset-db!)
        (call-in-transaction 
          qr
          #f
          (lambda ()
            (update qr "delete from TestTable")
            (test-equal
              '()
              (query qr "select * from TestTable" (handler/list-of (row-handler/alist))))
            (rollback)))
        (test-equal
          2
          (length (query qr "select * from TestTable" (handler/list-of (row-handler/alist))))))

      (test-group
        "Test transaction rollback on error"
        (reset-db!)
        (call-in-transaction
          qr
          #f
          (lambda ()
            (update qr "delete from TestTable")
            (call/cc
              (lambda (k)
                (with-exception-handler
                  (lambda (err) (k #t))
                  (lambda ()
                    (call-in-transaction
                      qr
                      #t
                      (lambda ()
                        (test-equal
                          0
                          (length (query qr "select * from TestTable" (handler/list-of (row-handler/alist)))))
                        (error "Error inside uncommited transaction")))))))
            (commit)))

        (test-equal
          2
          (length (query qr "select * from TestTable" (handler/list-of (row-handler/alist))))))

      (test-end))))
