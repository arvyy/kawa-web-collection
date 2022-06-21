(import 
  (scheme base)
  (arvyy slf4j)
  (arvyy mustache)
  (arvyy kawa-spark)
  (arvyy spring-password-encoder)
  (arvyy dbutils))

(define LOG (get-logger "main"))

(define-record-type <user>
  (make-user name password)
  user?
  (name user-name)
  (password user-password))

(define pw-encoder (make-bcrypt-password-encoder))

(define query-runner (make-query-runner "org.sqlite.JDBC" "jdbc:sqlite:sqlitedb" "" ""))
(define (init-db)
  (update query-runner "
          create table if not exists users (
              name text primary key, 
              password text not null);
          "))

(define (find-user-by-name name)
  (define users (query query-runner 
                       "select name, password from users where name = ?" 
                       (handler/list-of (row-handler/vector)) 
                       name))
  (if (= 0 (length users))
      #f
      (let ((row (car users)))
        (make-user (vector-ref row 0)
                   (vector-ref row 1)))))

(define (register-user user)
  (if (find-user-by-name (user-name user))
      "Name already taken"
      (begin
        (insert query-runner 
                "insert into users(name, password) values(?, ?)" 
                (user-name user) 
                (encode-password pw-encoder (user-password user)))
        #f)))

(define (login-user user)
  (define u (find-user-by-name (user-name user)))
  (cond
    ((not u) (begin
               (encode-password pw-encoder "foobarbaz")
               #f))
    ((passwords-match pw-encoder (user-password user) (user-password u)) u)
    (else #f)))

(define (partial-locator name)
  (open-input-file (string-append "templates/" name ".html")))
(define index-template (compile "index" partial-locator))
(define login-template (compile "login" partial-locator))

(static-files/external-location "static")

(get "/" (lambda (req resp)
           (define session (req/session req))
           (define user (and session (session/attribute session "user")))
           (if user
               (execute index-template `((name . ,(user-name user))))
               (resp/redirect resp "/login"))))

(get "/login" (lambda (req resp)
                (define session (req/session req))
                (define user (and session (session/attribute session "user")))
                (if user
                    (resp/redirect resp "/")
                    (execute login-template `((notifications . #()))))))

(post "/login" (lambda (req resp)
                 (define user (make-user (req/query-param req "name")
                                         (req/query-param req "password")))
                 (define u (login-user user))
                 (if u
                     (let ((session (req/create-session! req)))
                       (session/set-attribute! session "user" u)
                       (log-info LOG "Logged-in user {}" (user-name u))
                       (resp/redirect resp "/"))
                     (execute login-template `((notifications . #(((text . "Invalid credentials")
                                                                   (type . "error")))))))))

(post "/register" (lambda (req resp)
                    (define user (make-user (req/query-param req "name")
                                            (req/query-param req "password")))
                    (define err (register-user user))
                    (if err
                        (execute login-template `((notifications . #(((text . ,err)
                                                                      (type . "error"))))))
                        (execute login-template `((notifications . #(((text . "Registered. Please login")
                                                                      (type . "info")))))))))

(post "/logout" (lambda (req resp)
                  (define session (req/session req))
                  (when session
                    (session/remove-attribute! session "user"))
                  (resp/redirect resp "/login")))

(init-db)
