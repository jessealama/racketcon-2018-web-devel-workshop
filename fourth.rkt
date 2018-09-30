#lang racket/base

(provide start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/list
         racket/function
         racket/string
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         web-server/dispatch
         web-server/templates
         net/cookies/server
         txexpr
         css-expr
         racket/match
         net/url
         racket/contract
         racket/promise
         (file "./respond.rkt")) ;; convenient HTTP responses

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP request handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (cookie-header? header)
  (header? . -> . boolean?)
  (bytes=? #"Cookie" (header-field header)))

(define/contract (cookies req)
  (request? . -> . list?)
  (define headers (request-headers/raw req))
  (define cookie-headers
    (filter cookie-header? headers))
  (define cookie-values
    (map header-value cookie-headers))
  (define cookie-alists
    (map cookie-header->alist cookie-values))
  (apply append cookie-alists))

(define/contract (cookie->header cookie)
  (cookie? . -> . header?)
  (make-header #"Cookie"
               (cookie->set-cookie-header cookie)))

(module+ test
  (define empty-request
    (make-request
     #"GET" ; method
     (string->url "http://example.com") ; uri
     empty ; headers
     (delay (lambda () empty)) ; body (as a promise)
     #f ; bindings
     "127.0.0.1" ; host IP
     80 ; host PORT
     "172.10.0.1" ; client IP
     ))
  (define (empty-request/headers . headers)
    (struct-copy request
                 empty-request
                 [headers/raw headers])))

(module+ test
  (define-simple-check (check-pair-represents-cookie pair cookie)
    (check-true (pair? pair))
    (check-true (cookie? cookie))
    (check-true (string? (cookie-name cookie)))
    (check-true (string? (cookie-value cookie)))
    (let ([n (car pair)]
          [v (cdr pair)])
      (check-true (bytes? n))
      (check-true (bytes? v))
      (check-true (bytes=? n
                           (string->bytes/utf-8
                            (cookie-name cookie))))
      (check-true (bytes=? v
                           (string->bytes/utf-8
                            (cookie-value cookie)))))))

(module+ test
  ;; a request containing a single cookie header
  (let* ([cookie (make-cookie "jam" "rhubarb")]
         [header (cookie->header cookie)]
         [req (empty-request/headers header)]
         [cookies (cookies req)])
    (check-false (empty? cookies))
    (check-= 1 (length cookies) 0)
    (check-pair-represents-cookie (first cookies) cookie)))

(module+ test
  ;; a request that has no cookies (indeed, no headers) at all:
  (let* ([req (empty-request/headers)]
         [cookies (cookies req)])
    (check-true (empty? cookies))))

(module+ test
  ;; a request that has a single non-cookie header:
  (let* ([header (make-header #"Foo" #"bar")]
         [req (empty-request/headers header)]
         [cookies (cookies req)])
    (check-true (empty? cookies))))

(module+ test
  ;; a request that has two cookie headers
  (let* ([cookie-1 (make-cookie "jam" "rhubarb")]
         [cookie-2 (make-cookie "pumpkin" "pie")]
         [header-1 (cookie->header cookie-1)]
         [header-2 (cookie->header cookie-2)]
         [req (empty-request/headers header-1 header-2)]
         [cookies (cookies req)])
    (check-false (empty? cookies))
    (check-= 2 (length cookies) 0)
    (check-pair-represents-cookie (first cookies) cookie-1)
    (check-pair-represents-cookie (second cookies) cookie-2)))

(module+ test
  ;; a request that has repeated cookie headers
  (let* ([cookie (make-cookie "jam" "rhubarb")]
         [header (cookie->header cookie)]
         [req (empty-request/headers header header)]
         [cookies (cookies req)])
    (check-false (empty? cookies))
    (check-= 2 (length cookies) 0)
    (check-pair-represents-cookie (first cookies) cookie)
    (check-pair-represents-cookie (second cookies) cookie)))

(define (view-homepage req)
  (respond/html
   #:body (include-template "fourth/homepage.html")))

(define (hi req)
  (respond/html
   #:body (include-template "fourth/hi.html")))

(define default-theme #"black-on-white")

(define/contract (make-theme bg-color text-color)
  (string? string? . -> . string?)
  (define bg (string->symbol bg-color))
  (define text (string->symbol text-color))
  (css-expr->css
   (css-expr
     [body
      #:background-color ,bg
      #:color ,text])))

(define themes
  (hash
   #"black-on-white"
   (make-theme "white" "black")
   #"white-on-black"
   (make-theme "black" "white")
   #"white-on-red"
   (make-theme "red" "white")
   #"black-on-green"
   (make-theme "green" "black")
   #"yellow-on-red"
   (make-theme "red" "yellow")))

(define default-theme/css
  (hash-ref themes default-theme))

(define cookie-for-default-style
  (make-cookie "theme"
               default-theme
               #:path #f))

(define/contract (known-theme? theme)
  (bytes? . -> . boolean?)
  (hash-has-key? themes theme))

(define (css-for-theme theme)
  (if (known-theme? theme)
      (values (hash-ref themes theme)
              #f)
      (values default-theme/css
              #t)))

(module+ test
  (let-values ([(css default?) (css-for-theme #"")])
    (check-true (string? css))
    (check-false (string=? "" css))
    (check-true default?)))

(define (style.css req)
  (respond/css
   #:body (include-template "fourth/style.css")))

(define/contract (find-cookie key req)
  (bytes? request? . -> . (or/c false/c bytes?))
  (define cs (cookies req))
  (define c (findf (lambda (p)
                     (bytes=? key (car p)))
                   cs))
  (cond ((pair? c)
         (cdr c))
        (else
         #f)))

(define/contract (theme.css req)
  (request? . -> . response?)
  (define theme (find-cookie theme-key req))
  (cond ((bytes? theme)
         (define-values (css using-fallback?)
           (css-for-theme theme))
         (define c (if using-fallback?
                       cookie-for-default-style
                       (make-cookie theme-key
                                    theme)))
         (respond/css
          #:body css
          #:cookies (list c)))
        (else
         (respond/css
          #:cookies (list cookie-for-default-style)))))

(define theme-key #"theme")

(define/contract (extract-theme req)
  (request? . -> . (or/c false/c bytes?))
  (define data (request-bindings/raw req))
  (define b (bindings-assq theme-key
                           data))
  (cond ((binding:form? b)
         (binding:form-value b))
        (else
         #f)))

(define/contract (bytes->string bstr)
  (bytes? . -> . (or/c false/c string?))
  (define fail (const #f))
  (with-handlers ([exn:fail:contract? fail])
    (bytes->string/utf-8 bstr)))

(define/contract (referrer req fallback)
  (request? string? . -> . string?)
  (define r (headers-assq* #"Referer"
                           (request-headers/raw req)))
  (cond ((bytes? r)
         (let ([s (bytes->string r)])
           (if (string? s)
               s
               fallback)))
        (else
         fallback)))

(define/contract (change-theme req)
  (request? . -> . response?)
  (define ref (referrer req "/"))
  (define t (extract-theme req))
  (define theme
    (cond ((and (bytes? t)
                (known-theme? t))
           t)
          ((bytes? t)
           default-theme)
          (else
           default-theme)))
  (define c (make-cookie theme-key theme))
  (respond #:code 303
           #:headers (list (cons 'Location ref))
           #:cookies (list c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatcher and its associated URL generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (start url-generator)
  (dispatch-rules
   [("")
    #:method "get"
    view-homepage]
   [("hi")
    #:method "get"
    hi]
   [("style.css")
    #:method "get"
    style.css]
   [("theme.css")
    #:method "get"
    theme.css]
   [("change-theme")
    #:method "post"
    change-theme]))
