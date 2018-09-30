#lang racket/base

(provide start)

(require web-server/http
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/servlet-env
         web-server/dispatch
         racket/list
         web-server/dispatch
         web-server/templates
         racket/string
         racket/contract
         txexpr
         xml
         (file "./respond.rkt")
         (file "./util.rkt"))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-id 1)
(define db (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Products
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct product
  (name sku price))

(define/contract
  (product-input #:type [type "text"]
                 #:name [name #f]
                 #:required? [required? #f])
  (->* ()
       (#:type (one-of/c 'text 'number)
        #:name (or/c false/c non-empty-string?)
        #:required? boolean?)
      txexpr?)
  (txexpr* 'input
           (append (list (list 'type (symbol->string type))
                         (list 'name name))
                   (if required?
                       (list (list 'required "required"))
                       empty))))

(define/contract
  (label #:for [for #f]
         content)
  (-> #:for (or/c false/c non-empty-string?)
      txexpr-element?
      txexpr?)
  (txexpr* 'label
           (if (string? for)
               (list (list 'for for))
               empty)
           content))

(define/contract (product->form p)
  (product? . -> . txexpr?)
  (define name-input/bare
    (product-input #:type 'text
                   #:name "name"
                   #:required? #t))
  (define sku-input/bare
    (product-input #:type 'text
                   #:name "sku"
                   #:required? #t))
  (define price-input/bare
    (attr-set* (product-input #:type 'number
                              #:name "price"
                              #:required? #t)
               'min "0"
               'step "0.1"))
  (define name-input/enriched
    (cond ((string? (product-name p))
           (attr-set* name-input/bare
                      'value
                      (product-name p)))
          (else
           name-input/bare)))
  (define sku-input/enriched
    (cond ((string? (product-sku p))
           (attr-set* sku-input/bare
                      'value
                      (product-sku p)))
          (else
           sku-input/bare)))
  (define price-input/enriched
    (cond ((not (number? (product-price p)))
           price-input/bare)
          (else
           (attr-set* price-input/bare
                      'value
                      (format "~a" (product-price p))))))
  (define form-children
    (list (label #:for "name" "Name")
          name-input/enriched
          (txexpr* 'br)
          (label #:for "sku" "SKU")
          sku-input/enriched
          (txexpr* 'br)
          (label #:for "price" "Price")
          price-input/enriched
          (txexpr* 'input
                   (list (list 'type "submit")))))
  (txexpr 'form
           (list (list 'action "/catalog")
                 (list 'method "post"))
           (append form-children
                   (cond ((not (number? (product-price p)))
                          empty)
                         ((> (product-price p) 0)
                          empty)
                         (else
                          (list (txexpr* 'p
                                         (list (list 'class "error-message"))
                                         "Invalid price")))))))

(define empty-product-form
  (product->form (product #f #f #f)))

(define/contract (product-complete? p)
  (product? . -> . boolean?)
  (and (string? (product-name p))
       (string? (product-sku p))
       (and (number? (product-price p))
            (> (product-price p) 0))))

(define/contract (product-known? product-name)
  (string? . -> . boolean?)
  (hash-has-key? db product-name))

(define/contract (product-as-html p)
  (product? . -> . string?)
  (let ([name (xexpr->string (product-name p))]
        [sku (xexpr->string (product-sku p))]
        [price (real->decimal-string (product-price p))])
    (include-template "third/catalog/product.html")))

(module+ test
  (require xml/path)
  (require html-parsing)
  (test-begin
    (define p (product "sweet app"
                       "SAPP2017"
                       99.89))
    (define p/html (product-as-html p))
    (define p/xexp (html->xexp p/html))
    (define dl (se-path* '(dl) p/xexp))
    (define dds (se-path*/list '(dl dd) p/xexp))
    (define dts (se-path*/list '(dl dt) p/xexp))
    ;; is there a dl element here at all?
    (check-not-false dl)

    ;; are there exactly 3 dt and dd elements?
    (check-= 3 (length dds) 0)
    (check-= 3 (length dts) 0)

    ;; let's look at the elemenets:
    (define dd1 (first dds))
    (define dd2 (second dds))
    (define dd3 (third dds))
    (define dt1 (first dts))
    (define dt2 (second dts))
    (define dt3 (third dts))

    (check-pred string? dd1 (format "~a" dd1))
    (check-pred string? dd2 (format "~a" dd2))
    (check-pred string? dd3 (format "~a" dd3))

    (check-pred string? dt1 (format "~a" dt1))
    (check-pred string? dt2 (format "~a" dt2))
    (check-pred string? dt3 (format "~a" dt3))

    ;; they should have the content we expect:
    (check-true (string=? "sweet app" dd1))
    (check-true (string=? "SAPP2017" dd2))
    (check-true (string=? "99.89" dd3))))

(define/contract (get-catalog-item req x)
  (request? integer? . -> . response?)
  (define p (hash-ref db x #f))
  (cond ((product? p)
         (respond/html #:body (product-as-html p)))
        (else
         (let ([title "What?"]
               [body (include-template "third/catalog/not-found.html")])
           (respond/html #:code 404
                         #:body (include-template "third/wrapper.html"))))))

(define/contract (db-as-html #:messages [messages empty])
  (-> #:messages (listof string?)
      string?)
  (let ([products (hash-values db)]
        [title "Catalog"])
    (let ([body (include-template "third/catalog/index.html")])
      (include-template "third/wrapper.html"))))

(define/contract
  (db-as-html-with-form #:messages [messages empty]
                        #:form [product-form empty-product-form])
  (->* ()
       (#:messages (listof string?)
        #:form txexpr?)
      string?)
  (let ([products (hash-values db)]
        [title "Catalog"])
    (let ([body (include-template "third/catalog/index-form-as-variable.html")])
      (include-template "third/wrapper.html"))))

(define/contract (get-whole-catalog req)
  (request? . -> . response?)
  (respond/html
   #:body (db-as-html-with-form)))

(define/contract (form-binding/bytes b)
  (binding? . -> . (or/c false/c bytes?))
  (cond ((binding:form? b)
         (binding:form-value b))
        (else
         #f)))

(define/contract (bindings->product binds)
  ((listof binding?) . -> . product?)
  (define name/binding (bindings-assq #"name" binds))
  (define price/binding (bindings-assq #"price" binds))
  (define sku/binding (bindings-assq #"sku" binds))
  (product
   (bytes->string (form-binding/bytes name/binding))
   (bytes->string (form-binding/bytes sku/binding))
   (bytes->number
    (form-binding/bytes price/binding))))

;; request? -> product?
(define/contract (request->product req)
  (request? . -> . product?)
  (bindings->product (request-bindings/raw req)))

(define/contract (create-catalog-item req)
  (request? . -> . response?)
  (define p (request->product req))
  (cond ((product-complete? p)
         (hash-set! db
                    current-id
                    p)
         (set! current-id (add1 current-id))
         (respond
          #:code 303
          #:headers (list (cons "Location"
                                (dispatch-url get-whole-catalog)))))
        (else
         (respond/html #:code 400
                       #:body (product->form p)))))

(define/contract (view-catalog req)
  (request? . -> . response?)
  (respond/html
   #:body (db-as-html-with-form)))

(define-values (start dispatch-url)
  (dispatch-rules

   ;; a catalog item
   [("catalog" (integer-arg))
    #:method "get"
    get-catalog-item]

   ;; the whole catalog
   [("catalog")
    #:method "get"
    get-whole-catalog]

   ;; adding a new product
   [("catalog")
    #:method "post"
    create-catalog-item]

   [else
    not-found]))
