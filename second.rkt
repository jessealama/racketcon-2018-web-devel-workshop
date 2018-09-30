#lang racket/base

(provide start)

(require web-server/http/request-structs
         web-server/dispatch
         (only-in net/url
                  url->string)
         (only-in (file "./respond.rkt")
                  set-location
                  respond/text
                  not-found
                  not-allowed))

(define greetings/hash
  (hash "en" "Hello!"
        "de" "Hallo!"
        "es" "¡Hola!"
        "pt" "Ola!"
        "jp" "こんにちは"
        "ar" "مرحبا"))

;; all available languages
(define languages
  (hash-keys greetings/hash))

;; the number of available language
(define num-languages
  (length languages))

;; -> string?
(define (random-language)
  (list-ref languages
            (random num-languages)))

;; request? -> response?
(define (hello req)
  (define lang (random-language))
  (define greeting (hash-ref greetings/hash lang))
  (set-location (respond/text #:body greeting)
                (url-generator hello+lang lang)))

;; request? string? -> response?
(define (hello+lang req lang)
  (define greeting (hash-ref greetings/hash
                             lang
                             #f))
  (cond [(string? greeting)
         (respond/text #:body greeting)]
        [else
         (not-found)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dispatcher and URL generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (start url-generator)
  (dispatch-rules
   [("hello") hello]
   [("hello" (string-arg)) hello+lang]
   [("hello") #:method (regexp ".*") not-allowed]))
