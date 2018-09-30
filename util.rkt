#lang racket/base

(provide bytes->string
         bytes->number)

(require racket/contract
         racket/function)

(define/contract (bytes->string b)
  (bytes? . -> . (or/c false/c string?))
  (with-handlers ([exn:fail:contract? (const #f)])
    (bytes->string/utf-8 b)))

(define/contract (bytes->number b)
  (bytes? . -> . (or/c false/c number?))
  (define s (bytes->string b))
  (cond [(string? s)
         (string->number s 10 'number-or-false)]
        [else
         #f]))
