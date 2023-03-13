#lang info

(define collection "racketcon-2018-web-devel-workshop")

(define version "1.0")

(define deps
  '("base"
    "rackunit-lib"
    "txexpr"
    "http"
    "html-parsing"
    "css-expr"
    "web-server-lib"
    "net-cookies-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc "Tutorials for the web devel workshop at RacketCon 2018.")

(define pkg-authors '("jesse@lisp.sh"))

(define scribblings '(("scribblings/workshop.scrbl" ())))

(define raco-commands
  '(("2018-web-workshop"
     (submod racketcon-2018-web-devel-workshop/command raco)
     "get started with web devel in Racket—RacketCon 2018 edition"
     #f)))
