#lang scribble/manual

@title[#:style "toc"]{(eighth RacketCon) Workshop: Web Programming}
@author[(author+email "Jesse Alama" "jesse@lisp.sh")]

@defmodule[racketcon-2018-web-devel-workshop]

At the @tt{(eighth RacketCon)}, a number of workshops were held. This document covers the tutorials on web programming.

There are four tutorials. Once you've installed this package, use @tt{raco} to launch each of them, like so:

@verbatim|{
raco 2018-web-workshop first
}|

Doing that launches the first server (or, if you prefer, web application) on your local machine. Instead of @tt{first}, you can use @tt{second}, @tt{third}, and @tt{fourth} to launch the others.

(This material has been adapted from the first few chapters of my ebook @link["https://serverracket.com"]{@emph{Server: Racket—Practical Web Development with the Racket HTTP Server}}.)

@include-section["first.scrbl"]

@include-section["second.scrbl"]

@include-section["third.scrbl"]

@include-section["fourth.scrbl"]
