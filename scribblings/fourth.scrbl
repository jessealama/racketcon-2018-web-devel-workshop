#lang scribble/manual

@title{Fourth Tutorial—Can I have a cookie?}

Wherein we build a web server that has state in the form of cookies to implement a very basic theme switcher.

We start to use cookies now, an omnipresent technique for recognizability in HTTP (whereby the server can recognize that the request it's currently processing belongs to a known `user` or `session'. We also look at X-exprs, an alternative to @tt{include-template} for generating HTML. (We'll use Matthew Butterick's @tt{txexpr} library to help us generate the X-exprs.)
