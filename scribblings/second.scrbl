#lang scribble/manual

@title{Second Tutorial—Hello! & friends}

Wherein we build a web server that greets us in a random language (the body of the response is random), while also giving us, tucked away in the @tt{Location} header, a URL that tells us its own URL, which @emph{isn't} random.

In addition to learning how to modify bits of a @racket{response?}, we also learn about the powerful dispatch-rules macro and how it helps us to build URLs.
