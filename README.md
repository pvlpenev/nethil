# About

Nethil is a pico-framework for Common Lisp. It's very small, but eventually it will be bigger than Rails and EJB combined.

# FAQ

# How do I use it?

```common-lisp
(defpackage #:nethil-example
  (:use #:cl #:nethil))

(in-package #:nethil-example)

(define-route hello "/"
  "hello")

(define-route hello-you "/:you"
  (format nil "hello ~a" (cdr (assoc :you *bindings*))))

(start :port 8000)
```
now go to http://localhost:8000/ and http://localhost:8000/your-name and have fun!

You can also directly return a clack response

```common-lisp
(define-route hello-clack "/world"
  '(200 ("text/html") ("Hello World")))
```

There are 3 global variables:
`*env*` is the clack environment
`*request*` is the request object created from that environment.
`*bindings*` are the route bindings, an alist.

## Does it pass the dogfood test?

No.

## Will it ever?

If I find the time to actually make it usable and extensible...

# Why pico?

It's 40 lines. Everybody knows, micro is when it's less than 2k, nano is when it's just under a 100 and under 40 lines you get to call it pico :)

# What does the name stand for?

Something in Elvish, but I can't remember what. I was playing with a fantasy name generator and kept it as a cool name for a project.
