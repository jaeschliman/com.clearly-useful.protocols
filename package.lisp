;;;; package.lisp

(defpackage #:com.clearly-useful.protocols
  (:use #:cl #:iterate)
  (:export
   #:defprotocol
   #:extend-type
   #:reify
   #:extend-object
   #:protocol-extends-class-p
   #:class-implements-protocol-p))

