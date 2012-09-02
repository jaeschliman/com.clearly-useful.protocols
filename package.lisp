;;;; package.lisp

(defpackage #:com.clearly-useful.protocols
  (:use #:cl #:iterate)
  (:export
   #:defprotocol
   #:extend-type
   #:protocol-extends-class-p
   #:class-implements-protocol-p))

