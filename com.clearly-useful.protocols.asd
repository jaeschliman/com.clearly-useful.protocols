;;;; com.clearly-useful.protocols.asd

(asdf:defsystem #:com.clearly-useful.protocols
  :serial t
  :description "Simple protocol implementation for Common Lisp inspired by clojure."
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :version "0.1.2"
  :license "revised BSD"

  :depends-on (#:iterate)
  
  :components ((:file "package")
	       (:file "compile-time")
	       (:file "load-time")
	       (:file "validation")
	       (:file "parse")
	       (:file "codegen")
	       (:file "interface")
               (:file "protocols")))

