;;;; com.clearly-useful.protocols.asd

(asdf:defsystem #:com.clearly-useful.protocols
  :serial t
  :description "Simple protocol implementation for Common Lisp inspired by clojure."

  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  
  :license "revised BSD"
  :depends-on (:iterate)
  :components ((:file "package")
               (:file "protocols")))

