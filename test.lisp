(defpackage :com.clearly-useful.protocols.test
  (:use :cl :com.clearly-useful.protocols))

(in-package :com.clearly-useful.protocols.test)

;;;;simple tests and examples of the protocol implementation
;;; this file should compile and load without complaint or error

;;; to further test, compile this file and then in a fresh lisp do:

;; (ql:quickload :com.clearly-useful.protocols)
;; (load /path to compiled test.lisp fasl/)
;; (in-package :com.clearly-useful.protocols.test)
;; (sanity-check)

;; you should receive t

;; ------------------------------------------------------

;; a simple protocol with documentation and multiple
;; methods.
(defprotocol greeter
  "A Way to Say Hello"
  (hi (object) "Greet object to stdout")
  (bonjour (object) "Greet object to stdout en francais"))

;;;protocol definitions take effect at compile time.
(assert (fboundp 'greeter-p))

;; a simple protocol without documentation. 
(defprotocol reversible
  (backward (object)))

;; extend-type may implement multiple protocols.
(extend-type string 
    greeter
  (hi (self) (format t "hi! ~A~%" self))
  (bonjour (self) (format t "allo, ~A~%" self))

  reversible
  (backward (self) (reverse self)))

;; although extend-type provides simple error checking
;; at compile time, its definitions do not take effect
;; until execution.
(defun sanity-check ()
  (assert (class-implements-protocol-p 'string 'greeter))
  (assert (protocol-extends-class-p 'greeter 'string))
  (assert (greeter-p "hello"))
  (assert (typep "hello" 'greeter))
  t)

(eval-when (:execute)
  (sanity-check))

;;;types are composable
(deftype impressive-individual ()
  '(and reversible greeter))

;;;run-time checking
(defun wow-us (thing)
  (assert (typep thing 'impressive-individual))  
  (bonjour (backward thing)))

;;;compile-time checking
(declaim
 (ftype (function (impressive-individual) t)
	wow-us))

;;compiling the following should complain
(quote
 (defun xxyyrx ()
  (wow-us '(a b c))))

;;the following does not,
;;unfortunately
;;
;;(can't imagine how it would
;; with satisfies predicates)
;;
;; is there a way to specifiy
;; type predicates for classes
;; instead of instances?
(quote
 (defun xxyrz (a)
  (declare (type vector a))
  (wow-us a)))
