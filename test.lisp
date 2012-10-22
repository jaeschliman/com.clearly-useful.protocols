#|
(delete-package :com.clearly-useful.protocols.test)
|#

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

;;;test :require

(defprotocol foo
  (abc (o)))

(defprotocol bar
  (:require foo)
  (def (o)))

(extend-type string
  foo
  (abc (o) (reverse o))
  bar
  (def (o) (reverse (abc o))))

(defprotocol baz
  (:require bar)
  (ghi (o)))

(extend-type string
  baz
  (ghi (o) (concatenate 'string
			(abc o)
			(def o))))

(assert (not (fboundp 'baz)))


(defprotocol base-method-test
  (:base-method (o) (list 'ok o))
  (dummy (x)))


(extend-type string
  base-method-test
  (dummy (x) x))

(assert (equalp (base-method-test 'ok)
		'(ok ok)))
(assert (equalp (base-method-test "hello")
		'(ok "hello")))


(defprotocol eponymous-method-test
  (:eponymous-method t)
  (:base-method (o) (list 'ok o))
  (dummy2 (x)))


(extend-type string
  eponymous-method-test
  (dummy2 (x) x))


(assert (equalp (eponymous-method-test 'ok)
		'(ok ok)))
(assert (equalp (eponymous-method-test "hello")
		"hello"))


(defmacro on-error-return (form &body body)
  "return form if any errors occur in body"
  `(catch 'error-return
     (handler-bind ((error #'(lambda (condition)
			       (declare (ignore condition))
			       (throw 'error-return ,form))))
       (progn ,@body))))

(assert (eq :expected
	    (on-error-return :expected
	      (error "dead"))))


(defprotocol eponymous-generic-test
  (:eponymous-generic t)
  (dummy3 (x)))


(extend-type string
  eponymous-generic-test
  (dummy3 (x) x))


(assert (fboundp 'eponymous-generic-test))

(assert (eq :expected
	    (on-error-return :expected
	      (eponymous-generic-test "hello"))))


;;;;;;;;; subtypes should be able
;;;;;;;;; to inherited protocol implementations
;;;;;;;;; for use with require.


(defprotocol inherit-requires-test
  (ihtm (x)))

(defprotocol inherit-requires-test2
  (:require inherit-requires-test)
  (iht2m (c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct iht-class xx))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (iht2-class (:include iht-class))))


(extend-type iht-class
  inherit-requires-test
  (ihtm (x) x))

(assert (typep (make-iht-class) 'inherit-requires-test))
(assert (typep (make-iht2-class) 'inherit-requires-test))

(extend-type iht2-class
  inherit-requires-test2
  (iht2m (x) x))

;;;;; should be over the eval-when issues.





(defstruct still-okay foo)

(defprotocol okay-now?
  (okay-now (x)))

(extend-type still-okay
  okay-now?
  (okay-now (x) '(yes still okay)))

  
  
(let ((ok2 (make-still-okay :foo :bar)))
  (okay-now ok2))




;;;;; TODO: compiling a file containing
;; a protocol should not actually create
;;  protocol, just the compile-time
;; validator. make sure this is in line
;; with deftype etc.
