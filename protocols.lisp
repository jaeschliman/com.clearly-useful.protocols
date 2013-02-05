;;;; protocols.lisp

(in-package #:com.clearly-useful.protocols)




(defgeneric implements-protocol? (object protocol)
  (:method (object (name symbol))
    (implements-protocol? object (find-protocol name)))
  (:method (object (protocol protocol))
    (declare (ignore object))
    nil))


;;;utilities





;;;; validation

(defun method-implementations (name protocol specializer methods)
    (flet ((order (method-list)
	   (sort (copy-seq method-list) #'string< :key #'first))
	 (equivalent (a b)
	   (and (eq (first a) (first b))
		(= (length (second a))
		   (length (second b))))))
    (let ((new (order methods))
	  (def (order (methods protocol))))
     (loop for a in new for b in def
	  collect (transform-method a specializer)
	  when (not (equivalent a b))
	  do (error
	      "implementation ~A does not match ~A for type ~A in ~A"
	      a b specializer name)))))


;;;;; reify

(defclass %reified% () ())

(defun %implement-protocol-for-object (obj methods)
  `(progn
       ,@(mapcar
	  (lambda (list)
	    (protocol-implementation
	     (or (%find-protocol-compilation-note (first list))
		 (error "no known protocol ~A" (first list)))
	     obj
	     (rest list)))
	  (partition-methods methods))))




;;;;interface


(defmacro defprotocol (name &body methods)
  (%defprotocol name methods))



(defmacro extend-type (class &body methods)
  (%extend-type class methods))

(defun class-implements-protocol-p (class protocol)
  (implements-protocol? (find-class class)
			(find-protocol protocol)))

(defun protocol-extends-class-p (protocol class)
  (class-implements-protocol-p class protocol))


(defmacro reify (&body methods)
  (let ((g (gensym)))
    `(let ((,g (make-instance '%reified%)))
       ,(%implement-protocol-for-object `(eql ,g) methods)
       ,g)))

(defmacro extend-object (object &body methods)
  (let ((g (gensym)))
    `(let ((,g ,object))
       ,(%implement-protocol-for-object `(eql ,g) methods)
       ,g)))







