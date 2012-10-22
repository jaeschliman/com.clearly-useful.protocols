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

(defun method-implementations (name protocol type methods)
    (flet ((order (method-list)
	   (sort (copy-seq method-list) #'string< :key #'first))
	 (equivalent (a b)
	   (and (eq (first a) (first b))
		(= (length (second a))
		   (length (second b))))))
    (let ((new (order methods))
	  (def (order (methods protocol))))
     (loop for a in new for b in def
	  collect (transform-method a type)
	  when (not (equivalent a b))
	  do (error
	      "implementation ~A does not match ~A for type ~A in ~A"
	      a b type name)))))





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











