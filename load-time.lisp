(in-package #:com.clearly-useful.protocols)

(defclass protocol ()
  ((name :accessor name :initarg :name)
   (methods :accessor methods :initform (list))
   (properties :accessor properties :initform nil)
   (documentation :accessor protocol-documentation :initform nil)
   (includes-generic-pun :accessor protocol-includes-generic-pun :initform nil)
   (includes-method-pun :accessor protocol-includes-method-pun :initform nil)
   (implementors :initform (list))))

(defvar %protocols% (make-hash-table))

(defun find-protocol (name)
  (gethash name %protocols%))

(defun (setf find-protocol) (value name)
  (setf (gethash name %protocols%) value))

(defun ensure-protocol (name)
  (or (find-protocol name)
      (setf (find-protocol name)
	    (make-instance 'protocol :name name))))

(defmethod make-load-form ((p protocol) &optional env)
  (declare (ignore env))
  `(ensure-protocol ',(name p)))

