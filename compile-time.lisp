(in-package :com.clearly-useful.protocols)

(defclass %protocol-compilation-note ()
  ((name :accessor name :initarg :name)
   (methods :accessor methods :initform (list))
   (properties :accessor properties :initform nil)
   (documentation :accessor protocol-documentation :initform nil)
   (includes-generic-pun :accessor protocol-includes-generic-pun :initform nil)
   (includes-method-pun :accessor protocol-includes-method-pun :initform nil)
   (implementors :initform (list))))

(defvar %protocol-compilation-notes% (make-hash-table))

(defun %find-protocol-compilation-note (name)
  (gethash name %protocol-compilation-notes%))

(defun (setf %find-protocol-compilation-note) (value name)
  (setf (gethash name %protocol-compilation-notes%) value))

(defun %ensure-protocol-compilation-note (name)
  (or (%find-protocol-compilation-note name)
      (setf (%find-protocol-compilation-note name)
	    (make-instance '%protocol-compilation-note :name name))))

(defmethod make-load-form ((p %protocol-compilation-note) &optional env)
  (declare (ignore env))
  `(%ensure-protocol-compilation-note `,(name p)))
