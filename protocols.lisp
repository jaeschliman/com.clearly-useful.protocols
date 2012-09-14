;;;; protocols.lisp

(in-package #:com.clearly-useful.protocols)

(defclass protocol ()
  ((name :accessor name :initarg :name)
   (methods :accessor methods :initform (list))
   (documentation :accessor protocol-documentation :initform nil)))

(defmethod make-load-form ((p protocol) &optional env)
  (declare (ignore env))
  `(ensure-protocol ',(name p)))

(defvar %protocols% (make-hash-table))

(defun find-protocol (name)
  (gethash name %protocols%))

(defun (setf find-protocol) (value name)
  (setf (gethash name %protocols%) value))

(defun ensure-protocol (name)
  (or (find-protocol name)
      (setf (find-protocol name)
	    (make-instance 'protocol :name name))))


(defgeneric implements-protocol? (object protocol)
  (:method (object (name symbol))
    (implements-protocol? object (find-protocol name)))
  (:method (object (protocol protocol))
    (declare (ignore object))
    nil))

(defun protocol-test-name (name)
  (intern (concatenate 'string (symbol-name name)
		       (load-time-value (symbol-name '-p)))))

(defun protocol-test-function (name)
  `(defun ,(protocol-test-name name) (object)
     ,(format nil "test if object implements ~S" name)
     (implements-protocol? object ,(ensure-protocol name))))

(defun protocol-deftype (name)
  `(deftype ,name () ,(protocol-documentation
		       (ensure-protocol name))
	    '(satisfies ,(protocol-test-name name))))

(defun protocol-register (name class)
  (let ((protocol (ensure-protocol name)))
    `(progn
       (defmethod implements-protocol? ((object ,class)
					(protocol (eql ,protocol)))
	 (declare (ignorable object)
		  (ignorable protocol))
	 t)
       (defmethod implements-protocol? ((object (eql ,(find-class class)))
					(protocol (eql ,protocol)))
	 (declare (ignorable object protocol))
	 t))))

(defun transform-method (method type)
  (list* 'defmethod
	 (car method)
	 (cons (list (caadr method) type) (cdadr method))
	 (cddr method)))

(defun method-implementations (name type methods)
    (flet ((order (method-list)
	   (sort (copy-seq method-list) #'string< :key #'first))
	 (equivalent (a b)
	   (and (eq (first a) (first b))
		(= (length (second a))
		   (length (second b))))))
    (let ((new (order methods))
	  (def (order (methods (find-protocol name)))))
     (loop for a in new for b in def
	  collect (transform-method a type)
	  when (not (equivalent a b))
	  do (error
	      "implementation ~A does not match ~A for type ~A in ~A"
	      a b type name)))))


(defun transform-method-to-defgeneric (protocol-name method)
  (let* ((name (first method))
	 (arglist (second method))
	 (documentation (third method))
	 (prim (first arglist)))
    (list* 'defgeneric name arglist
	   `(:method ,arglist
	      (declare (ignore ,@(rest arglist)))
	      (error "Object ~S does not implement protocol ~A" ,prim ',protocol-name))
	   (and documentation
		`((:documentation ,documentation))))))

(defun protocol-definition-defgeneric-forms (protocol-name methods)
  (loop
     with name = protocol-name
     for method in methods
     collect (transform-method-to-defgeneric name method)))

(defun protocol-definition (name methods)
  (let ((p (gensym)))
    `(let ((,p (ensure-protocol ',name)))
       (setf (methods ,p) ',methods)
       ,@(protocol-definition-defgeneric-forms name methods)
       ,(protocol-test-function name)
       ,(protocol-deftype name))))

(defun valid-protocol-method-name-p (thing)
  (symbolp thing))

(defun valid-protocol-lambda-list-p (params)
  (and (consp params)
       (every #'symbolp params)
       (not (member '&key params))
       (not (member '&optional params))
       (not (member '&allow-other-keys params))))

(defun validate-protocol-implementation-methods (protocol-name type methods)
  (let* ((protocol (find-protocol protocol-name))
	 (method-definitions (methods protocol)))
    (dolist (method methods)
      (let* ((name (first method))
	     (params (second method))
	     (def (find name method-definitions :key #'first)))
	(unless (and def
		     (valid-protocol-method-name-p name)
		     (valid-protocol-lambda-list-p params)
		     (= (length (second def))
			(length params)))
	  (error "invalid method ~A for ~A implementation of protocol ~A"
		 method type protocol-name))))))

(defun protocol-implementation (name type methods)
  (validate-protocol-implementation-methods name type methods)
  `(progn ,@(method-implementations name type methods)
	  ,(protocol-register name type)))

(defun validate-protocol-definition-methods (name methods)
  (flet ((ok (method) (let ((name (first method))
			    (params (second method))
			    (documentation (third method)))
			(and (valid-protocol-method-name-p name)
			     (valid-protocol-lambda-list-p params)
			     (or (null documentation)
				 (stringp documentation))))))
    (iter (for method in methods)
	  (unless (ok method)
	    (error "malformed method declaration in protocol definition: ~A ~A" name method)))
    t))

(defmacro defprotocol (name &body methods)
  (when (stringp (first methods))
    (setf (protocol-documentation (ensure-protocol name)) (first methods)
	  methods (rest methods)))
  (validate-protocol-definition-methods name methods)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(protocol-definition name methods)))

(defun partition-methods (list) ;;doesn't maintain order
  (iter (for i in list)
	(when (and (atom i) group)
	  (collect group into result)
	  (setq group nil))
	(collect i into group)
	(finally
	 (return (nreverse (cons group result))))))

(defmacro extend-type (class &body methods)
  `(progn
     ,@(mapcar
	(lambda (list)
	  (protocol-implementation (first list)
				   class
				   (rest list)))
	(partition-methods methods))))

(defun class-implements-protocol-p (class protocol)
  (implements-protocol? (find-class class)
			(find-protocol protocol)))

(defun protocol-extends-class-p (protocol class)
  (class-implements-protocol-p class protocol))











