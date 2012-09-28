;;;; protocols.lisp

(in-package #:com.clearly-useful.protocols)

(defclass protocol ()
  ((name :accessor name :initarg :name)
   (methods :accessor methods :initform (list))
   (properties :accessor properties :initform nil)
   (documentation :accessor protocol-documentation :initform nil)
   (includes-generic-pun :accessor protocol-includes-generic-pun :initform nil)
   (includes-method-pun :accessor protocol-includes-method-pun :initform nil)
   (implementors :initform (list))))

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


;;;utilities

(defun partition-methods (list)
  (iter (for i in list)
	(when (and (atom i) group)
	  (collect group into result)
	  (setq group nil))
	(collect i into group)
	(finally
	 (return (append result (list group))))))

(defstruct protocol-body
  properties ;;plist
  methods )

(defun validate-defprotocol-option (list)
  (and (member (first list) '(:eponymous-method
			      :eponymous-generic
			      :require
			      :base-method)))
  ;;TODO: validate option bodies
  t
  )

(defun parse-protocol-body (methods)
  (let ((body (make-protocol-body
	       :properties (list)
	       :methods (list))))
    (loop
       for list in methods
       for thing = (first list)
       do (if (keywordp thing)
	      (if (validate-defprotocol-option list)
		  (setf (getf (protocol-body-properties body) thing)
		    (rest list))
		  (error "invalid option in defprotocol: ~S" list))
	      (push list (protocol-body-methods body))))
    body))



;;;; validation
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

;;definition code gen 


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


;;implementation code gen
(defun %implements? (typename protocolname)
  (or (class-implements-protocol-p typename protocolname)
      (loop for i in (slot-value (find-protocol protocolname) 'implementors)
	   thereis (subtypep typename i))))

(defun generate-implements? (class protocol)
  `((defmethod implements-protocol? ((object ,class)
				     (protocol (eql ,protocol)))
      (declare (ignorable object)
	       (ignorable protocol))	 t)
    (defmethod implements-protocol? ((object (eql ,(find-class class)))
				     (protocol (eql ,protocol)))
      (declare (ignorable object protocol)) 	 t)))

(defun generate-requires (requires class name)
  (when requires
    (list
     `(dolist (required ',requires)
	(unless (%implements? ',class required)
	  (error "for class ~S: protocol ~S requires protocol ~S be implemented"
		 ',class ',name required))))))

(defun protocol-implementation-register (name class)
  (let* ((protocol (ensure-protocol name))
	 (requires (getf (properties protocol) :require)))
    `(progn
       ,@(generate-implements? class protocol)
       (pushnew ',class (slot-value (ensure-protocol ',name) 'implementors))
       ,@(generate-requires requires class name))))

(defun transform-method (method type)
  (list* 'defmethod
	 (car method)
	 (cons (list (caadr method) type) (cdadr method))
	 (cddr method)))

;;xx


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

(defun protocol-definition-defgeneric-forms (protocol-name methods properties)
  (declare (ignorable properties))
  (loop
     with name = protocol-name
     for method in methods
     collect (transform-method-to-defgeneric name method)))

(defun protocol-definition-eponymous-generic (name properties)
  (let ((base-method (getf properties :base-method))
	(base-doc (or (getf properties :base-documentation)
		      (format nil
			      "convert an object to ~S protocol, or error."
			      name))))
    `(defgeneric ,name (object)
       (:documentation ,base-doc)
       ,@(when base-method
	       (list `(:method ,@base-method))))))
(defun %bool (o) (not (null o)))

(defun protocol-definition (name methods properties)
  (let ((protocol (ensure-protocol name)))
    (let ((p (gensym)))
      `(let ((,p (ensure-protocol ',name)))
	 (setf (methods ,p) ',methods)
	 (setf (properties ,p) ',properties)
	 ,@(when (protocol-includes-generic-pun protocol)
		 (list (protocol-definition-eponymous-generic name properties)))
	 ,@(protocol-definition-defgeneric-forms name methods properties)
	 ,(protocol-test-function name)
	 ,(protocol-deftype name)
	 (setf (protocol-includes-generic-pun ,p)
	       ,(%bool (protocol-includes-generic-pun protocol))
	       (protocol-includes-method-pun ,p)
	       ,(%bool (protocol-includes-method-pun protocol)))))))



(defun protocol-implementation-base-method (name type)
  `(defmethod ,name ((object ,type)) object))

(defun protocol-implementation (name type methods)
  "protocol name, implementation type name, method list"
  (validate-protocol-implementation-methods name type methods)
  `(progn ,@(method-implementations name type methods)
	  ,@(when (protocol-includes-method-pun (ensure-protocol name))
		  (list (protocol-implementation-base-method name type)))
	  ,(protocol-implementation-register name type)))



;;;;interface


(defmacro defprotocol (name &body methods)
  (let ((protocol (ensure-protocol name)))
    (when (stringp (first methods))
      (setf (protocol-documentation protocol) (first methods)
	    methods (rest methods)))
    (let* ((body (parse-protocol-body methods))
	   (methods (protocol-body-methods body))
	   (properties (protocol-body-properties body)))
      ;;validate structure
      (validate-protocol-definition-methods name methods)

      (setf
       ;;wether to generate punning methods
       (protocol-includes-method-pun protocol)
       (getf properties :eponymous-method)

       ;; including methods implies including
       ;; a generic
       (protocol-includes-generic-pun protocol)
       (or (getf properties :eponymous-generic)
	   (protocol-includes-method-pun protocol)
	   ;;base-method implies eponymous
	   (not (null (getf properties :base-method)))))
      
      ;; the actual codegen
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 ,(protocol-definition name methods properties)))))



(defmacro extend-type (class &body methods)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       ,@(mapcar
	  (lambda (list)
	    (protocol-implementation (first list)
				     class
				     (rest list)))
	  (partition-methods methods)))))

(defun class-implements-protocol-p (class protocol)
  (implements-protocol? (find-class class)
			(find-protocol protocol)))

(defun protocol-extends-class-p (protocol class)
  (class-implements-protocol-p class protocol))











