(in-package #:com.clearly-useful.protocols)


;;definition code gen 


(defun protocol-test-name (name)
  (intern (concatenate 'string (symbol-name name)
		       (load-time-value (symbol-name '-p)))))

(defun protocol-test-function (name)
  `(defun ,(protocol-test-name name) (object)
     ,(format nil "test if object implements ~S" name)
     (implements-protocol? object ,(ensure-protocol name))))

(defun protocol-deftype (name documentation)
  `(deftype ,name () ,documentation
	    '(satisfies ,(protocol-test-name name))))


;;implementation code gen
(defun %implements? (typename protocolname)
  (or (class-implements-protocol-p typename protocolname)
      (loop for i in (slot-value (find-protocol protocolname) 'implementors)
	   thereis (subtypep typename i))))

(defun %compile-time-implements? (typename protocolname)
  (or (%implements? typename protocolname)
      (loop for i in (slot-value (%find-protocol-compilation-note protocolname) 'implementors)
	   thereis (subtypep typename i))))

(defun generate-implements? (class name)
  `((defmethod implements-protocol? ((object ,class)
				     (protocol (eql (ensure-protocol ',name))))
      (declare (ignorable object)
	       (ignorable protocol))	 t)
    (defmethod implements-protocol? ((object (eql (find-class ',class)))
				     (protocol (eql (ensure-protocol ',name))))
      (declare (ignorable object protocol)) 	 t)))

(defun generate-requires (requires class name)
  (when requires
    (list
     `(dolist (required ',requires)
	(unless (%implements? ',class required)
	  (error "for class ~S: protocol ~S requires protocol ~S be implemented"
		 ',class ',name required))))))

(defun generate-compile-time-requires (requires class name)
  (when requires
    (list
     `(dolist (required ',requires)
	(unless (%compile-time-implements? ',class required)
	  (error "for class ~S: protocol ~S requires protocol ~S be implemented"
		 ',class ',name required))))))

(defun protocol-implementation-register (protocol class)
  (let* ((name (name protocol))
	 (requires (getf (properties protocol) :require)))
    `(progn
       ,@(generate-implements? class name)
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

(defun protocol-definition (protocol unparsed-body)
  (let* ((name (name protocol))
	 (methods (methods protocol))
	 (properties (properties protocol)))
    (let ((p (gensym)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   ,(protocol-test-function name)
	   ,(protocol-deftype name (protocol-documentation protocol))
	   ,@(protocol-definition-defgeneric-forms name methods properties)
	   ,@(when (protocol-includes-generic-pun protocol)
		 (list (protocol-definition-eponymous-generic name properties))))
	 (let ((,p (ensure-protocol ',name)))
	 (%build-protocol-object ,p ',unparsed-body)
	 (setf (protocol-includes-generic-pun ,p)
	       ,(%bool (protocol-includes-generic-pun protocol))
	       (protocol-includes-method-pun ,p)
	       ,(%bool (protocol-includes-method-pun protocol))))))))


(defun protocol-implementation-base-method (protocol type)
  (let ((name (name protocol)))
    `(defmethod ,name ((object ,type)) object)))

(defun protocol-implementation-compile-time (protocol type methods)
  (declare (ignore methods))
  (let* ((requires (getf (properties protocol) :require))
	 (name (name protocol)))
    `(progn
       (pushnew ',type (slot-value
			(%ensure-protocol-compilation-note ',name) 'implementors))
       ,@(generate-compile-time-requires requires type name))))

(defun protocol-implementation (protocol type methods)
  "protocol, implementation type name, method list"
  
  (let ((name (name protocol)))
    `(progn
       (eval-when (:compile-toplevel)
	 
	 ,(protocol-implementation-compile-time protocol type methods)
	 (validate-protocol-implementation-methods ,protocol ',type ',methods)
	 
	 )
       ,@(method-implementations name type methods)
       ,@(when (protocol-includes-method-pun protocol)
	       (list (protocol-implementation-base-method protocol type)))
       ,(protocol-implementation-register protocol type))))




(defun %build-protocol-object (protocol methods)
  (let ((name (name protocol)))
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
	   (not (null (getf properties :base-method))))

       (properties protocol) properties
       (methods protocol) methods)

      
      protocol
      
      )))


;; (defun %defprotocol (name methods)
;;   (let ((protocol (ensure-protocol name)))
;;     (%build-protocol-object protocol methods)
;;     (let* ((methods (methods protocol)
;; 	     )
;; 	  (properties (properties protocol)))
;;       ;; the actual codegen
;;       `(eval-when (:compile-toplevel :load-toplevel :execute)
;; 	 ,(protocol-definition name methods properties)))))

(defun %defprotocol (name body)

  (let ((protocol (%ensure-protocol-compilation-note name)))
    ;;compile time effects
    (%build-protocol-object protocol body)
    `(progn
	 ,(protocol-definition protocol body))
    
    ))

;; (defun %extend-type (class methods)
  
;;   `(eval-when (:compile-toplevel :load-toplevel :execute)
;;      (progn
;;        ,@(mapcar
;; 	  (lambda (list)
;; 	    (protocol-implementation (ensure-protocol (first list))
;; 				     class
;; 				     (rest list)))
;; 	  (partition-methods methods)))))

(defun %extend-type (class methods)
  `(progn
       ,@(mapcar
	  (lambda (list)
	    
	    (protocol-implementation
	     ;;this should work with compilation note, not actual.
	     (%ensure-protocol-compilation-note (first list))
	     class
	     (rest list)))
	  (partition-methods methods))))