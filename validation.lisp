(in-package #:com.clearly-useful.protocols)

(defun validate-defprotocol-option (list)
  (and (member (first list) '(:eponymous-method
			      :eponymous-generic
			      :require
			      :base-method)))
  ;;TODO: validate option bodies
  t)


(defun valid-protocol-method-name-p (thing)
  (symbolp thing))

(defun valid-protocol-lambda-list-p (params)
  (and (consp params)
       (every #'symbolp params)
       (not (member '&key params))
       (not (member '&optional params))
       (not (member '&allow-other-keys params))))

(defmacro all (&body body)
  "(all x1 y1 x2 y2 ...) -> (and (or x1 y1) (or x2 y2) ...)"
  (let ((pairs (loop for (test error) on body by #'cddr collect
		    `(or ,test ,error))))
    (cons 'and pairs)))

(defun validate-protocol-implementation-methods (protocol type methods)
  (let ((method-definitions (methods protocol))
	(protocol-name (name protocol)))
    (dolist (method methods)
      (let* ((name (first method))
	     (params (second method))
	     (def (find name method-definitions :key #'first)))

	(all def
	     (error "undefined method ~A for protocol ~A in implementation for ~A"
		    name protocol-name type)
	     (valid-protocol-method-name-p name)
	     (error "invalid protocol method name ~A in implementation of ~A for ~A"
		    name protocol-name type)
	     (valid-protocol-lambda-list-p params)
	     (error "invalid protocol method lambda list ~A ~A in implementation of ~A for ~A"
		    name params protocol-name type)
	     (= (length (second def))
		(length params))
	     (error "parameters for method ~A don't match protocol: ~A ~A"
		    name  params (second def)))))))

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


