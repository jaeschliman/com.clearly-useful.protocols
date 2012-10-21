(in-package :com.clearly-useful.protocols)

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
