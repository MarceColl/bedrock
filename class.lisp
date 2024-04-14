;;; This file contains extensions to defclass
;;;
;;; Extensions:
;;;  You can add delegates to slots of the class ``(:delegate-to slot-accessor ((method . delegate-method)))``


(in-package #:bedrock)

(defun codegen-delegate (class delegate-obj delegator-method delegate-method)
  `((defmethod ,delegator-method ((m ,class))
       (funcall #',delegate-method (,delegate-obj m)))

    (defmethod (setf ,delegator-method) (new-value (m ,class))
      (setf (,delegate-method (,delegate-obj m)) new-value))))

(defun codegen-delegates (class delegate-def)
  (let ((expected-delegate (car delegate-def))
	(delegate-obj (cadr delegate-def))
	(delegate-mappings (caddr delegate-def)))
    (when (not (eq expected-delegate :delegate-to))
      (error "Malformed delegate expression"))

    (reduce #'append (loop for mapping in delegate-mappings
	  collect (codegen-delegate class delegate-obj (car mapping) (cdr mapping))))))
	
(defmacro define-class (name superclasses slots &rest properties)
  "Extends defclass with some extras like delegates."
  (let* ((name (intern (symbol-name name) *package*))
	 (delegates-defs (remove-if-not (lambda (p) (eq :delegate-to (car p))) properties))
	 (delegates-codegen (reduce #'append (mapcar (lambda (delegate-def) (codegen-delegates name delegate-def)) delegates-defs))))
	 
  `(progn
     (defclass ,name ,superclasses ,slots)
     ,@delegates-codegen)))
