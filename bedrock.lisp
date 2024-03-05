(in-package :cl-user)
(defpackage bedrock
  (:use :cl)
  (:export :with-gensyms :aif :aand :with-local-symbols :make-keyword :define-error :in :λ :define-simple-class :maphashmap :is-keyword :any :pushback))
(in-package :bedrock)

(defmacro with-gensyms (gensyms &body body)
  `(let (,@(mapcar
	    (lambda (g)
	      (let ((g-str (format nil "~a-" g)))
		`(,g (gensym ,g-str))))
	    gensyms))
     ,@body))

(defmacro with-local-symbols (symbols &body body)
  `(let (,@(mapcar
	     (lambda (sym)
	       `(,sym ',(intern (symbol-name sym) *package*)))
	     symbols))
     ,@body))
	
(defmacro aif (condition if-true &optional else)
  "Anaphoric version of aif where you can refer to the condition with the variable ~it~"
  (let ((it (intern "IT")))
    `(let ((,it ,condition))
       (if ,it
	   ,if-true
	   ,else))))

(defmacro aand (&body conditions)
  (let ((first-cond (car conditions)))
    (if first-cond 
	`(let ((it ,first-cond))
	   (and it (aand ,@(cdr conditions))))
         'it)))

(defun make-keyword (sym)
  (read-from-string (format nil ":~a" (write-to-string sym))))

(defmacro λ (args &body body)
  `(lambda ,args ,@body))

(defmacro define-error (name fields)
  `(progn
     (define-condition ,name (error)
       ,(mapcar
	 (λ (field) `(,field :initarg ,(make-keyword field) :reader ,field))
	 fields))
     (defmethod print-object ((c ,name) out)
       (print-unreadable-object (c out :type t)
	 (format out "~d" (mapcar (λ (f) `(,f ,(funcall f c))) ',fields))))))

(defun in (item list)
  "Checks if item is in list"
  (if (member item list)
      t
      nil))

(defmacro define-simple-class (name superclass fields)
  `(progn
     (defclass ,name ,superclass
       ,(mapcar (lambda (field) `(,field :accessor ,field :initform nil :initarg ,(make-keyword field))) fields))
     (dolist (field ',fields)
       (export field))))

(defmacro maphashmap (function hashmap)
  (let ((result-sym (gensym)))
    `(let ((,result-sym (list)))
       (maphash
	(lambda (key val)
	  (push (funcall ,function key val) ,result-sym))
	,hashmap)
       ,result-sym)))

(defun is-keyword (val)
  "Check if value is a keyword"
  (eq (type-of val) 'keyword))

(defun any (pred l)
  "Run a predicate over everyitem of the list. Return true if any of them is true."
  (dolist (i l)
    (when (funcall pred i)
      (return t))))

(defmacro pushback (obj place)
  "Push at the end of a list"
  `(setf ,place (append ,place (list ,obj))))
