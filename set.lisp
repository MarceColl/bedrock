(in-package :bedrock)

(defclass hashset ()
  ((table :accessor table
	  :initform (make-hash-table :test 'equal))))

(defmethod print-object ((set hashset) out)
  (print-unreadable-object (set out :type t)
    (format out "~a items" (length (set-items set)))))

(defun make-hash-set ()
  (make-instance 'hashset))

(defmethod set-contains ((set hashset) item)
  (let ((val (gethash item (table set))))
    (if val t nil)))

(defmethod set-items ((set hashset))
  (alexandria:hash-table-keys (table set)))

(defmethod set-add ((set hashset) item)
  (setf (gethash item (table set)) t)
  set)

(defmethod set-diff ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-instance 'hashset)))
    (loop for val in (set-items set-a)
	  when (not (set-contains set-b val))
	  do (set-add out-set val))
    out-set))

(defmethod set-join ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-instance 'hashset)))
    (loop for val in (set-items set-a)
	  do (set-add out-set val))
    (loop for val in (set-items set-b)
	  do (set-add out-set val))
    out-set))

(defmethod set-intersect ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-instance 'hashset)))
    (loop for val in (set-items set-a)
	  when (set-contains set-b val)
	    do (set-add out-set val))
    out-set))
