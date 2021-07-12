;;;; data-structures.lisp

;;; queue
(defun make-queue ()
  (let ((front-ptr nil)
	(rear-ptr nil))
    (labels ((empty-p ()
	       (null front-ptr))
	     (_insertf (item)
	       (let ((new-pair (cons item nil)))
		 (cond ((empty-p)
			(setf front-ptr new-pair)
			(setf rear-ptr new-pair))
		       (t (setf (cdr rear-ptr) new-pair)
			  (setf rear-ptr new-pair)))
		 #'_dispatch))
	     (_deletef ()
	       (cond ((empty-p) (error "DELETE! called with empty queue."))
		     (t
		      (setf front-ptr (cdr front-ptr))
		      #'_dispatch)))
	     (_front ()
	       (car front-ptr))
	     (_call-each (proc)
	       (cond ((null front-ptr) nil)
		     (t
		      (funcall proc (_front))
		      (_deletef)
		      (_call-each proc))))
	     (_dispatch (m)
	       (cond ((eq m 'insertf) #'_insertf)
		     ((eq m 'deletef) #'_deletef)
		     ((eq m 'front) #'_front)
		     ((eq m 'call-each) #'_call-each))))
      #'_dispatch)))


(defun insert-queue (item queue)
  (funcall (funcall queue 'insertf) item))

(defun delete-queue (queue)
  (funcall (funcall queue 'deletef)))

(defun front-queue (queue)
  (funcall (funcall queue 'front)))

(defun call-each-queue (proc queue)
  (funcall (funcall queue 'call-each) proc))

;;; table
