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
(defun make-table ()
  (let ((local-table (list '*table*)))
    (labels ((_insertf (keys value)
	       (let ((record (assoc (car keys) (cdr local-table))))
		   (if record
		       (setf (cdr record) value)
		       (setf (cdr local-table) (cons (cons keys value)
						     (cdr local-table))))))
	     (_dispatch (m)
	       (cond ((eq m 'insert) #'_insertf))))
      #'_dispatch)))
	       
  
(defun make-table (&key (name nil))
  (let ((local-table (list name)))
    (labels ((__insertf-kv (key value table &key (proc #'cons))
	       (let ((record (assoc key (cdr table))))
		 (if record
		     (setf (cdr record) (funcall proc value (cons (cdr record) nil)))
		     (setf (cdr table) (cons (cons key value) (cdr table))))
		 table))
	     (__insertf-multiple-key (keys value &optional (table local-table) (old-keys nil))
	       (print local-table)
	       (cond ((null keys) table)
		     ((null (cdr keys))
		      (__insertf-kv (car keys) value table)
		      (__insertf-multiple-key (cdr keys) value table (cons (car keys) old-keys)))
		     (t (let ((sub-table (assoc (car keys) (cdr table))))
			  (cond (sub-table
				 (__insertf-multiple-key (cdr keys) value sub-table (cons (car keys) old-keys)))
				(t
				 (let ((s-table (list (car keys))))
				   (setf (cdr table) (cons s-table (cdr table)))
				   (__insertf-multiple-key (cdr keys) value s-table (cons (car keys) old-keys)))))))))

	     (_insert (keys value)
	       (__insertf-multiple-key keys value)
	       #'_dispatch)
	     (_lookup (keys &optional (table local-table))
	       (cond ((null keys) (cdr table))
		     (t (let ((r (assoc (car keys) (cdr table))))
			  (cond (r (_lookup (cdr keys) r))
				(t nil))))))
	     (_dispatch (m)
	       (cond ((eq m 'insert) #'_insert)
		     ((eq m 'lookup) #'_lookup))))
      #'_dispatch)))




      ;; (__insertf-multiple-key '(2 1 0 3) '(c d h s))
      ;; (__insertf-multiple-key '(2 1 0 1) '(d d h d))
      ;; (__insertf-multiple-key '(1 2 0 1) '(d d h d))
      ;; (__insertf-multiple-key '(1 1 0 1) '(d d h d))
      ;; (__insertf-multiple-key '(0 1 0 1) '(h d h d))
      ;; (__insertf-multiple-key '(1 0 0 1) '(d h h d))
      ;; (__insertf-multiple-key '(3 1 0 1) '(s d h d))
      ;; (__insertf-multiple-key '(2 2 2 1) '(c c c c))
      ;; (__insertf-multiple-key '(2 2 2 1) '(c c c d))
	     
      ;; (_lookup '(2 2 2 1))

