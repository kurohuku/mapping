(defvar *mapping-action-keywords*
  `(:collect :collect-if
     :remove :remove-if
     :funcall
     :action))

(defvar *mapping-action-optional-keywords*
  `(:if :key :reverse))

(defvar *mapping-action-optional-sub-keywords*
  `(:else))

(defun action-keyword-p (sym)
  (member sym *mapping-action-keywords*))
(defun action-optional-keyword-p (sym)
  (member sym *mapping-action-optional-keywords*))
(defun action-optional-sub-keyword-p (sym)
  (member sym *mapping-action-optional-sub-keywords*))


(defun parse-actions (actions acc)
  (cond
    ((null actions) (nreverse acc))
    ((atom actions) (error "Invalid action form"))
    (T
     (unless (action-keyword-p (car actions))
       (error "Invalid action form"))
     (let ((rest (cdr actions)))
       (let ((act
	      (cons (car actions)
		    (loop
		       :for exp = (car rest)
		       :until (or (null exp) (action-keyword-p exp))
		       :do (pop rest)
		       :collect exp))))
	 (parse-actions rest (cons act acc)))))))

(defun mapping-expander (actions expr)
  (if (null actions)
      expr
      (let ((action (car actions))
	    (rest (cdr actions)))
	(case (car action)
	  (:collect
	      (mapping-expander rest (collect-expander action expr)))
	  (:collect-if
	   (mapping-expander rest (collect-if-expander action expr)))
	  (:remove
	   (mapping-expander rest (remove-expander action expr)))
	  (:remove-if
	   (mapping-expander rest (remove-if-expander action expr)))
	  (:funcall
	   (mapping-expander rest (funcall-expander action expr)))
	  (:action
	   (mapping-expander rest (action-expander action expr)))
	  (error "Invalid mapping action keyword")))))

(defun collect-expander (action expr)
  (let ((sym1 (gensym))
	(sym2 (gensym)))
    (destructuring-bind (obj &key if key &allow-other-keys) (cdr action)
      `(remove ,obj ,expr
	       ,@(if key `(:key ,key) nil)
	       ,@(if if `(:test ,if) nil)
	       :test-not (lambda (,sym1 ,sym2) (eq ,sym1 ,sym2))))))

(defun collect-if-expander (action expr)
  (destructuring-bind (fn &key if key &allow-other-keys) (cdr action)
    `(remove-if-not ,fn ,expr
		    ,@(if key `(:key ,key) nil))))

(defun remove-expander (action expr)
  (destructuring-bind (obj &key if key &allow-other-keys) (cdr action)
    `(remove ,obj ,expr
	     ,@(if key `(:key ,key) nil)
	     ,@(if if `(:test ,if) nil))))

(defun remove-if-expander (action expr)
  (destructuring-bind (fn &key if key &allow-other-keys) (cdr action)
    `(remove-if ,fn ,expr
		,@(if key `(:key ,key)))))

(defun funcall-expander (action expr)
  `(funcall ,(second action) ,expr))

(defun action-expander (action expr)
  (let ((sym (gensym)))
  (destructuring-bind (fn &key if key &allow-other-keys) (cdr action)
    (cond
      ((and if key)
       `(mapcar
	 (lambda (,sym)
	   (if (funcall ,if (funcall ,key ,sym))
	       (funcall ,fn (funcall ,key ,sym))
	       ,sym))
	 ,expr))
      (if
       `(mapcar
	 (lambda (,sym)
	   (if (funcall ,if ,sym)
	       (funcall ,fn ,sym)
	       ,sym))
	 ,expr))
      (key
       `(mapcar
	 (lambda (,sym)
	   (funcall ,fn (funcall ,key ,sym)))
	 ,expr))
      (T `(mapcar ,fn ,expr))))))


(defmacro mapping (lst &rest actions)
  (mapping-expander (parse-actions actions nil) lst))

;; (parse-actions '(:collect #'oddp :action #'1+ :key #'car) nil)
