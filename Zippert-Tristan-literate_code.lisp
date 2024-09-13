(defclass listwrapper()
  (
   (elements :initarg :elements :initform 'nil :accessor elements)
   (len :initform 0 :accessor len :initarg len)
   (table :initform (make-hash-table) :accessor table)
   )
  )

(defmethod initialize-instance :after ((self ListWrapper)&key)
  (with-slots (elements) self
    (if elements
	(setf (len self) (length (elements self)))
	)
    )
  )

(defmethod my-append ((self ListWrapper) (other ListWrapper))
  (with-slots (elements len) self
    (setf elements (append elements (elements other)))
    (incf len (len other))
    )
  )

(defmethod power-jump ((self ListWrapper) n (d ListWrapper) &optional (jmp-list '()))
  "Optimal-jump algorithm but within a recursive object"
  (declare (type integer n))
  (declare (type ListWrapper d))
  (with-slots (elements len) self
    (cond
      ((= n 0) (make-instance 'ListWrapper :elements (reverse (elements self))))
      ((null (elements d)) nil)
      ((< n 0) nil)
      (t
       (let ((output nil)
             (new-n nil))
         (dolist (elem (elements d))
           (when (<= elem n)
             (setf new-n (- n elem))
             (let ((result (power-jump self new-n d (cons elem jmp-list))))
               (when result
                 (setq output (my-append (make-instance 'listwrapper :elements result))))))
           )
	 output))
      ))))


(defvar start-list (make-instance 'ListWrapper :elements '(1 2 3) ))
(defvar pogo-list (make-instance 'ListWrapper :elements '(1 2 3 4 5) ))

;; Call the power-jump method
(let ((result (power-jump start-list 5 pogo-list)))
  (if result
      (format t "Possible jump sequences: ~a~%" (elements result))
      (format t "No valid jump sequences found.~%")))

;; [[file:code.org::optimal-jump][optimal-jump]]
(let((ls make-cache))
  (defun optimal-jump(n d &optional (jmp-lst '()))
    "Optimal-jump algorithm returns a list of all possible iterations of the pogo-list,d, to get to the goal n"
    (declare (type integer n))
    (declare (type list d jmp-lst))
    (let((output nil)(new-n nil))
      (cond
       ((= n 0) (list (reverse jmp-lst))) 
       ((null d) nil)
       ((< n 0) nil)
       (t
	(dolist (elem d)
	  (when (<= elem n)
	    (setf new-n (- n elem))
	    (setq output
		  (append output (optimal-jump new-n d
					       (cons elem jmp-lst)))))
	  )
	output))))
  )
;; optimal-jump ends here

;; [[file:code.org::optimal-jump-results][optimal-jump-results]]
(format t "~s" (optimal-jump 5 '(5 10 1 3)))
;; optimal-jump-results ends here

;; [[file:code.org::*IO interaction code][IO interaction code:1]]
(defun input-from-user()
  (let ((inp '()) (check nil) (result nil))
    (setq check (uiop:split-string (read-line) :separator " "))
    (if check
	(progn
	  (dolist (elem check)
	    (push (parse-integer elem :junk-allowed t) inp)
	    )
	  (setq inp (reverse inp))
	  (setq result (optimal-jump (car inp) (cdr inp)))
	  (if result
	      (dolist (elem result)
		(format t "~s~%" elem)
		)
	      (format t "~s~%" nil)
	      )
	  )
	)
    )
  )
;; IO interaction code:1 ends here

;; [[file:code.org::optimal-jump-test][optimal-jump-test]]
(time (optimal-jump 5 '(1 2 3 4 5)))
;; optimal-jump-test ends here

(input-from-user)
