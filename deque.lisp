;;;; deque.lisp

(in-package #:deque)


(defclass deque (circular-buffer)
  ((contents-size
    :type integer
    :accessor contsize
    :initarg :size
    :initform 0)))

(defclass dblock ()
  ((block-contents
    :type (vector * 128)
    :accessor bl-contents
    :initform (make-array 128 :initial-element nil))
   (block-front
    :type integer
    :accessor bl-front
    :initform 0
    :initarg :fr)
   (block-back
    :type integer
    :accessor bl-back
    :initform 127
    :initarg :bck)
   (block-index
    :type fixnum
    :accessor bl-index
    :initarg :ind)))

(defgeneric dqlength (deq)
  (:documentation "Returns length; static"))

(defgeneric dqemptyp (deq)
  (:documentation "T if empty, nil otherwise; static"))

(defgeneric dq-coerce (vector-sequence)
  (:documentation "Coerces vector into deque and returns that deque"))

(defgeneric peek-front (deq)
  (:documentation "Returns first element; static"))

(defgeneric peek-back (deq)
  (:documentation "Returns last element; static"))

(defgeneric dqref (deq index)
  (:documentation "Returns element by index, counting from the start; static"))

(defgeneric print-readable-deque (deq &optional start end)
  (:documentation "Prints deque fromt start to end in readable form, enclosed in []"))

(defgeneric push-front (deq newelem)
  (:documentation "Pushes newelem to the front of deque; returns new length"))

(defgeneric push-back (deq newelem)
  (:documentation "Pushes newelem to the back of deque; returns new length"))

(defgeneric pop-front (deq)
  (:documentation "Pops first element of the deque and returns it;
                  does not delete it; works only if deque is not empty"))

(defgeneric pop-back (deq)
  (:documentation "Pops last element of the deque and returns it;
                  does not delete it; works only if deque is not empty"))

;;;; ========================================= Implementation =========================================

(defmethod initialize-instance :after ((deq deque) &key)
  (iter (for i from 0 to (1- (length (home deq))))
	(setf (aref (home deq) i) (make-instance 'dblock :ind i))))

(defmethod dqshow ((deq deque))
  "Prints contetns slot; not for users"
  (format t "#DQ( ")
  (iter (for dblck in-vector (home deq))
	(iter (for elem in-vector (bl-contents dblck))
	      (format t "~s " elem))
	(format t "|| "))
  (format t ")~%"))

(defmethod info :after ((deq deque))
  "Prints other slots; not for users"
  (format t "Contents size ~s ~%" (contsize deq)))

(defmethod dq-print ((deq deque))
  (dqshow deq)
  (info deq))

(defmethod peek-front ((deq deque))
  (with-slots (contents front) deq
    (with-slots (block-contents block-front) (aref contents front)
      (aref block-contents block-front))))

(defmethod peek-back ((deq deque))
  (with-slots (contents back) deq
    (with-slots (block-contents block-back) (aref contents back)
      (aref block-contents block-back))))

(defmethod dqref ((deq deque) ind)
  (with-slots (contents front) deq
    (with-slots (block-front) (aref contents front)
      (multiple-value-bind (column place) (floor (+ block-front ind) 128)
	(with-slots (block-contents) (cbref deq column)
          (aref block-contents place))))))

(defmethod update-deque-place ((deq deque) ind newelem)
  "Updates element on given place by newvalue; for setf, not for users"
  (if (<= ind (1- (contsize deq)))
      (with-slots (contents front) deq
	(with-slots (block-contents block-front) (aref contents front)
          (multiple-value-bind (column place) (floor (+ block-front ind) 128)
            (setf (aref (cbref deq column) place) newelem))))))

(defsetf dqref update-deque-place)

(defmethod dqemptyp ((deq deque))
  (zerop (contsize deq)))

(defmethod dqlength ((deq deque))
  (contsize deq))

(defmethod dq-coerce (seq)
  (let* ((block-amount (1+ (floor (length seq) 128)))
         (res (make-instance 'deque :size block-amount)))
    (with-slots (contents front back actual-size contents-size) res
      (iter (for i from 0 to (1- (length seq)))
            (multiple-value-bind (m n) (floor i 128)
              (setf (aref (bl-contents (aref contents m)) n) (aref seq i)
                    (bl-front (aref contents m)) 0
                    (bl-back (aref contents m)) (if (< m (floor (1- (length seq)) 128)) 127 n))))
      (setf front 0 back (floor (1- (length seq)) 128)
            actual-size (1+ (floor (1- (length seq)) 128))
            contents-size (length seq)))
    res))

(defmethod print-readable-deque ((deq deque) &optional (start 0) (end nil))
  (with-slots (contents front contents-size) deq
    (let ((step start) (last (or end contents-size)))
      (format t "[")
      (iter (for i from step to (1- last))
            (format t "~s" (dqref deq i))
            (unless (= (1- last) i) (format t " ")))
      (format t "]"))))


(defmethod shape-buffer ((deq deque) newsize default-element)
  (call-next-method)
  (with-slots (contents front back) deq
    (unless (= (mod (1+ back) (length contents)) front)
      (circle-iterate (mod (1+ back) (length contents)) (mod (1- front) (length contents)) contents
                      (setf (bl-index (aref contents i)) i)))))

(defmethod push-front ((deq deque) newelem)
  (with-slots (contents front actual-size contents-size) deq
    (with-slots (block-contents block-front block-index) (aref contents front)
      (if (zerop block-front)
          (progn (when (= actual-size (length contents)) (extend-buffer deq 2 (lambda () (make-instance 'dblock))))
		 (setf front (mod (1- front) (length contents)))
		 (incf actual-size)
		 (with-slots (block-contents block-front) (aref contents front)
                   (setf (aref block-contents 127) newelem block-front 127)))
          (progn (decf block-front)
		 (setf (aref block-contents block-front) newelem)))
      (incf contents-size))))

(defmethod push-back ((deq deque) newelem)
  (with-slots (contents back actual-size contents-size) deq
    (with-slots (block-contents block-back block-index) (aref contents back)
      (if (= 127 block-back)
          (progn (when (= actual-size (length contents)) (extend-buffer deq 2 (lambda () (make-instance 'dblock))))
		 (setf back (mod (1+ back) (length contents)))
		 (incf actual-size)
		 (with-slots (block-contents block-back) (aref contents back)
                   (setf (aref block-contents 0) newelem block-back 0)))
          (progn (incf block-back)
		 (setf (aref block-contents block-back) newelem)))
      (incf contents-size))))


(defmethod pop-back ((deq deque))
  (with-slots (contents back actual-size contents-size) deq
    (unless (zerop contents-size)
      (with-slots (block-contents block-back) (aref contents back)
	(let ((return-value (aref block-contents block-back)))
	  (decf contents-size)
	  (if (zerop block-back)
              (progn (incf block-back 127)
                     (setf back (mod (1- back) (length contents)))
                     (decf actual-size)
                     (when (> (length contents) (* 3 actual-size))
                       (shrink-buffer deq (lambda () (make-instance 'dblock)))))
              (decf block-back))
	  return-value)))))

(defmethod pop-front ((deq deque))
  (with-slots (contents front actual-size contents-size) deq
    (unless (zerop contents-size)
      (with-slots (block-contents block-front) (aref contents front)
	(let ((return-value (aref block-contents block-front)))
	  (decf contents-size)
	  (if (= 127 block-front)
              (progn (decf block-front 127)
                     (setf front (mod (1+ front) (length contents)))
                     (decf actual-size)
                     (when (> (length contents) (* 3 actual-size))
                       (shrink-buffer deq (lambda () (make-instance 'dblock)))))
              (incf block-front))
	  return-value)))))
