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


(defmethod initialize-instance :after ((deq deque) &key)
  (iter (for i from 0 to (1- (length (home deq))))
    (setf (aref (home deq) i) (make-instance 'dblock :ind i))))

(defmethod dqshow ((deq deque))
  (format t "#DQ( ")
  (iter (for dblck in-vector (home deq))
    (iter (for elem in-vector (bl-contents dblck))
      (format t "~s " elem))
    (format t "|| "))
  (format t ")~%"))

(defmethod info :after ((deq deque))
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
    (with-slots (block-contents block-front) (aref contents front)
      (multiple-value-bind (column place) (floor (+ block-front ind) 128)
       (aref (cbref deq column) place)))))

(defmethod update-deque-place ((deq deque) ind newelem)
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



;;;;------------------------------------------------------------------------------------
;;;;------------------------------------------------------------------------------------
;;;;------------------------------------------------------------------------------------
;;;;------------------------------------------------------------------------------------
(defun make-num-vec (size)
 (let ((res (make-array size :element-type 'fixnum)))
  (dotimes (i size) (setf (aref res i) i))
  res))



(defun vector-test (upto)
  (let ((vec (make-array 0 :adjustable t :element-type 'fixnum :fill-pointer 0)))
    (declare (type (vector fixnum *) vec))
    (loop :for i from 0 to upto
     do (progn (loop :for j from i downto (floor i 2)
                do (unless (emptyp vec) (vector-pop vec)))
         (loop :for j from (floor i 2) to i
          do (vector-push-extend i vec))
         (vector-push-extend i vec))))
  t)

(defun cb-test (upto)
  (let ((vec (make-instance 'circular-buffer :size 1)))
    (loop :for i from 0 to upto
     do (progn (loop :for j from i downto (floor i 2)
                do (unless (cbemptyp vec) (pop-back vec)))
         (loop :for j from (floor i 2) to i
          do (push-front vec i))
         (push-back vec i))))
  t)
