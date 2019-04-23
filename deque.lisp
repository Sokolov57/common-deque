;;;; deque.lisp

(in-package #:deque)


(defclass deque (circular-buffer)
 ((contents-size
     :type integer
     :accessor actsize
     :initarg :size
     :initform 0)))




#||


(defclass deque ()
 ((block-holder
   :type (simple-vector 128)
   :accessor home
   :initform nil)
  (front
    :type integer
    :accessor front
    :initform 0)
  (back
    :type integer
    :accessor back
    :initform 0)
  (actual-size
   :type integer
   :accessor actsize
   :initarg :size
   :initform 0)
  (volume
   :type integer
   :accessor vol
   :initform 0)
  (expand-rate
    :type float
    :reader expr
    :initform 1.5)
  (shrink-rate
     :type float
     :reader shrr
     :initform 3)))

(defmethod initialize-instance :after ((deq deque) &key)
  (let* ((s (1+ (floor (actsize deq) 128)))
         (mid (floor s 2)))
    (setf (vol deq) s (home deq) (make-array s))
    (iter (for i from 0 to (1- s))
       (setf (aref (home deq) i) (make-instance 'dblock)))
    (setf (front (aref (home deq) mid)) 63 (back (aref (home deq) mid)) 63
     (front deq) mid (back deq) mid)
   (iter (for i from 0 to (1- s))
     (setf (index (aref (home deq) i)) i))))



(defgeneric show-deque (deq)
 (:documentation "Outputs the deque, using the \[ \]"))

(defmethod show-deque ((deq deque))
 (format t "[ ")
 (iter (for blck in-vector (home deq))
  (iter (for elem in-vector (contents blck))
      (format t "~s " elem)))
 (format t "] ~%")
 (format t "Actual size ~s ~%Volume ~s ~%Front ~s ~%Back ~s ~%" (actsize deq) (vol deq) (front deq) (back deq)))

(defmethod show-debug-deque ((deq deque))
 (format t "[ ")
 (iter (for blck in-vector (home deq))
  (iter (for elem in-vector (contents blck))
      (format t "~s " elem))
  (format t "~%Local block front ~s ~%back ~s ~%index ~s ~%" (front blck) (back blck) (index blck)))
 (format t "] ~%")
 (format t "Actual size ~s ~%Volume ~s ~%Front ~s ~%Back ~s ~%" (actsize deq) (vol deq) (front deq) (back deq)))

(defclass dblock ()
 ((contents
   :type (vector * 128)
   :accessor contents
   :initform (make-array 128 :initial-element nil))
  (front
   :type integer
   :accessor front
   :initform 0
   :initarg :fr)
  (back
    :type integer
    :accessor back
    :initform 127
    :initarg :bck)
  (index
   :type fixnum
   :accessor index)))

(defun equal-division (biga smallb)
 "Takes a long segment and a shorter one, and shows the bounds after
  a short one is inserted into a long one as close as possible to the middle.
 It returns 3 values, like shown:
[     outer front bound [inner bound     ] outer back bound     ]"
 (let* ((dif (- biga smallb))
        (fpart (floor dif 2)))
  (values (1- fpart) fpart  (+ smallb fpart))))


(defun coerce-deque (seq)
 "So far coerces only list, by inserting it in the middle of smallest
  deque that is able to hold it"
  (let* ((l (length seq))
         (res (make-instance 'deque :size l)))
    (multiple-value-bind (fr start fin) (equal-division (* 128 (vol res)) (actsize res))
      (progn (iter (for i from 0 to (1- l))
              (multiple-value-bind (precolumn preplace) (floor i 128)
               (multiple-value-bind (column place) (floor (+ preplace start) 128)
                (setf (aref (contents (aref (home res) (+ column precolumn))) place) (elt seq i)))))
       (let ((mid (floor (1+ (floor (actsize res) 128)) 2)))
         (setf (front (aref (home res) mid)) 0 (back (aref (home res) mid)) 127))
       (setf (front res) 0 (back res) (1- (vol res))
             (front (aref (home res) 0)) fr
             (back (aref (home res) (1- (vol res)))) (- fin (* 128 (1- (vol res)))))
       (unless (= 0 (1- (vol res)))
        (setf (back (aref (home res) 0)) 127
              (front (aref (home res) (1- (vol res)))) 0))))
   res))

(defmethod push-front ((deq deque) newelt)
 (expand-deque deq)
 (setf (aref (contents (aref (home deq) (front deq))) (front (aref (home deq) (front deq)))) newelt)
 (incf (actsize deq))
 (if (zerop (front (aref (home deq) (front deq))))
  (incf (front deq))
  (decf (front (aref (home deq) (front deq))))))

(defmethod push-back ((deq deque) newelt)
 (expand-deque deq)
 (setf (aref (contents (aref (home deq) (back deq))) (back (aref (home deq) (back deq)))) newelt)
 (incf (actsize deq))
 (if (= 127 (back (aref (home deq) (back deq))))
  (decf (back deq))
  (incf (back (aref (home deq) (back deq))))))

(defmethod deqref ((deq deque) ind)
  (multiple-value-bind (precolumn preplace) (floor ind 128)
   (multiple-value-bind (column place) (floor (+ preplace (1+ (front (aref (home deq) (front deq))))) 128)
                                       (aref (contents (aref (home deq) (+ column precolumn))) place))))

(defmethod deque-update ((deq deque) ind newelt)
  (multiple-value-bind (precolumn preplace) (floor ind 128)
   (multiple-value-bind (column place) (floor (+ preplace (1+ (front (aref (home deq) (front deq))))) 128)
                                      (setf (aref (contents (aref (home deq) (+ column precolumn))) place) newelt))))

(defsetf deqref deque-update)

;;Something weird is going on, how do I even modify it when reallocating??

(defmethod expand-deque ((deq deque) &optional (fact 1.5))
 (when (= (* 128 (vol deq)) (actsize deq))
  (let ((newdeq (make-instance 'deque :size (ceiling (* fact (vol deq))))))
    (multiple-value-bind (newfront oldfront newback) (equal-division (vol newdeq) (vol deq))
      (iter (for i from oldfront to (1- newback))
        (setf (aref (home newdeq) i) (aref (home deq) (- i oldfront))
              (front newdeq) newfront (back newdeq) newback)))
   newdeq)))

(defmethod expand-deque ((deq deque) &optional (fact 1.5))
 (when (= (* 128 (vol deq)) (actsize deq))
  (let* ((newsize (ceiling (* fact (vol deq))))
         (newdeq (make-array (* 128 newsize))))
    (multiple-value-bind (newfront oldfront newback) (equal-division newsize (vol deq))
      (iter (for i from oldfront to (1- newback))
        (setf (aref newdeq i) (aref (home deq) (- i oldfront))))
     (iter (for i from 0 to (1- oldfront))
       (setf (aref newdeq i) (make-instance 'dblock)))
     (iter (for i from newback to (1- newsize))
           (setf (aref newdeq i) (make-instance 'dblock)))
     (setf (front deq) newfront (back deq) newback
         (vol deq) newsize (home deq) newdeq)))))

(defmethod peek-front ((deq deque))
 (let ((place (front (aref (home deq) (front deq)))))
  (if (= 127 place)
   (aref (contents (aref (home deq) (1+ (front deq)))) 0)
   (aref (contents (aref (home deq) (front deq))) (1+ place)))))

(defmethod peek-back ((deq deque))
 (let ((place (back (aref (home deq) (back deq)))))
  (if (zerop place)
   (aref (contents (aref (home deq) (1- (back deq)))) 127)
   (aref (contents (aref (home deq) (back deq))) (1- place)))))


(defmethod pop-front ((deq deque))
  (shrink-deque deq)
  (let ((place (front (aref (home deq) (front deq))))
        (ret-val))
   (if (= 127 place)
    (progn  (setf ret-val (aref (contents (aref (home deq) (1+ (front deq)))) 0)
                  (aref (contents (aref (home deq) (1+ (front deq)))) 0) nil)
       (incf (front deq)))
    (progn (setf ret-val (aref (contents (aref (home deq) (front deq))) (1+ place))
                 (aref (contents (aref (home deq) (front deq))) (1+ place)) nil)
      (incf (front (aref (home deq) (front deq))))))
   (decf (actsize deq))
   ret-val))

(defmethod pop-back ((deq deque))
 (when (> (* 128 (vol deq)) (* (shrr deq) (actsize deq)))
  (change-class deq (shrink-deque deq)))
 (let ((place (back (aref (home deq) (back deq))))
       (ret-val))
  (if (zerop place)
   (progn  (setf ret-val (aref (contents (aref (home deq) (1- (back deq)))) 127)
                 (aref (contents (aref (home deq) (1- (back deq)))) 127) nil)
      (decf (back deq)))
   (progn (setf ret-val (aref (contents (aref (home deq) (back deq))) (1- place))
                (aref (contents (aref (home deq) (back deq))) (1- place)) nil)
     (decf (back (aref (home deq) (back deq))))))
  (decf (actsize deq))
  ret-val))

(defmethod shrink-deque ((deq deque))
    (multiple-value-bind (newfront oldfront newback) (equal-division (floor (vol deq) (ceiling (sqrt (shrr deq)))) (1+ (- (back deq) (front deq))))
     (declare (ignore newfront))
     (let ((newdeq (make-instance 'deque :size (* 128 (floor (vol deq) (ceiling (sqrt (shrr deq))))))))
         (iter (for i from (front deq) to (back deq))
               (for j from oldfront to (1- newback))
           (setf (aref (home newdeq) j) (aref (home deq) i)
                 (front newdeq) oldfront (back newdeq) (1- newback)))
       newdeq)))
||#
