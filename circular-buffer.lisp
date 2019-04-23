;;;; circular-buffer.lisp

(in-package #:deque)

(defclass circular-buffer ()
 ((contents
   :type (simple-vector *)
   :initform nil
   :accessor home)
  (front
     :type integer
     :accessor front
     :initform 0)
  (back
     :type integer
     :accessor back
     :initform 1)
  (actual-size
     :type integer
     :accessor actsize
     :initform 0
     :initarg :size)))

(defmethod initialize-instance :after ((cirb circular-buffer)&key)
 (setf (home cirb) (make-array (max 2 (actsize cirb)) :initial-element nil) (actsize cirb) 0))

(defmethod cbshow ((cirb circular-buffer))
 (format t "Contents: ~s ~%Front: ~s ~%Back: ~s ~%Actual-size ~s ~%" (home cirb) (front cirb) (back cirb) (actsize cirb)))

(defmethod cbref ((cirb circular-buffer) index)
 (let ((displaced-index (+ (front cirb) index)))
  (if (> displaced-index (back cirb))
     (aref (home cirb) (- displaced-index (length (home cirb))))
     (aref (home cirb) displaced-index))))

(defmethod (setf cbref) ((cirb circular-buffer) index newelem)
 (let ((displaced-index (+ (front cirb) index)))
  (if (> displaced-index (back cirb))
    (setf (aref (home cirb) (- displaced-index (length (home cirb)))) newelem)
    (setf (aref (home cirb) displaced-index) newelem))))

(defgeneric ensure-capacity (cirb)
 (:documentation "Does the reallocation"))

(defmethod push-elem ((cirb circular-buffer) newelem &key direction)
  (when (= (length (home cirb)) (actsize cirb)) (extend-buffer cirb))
  (multiple-value-bind (reader writer offset)
      (ecase direction
        (front (values #'front #'(setf front) -1))
        (back (values #'back #'(setf back) +1)))
   (setf (aref (home cirb) (funcall reader cirb)) newelem)
   (funcall writer (mod (+ (funcall reader cirb) offset) (length (home cirb))) cirb))
 (incf (actsize cirb)))

(defmethod pop-elem ((cirb circular-buffer) &key direction)
 (unless (zerop (actsize cirb))
;;;;shrink probably
  (when (> (length (home cirb)) (* 3 (actsize cirb)))
   (shrink-buffer cirb))
  (multiple-value-bind (reader writer offset)
      (ecase direction
        (front (values #'front #'(setf front) +1))
        (back (values #'back #'(setf back) -1)))
   (funcall writer (mod (+ (funcall reader cirb) offset) (length (home cirb))) cirb)
   (prog1 (aref (home cirb) (funcall reader cirb))
    (setf (aref (home cirb) (funcall reader cirb)) nil) (decf (actsize cirb))))))


(defmethod push-front ((cirb circular-buffer) newelem)
 (push-elem cirb newelem :direction 'front))

(defmethod push-back ((cirb circular-buffer) newelem)
 (push-elem cirb newelem :direction 'back))

(defmethod peek-front ((cirb circular-buffer))
 (aref (home cirb) (mod (1+ (front cirb)) (length (home cirb)))))

(defmethod peek-back ((cirb circular-buffer))
 (aref (home cirb) (mod (1- (back cirb)) (length (home cirb)))))

(defmethod pop-front ((cirb circular-buffer))
 (pop-elem cirb :direction 'front))

(defmethod pop-back ((cirb circular-buffer))
 (pop-elem cirb :direction 'back))



(defmacro iterate-elements (cirb &rest body)
 `(let ((first (mod (1+ (front ,cirb)) (length (home ,cirb))))
        (last (mod (1- (back ,cirb)) (length (home ,cirb)))))
   (if (<= first last)
      (iter (for i from first to last)
            (for element = (aref (home ,cirb) i))
        ,@body)
      (progn (iter (for i from first to (1- (length (home ,cirb))))
                   (for element = (aref (home ,cirb) i))
              ,@body)
        (iter (for i from 0 to last)
              (for element = (aref (home ,cirb) i))
         ,@body)))))


(defmethod shape-buffer ((cirb circular-buffer) newsize)
 (let ((newhome (make-array newsize :initial-element nil))
       (j 0))
  (iterate-elements cirb (setf (aref newhome j) element j (1+ j)))
  (setf (front cirb) (1- newsize) (back cirb) j
   (home cirb) newhome)))

(defgeneric extend-buffer (cirb &optional fact)
 (:documentation "Reallocates buffer, making it bigger. Triggers when actual size is equal
to vector size, extends by factor given"))

(defmethod extend-buffer ((cirb circular-buffer) &optional (fact 1.5))
 (shape-buffer cirb (ceiling (* fact (actsize cirb)))))

(defgeneric shrink-buffer (cirb)
 (:documentation "Reallocates buffer, making it smaller. Triggers when actual size is less
then 1/3, shrink to the exact size"))

(defmethod shrink-buffer ((cirb circular-buffer))
 (shape-buffer cirb (actsize cirb)))
