;;;; circular-buffer.lisp

(in-package #:deque)


(defclass circular-buffer ()
  ((contents
    :type (vector *)
    :initform nil
    :accessor home)
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
    :initform 0
    :initarg :size)))

(defmethod initialize-instance :after ((cirb circular-buffer)&key)
  (setf (home cirb) (make-array (max 2 (actsize cirb)) :initial-element nil) (actsize cirb) 0))

(defmethod cb-print ((cirb circular-buffer))
  (cbshow cirb)
  (info cirb))

(defmethod cbshow ((cirb circular-buffer))
  (format t "#CB( ")
  (iter (for elem in-vector (home cirb))
	(format t "~s " elem))
  (format t ")~%"))

(defmethod cb-coerce (seq)
  (let* ((len (length seq))
         (res (make-instance 'circular-buffer :size (1+ len))))
    (setf (front res) (1- len) (back res) 0 (actsize res) len)
    (iter (for i from 0 to (1- len))
	  (setf (aref (home res) i) (elt seq i)))
    res))


(defgeneric info (obj)
  (:documentation "Shows all the slots, except for contents themselves"))

(defmethod info ((cirb circular-buffer))
  (format t "Front: ~s ~%Back: ~s ~%Actual-size ~s ~%" (front cirb) (back cirb) (actsize cirb)))

(defmethod cbref ((cirb circular-buffer) index)
  (with-slots (contents front actual-size) cirb
    (if (<= index (1- actual-size))
	(aref contents (mod (+ front index) (length contents))))))

(defmethod update-circular-buffer-place ((cirb circular-buffer) index newelem)
  (with-slots (contents front actual-size) cirb
    (if (<= index (1- actual-size))
	(setf (aref contents (mod (+ front index) (length contents))) newelem))))

(defsetf cbref update-circular-buffer-place)

(defmethod cbemptyp ((cirb circular-buffer))
  (zerop (actsize cirb)))

(defmethod cblength ((cirb circular-buffer))
  (actsize cirb))

(defgeneric ensure-capacity (cirb)
  (:documentation "Does the reallocation"))


(defmethod push-front ((cirb circular-buffer) newelem)
  (with-slots (contents front actual-size) cirb
    (when (= (length contents) actual-size) (extend-buffer cirb))
    (setf front (mod (1- front) (length contents))
          (aref contents front) newelem)
    (incf actual-size)))

(defmethod push-back ((cirb circular-buffer) newelem)
  (with-slots (contents back actual-size) cirb
    (when (= (length contents) actual-size) (extend-buffer cirb))
    (setf back (mod (1+ back) (length contents))
          (aref contents back) newelem)
    (incf actual-size)))

(defmethod pop-front ((cirb circular-buffer))
  (with-slots (contents actual-size front) cirb
    (unless (zerop actual-size)
      (when (> (length contents) (* 3 actual-size))
	(shrink-buffer cirb))
      (prog1 (aref contents front)
        (setf front (mod (1+ front) (length contents)))
        (decf actual-size)))))

(defmethod pop-back ((cirb circular-buffer))
  (with-slots (contents actual-size back) cirb
    (unless (zerop actual-size)
      (when (> (length contents) (* 3 actual-size))
	(shrink-buffer cirb))
      (prog1 (aref contents back)
        (setf back (mod (1- back) (length contents)))
        (decf actual-size)))))

(defmethod peek-front ((cirb circular-buffer))
  (with-slots (contents front) cirb
    (aref contents front)))

(defmethod peek-back ((cirb circular-buffer))
  (with-slots (contents back) cirb
    (aref contents back)))


(defmacro circle-iterate (front back contents &rest body)
  `(let* ((len (length ,contents))
          (first ,front)
          (last ,back))
     (if (<= first last)
	 (iter (for i from first to last)
               (for element = (aref ,contents i))
               ,@body)
	 (progn (iter (for i from first to (1- len))
                      (for element = (aref ,contents i))
		      ,@body)
		(iter (for i from 0 to last)
                      (for element = (aref ,contents i))
		      ,@body)))))



(defmethod shape-buffer ((cirb circular-buffer) newsize default-element)
  (with-slots (contents front back) cirb
    (let ((newhome (make-array newsize :initial-element nil))
          (j 0))
      (circle-iterate front back contents (setf (aref newhome j) element) (incf j))
      (setf front 0 back (1- j))
      (unless (= (1+ back) newsize)
        (circle-iterate (mod (1+ back) newsize) (mod (1- front) newsize) newhome
			(setf (aref newhome i) (and default-element (funcall default-element)))))
      (setf contents newhome))))


(defgeneric extend-buffer (cirb &optional fact with-default)
  (:documentation "Reallocates buffer, making it bigger. Triggers when actual size is equal
to vector size, extends by factor given"))

(defmethod extend-buffer ((cirb circular-buffer) &optional (fact 2) (with-default nil))
  (with-slots (actual-size) cirb
    (shape-buffer cirb (ceiling (* fact actual-size)) with-default)))

(defgeneric shrink-buffer (cirb &optional with-default)
  (:documentation "Reallocates buffer, making it smaller. Triggers when actual size is less
then 1/3, shrink to the exact size"))

(defmethod shrink-buffer ((cirb circular-buffer) &optional (with-default nil))
  (with-slots (actual-size) cirb
    (shape-buffer cirb (* 2 actual-size) with-default)))
