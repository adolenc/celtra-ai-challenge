(in-package :celtra)


(defclass video-stream ()
  ((id :initform (string (gensym "s")) :initarg id :accessor id)
   (alpha :initform 0 :initarg :alpha :accessor alpha)
   (beta :initform 359 :initarg :beta :accessor beta)
   (quality :initform 1 :initarg :quality :accessor quality)))

(defmethod print-object ((stream video-stream) out)
  (print-unreadable-object (stream out :type t)
    (with-slots (id alpha beta quality) stream
      (format out "~S: (~A, ~A, ~A%)" id alpha beta quality))))


(defmethod angle-in-stream-p ((stream video-stream) angle)
  (with-slots (alpha beta) stream
    (angle-between-p (mod angle 360) alpha beta)))

(defun find-used-quality (theta streams)
  (loop for stream in streams
        when (angle-in-stream-p stream theta)
        maximizing (quality stream)))

(defmethod bandwidth ((stream video-stream))
  (with-slots (alpha beta quality) stream
    (* quality (1+ (ang-span alpha beta)))))
