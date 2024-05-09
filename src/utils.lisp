(in-package :celtra)


(defmacro while-read-line ((var &optional (stream *standard-input*)) &body body)
  "Macro that reads from stream into var until there is nothing left to read."
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (loop for ,var = (read-line ,s nil nil)
             while ,var
             do (progn ,@body)))))

(defmacro while (condition &body body)
  `(loop while ,condition
        do (progn ,@body)))

(defmacro do-relative-angles ((theta start-angle offset-min offset-max &optional (resolution 1)) &body body)
  (with-gensyms (angle)
    `(dotimes (,angle (1+ (* (- ,offset-max ,offset-min) (/ 1 ,resolution))))
       (let ((,theta (mod (+ (* ,angle ,resolution) ,start-angle ,offset-min) 360)))
         ,@body))))

(defmacro do-viewing-angles ((theta &optional (start-angle 0) (resolution 1)) &body body)
  (with-gensyms (angle)
    `(dotimes (,angle (1+ (* 60 (/ 1 ,resolution))))
       (let ((,theta (mod (+ (* ,angle ,resolution) -30 ,start-angle) 360)))
         ,@body))))

(defmacro doseq ((var sequence) &body body)
  `(map 'NIL #'(lambda (,var) ,@body) ,sequence))

; (defun ang- (y1 y2)
;   (let ((d (- y1 y2)))
;     (cond ((> d  180) (- y1 y2  360))
;           ((< d -180) (- y1 y2 -360))
;           (t d))))

(defun ang- (y1 y2)
  (let ((d1 (mod (- y2 y1) 360))
        (d2 (mod (- y1 y2) 360)))
    (if (< d1 d2)
      (- d1)
      d2)))

(defun ang-span (from to)
  (if (< from to)
    (- to from)
    (+ 360 (- to from))))

(defun ang+ (y1 y2)
  (mod (+ y1 y2) 360))

(defun normalize-angle (theta)
  (if (numberp theta)
    (mod theta 360)
    :NA))

(defun change-angle-resolution (theta &optional (resolution *interp-angle-resolution*))
  (if (numberp theta)
    (float (* (round (* theta (/ 1 resolution))) resolution))
    :NA))

(defun ang-to-idx (theta)
  (floor (+ (/ theta *interp-angle-resolution*) 0.001)))

(defun round-down-time (time)
  (* (floor (* time (/ 1 *interp-time-resolution*)))
     *interp-time-resolution*))

(defun array-row (arr row)
  (make-array (array-dimension arr 1) 
              :displaced-to arr 
              :displaced-index-offset (* row (array-dimension arr 1))))

(defun angle-between-p (theta alpha beta)
  "Is angle theta between [alpha, beta]"
  (if (< alpha beta)
    (<= alpha theta beta)
    (let* ((diff (- 360 alpha))
           (alpha 0)
           (beta (+ beta diff))
           (theta (mod (+ theta diff) 360)))
      (angle-between-p theta alpha beta))))


(defun downsample (source target-times)
  (loop for time in target-times
        collect (nth (round (/ (- time (t-time (first source))) *interp-time-resolution*)) source)))
