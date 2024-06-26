(in-package :celtra)


(defun move-probability (relative-angle &optional (std (sqrt 80)))
  (let* ((mean 0)
         (2var (* 2 std std)))
    (* (/ 1 (sqrt (* pi 2var)))
       (exp (- (/ (expt (- relative-angle mean) 2)
                  2var))))))

(defun seam-carving (heatmap &optional (max-angle-offset 4) (move-probability #'move-probability))
  (destructuring-bind (n-times n-angles) (array-dimensions heatmap)
    (let ((scores (make-array (array-dimensions heatmap) :initial-element 0d0 :element-type 'double-float))
          (directions (make-array (array-dimensions heatmap) :element-type 'integer)))
      (declare (type (simple-array double-float (6001 360)) scores))
      (declare (type (simple-array integer (6001 360)) directions))
      (dotimes (time (1- n-times))
        (let ((current-time (1- (- n-times time 1))))
          (dotimes (current-angle n-angles)
            (do-relative-angles (theta (* current-angle *interp-angle-resolution*) (- max-angle-offset) max-angle-offset *interp-angle-resolution*)
              (let ((new-score (* (aref scores (1+ current-time) (ang-to-idx theta))
                                  (funcall move-probability (ang- current-angle theta)))))
                ; (format t "~A:~A[~A], " (ang- current-angle theta) new-score (aref scores (1+ current-time) (ang-to-idx theta)))
                (when (> new-score (aref scores current-time current-angle))
                  (setf (aref scores current-time current-angle) new-score 
                        (aref directions current-time current-angle) (ang-to-idx theta)))))
            ; (format t "~%")
            (setf (aref scores current-time current-angle)
                  (/ (+ (aref scores current-time current-angle)
                        (aref heatmap current-time current-angle))
                     86))))) ;; just normalize by some number so that we dont get too big
      (values directions scores))))

(defun traceback (directions start-time start-theta)
  (let ((start-time (floor (/ start-time *interp-time-resolution*)))
        (start-theta (floor (/ start-theta *interp-angle-resolution*))))
    (loop for current-time from start-time below (first (array-dimensions directions))
          with current-theta = start-theta
          do (setf current-theta (aref directions current-time current-theta))
          collect (make-trace-point :angle (* (aref directions current-time current-theta) *interp-angle-resolution*)
                                    :time (float (* current-time *interp-time-resolution*))))))
