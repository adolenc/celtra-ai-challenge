(in-package :celtra)
(hu.dwim.syntax-sugar:enable-sharp-l-syntax)


(defparameter *stream-dummy* (make-instance 'video-stream :alpha 0 :beta 359 :quality 1))
(defparameter *streams-100* 
  (let ((stream-size 4))
    (loop for theta below 360 by stream-size
          collect (make-instance 'video-stream
                                 :alpha theta
                                 :beta (mod (+ -1 theta stream-size) 360)
                                 :quality 100))))
(defparameter *streams-90* 
  (let ((stream-size 12))
    (loop for theta below 360 by stream-size
          collect (make-instance 'video-stream
                                 :alpha theta
                                 :beta (mod (+ -1 theta stream-size) 360)
                                 :quality 90))))
(defparameter *streams-80* 
  (let ((stream-size 12))
    (loop for theta below 360 by stream-size
          collect (make-instance 'video-stream
                                 :alpha theta
                                 :beta (mod (+ -1 theta stream-size) 360)
                                 :quality 80))))

(defparameter *streams-60* 
  (let ((stream-size 12))
    (loop for theta below 360 by stream-size
          collect (make-instance 'video-stream
                                 :alpha theta
                                 :beta (mod (+ -1 theta stream-size) 360)
                                 :quality 60))))

(defun use-up-most-bandwidth (theta &optional (streams *streams-100*) (bandwidth (- 10000 360)))
  (let* ((opt-index (position-if #l(angle-in-stream-p !1 theta) streams))
         (stream-size (1+ (ang- (beta (first streams)) (alpha (first streams)))))
         (stream-quality (quality (first streams)))
         (number-of-streams (floor (/ bandwidth (* stream-quality stream-size))))
         (starting-side (if (< (ang- theta (alpha (nth opt-index streams)))
                               (ang- (beta (nth opt-index streams)) theta))
                          0 1))
         (next-indices (loop for i from 1 upto number-of-streams
                             collect (* (1- (* (mod (+ i starting-side) 2) 2))
                                        (floor (/ i 2))))))
    (mapcar #l(nth (mod (+ opt-index !1) (length streams)) streams) next-indices)))

(defun streams-for-known-angles (theta streams)
  (list* *stream-dummy* (use-up-most-bandwidth theta streams)))

(defun fill-zero-angles (train-set test-set)
  (declare (ignore train-set))
  (dolist (trace test-set)
    (doseq (pt (subseq (sorted-data trace) (prediction-start-idx trace)))
      (setf (t-angle pt) 20))))

(defun fill-last-known-angles (train-set test-set)
  (declare (ignore train-set))
  (dolist (trace test-set)
    (doseq (pt (subseq (sorted-data trace) (prediction-start-idx trace)))
      (setf (t-angle pt) (t-angle (nth (1- (prediction-start-idx trace)) (sorted-data trace)))))))

(defun fill-seam-carve-angles (train-set test-set)
  (declare (ignore train-set))
  (dolist (trace test-set)
    (let* ((last-known-idx (1- (prediction-start-idx trace)))
           (last-known-time (t-time (nth last-known-idx (sorted-data trace))))
           (last-known-angle (t-angle (nth last-known-idx (sorted-data trace))))
           (opt-path (traceback *dp* (round-down-time last-known-time) last-known-angle))
           (new-trace (downsample opt-path (mapcar #'t-time (subseq (sorted-data trace) last-known-idx)))))
      (map NIL #'(lambda (target prediction)
                   (setf (t-angle target) (t-angle prediction)))
           (subseq (sorted-data trace) last-known-idx)
           new-trace))))

(defun fill-streams (train-set test-set)
  (declare (ignore train-set))
  (dolist (trace test-set)
    (dotimes (time (length (sorted-data trace)))
      (let ((pt (nth time (sorted-data trace)))
            (prediction-idx (prediction-start-idx trace)))
        (cond ((< time (+ prediction-idx 25))
               (setf (t-streams pt) (streams-for-known-angles (t-angle pt) *streams-100*)))
              ((< time (+ prediction-idx 30))
               (setf (t-streams pt) (streams-for-known-angles (t-angle pt) *streams-100*)))
              (t
               (setf (t-streams pt) (streams-for-known-angles (t-angle pt) *streams-100*))))))))

