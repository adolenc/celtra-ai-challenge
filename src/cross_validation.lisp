(in-package :celtra)
(hu.dwim.syntax-sugar:enable-sharp-l-syntax)


(defun avg-prediction-quality (score &key (predictions 28211) (known 28047) (ignore-known T))
  (let ((max-quality 100)
        (angles-to-check 61))
    (float
      (if ignore-known
        (/ (- score (* known max-quality angles-to-check)) predictions angles-to-check)
        (/ score (+ predictions known) angles-to-check)))))

(defun all-viewing-angles-covered-p (test-traces)
  (dolist (trace test-traces)
    (doseq (pt (data trace))
      (dotimes (theta 360)
        (if (notany #l(angle-in-stream-p !1 theta) (t-streams pt))
          (error "NOT ALL VIEWING ANGLES COVERED LOL")))))
  T)

(defun doesnt-exceed-bandwidth (test-traces &optional (max-bandwidth 100000))
  (dolist (trace test-traces)
    (doseq (pt (data trace))
      (let ((current-bandwidth (reduce #'+ (mapcar #'bandwidth (t-streams pt)))))
        (if (> current-bandwidth max-bandwidth)
          (error "BANDWIDTH EXCEEDED")))))
  T)

(defun validate-predictions (test-traces)
  (and (doesnt-exceed-bandwidth test-traces)
       (all-viewing-angles-covered-p test-traces)))


(defun write-predictions (output-file streams test-traces)
  (with-open-file (out output-file :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (dolist (stream streams)
      (with-slots (id alpha beta quality) stream
        (format out "~A,~A,~A,~A~%" id alpha beta quality)))
    (format out "~%")
    (dolist (trace test-traces)
      (doseq (pt (data trace))
        (format out "~{~A~^,~}~%" (mapcar #'id (t-streams pt)))))))

(defun split-to-train-test (set test-set-size)
  (let ((test-traces (subseq set 0 test-set-size))
        (train-traces (subseq set test-set-size)))
    (values train-traces (break-up-test-set test-traces))))

(defun insert-fake-NAs (data)
  (let ((data-length (length data)))
    (loop for i below data-length
          do (let ((current-pt (nth i data)))
               (setf (t-true-angle current-pt) (t-angle current-pt))
               (if (> i (floor (/ data-length 2)))
                 (setf (t-angle current-pt) :NA)))))
  data)

(defun break-up-test-set (set)
  ; split up individual traces to 8 second long traces
  ; replace true-angle for each trace with values of angles
  ; replace last 4 seconds of each trace with NA angles
  (loop for trace in set
        append
        (let ((total-trace-length (length (data trace))))
          (loop for start from 0 below (- total-trace-length 70) by 10
                collect
                (let ((data (insert-fake-NAs
                              (mapcar #'copy-trace-point
                                      (subseq (data trace)
                                              start
                                              (min (+ start 80)
                                                   total-trace-length)))))
                      (new-id (gensym "CV-TRACE-")))
                  (make-instance 'test-trace
                                 :id new-id
                                 :data data
                                 :sorted-data (sort (copy-seq data) #'< :key #'t-time)))))))

(defun cv-score (test-set)
  (let ((score 0)
        (n-predictions 0)
        (n-known 0))
    (dolist (trace test-set)
      (incf n-known (prediction-start-idx trace))
      (incf n-predictions (- (length (data trace)) (prediction-start-idx trace)))
      (doseq (pt (data trace))
        (let ((true-angle (t-true-angle pt))
              (used-streams (t-streams pt)))
          (do-viewing-angles (theta true-angle)
            (incf score (find-used-quality theta used-streams))))))
    (values score n-known n-predictions)))


(defun cv-split (&key filename (test-set-size 5))
  (let ((train-set (shuffle (load-train-traces filename))))
    (split-to-train-test train-set test-set-size)))
     

(defun cross-validate (angle-strategy stream-strategy train-filename &optional (cv-test-set-size 10))
  (multiple-value-bind (train validate) (cv-split :filename train-filename :test-set-size cv-test-set-size)
    (funcall angle-strategy train validate)
    (funcall stream-strategy train validate)
    (multiple-value-bind (score n-known n-predictions) (cv-score validate)
      (values (avg-prediction-quality score :known n-known :predictions n-predictions :ignore-known T)
              (avg-prediction-quality score :known n-known :predictions n-predictions :ignore-known NIL)))))

(defun avg-angle-prediction-offset (strategy train-filename &optional (cv-test-set-size 10))
  (multiple-value-bind (train validate) (cv-split :filename train-filename :test-set-size cv-test-set-size)
    (funcall strategy train validate)
    (dotimes (time 35)
      (print (float (/ (loop for trace in validate
                             sum (let ((offset (+ time (prediction-start-idx trace))))
                                   (abs (ang- (t-angle (nth offset (sorted-data trace)))
                                              (t-true-angle (nth offset (sorted-data trace)))))))
                       (length validate)))))))

