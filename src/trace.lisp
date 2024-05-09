(in-package :celtra)
(hu.dwim.syntax-sugar:enable-sharp-l-syntax)


(defparameter *interp-time-resolution* 1/100)
(defparameter *interp-angle-resolution* 1/1)

(defstruct (trace-point (:conc-name t-))
  angle               ; angle from data file or interpolated or NA
  (true-angle NIL)    ; always contains the actual angle (for cv)
  time
  (streams NIL))

(defmethod print-object ((point trace-point) out)
  (format out "(t=~A, a=~A)" (t-time point) (t-angle point)))


(defclass user-trace ()
  ((id :initform "" :initarg :id :accessor id)
   (data :initform #() :initarg :data :accessor data)
   (sorted-data :initform #() :initarg :sorted-data :accessor sorted-data)
   (interpolated-data :initform #() :initarg :interpolated-data :accessor interpolated-data)))

(defclass train-trace (user-trace)
  ())


(defmethod initialize-instance :after ((trace train-trace) &key)
  (with-slots (interpolated-data sorted-data) trace
    (setf interpolated-data (clean-up-trace sorted-data))))


(defclass test-trace (user-trace)
  ((prediction-start-idx :initform 0 :initarg :prediction-start-idx :accessor prediction-start-idx)))

(defmethod initialize-instance :after ((trace test-trace) &key)
  (labels ((round-down (time)
             (* (floor (* time (/ 1 *interp-time-resolution*)))
                *interp-time-resolution*))
           (round-up (time)
             (+ (round-down time) *interp-time-resolution*)))
    (with-slots (interpolated-data sorted-data prediction-start-idx) trace
      (let ((time-start (round-down (t-time (first sorted-data))))
            (time-end (round-up (t-time (first (last sorted-data)))))
            (angle-start (t-angle (first sorted-data))))
        (setf interpolated-data (clean-up-trace sorted-data
                                                :min-time time-start
                                                :max-time time-end
                                                :angle-start angle-start)
              prediction-start-idx (position :NA sorted-data :key #'t-angle))))))




(defun extend-trace (trace min-time max-time &optional (min-time-angle 0))
  (append (if (equalp (float min-time) (t-time (first trace)))
            '()
            `(,(make-trace-point :time min-time :angle min-time-angle)))
          trace
          `(,(make-trace-point :time max-time :angle (t-angle (car (last trace)))))))

(defun valid-angle-p (angle)
  (not (and (symbolp angle)
            (equal (symbol-name angle) "NA"))))

(defun ang-lerp (prev-step next-step time)
  (if (not (valid-angle-p (t-angle next-step)))
    :NA
    (let ((prev-x (t-time prev-step))
          (prev-y (t-angle prev-step))
          (next-x (t-time next-step))
          (next-y (t-angle next-step))
          (curr-x time))
      (let ((dx (- next-x prev-x))
            (dy (ang- next-y prev-y))
            (tx (- curr-x prev-x)))
        (ang+ prev-y (* dy (/ tx dx)))))))

(defun resample (trace &key (time-step *interp-time-resolution*) (max-time 60) (start-time 0))
  (cond ((> start-time max-time) '())
        ((< (t-time (cadr trace)) start-time) (resample (rest trace) :time-step time-step :max-time max-time :start-time start-time))
        (t (cons (make-trace-point :time (float start-time)
                                   :angle (normalize-angle
                                            (change-angle-resolution
                                              (ang-lerp (car trace) (cadr trace) (float start-time)))))
                 (resample trace :time-step time-step :max-time max-time :start-time (+ start-time time-step))))))

(defun clean-up-trace (trace &key (time-step *interp-time-resolution*) (min-time 0) (max-time 60) (angle-start 0))
  (-<> trace
    (extend-trace min-time max-time angle-start)
    (resample :time-step time-step :max-time max-time :start-time min-time)
    (make-array (length <>) :initial-contents <>)))

(defun read-traces (filename)
  "Read traces from csv file into a list of (:time time :angle angle), and
   group them by the id."
  (flet ((parse-line (line)
           (cons (nth 0 line)
                 (make-trace-point :time (read-from-string (nth 1 line))
                                   :angle (let ((angle (read-from-string (nth 2 line))))
                                            (if (symbolp angle)
                                              :NA
                                              angle))))))
    (group-by (read-csv filename :map-fn #'parse-line))))

(defun %load-traces (trace-type filename)
  (let ((traces (read-traces filename)))
    (mapcar #l(make-instance trace-type
                             :id (first !1)
                             :data (rest !1)
                             :sorted-data (sort (copy-seq (rest !1)) #'< :key #'t-time))
            traces)))

(defun load-test-traces (&optional (filename #P"/home/andr3/src/lisp/celtra/data/testData.csv"))
  (%load-traces 'test-trace filename))

(defun load-train-traces (&optional (filename #P"/home/andr3/src/lisp/celtra/data/learnData.csv"))
  (%load-traces 'train-trace filename))
