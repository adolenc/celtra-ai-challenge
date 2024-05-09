(in-package :celtra)


(defun rectangle-window (theta &optional (min-angle -30) (max-angle 30))
  (if (<= min-angle theta max-angle) 1 0))

(defun kronecker-delta-window (theta &optional (min-angle -30) (max-angle 30))
  (declare (ignore min-angle max-angle))
  (if (= theta 0) 1 0))

(defun triangle-window (theta &optional (min-angle -30) (max-angle 30))
  (/ (if (< theta 0)
       (max (- theta min-angle) 0)
       (max (- max-angle theta) 0))
     30.0))

(defun welch-window (theta &optional (min-angle -30) (max-angle 30))
  (if (< min-angle theta max-angle)
    (let ((N-1/2 (/ (1- (- max-angle min-angle)) 2)))
      (- 1 (expt (/ (- theta N-1/2 -30) N-1/2) 2)))
    0))
