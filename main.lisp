(ql:quickload :celtra)
(in-package :celtra)
(hu.dwim.syntax-sugar:enable-sharp-l-syntax)


(defvar *here* #.(directory-namestring (or *compile-file-pathname* *load-pathname* *default-pathname-defaults*)))

(defparameter *streams* (append (list *stream-dummy*)
                                *streams-100*))

(defparameter *train-data-filename* (merge-pathnames "data/learnData.csv" *here*))
(defparameter *test-data-filename* (merge-pathnames "data/testData.csv" *here*))

(defparameter *test-traces* (load-test-traces *test-data-filename*))
(defparameter *train-traces* (load-train-traces *train-data-filename*))
; (defparameter *heatmap* (traces->heatmap (append *train-traces* *test-traces*) #'welch-window))
(defparameter *heatmap* (traces->heatmap *train-traces* #'welch-window))
(defparameter *dp* (seam-carving *heatmap* 4 #l(move-probability !1 (sqrt 75))))

(draw-heatmap (merge-pathnames "mp.png" *here*) *heatmap*
              (loop for theta below 360 by 30
                    append (loop for time below 30 by 1
                                 collect (subseq (traceback *dp* time theta) 0 40))))




(defun k-cv (k method)
  (/ (loop for ck below k
           sum (let ((cv (cross-validate method #'fill-streams *train-data-filename* 20)))
                 (print (cons ck cv)) 
                 cv))
     10.0))

; (avg-angle-prediction-offset #'fill-last-known-angles *train-data-filename*)
; (avg-angle-prediction-offset #'fill-seam-carve-angles *train-data-filename*)
; (avg-angle-prediction-offset #'fill-zero-angles *train-data-filename*)
(k-cv 10 #'fill-seam-carve-angles)
; (k-cv 10 #'fill-last-known-angles)
; (k-cv 10 #'fill-zero-angles)

(defun predict (train-set test-set filename)
  (fill-seam-carve-angles train-set test-set)
  (fill-streams train-set test-set)
  (print (validate-predictions test-set))
  (write-predictions filename *streams* test-set))

(predict *train-traces* *test-traces* (merge-pathnames "submission.txt" *here*))
