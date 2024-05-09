(asdf:defsystem #:celtra
  :description "My entries for celtra competition"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-csv
               #:group-by
               #:arrow-macros
               #:alexandria
               #:opticl
               #:hu.dwim.syntax-sugar/lambda-with-bang-args)
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/trace")
               (:file "src/stream")
               (:file "src/windows")
               (:file "src/heatmap")
               (:file "src/seam_carving")
               (:file "src/cross_validation")
               (:file "src/strategies"))
  :in-order-to ((test-op (test-op celtra-tests))))
