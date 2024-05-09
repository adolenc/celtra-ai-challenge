Celtra AI Challenge
===================

This repository contains files used to create my winning entry to the Celtra
2017 AI Challenge. The challenge was to predict the viewer orientation in a 360
degree video based on previous viewer orientation data. Using these predictions
the video was to be broken up into non-overlapping segments with different
video qualities that could be changed on the fly based on the predicted viewer
orientation.

The challenge allowed for 10 submissions to the global leaderboard in total, so
implementing good local cross-validation was crucial.

My solution used (linear) resampling, windowing, and seam-carving (dynamic
programming) to produce the final predictions. Cross validation was
successfully used to tune the parameters for submissions.

Brief description of files in the repository:

 - `challenge_description_eng.pdf` - English description of the challenge
 - `data/` - contains the input train and predict datasets along with sample submission
 - `src/` - the source code of the solution
 - `main.lisp` - the main entry point of the solution
 - `final_*` - the final leaderboard, report, and presentation (in Slovenian)

To reproduce the results, install [sbcl](https://www.sbcl.org/) and
[quicklisp](https://www.quicklisp.org/), then run:

```bash
$ sbcl --eval '(progn (push (sb-posix:getcwd) ql:*local-project-directories*) (ql:register-local-projects) (ql:quickload :celtra))' --load main.lisp
```
