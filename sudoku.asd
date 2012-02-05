(defpackage :sudoku-system
  (:use :cl :asdf))
          
(in-package :sudoku-system)

(defsystem "sudoku"
  :version "0.1.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "GPLv3"
  :description "Sudoku web viewer"
  :serial t
  :components ((:file "packages")
	       (:file "board-svg")
	       (:file "loader-sdm")
	       (:file "board")
	       (:file "sudoku")
	       (:static-file "learningcurve.sdm")
	       (:static-file "index.html"))
  :depends-on ("hunchentoot" "parenscript" "xml-emitter"))

