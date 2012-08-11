(defpackage :sudoku-system
  (:use :cl :asdf))

(in-package :sudoku-system)

(defsystem "sudoku"
  :version "0.1.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "GPLv3"
  :description "Sudoku web viewer"
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "board-svg" :depends-on ("packages"))
                             (:file "loader-sdm" :depends-on ("packages"))
                             (:file "board" :depends-on ("packages"))
                             (:file "sudoku" :depends-on ("board" "loader-sdm" "board-svg"))))
               (:module "static"
                :components ((:static-file "sudoku.sdm")
                             (:static-file "index.html"))))
  :depends-on ("hunchentoot" "xml-emitter"))
