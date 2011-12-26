;;;; board-canvas.lisp --- Board HTML5 Canvas Implementation

;;; Copyright (C) 2011  Kan-Ru Chen

;;; Author: Kan-Ru Chen <kanru@kanru.info>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:

;;; 

;;;; Code:

(defpackage :sudoku
  (:use :cl :parenscript))

(in-package :sudoku)

(defmacro with-draw-context ((width height) &body body)
  `(progn
     (print (who-ps-html (:canvas :width ,width :height ,height :id "canvas")))
     (print "<script>")
     (print (ps (setf canvas (chain document (getElementById "canvas")))))
     (print (ps (setf context (chain canvas (getContext "2d")))))
     ,@body
     (print "</script>")))

(defmacro with-offset ((tx ty) &body body)
  `(progn
     (print (ps (chain context (translate ,tx ,ty))))
     ,@body))

;;; board-canvas.lisp ends here
