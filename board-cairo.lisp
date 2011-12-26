;;;; board-cairo.lisp --- Board Cairo Implementation

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
  (:use :cl :cairo))

(in-package :sudoku)

(defmacro with-draw-context ((width height) &body body)
  `(progn ,@body))

(defmacro with-offset ((x-offset y-offset) &body body)
  `(progn
     (save)
     (translate ,x-offset ,y-offset)
     ,@body
     (restore)))

(defun draw-rect (x y width height &optional (line-width 1))
  (save)
  (set-line-width line-width)
  (rectangle x y width height)
  (stroke)
  (restore))

(defun draw-line (x1 y1 x2 y2 &optional (width 1))
  (save)
  (set-line-width width)
  (move-to x1 y1)
  (line-to x2 y2)
  (stroke)
  (restore))

(defun draw-number-centered (num x y &optional (size 45) (family "serif"))
  (when num
    (save)
    (select-font-face family :normal :normal)
    (set-font-size size)
    (let* ((text (format nil "~A" num))
	   (extents (get-text-extents text))
	   (width (text-width extents))
	   (height (text-height extents))
	   (x-bearing (text-x-bearing extents))
	   (y-bearing (text-y-bearing extents))
	   (x-offset (- x x-bearing (/ width 2)))
	   (y-offset (- y y-bearing (/ height 2))))
      (move-to x-offset y-offset)
      (text-path text)
      (fill-path))
    (restore)))

;; (sudoku::with-sdm-file ("learningcurve.sdm")
;; 	   (cairo:with-png-file ("test.png" :rgb24 700 700)
;; 	     (cairo:set-source-rgb 1 1 1)
;; 	     (cairo:rectangle 0 0 700 700)
;; 	     (cairo:fill-path)
;; 	     (cairo:set-source-rgb 0 0 0)
;; 	     (sudoku::board-draw 23)))

;;; board-cairo.lisp ends here
