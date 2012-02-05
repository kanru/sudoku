;;;; board-svg.lisp --- Board SVG Implementation

;;; Copyright (C) 2011  Kan-Ru Chen

;;; Author: Kan-Ru Chen  <kanru@kanru.info>

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

(in-package :sudoku)

(defparameter +svg-1.1-namespace+ "http://www.w3.org/2000/svg")
(defparameter +svg-1.1-doctype+ "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
         \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")

(defmacro with-svg-output ((stream width height) &body body)
  `(with-xml-output (,stream)
     (princ +svg-1.1-doctype+)
     (with-tag ("svg" `(("width" ,,width)
			("height" ,,height)) +svg-1.1-namespace+)
       ,@body)))

(defmacro with-draw-context ((width height) &body body)
  `(progn
     (with-svg-output (*standard-output* ,width ,height)
       ,@body)))

(defmacro with-offset ((tx ty) &body body)
  `(progn
     (with-tag ("g" `(("transform" ,(format nil "translate(~A,~A)" ,tx ,ty))))
       ,@body)))

(defun draw-rect (x y width height &optional (line-width 1))
  (with-simple-tag ("rect"
		    `(("x" ,x)
		      ("y" ,y)
		      ("width" ,width)
		      ("height" ,height)
		      ("style" ,(format
				 nil "fill:none;stroke:black;stroke-width:~A"
				 line-width))))))

(defun draw-line (x1 y1 x2 y2 &optional (width 1))
  (with-simple-tag ("line"
		    `(("x1" ,x1)
		      ("x2" ,x2)
		      ("y1" ,y1)
		      ("y2" ,y2)
		      ("style" ,(format
				 nil "stroke:black;stroke-width:~A"
				 width))))))

(defun draw-number-centered (num x y &optional (size 45) (family "serif"))
  (when num
    (let ((style "text-anchor:middle;dominant-baseline:middle;"))
      (with-simple-tag ("text"
			`(("x" ,x)
			  ("y" ,y)
			  ("font-size" ,size)
			  ("font-family" ,family)
			  ("style" ,style)))
	(xml-out (format nil "~A" num))))))

;;; board-svg.lisp ends here
