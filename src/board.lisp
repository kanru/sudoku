;;;; board.lisp --- Board Abstraction

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

(in-package :sudoku)

(defparameter *canvas-width*  580.0)
(defparameter *canvas-height* 580.0)
(defparameter *board-width*  550.0)
(defparameter *board-height* 550.0)
(defparameter *board-line-width*   2.0)
(defparameter *board-border-width* 5.0)
(defparameter *board-font-family* "serif")
(defparameter *board-font-size* 45)

(defun center-offset (width box-width)
  (/ (- box-width width) 2.0))

(defun board-draw-border ()
  (draw-rect 0 0
	     *board-width*
	     *board-height*
	     *board-border-width*))

(defun board-draw-cells ()
  (let* ((cell-width (/ *board-width* 9.0))
	 (first-cell cell-width)
	 (last-cell (- *board-width* cell-width)))
    (loop for x from first-cell to last-cell by cell-width
	  for i from 0
	  with y = *board-width*
	  do (draw-line x 0
			x y (if (find i '(2 5))
				*board-border-width*
				*board-line-width*)))
    (loop for y from first-cell to last-cell by cell-width
	  for i from 0
	  with x = *board-width*
	  do (draw-line 0 y
			x y (if (find i '(2 5))
				*board-border-width*
				*board-line-width*)))))

(defun board-draw (n &optional (step 0))
  (with-draw-context (*canvas-width* *canvas-height*)
    (with-offset ((center-offset *board-width* *canvas-width*)
		  (center-offset *board-height* *canvas-height*))
      (board-draw-cells)
      (board-draw-border)
      (let ((game (sudoku-game n))
	    (half-cell (/ *board-width* 18.0)))
	(loop :for row :from 0 :below (row-max game)
	      :for y :from half-cell :by (* 2 half-cell)
	      :do
		 (loop :for col :from 0 :below (col-max game)
		       :for x :from half-cell :by (* 2 half-cell)
		       :do
			  (draw-number-centered (game-value game row col) x y
						*board-font-size*
						*board-font-family*)))))))

;;; board.lisp ends here
