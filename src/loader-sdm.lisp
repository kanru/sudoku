;;;; loader-sdm.lisp --- SDM loader

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

(defvar *game*)

(defun split (line)
  (let ((cells (loop for char across line
		     collect (if (eql char #\0) nil char))))
    (loop for n from 0 to 72 by 9
	  collect (subseq cells n (+ 9 n)))))

(defun load-sdm (filename)
  (with-open-file (sdm filename)
    (loop for line = (read-line sdm nil)
	  while line
	  collect (split line))))

(defun sudoku-game (n)
  (nth n *game*))

(defmacro with-sdm-file ((filename) &body body)
  `(let ((*game* (load-sdm ,filename)))
     ,@body))

;;; loader-sdm.lisp ends here
