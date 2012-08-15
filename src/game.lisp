;;;; game.lisp --- Game Definition

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

(deftype candidates () '(integer 1 9))

(defclass cell ()
  ((candidates :initform (list 1 2 3 4 5 6 7 8 9)
               :accessor candidates)))

(defclass game ()
  ((row-max :initform 9
            :accessor row-max)
   (col-max :initform 9
            :accessor col-max)
   (cells :initform (make-array '(9 9) :element-type 'cell)
          :accessor cells)))

(defmethod initialize-instance :after ((instance game) &rest initargs)
  (with-accessors ((cells cells))
      instance
    (loop :for index :from 0 :below (array-total-size cells)
          :do (setf (row-major-aref cells index)
                    (make-instance 'cell)))))

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t :identity t)
    (princ (slot-value cell 'candidates))))

(defmethod cell-value ((cell cell))
  (with-accessors ((candidates candidates))
      cell
    (when (endp (cdr candidates))
      (car candidates))))

(defmethod (setf cell-value) (value (cell cell))
  (check-type value candidates)
  (with-accessors ((candidates candidates))
      cell
    (setf candidates (list value))))

(defmethod cell-candi ((cell cell))
  (with-accessors ((candidates candidates))
      cell
    candidates))

(defmethod cell-strike ((cell cell) (candidate candidates))
  (with-accessors ((candidates candidates))
      cell
    (setf candidates (remove candidate candidates))))

(defmethod copy-cell ((cell cell))
  (let ((new (make-instance 'cell)))
    (setf (slot-value new 'candidates)
          (slot-value cell 'candidates))))

(defmethod game-candi ((game game) y x)
  (candidates (aref (cells game) y x)))

(defmethod (setf game-candi) (value (game game) y x)
  (setf (candidates (aref (cells game) y x)) value))

(defmethod game-value ((game game) y x)
  (value (aref (cells game) y x)))

(defmethod (setf game-value) (value (game game) y x)
  (setf (value (aref (cells game) y x)) value)
  (setf (candidates (aref (cells game) y x)) nil))

(defun make-game-from-list (list)
  (let ((game (make-instance 'game)))
    (loop :for y :from 0
          :for row :in list
          :do
             (loop :for x :from 0
                   :for col :in row
                   :do (setf (game-value game y x) col))
          :finally (return game))))

;;; Sudoku Resolver

(defun copy-game (game)
  (let ((new-game (make-instance 'game)))
    (loop :for row :from 0 :below (row-max game)
          :do (loop :for col :from 0 :below (col-max game)
                    :do (setf (game-value new-game row col)
                              (game-value game row col))
                        (setf (game-candi new-game row col)
                              (game-candi game row col))))))

(defun resolver-identity (game)
  (copy-game game))

(defun strike (game row col value)
  (loop :for row :from 0 :below (row-max game)
        :do (setf (game-candi game row col)
                  (remove value (game-candi game row col))))
  (loop :for col :from 0 :below (col-max game)
        :do (setf (game-candi game row col)
                  (remove value (game-candi game row col)))))

(defun resolver-strike-row (game)
  (let ((new-game (copy-game game)))
    (loop :for row :from 0 :below (row-max game)
          :do (loop :for col :from 0 :below (col-max game)
                    :do (when (game-value game row col)
                          (strike game row col
                                  (game-value game row col)))))))

;;; SDM Loader

(defun split (line)
  (let ((cells (loop for char across line
		     collect (if (eql char #\0) nil char))))
    (loop for n from 0 to 72 by 9
	  collect (subseq cells n (+ 9 n)))))

(defun load-sdm (filename)
  (with-open-file (sdm filename)
    (loop :for line = (read-line sdm nil)
	  :while line
          :collect (make-game-from-list (split line)))))

(defvar *game*)

(defun sudoku-game (n)
  (nth n *game*))

(defmacro with-sdm-file ((filename) &body body)
  `(let ((*game* (load-sdm ,filename)))
     ,@body))

;;; game.lisp ends here
