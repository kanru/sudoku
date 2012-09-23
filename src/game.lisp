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
               :accessor candidates)
   (answer :initform nil
           :accessor answer)))

(defclass game ()
  ((row-max :initform 9
            :accessor row-max)
   (col-max :initform 9
            :accessor col-max)
   (cells :initform (make-array '(9 9))
          :accessor cells)))

(defmethod initialize-instance :after ((instance game) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((cells cells))
      instance
    (loop :for index :from 0 :below (array-total-size cells)
          :do (setf (row-major-aref cells index)
                    (make-instance 'cell)))))

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t :identity nil)
    (princ (candidates cell) stream)
    (when (answer cell)
      (format stream "[~a]" (answer cell)))))

(defun make-game-from-list (list)
  (let ((game (make-instance 'game)))
    (loop :for y :from 0
          :for row :in list
          :do
             (loop :for x :from 0
                   :for col :in row
                   :do (when col
                         (let ((n (- (char-code col) (char-code #\0))))
                           (setf (answer (cell (coord x y) game)) n)
                           (setf (candidates (cell (coord x y) game)) nil)
                           (cleanup (coord x y) game))))
          :finally (return game))))

(defun cell (coord game)
  (aref (cells game)
        (row coord)
        (col coord)))

(defun single-candidate-p (coord game)
  (let ((cell (cell coord game)))
    (= 1 (length (candidates cell)))))

(defun answered-p (coord game)
  (answer (cell coord game)))

(defun remove-n (n coord game)
  (let ((cell (cell coord game)))
    (setf (candidates cell)
          (remove n (candidates cell)))))

(defun mark-answer (coord game)
  (cond
    ((single-candidate-p coord game)
     (let ((cell (cell coord game)))
       (setf (answer cell) (first (candidates cell)))
       (setf (candidates cell) nil)
       (answer cell)))
    ((answer (cell coord game))
     (answer (cell coord game)))
    (t (error "Only single candidate cell could make answer."))))

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
