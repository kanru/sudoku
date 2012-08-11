;;;; sudoku.lisp --- Sudoku Renderer

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

(defvar *game-source* "sudoku.sdm")
(defvar *index-file* "index.html")

(defvar *static-directory* (make-pathname
                            :directory
                            (append (pathname-directory
                                     #.(or *load-truename* *compile-file-truename*))
                                    '(:up "static"))))

(defvar *server*)

(define-easy-handler (index-html :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (handle-static-file (merge-pathnames *index-file* *static-directory* )))

(define-easy-handler (svg :uri "/board.svg") (level)
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (with-sdm-file ((merge-pathnames *game-source* *static-directory*))
    (with-output-to-string (*standard-output*)
      (let ((level (parse-integer (or level "0"))))
	(board-draw level)))))

(defun server-start ()
  (setf *server* (make-instance 'easy-acceptor :port 4242))
  (start *server*))

(defun server-stop ()
  (stop *server* :soft t))

;;; sudoku.lisp ends here
