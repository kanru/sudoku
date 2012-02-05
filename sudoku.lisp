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
(defvar *load-path* (list *default-pathname-defaults*))

(defvar *server*)

(defun locate-source (source)
  (loop for path in *load-path*
        when (probe-file (merge-pathnames source path))
          return it))

(define-easy-handler (index-html :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (handle-static-file "index.html"))

(define-easy-handler (svg :uri "/board.svg") (level)
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (with-sdm-file (*game-source*)
    (with-output-to-string (*standard-output*)
      (let ((level (parse-integer (or level "0"))))
	(board-draw level)))))

(defun server-start ()
  (setf *server* (make-instance 'easy-acceptor :port 4242))
  (start *server*))

(defun server-stop ()
  (stop *server* :soft t))

;;; sudoku.lisp ends here
