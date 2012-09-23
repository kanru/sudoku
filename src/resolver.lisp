;;;; resolver.lisp --- Dumb Sudoku Resolver

;;; Copyright (C) 2012  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(in-package :sudoku)

;;; Dumb Algorithm
;;; 1. Find next slot that has only one candidate N
;;; 1.1 If couldn't find any one candidate slot [then use a smarter
;;;     algorithm]
;;; 1.2 [D] If all slots have values filled in, we have found the solution
;;; 2. [D] Fill the slot with N
;;; 3. Remove N from other slots on the same row and same column
;;; 4. Remove N from other slots in the same block
;;; 5. Remove N from other slots in the same diagonal
;;; 6. Goto 1

;;; Utilities
;;; 1. Loop through the same row
;;; 1.1 Generate coords for the same row
;;; 2. Loop through the same column
;;; 2.1 Generate coords for the same column
;;; 3. Loop through the same block
;;; 3.1 Generate coords for the same block!
;;; 4. Loop through the same diagonal
;;; 4.1 Generate coords for the same diagonal!
;;; 5. Loop through the entire board

(defun row (coord)
  "Return the ROW of COORD."
  (car coord))

(defun col (coord)
  "Return the COLUMN of COORD."
  (cdr coord))

(defun coord (row column)
  "Return a new coordinate at ROW and COLUMN"
  (cons row column))

(defun coords-in-same-row (coord)
  "Return list of coordinates in the same row of COORD."
  (let ((max 9))
    (loop :for i :from 0 :below max
          :collect (coord (row coord) i))))

(defun coords-in-same-col (coord)
  "Return list of coordinates in the same column of COORD."
  (let ((max 9))
    (loop :for i :from 0 :below max
          :collect (coord i (col coord)))))

(defun block-start (coord)
  (flet ((start-of (n)
           (ecase n
             ((0 1 2) 0)
             ((3 4 5) 3)
             ((6 7 8) 6))))
    (coord (start-of (row coord))
           (start-of (col coord)))))

(defun coords-in-same-block (coord)
  (let ((top-left (block-start coord))
        (max 3))
    (loop :for i :from (row top-left) :below (+ (row top-left) max)
          :append (loop :for j :from (col top-left) :below (+ (col top-left) max)
                        :collect (coord i j)))))

(defun coords-in-board ()
  (let ((max 9))
    (loop :for i :from 0 :below max
          :append (loop :for j :from 0 :below max
                        :collect (coord i j)))))

(defun find-next-single-candidate-slot (board)
  "Find next slot with only one candidate in BOARD.
Return a coordinate in (ROW . COLUMN) pair. NIL if none is found.
Secound value is returned to indicate the game is end or not."
  (let ((coord (find-if #'(lambda (coord)
                            (single-candidate-p coord board))
                        (coords-in-board))))
    (if coord
        (values coord nil)
        (values nil (game-finished-p board)))))

(defun game-finished-p (board)
  (every #'(lambda (coord)
             (answered-p coord board))
         (coords-in-board)))

(defun remove-n-in-coords (n coords board)
  (mapc #'(lambda (coord)
            (remove-n n coord board))
        coords))

(defun remove-n-in-same-row (n coord board)
  (remove-n-in-coords n (coords-in-same-row coord) board))

(defun remove-n-in-same-col (n coord board)
  (remove-n-in-coords n (coords-in-same-col coord) board))

(defun remove-n-in-same-block (n coord board)
  (remove-n-in-coords n (coords-in-same-block coord) board))

(defun cleanup (coord board)
  (when coord
    (let ((n (mark-answer coord board)))
      (remove-n-in-same-row n coord board)
      (remove-n-in-same-col n coord board)
      (remove-n-in-same-block n coord board))))

(defun resolve-1 (board)
  (multiple-value-bind (coord endp)
      (find-next-single-candidate-slot board)
    (cleanup coord board)
    endp))

(defun resolve (board)
  (let ((endp (resolve-1 board)))
    (unless endp
      (resolve board))))

;;; resolver.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
