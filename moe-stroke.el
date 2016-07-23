;;; moe-stroke.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;;;;;;;(defconst moe-stroke-directory (file-name-directory load-file-name))

(setq moe-stroke-directory (file-name-directory (buffer-file-name)))
(setq moe-stroke-xml-directory (concat moe-stroke-directory "zh-stroke-data/utf8/"))
(setq moe-stroke-json-directory (concat moe-stroke-directory "zh-stroke-data/json/"))
(setq moe-stroke-buffer-name "*moe-stroke*")

;; ======================================================
;; Get Data
;; ======================================================

(defun moe-stroke-dec-to-hex (decimal-number)
  (format "%x" decimal-number))

(defun moe-stroke-get-file-path (character)
  "CHARACTER is a Chinese STRING, not CHAR type in Emacs Lisp"
  (let ((hex-string (moe-stroke-dec-to-hex (string-to-char character))))
    (concat moe-stroke-json-directory hex-string ".json")))

(defun moe-stroke-get-raw-tracks (character)
  "Return a list. <ex>
 ([((y . 216) (x . 703))                   ; A sub-track (sub-stroke)
   ((size . 85) (y . 688) (x . 792))]      ; A full track done (stroke #1).
  [((y . 527) (x . 436))                   ; A sub-track (sub-stroke)
   ((size . 95) (y . 416) (x . 956))] ...) ; A full track done (stroke #2)"
  (mapcar (lambda (x) (cdr (car x)))
        (json-read-file (moe-stroke-get-file-path character))))

(defun moe-stroke-get-stroke-data (character)
  "Return a list. <ex>
 (((703 . 216) (792 . 688) (300 . 500))   ; (sub-stroke sub-stroke sub-stroke) ; (stroke #1)
  ((436 . 527) (956 . 416))               ; (sub-stroke sub-stroke) ; (stroke #2)
  ((1082 . 459) (1615 . 372))             ; (sub-stroke sub-stroke) ; (stroke #3)
 ... )
"
  (mapcar (lambda (stroke)
            (map 'list (lambda (sub-stroke)
                         (let ((x (cdr (assq 'x sub-stroke)))
                               (y (cdr (assq 'y sub-stroke))))
                           (cons x y)))
                 stroke))
          (moe-stroke-get-raw-tracks character)))

;; ======================================================
;; Canvas
;; ======================================================
(defun moe-stroke-create-buffer-if-not-exist ()
  (if (not (get-buffer moe-stroke-buffer-name))
      (with-temp-buffer-window moe-stroke-buffer-name t nil)))

(defun moe-stroke-get-canvas-size ()
  (moe-stroke-create-buffer-if-not-exist)
  (with-current-buffer moe-stroke-buffer-name
    (let* ((size (min (/ (window-width) 2)
                      (window-height)))
           (x (* size 2))
           (y size))
      (setq-local moe-stroke-canvas-width x)
      (setq-local moe-stroke-canvas-height y)
      (cons x y))))

(defun moe-stroke-init-canvas ()
  "Fill canvas buffer with spaces"
  (let* ((xy (moe-stroke-get-canvas-size))
         (x (car xy))
         (y (cdr xy)))
    (with-current-buffer moe-stroke-buffer-name
      (let (buffer-read-only)
        (delete-region (point-min) (point-max))
        (loop repeat y do
              (insert (make-string x (string-to-char " ")) "\n"))))))

(defmacro ttt ()
  `(cons (line-number-at-pos (point))
         (current-column)
         ))

(defun moe-stroke-draw-line (raw-p1 raw-p2)
  (with-current-buffer moe-stroke-buffer-name
    (let* ((buffer-read-only nil)
           (m (moe-stroke-get-slope-rate raw-p1 raw-p2))
           (canvas-size (moe-stroke-get-canvas-size))
           (canvas-height (cdr canvas-size))
           (canvas-height-zoom-rate (/ canvas-height 2050.0))
           ;;(raw-b (moe-stroke-get-raw-b raw-p1x raw-p1y m))
           ;; (b (floor (* raw-b canvas-height-zoom-rate))) ; `b' means y = ax + b
           (p1 (moe-stroke-calculate-xy-on-canvas raw-p1 moe-stroke-canvas-width moe-stroke-canvas-height))
           (p2 (moe-stroke-calculate-xy-on-canvas raw-p2 moe-stroke-canvas-width moe-stroke-canvas-height))
           (p1/x (car p1))
           (p2/x (car p2))
           (p1/y (cdr p1))
           (p2/y (cdr p2))
           (b (moe-stroke-get-b p1/x p1/y m))
           )
      (loop for x from p1/x to p2/x do
            (let* ((y (moe-stroke-get-y m x b))
                   (original-char (progn (goto-char (point-min)) (ttt)
                                         (forward-line y) (ttt)
                                         (forward-char x) (ttt)
                                         (char-to-string (char-before (point)))))
                   (new-char (moe-stroke-get-pixel-char original-char m)))
              (delete-char 1)
              (insert new-char)
              (sit-for 0.1)
              )))))

(defun moe-stroke-get-slope-rate (p1 p2)
  "Get slope rate of two points. <ex>
P1              P2
'(703 . 216)    '(792 . 688)"
  (let ((x1 (float (car p1)))
        (y1 (float (cdr p1)))
        (x2 (float (car p2)))
        (y2 (float (cdr p2))))
    (cond ((eq y1 y2) 0)         ; horizontal
          ((eq x1 x2) 50)        ;'vertical
          (t (/ (- y1 y2) (- x2 x1))))))

(defun moe-stroke-get-raw-b (raw-x raw-y m)
  "y = mx + b
   b = y - mx
But the base point of stroke data is left-top, so Y := -Y
   b = (-y) - mx"
  (- (- raw-y)
     (* m raw-x)))

(defun moe-stroke-get-y (m x b)
  "y = mx + b,
But the base point of stroke data is left-top, so Y := -Y
   y = -1 * (mx + b)"
  (floor (- (+ (* m x) b))))


(defun moe-stroke-get-pixel-char (original-char m)
  "回傳位置應該放啥char，
X Y is counted from 1,
M is slope rate."
  ;; Available chars:
  ;;  - / \ | +
  (cond ((eq original-char " ")
         (cond ((> (abs m) 2)      '|)
               ((> m 1)            '/)
               ((< (abs m) 1)      '-)
               ((< m  -1)          '\\)
               (t                  '*)))
        ((memq original-char '("|" "/" "\\"))
         (if (< (abs m) 1) "+" original-char))
        (t   ;; sym is - or *
         (if (< (abs m) 1) original-char "+"))))

(defun moe-stroke-calculate-xy-on-canvas (raw-xy canvas-width canvas-height)
  "Example:
RAW-XY       '(703 . 216)
percents     '(0.3429268292682927 . 0.10536585365853658)
CANVAS-SIZE  '(100 . 50)
XY           '(34  . 5)

2050 is the max value of raw data
"
  (let ((percents (cons (/ (car raw-xy) 2050.0)
                        (/ (cdr raw-xy) 2050.0))))
    (cons (round (* canvas-width (car percents)))
          (round (* canvas-height (cdr percents))))))
(progn
  (moe-stroke-init-canvas)
  (mapc (lambda (stroke-points)
          (loop for i from 0 to (- (length stroke-points) 2)
                with p1 = (nth i stroke-points)
                with p2 = (nth (1+ i) stroke-points)
                do (moe-stroke-draw-line p1 p2)))
        (moe-stroke-get-stroke-data "萌")
        ))


(provide 'moe-stroke)
;;; stroke.el ends here
