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
(setq moe-stroke-json-directory (concat moe-stroke-directory "stroke-data-json/"))
(setq moe-stroke-buffer-name "*moe-stroke*")
(setq moe-stroke-char "█")
;; ======================================================
;; Faces
;; ======================================================

(defface moe-stroke-background
  '((((class color)) (:foreground "#ffffff" :background "#ffffff")))
  "This comment is necessary")

(defface moe-stroke-d1
  '((((class color)) (:foreground "#c6c6c6" :background "##c6c6c6")))
  "This comment is necessary")

(defface moe-stroke-d2
  '((((class color)) (:foreground "#949494" :background "##949494")))
  "This comment is necessary")

'((((class color)) (:foreground "#626262" :background "##626262")))
(defface moe-stroke-d3
  "This comment is necessary")

(defface moe-stroke-d4
  '((((class color)) (:foreground "#3a3a3a" :background "##3a3a3a")))
  "This comment is necessary")

(defface moe-stroke-d5
  '((((class color)) (:foreground "#080808" :background "##080808")))
  "This comment is necessary")

;; ======================================================
;; Get Data
;; ======================================================

(defun moe-stroke-dec-to-hex (decimal-number)
  (format "%x" decimal-number))

(defun moe-stroke-get-file-path (character)
  "CHARACTER is a Chinese STRING, not CHAR type in Emacs Lisp"
  (let ((hex-string (moe-stroke-dec-to-hex (string-to-char character))))
    (concat moe-stroke-json-directory hex-string ".json")))

(defun moe-stroke-get-raw-data (character)
  "Return a list. <ex>
 ([((y . 216) (x . 703))                   ; A sub-track (sub-stroke)
   ((size . 85) (y . 688) (x . 792))]      ; A full track done (stroke #1).
  [((y . 527) (x . 436))                   ; A sub-track (sub-stroke)
   ((size . 95) (y . 416) (x . 956))] ...) ; A full track done (stroke #2)"
  (mapcar (lambda (x) (cdr (car x)))
          (json-read-file (moe-stroke-get-file-path character))))

(defun moe-stroke-get-raw-strokes-data (character)
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
          (moe-stroke-get-raw-data character)))

(defun moe-stroke-get-scaled-strokes-data (character)
  (let ((canvas-size (moe-stroke-get-canvas-size)))
    (mapcar (lambda (raw-stroke)
              (mapcar (lambda (raw-xy)
                        (moe-stroke-calculate-xy-on-canvas raw-xy (car canvas-size) (cdr canvas-size)))
                      raw-stroke))
            (moe-stroke-get-raw-strokes-data character))))

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

(defun moe-stroke-reset-canvas ()
  "Fill canvas buffer with spaces"
  (let* ((xy (moe-stroke-get-canvas-size))
         (x (car xy))
         (y (cdr xy)))
    (with-current-buffer
        moe-stroke-buffer-name
      (let (buffer-read-only)
        (delete-region (point-min) (point-max))
        (loop repeat y do
              (insert (propertize (make-string x (string-to-char moe-stroke-char))
                                  'face 'moe-stroke-background) "\n"))))))


;; ======================================================
;; Xiaolin Wu's Algorithm
;; ======================================================


(defun moe-stroke-plot (x y brightness)
  (with-current-buffer moe-stroke-buffer-name
    (let (buffer-read-only)
      (goto-char (point-min))
      (forward-line (1- y))
      (forward-char x)
      (delete-char 1)
      (insert (cond
               ((<= brightness 0.2) (propertize moe-stroke-char 'face 'moe-stroke-d1))
               ((<= brightness 0.4) (propertize moe-stroke-char 'face 'moe-stroke-d2))
               ((<= brightness 0.6) (propertize moe-stroke-char 'face 'moe-stroke-d3))
               ((<= brightness 0.8) (propertize moe-stroke-char 'face 'moe-stroke-d4))
               (t (propertize moe-stroke-char 'face 'moe-stroke-d5))
               ))
      (sit-for 0.001))))

(progn
  (moe-stroke-plot 1 5 0.2)
  (moe-stroke-plot 2 5 0.4)
  (moe-stroke-plot 3 5 0.6)
  (moe-stroke-plot 4 5 0.8)
  (moe-stroke-plot 5 5 1))

(defun moe-stroke-ipart (n)
  (floor n))

(defun moe-stroke-round (n)
  (moe-stroke-ipart (+ n 0.5)))

(defun moe-stroke-fpart (n)
  (if (< n 0)
      (- 1 (- n (floor n)))
    (- n (floor n))))

(defun moe-stroke-rfpart (n)
  (- 1 (moe-stroke-fpart n)))

(moe-stroke-draw-line '(1 . 1) '(5 . 5))

(defun moe-stroke-draw-line (p0 p1)
  (let* ((x0 (car p0))
         (y0 (cdr p0))
         (x1 (car p1))
         (y1 (cdr p1))
         (steep (> (abs (- y1 y0)) (abs (- x1 x0))))
         dx dy gradient
         xend yend
         xgap
         xpxl1 xpxl2
         ypxl1 ypxl2
         intery)
    (when steep
      (rotatef x0 y0)
      (rotatef x1 y1))
    (when (> x0 x1)
      (rotatef x0 x1)
      (rotatef y0 y1))

    (setq dx (- x1 x0)
          dy (- y1 y0)
          gradient (float (/ dy dx)))

    ;; handle first endpoint
    (setq xend (moe-stroke-round x0)
          yend (+ y0 (* gradient (- xend x0)))
          xgap (moe-stroke-rfpart (+ x0 0.5))
          xpxl1 xend              ; this will be used in the main loop
          ypxl1 (moe-stroke-ipart yend))
    (if steep
        (progn (moe-stroke-plot ypxl1      xpxl1 (* (moe-stroke-rfpart yend) xgap))
               (moe-stroke-plot (1+ ypxl1) xpxl1 (* (moe-stroke-fpart yend)  xgap)))
      (progn (moe-stroke-plot xpxl1 ypxl1      (* (moe-stroke-rfpart yend) xgap))
             (moe-stroke-plot xpxl1 (1+ ypxl1) (* (moe-stroke-fpart  yend) xgap))))
    (setq intery (+ yend gradient)) ;; first y-intersection for the main loop
    ;; handle second endpoint
    (setq xend (moe-stroke-round x1)
          yend (+ y1 (* gradient (- xend x1)))
          xgap (moe-stroke-fpart (+ x1 0.5))
          xpxl2 xend
          ypxl2 (moe-stroke-ipart yend))
    ;; main loop
    (if steep
        (loop for x from (1+ xpxl1) to (1- xpxl2)
              do (progn (moe-stroke-plot (moe-stroke-ipart intery) x (moe-stroke-rfpart intery))
                        (moe-stroke-plot (1+ (moe-stroke-ipart intery)) x (moe-stroke-fpart intery))
                        (setq intery (+ intery gradient))))
      (loop for x from (1+ xpxl1) to (1- xpxl2)
            do (progn (moe-stroke-plot x (moe-stroke-ipart intery) (moe-stroke-rfpart intery))
                      (moe-stroke-plot x (1+ (moe-stroke-ipart intery)) (moe-stroke-fpart intery))
                      (setq intery (+ intery gradient)))))
    ))

(defun moe-stroke (char)
  (moe-stroke-reset-canvas)
  (mapc (lambda (stroke-points)
          (loop for i from 0 to (- (length stroke-points) 2)
                for p1 = (nth i stroke-points)
                for p2 = (nth (1+ i) stroke-points)
                do (moe-stroke-draw-line p1 p2)))
        (moe-stroke-get-scaled-strokes-data char)
        ))


(provide 'moe-stroke)
;;; stroke.el ends here
