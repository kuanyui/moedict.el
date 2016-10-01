;;; moedict-stroke.el ---                                    -*- lexical-binding: t; -*-

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
;;;;;;;(defconst moedict-stroke-directory (file-name-directory load-file-name))
(require 'helm)
(setq moedict-stroke-directory (file-name-directory (buffer-file-name)))
(setq moedict-stroke-xml-directory (concat moedict-stroke-directory "zh-stroke-data/utf8/"))
(setq moedict-stroke-json-directory (concat moedict-stroke-directory "stroke-data-json/"))
(defvar moedict-stroke-candidates nil)
(setq moedict-stroke-buffer-name "*[萌典筆劃] 動畫*")
(setq moedict-stroke-candidate-buffer-name "*[萌典筆劃] 候選字*")
(setq moedict-stroke-prompt "*[萌典筆劃] 請輸入您欲查詢的筆劃動畫：")
(setq moedict-stroke-char "█")
;; ======================================================
;; Faces
;; ======================================================

(defface moedict-stroke-background
  '((((class color)) (:foreground "#ffffff" :background "#ffffff")))
  "This comment is necessary")

(defface moedict-stroke-d1
  '((((class color)) (:foreground "#c6c6c6" :background "##c6c6c6")))
  "This comment is necessary")

(defface moedict-stroke-d2
  '((((class color)) (:foreground "#949494" :background "##949494")))
  "This comment is necessary")

(defface moedict-stroke-d3
  '((((class color)) (:foreground "#626262" :background "##626262")))
  "This comment is necessary")

(defface moedict-stroke-d4
  '((((class color)) (:foreground "#3a3a3a" :background "##3a3a3a")))
  "This comment is necessary")

(defface moedict-stroke-d5
  '((((class color)) (:foreground "#080808" :background "##080808")))
  "This comment is necessary")

;; ======================================================
;; Get Data
;; ======================================================

(defun moedict-stroke-dec-to-hex (decimal-number)
  (format "%x" decimal-number))

(defun moedict-stroke-get-file-path (character)
  "CHARACTER is a Chinese STRING, not CHAR type in Emacs Lisp"
  (let ((hex-string (moedict-stroke-dec-to-hex (string-to-char character))))
    (concat moedict-stroke-json-directory hex-string ".json")))

(defun moedict-stroke-get-candidates ()
  (when (null moedict-stroke-candidates)
    (message "Making cache...")
    (setq moedict-stroke-candidates
          (remove-if #'null
                     (mapcar
                      (lambda (filename)
                        (if (string-suffix-p ".json" filename)
                            (let ((hex-string (substring filename 0 -5)))
                              (char-to-string (string-to-number hex-string 16)))))
                      (directory-files moedict-stroke-json-directory))
                     )))
  moedict-stroke-candidates)

(defun moedict-stroke-get-raw-data (character)
  "Return a list. <ex>
 ([((y . 216) (x . 703))                   ; A sub-track (sub-stroke)
   ((size . 85) (y . 688) (x . 792))]      ; A full track done (stroke #1).
  [((y . 527) (x . 436))                   ; A sub-track (sub-stroke)
   ((size . 95) (y . 416) (x . 956))] ...) ; A full track done (stroke #2)"
  (map 'list (lambda (x)
               (cdr (assoc 'track x)))
       (json-read-file (moedict-stroke-get-file-path character))))

(defun moedict-stroke-get-raw-strokes-data (character)
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
          (moedict-stroke-get-raw-data character)))

(defun moedict-stroke-get-scaled-strokes-data (character)
  (let ((canvas-size (moedict-stroke-get-canvas-size)))
    (mapcar (lambda (raw-stroke)
              (mapcar (lambda (raw-xy)
                        (moedict-stroke-calculate-xy-on-canvas raw-xy (car canvas-size) (cdr canvas-size)))
                      raw-stroke))
            (moedict-stroke-get-raw-strokes-data character))))

(defun moedict-stroke-calculate-xy-on-canvas (raw-xy canvas-width canvas-height)
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
(defun moedict-stroke-create-buffer-if-not-exist ()
  (if (not (get-buffer moedict-stroke-buffer-name))
      (with-temp-buffer-window moedict-stroke-buffer-name t nil)))

(defun moedict-stroke-get-canvas-size ()
  (moedict-stroke-create-buffer-if-not-exist)
  (with-current-buffer moedict-stroke-buffer-name
    (let* ((size (min (/ (window-width) 2)
                      (window-height)))
           (x (* size 2))
           (y size))
      (setq-local moedict-stroke-canvas-width x)
      (setq-local moedict-stroke-canvas-height y)
      (cons x y))))

(defun moedict-stroke-reset-canvas ()
  "Fill canvas buffer with spaces"
  (let* ((xy (moedict-stroke-get-canvas-size))
         (x (car xy))
         (y (cdr xy)))
    (with-current-buffer
        moedict-stroke-buffer-name
      (let (buffer-read-only)
        (delete-region (point-min) (point-max))
        (loop repeat y do
              (insert (propertize (make-string x (string-to-char moedict-stroke-char))
                                  'face 'moedict-stroke-background) "\n"))))))


;; ======================================================
;; Xiaolin Wu's Algorithm
;; ======================================================


(defun moedict-stroke-plot (x y brightness)
  (with-current-buffer moedict-stroke-buffer-name
    (let (buffer-read-only)
      (goto-char (point-min))
      (forward-line (1- y))
      (forward-char x)
      (delete-char 1)
      (insert (cond
               ((<= brightness 0.2) (propertize moedict-stroke-char 'face 'moedict-stroke-d1))
               ((<= brightness 0.4) (propertize moedict-stroke-char 'face 'moedict-stroke-d2))
               ((<= brightness 0.6) (propertize moedict-stroke-char 'face 'moedict-stroke-d3))
               ((<= brightness 0.8) (propertize moedict-stroke-char 'face 'moedict-stroke-d4))
               (t (propertize moedict-stroke-char 'face 'moedict-stroke-d5))
               ))
      (sit-for 0.001))))

(defun moedict-stroke-ipart (n)
  (floor n))

(defun moedict-stroke-round (n)
  (moedict-stroke-ipart (+ n 0.5)))

(defun moedict-stroke-fpart (n)
  (if (< n 0)
      (- 1 (- n (floor n)))
    (- n (floor n))))

(defun moedict-stroke-rfpart (n)
  (- 1 (moedict-stroke-fpart n)))

(defun moedict-stroke-draw-line (p0 p1)
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
    (setq xend (moedict-stroke-round x0)
          yend (+ y0 (* gradient (- xend x0)))
          xgap (moedict-stroke-rfpart (+ x0 0.5))
          xpxl1 xend              ; this will be used in the main loop
          ypxl1 (moedict-stroke-ipart yend))
    (if steep
        (progn (moedict-stroke-plot ypxl1      xpxl1 (* (moedict-stroke-rfpart yend) xgap))
               (moedict-stroke-plot (1+ ypxl1) xpxl1 (* (moedict-stroke-fpart yend)  xgap)))
      (progn (moedict-stroke-plot xpxl1 ypxl1      (* (moedict-stroke-rfpart yend) xgap))
             (moedict-stroke-plot xpxl1 (1+ ypxl1) (* (moedict-stroke-fpart  yend) xgap))))
    (setq intery (+ yend gradient)) ;; first y-intersection for the main loop
    ;; handle second endpoint
    (setq xend (moedict-stroke-round x1)
          yend (+ y1 (* gradient (- xend x1)))
          xgap (moedict-stroke-fpart (+ x1 0.5))
          xpxl2 xend
          ypxl2 (moedict-stroke-ipart yend))
    ;; main loop
    (if steep
        (loop for x from (1+ xpxl1) to (1- xpxl2)
              do (progn (moedict-stroke-plot (moedict-stroke-ipart intery) x (moedict-stroke-rfpart intery))
                        (moedict-stroke-plot (1+ (moedict-stroke-ipart intery)) x (moedict-stroke-fpart intery))
                        (setq intery (+ intery gradient))))
      (loop for x from (1+ xpxl1) to (1- xpxl2)
            do (progn (moedict-stroke-plot x (moedict-stroke-ipart intery) (moedict-stroke-rfpart intery))
                      (moedict-stroke-plot x (1+ (moedict-stroke-ipart intery)) (moedict-stroke-fpart intery))
                      (setq intery (+ intery gradient)))))
    ))

(defun moedict-stroke-internal (char)
  (moedict-stroke-reset-canvas)
  (with-temp-buffer-window moedict-stroke-buffer-name t nil)
  (mapc (lambda (stroke-points)
          (loop for i from 0 to (- (length stroke-points) 2)
                for p1 = (nth i stroke-points)
                for p2 = (nth (1+ i) stroke-points)
                do (moedict-stroke-draw-line p1 p2)))
        (moedict-stroke-get-scaled-strokes-data char)))

(defun moedict-stroke ()
  (interactive)
  (helm :sources (helm-build-sync-source "[萌典] 請輸入您欲查詢的單字："
                   :candidates (moedict-stroke-get-candidates)
                   :volatile t
                   :candidate-number-limit 100
                   :action #'moedict-stroke-internal
                   )
        :input (or "")
        :buffer moedict-stroke-candidate-buffer-name
        :prompt moedict-stroke-prompt))


(provide 'moedict-stroke)
;;; stroke.el ends here
