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
;;(defconst moe-stroke-directory (file-name-directory load-file-name))
(defconst moe-stroke-directory (file-name-directory (buffer-file-name)))
(setq moe-stroke-xml-directory (concat moe-stroke-directory "zh-stroke-data/utf8/"))
(setq moe-stroke-json-directory (concat moe-stroke-directory "zh-stroke-data/json/"))
(setq moe-stroke-buffer-name "*moe-stroke*")

(string-to-char "萌")

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
(((703 . 216) (792 . 688) (300 . 500))   ; (sub-stroke sub-stroke) ; (stroke #1)
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
      (cons x y))))

(defun moe-stroke-get-empty-canvas ()
  "0 means empty pixel"
  (let ((xy (moe-stroke-get-canvas-size)))
    (make-list (cdr xy)
               (make-list (car xy) 0))))

(moe-stroke-get-stroke-data "萌")

'(((703 . 216) (792 . 688)) ((436 . 527) (956 . 416)) ((1082 . 459) (1615 . 372)) ((1359 . 149) (1195 . 672)) ((433 . 853) (430 . 1529)) ((481 . 836) (756 . 761) (870 . 828) (834 . 1079) (809 . 1546)) ((493 . 1186) (764 . 1162)) ((483 . 1476) (744 . 1456)) ((1110 . 740) (1099 . 1276) (1000 . 1648) (743 . 1888)) ((1170 . 738) (1489 . 646) (1612 . 692) (1589 . 1080) (1588 . 1574) (1616 . 1942) (1319 . 1696)) ((1158 . 1099) (1516 . 1033)) ((1136 . 1373) (1525 . 1318)))


(defun moe-stroke-draw-canvas (canvas)
  "Overwrite *moe-stroke* buffer with CANVAS"
  (with-current-buffer moe-stroke-buffer-name
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert (moe-stroke-format-canvas canvas)))))


(defun moe-stroke-draw-line (canvas p1 p2)
  (let* ((canvas-size (moe-stroke-get-canvas-size))
         (m (moe-stroke-get-slope-rate p1 p2))
         (p1-xy (moe-stroke-calculate-xy-on-canvas p1 canvas-size))
         (p2-xy (moe-stroke-calculate-xy-on-canvas p2 canvas-size))
         (p1x (car p1-xy))
         (p2x (car p2-xy))
         (p1y (cdr p1-xy))
         (p2y (cdr p2-xy))
         (b   (moe-stroke-get-b p1y m p1x))
         (delta-x (- p2x p1x)))
    (if (>= delta-x 0)
        (loop for x from p1x to p2x do
              (let* ((y (moe-stroke-get-y m x b))
                     (char (moe-stroke-get-pixel-char canvas x y m))
                     (new-canvas (moe-stroke-replace-pixel canvas x y char)))
                (moe-stroke-draw-canvas new-canvas)
                (sit-for 0.05)
                ))
      (dotimes (x delta-x)
        )
      ))
  )

(apply #'moe-stroke-draw-line (moe-stroke-get-empty-canvas)
       (car (moe-stroke-get-stroke-data "萌")))

(apply #'moe-stroke-get-slope-rate (car (moe-stroke-get-stroke-data "萌")))

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

(defun moe-stroke-get-b (y m x)
  "y - mx = b"
  (- y (* m x)))

(defun moe-stroke-get-y (m x b)
  "y = mx + b"
  (floor (+ (* m x) b)))


(defun moe-stroke-get-pixel-char (canvas x y m)
  "回傳位置應該放啥char，
X Y is counted from 1,
M is slope rate."
  ;; Available chars:
  ;;  - / \ | +
  (let ((sym (nth (1- x) (nth (1- y) canvas))))
    (cond ((eq sym 0)
           (cond ((> (abs m) 2)      '|)
                 ((> m 1)            '/)
                 ((< (abs m) 1)      '-)
                 ((< m  -1)          '\\)
                 (t                  '*)))
          ((memq sym '(| / \\))
           (if (< (abs m) 1) '+ sym))
          (t   ;; sym is - or *
           (if (< (abs m) 1) sym '+)))))

(defun moe-stroke-replace-pixel (canvas x y elem)
  "Return a new CANVAS. X, Y is counted from 1
'((x1 x2 x3 x4 x5)  ; y1
  (x1 x2 x3 x4 x5)  ; y2
  (x1 x2 x3 x4 x5)  ; y3 "
  (let* ((-x (1- x))
         (-y (1- y))
         (line (nth -y canvas)))
    (moe-stroke-set-nth -x elem line)
    (moe-stroke-set-nth -y line canvas)
    canvas))


(defun moe-stroke-set-nth (n elem list)
  (setcar (nthcdr n list) elem))

(defun moe-stroke-format-canvas (canvas)
  (mapconcat (lambda (line)
               (mapconcat (lambda (sym) (if (eq sym 0) " " (symbol-name sym)))
                          line
                          ""))
             canvas
             "\n"))

(defun moe-stroke-calculate-xy-on-canvas (raw-xy canvas-size)
  "Example:
RAW-XY       '(703 . 216)
percents     '(0.3429268292682927 . 0.10536585365853658)
CANVAS-SIZE  '(100 . 50)
XY           '(34  . 5)

2050 is the max value of raw data
"
  (let ((percents (cons (/ (car raw-xy) 2050.0)
                        (/ (cdr raw-xy) 2050.0))))
    (cons (round (* (car canvas-size) (car percents)))
          (round (* (cdr canvas-size) (cdr percents))))))


(defun moe-stroke-get-point-in-canvas
    ())

(provide 'moe-stroke)
;;; stroke.el ends here
