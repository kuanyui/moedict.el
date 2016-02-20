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

(make-list 2050
           (make-list 2050 0))

(string-to-char "萌")

(defun moe-stroke-dec-to-hex (decimal-number)
  (format "%x" decimal-number))

(defun moe-stroke-get-file-path (character)
  "CHARACTER is a Chinese STRING, not CHAR type in Emacs Lisp"
  (let ((hex-string (moe-stroke-dec-to-hex (string-to-char character))))
    (concat moe-stroke-json-directory hex-string ".json")))

(defun moe-stroke-get-raw-tracks (character)
  "Return a list. <ex>
([((y . 216) (x . 703))                   ; A sub-track
  ((size . 85) (y . 688) (x . 792))]      ; A full track done (stroke #1).
 [((y . 527) (x . 436))                   ; A sub-track
  ((size . 95) (y . 416) (x . 956))] ...) ; A full track done (stroke #2)"
  (mapcar (lambda (x) (cdr (car x)))
          (json-read-file (moe-stroke-get-file-path character))))

(defun moe-stroke-get-stroke (character)
  "Return a list. <ex>

"
  (mapcar (lambda (stroke)
            (map 'list (lambda (sub-stroke)
                         (let ((x ())
                               (y ()))
              )))
          (moe-stroke-get-raw-tracks character)))

(moe-stroke-get-raw-tracks "萌")




(provide 'moe-stroke)
;;; stroke.el ends here
