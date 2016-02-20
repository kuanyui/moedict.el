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

(make-list 2050
           (make-list 2050 0))

(string-to-char "萌")

(defun moe-stroke-dec-to-hex (decimal-number)
  (format "%x" decimal-number))

(defun moe-stroke-get-filename (zh-string)
  (let* ((hex-string (moe-stroke-dec-to-hex (string-to-char zh-string)))
         ())
    )
  )

(moe-stroke-get-filename "萌")



(provide 'moe-stroke)
;;; stroke.el ends here
