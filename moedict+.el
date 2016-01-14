;;; moedict+.el ---                                  -*- lexical-binding: t; -*-

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
(require 'esqlite)

(setq moedict-prompt "萌典：")
(setq moedict-buffer "*萌典*")
(setq moedict-candidate-buffer-title "候選字列表")
(setq moedict-candidates-limit 200)
(setq moedict-synonyms-tag (propertize "同" 'face 'moedict-syn/antonyms-tag))
(setq moedict-antonyms-tag (propertize "反" 'face 'moedict-syn/antonyms-tag))


(defvar moedict-sqlite-stream nil)

;; ======================================================
;; Query
;; ======================================================

(defun moedict-open-sqlite-process ()
  (if (not (process-live-p moedict-sqlite-stream))
      (setq moedict-sqlite-stream (esqlite-stream-open "dict.sqlite3"))))

(defmacro moedict-query (query-string)
  `(progn
     (moedict-open-sqlite-process)
     (esqlite-stream-read moedict-sqlite-stream ,query-string)))

(defmacro moedict-query-with-limit (query-string)
  `(progn
     (moedict-open-sqlite-process)
     (esqlite-stream-read moedict-sqlite-stream
                          (format "%s LIMIT %s" ,query-string ,moedict-candidates-limit))))

(defun moedict-get-candidates-list (string)
  (cl-remove-if
   (lambda (x) (string-prefix-p "{" x))
   (mapcon #'car
           (moedict-query-with-limit
            (format "SELECT title FROM entries WHERE title LIKE %s"
                    (esqlite-format-text (concat string "%")))))))

(defun moedict-close-sqlite-process ()
  (esqlite-stream-close moedict-sqlite-stream))

(defun moedict-query-vocabulary (vocabulary)
  "title, radical, stroke_count, non_radical_stroke_count,
   bopomofo, bopomofo2, pinyin,
   type, def, example, quote, source,
   synonyms, antonyms, link
"
  (moedict-query
   (format "
SELECT entries.title, entries.radical, entries.stroke_count, entries.non_radical_stroke_count,
    heteronyms.bopomofo, heteronyms.bopomofo2, heteronyms.pinyin,
    definitions.type, definitions.def, definitions.example, definitions.quote, definitions.source,
    definitions.synonyms, definitions.antonyms, definitions.link
FROM entries, heteronyms, definitions
WHERE entries.title = %s
    AND heteronyms.entry_id = entries.id
    AND definitions.heteronym_id = heteronyms.id;
" (esqlite-format-text vocabulary))))

;; ======================================================
;; Tools
;; ======================================================

(defun moedict--get-column (row attr)
  "Return column's content from a row directly.
Don't borthered by the serial numbers."
  (nth
   (cdr (assq attr '((title                    .  0)
                     (radical                  .  1)
                     (stroke_count             .  2)
                     (non_radical_stroke_count .  3)
                     (bopomofo                 .  4)
                     (bopomofo2                .  5)
                     (pinyin                   .  6)
                     (type                     .  7)
                     (def                      .  8)
                     (example                  .  9)
                     (quote                    . 10)
                     (source                   . 11)
                     (synonyms                 . 12)
                     (antonyms                 . 13)
                     (link                     . 14))))
   row
   ))



(defun moedict-concat (&rest args)
  "Ignore nil, seperator is \n."
  (mapconcat #'identity
             (remove-if #'null args)
             "\n"))

(defun moedict--replace-null-with-nil (list)
  "replace all :null in (two-level) list with nil"
  (mapcar (lambda (x)
            (mapcar (lambda (y) (if (eq y :null) nil y))
                    x))
          list)
  )

;; ======================================================
;; Render
;; ======================================================

(defmacro moedict--render-type ()
  `(let ((type (moedict--get-column row 'type)))
     (if type
         (concat " " (propertize (format "[%s]" type) 'face 'moedict-type))
       "")
  ))


(defmacro moedict--render-def ()
  `(let* ((def (moedict--get-column row 'def))
          (example (moedict--get-column row 'example))
          (quote (moedict--get-column row 'quote))
          (quote (if (stringp quote) (replace-regexp-in-string "," "\n        " quote)))
          (link (moedict--get-column row 'link))
          (synonyms (moedict--get-column row 'synonyms))
          (antonyms (moedict--get-column row 'antonyms)))
     (moedict-concat
       (if def      (format "    %s" (propertize def 'face 'moedict-def)))
       (if example  (format "        %s" (propertize example 'face 'moedict-example)))
       (if quote    (format "        %s" (propertize quote 'face 'moedict-quote)))
       (if link     (format "        %s" (propertize link 'face 'moedict-link)))
       (if synonyms (format "            %s %s"
                            moedict-synonyms-tag (propertize synonyms 'face 'moedict-synonyms)))
       (if antonyms (format "            %s %s"
                            moedict-antonyms-tag (propertize antonyms 'face 'moedict-antonyms))))))



(defun moedict--render-rows (rows)
  "ROWS is the query result retrieved from `moedict-query-vocabulary',
Return value is rendered string."
  (mapconcat
   #'identity
   (cons
    ;; car
    (format "%s + %s = %s"
            (propertize (moedict--get-column (car rows) 'radical) 'face 'moedict-radical)
            (moedict--get-column (car rows) 'non_radical_stroke_count)
            (moedict--get-column (car rows) 'stroke_count))
    ;; cdr
    (let ( bopomofo type )
      (mapcar
       (lambda (row)
         (cond ((not (equal (moedict--get-column row 'bopomofo) bopomofo))
                (setq bopomofo (moedict--get-column row 'bopomofo))
                (setq type (moedict--get-column row 'type))
                (format "%s %s %s %s\n\n%s\n\n%s"
                        (propertize (moedict--get-column row 'title) 'face 'moedict-title)
                        (propertize (moedict--get-column row 'bopomofo) 'face 'moedict-bopomofo)
                        (propertize (moedict--get-column row 'pinyin) 'face 'moedict-pinyin)
                        (propertize (moedict--get-column row 'bopomofo2) 'face 'moedict-bopomofo2)
                        (moedict--render-type)
                        (moedict--render-def)))
               ((not (equal (moedict--get-column row 'type) type))
                (setq type (moedict--get-column row 'type))
                (moedict--render-type)
                (moedict--render-def))
               (t
                (moedict--render-def))))
       (moedict--replace-null-with-nil rows))
      ))
   "\n\n")
  )

(defun moedict-render (vocabulary)
  (moedict--render-rows (moedict-query-vocabulary vocabulary)))

;; ======================================================
;; Helm
;; ======================================================

(moedict-render "王")

;;(with-selected-window "*萌典::查詢結果*" "hello")

(helm :sources
      (helm-build-sync-source moedict-candidate-buffer-title
        :candidates (lambda () (moedict-get-candidates-list helm-pattern))
        :volatile t
        :candidate-number-limit moedict-candidates-limit
        )
      :buffer moedict-buffer
      :prompt moedict-prompt)










(provide 'moedict+)
;;; moedict+.el ends here
