;;; moedict.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 ono hiroko

;; Author: ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Keywords: tools

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


;;; Code:

;; ======================================================
;; Variables
;; ======================================================
(require 'esqlite)
(require 'helm)

(setq moedict-prompt "萌典：")
(setq moedict-buffer-name "*[萌典] 查詢結果*")
(setq moedict-candidate-buffer-name "*[萌典] 候選字*")
(setq moedict-candidates-limit 200)
(setq moedict-synonyms-tag (propertize "同" 'face 'moedict-syn/antonyms-tag))
(setq moedict-antonyms-tag (propertize "反" 'face 'moedict-syn/antonyms-tag))
(defvar moedict-sqlite-stream nil)

(defcustom moedict-mode-hook nil
  "Normal hook run when entering moedict-mode."
  :type 'hook
  :group 'moedict)

(defvar moedict-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "<enter>") 'moedict:enter)
    (define-key map (kbd "RET") 'moedict:enter)
    (define-key map (kbd "<tab>") 'moedict:tab)
    (define-key map (kbd "TAB") 'moedict:tab)
    (define-key map (kbd "<backtab>") 'moedict:shift+tab)
    map)
  "Keymap for Moedict major mode.")

(defvar moedict-history nil
  "History list of current moedict buffer.")

(define-derived-mode moedict-mode nil "萌典"
  "Major mode for looking up Chinese vocabulary via Moedict API."
  (set (make-local-variable 'buffer-read-only) t))

(defgroup moedict nil
  "Major mode for looking up Chinese vocabulary via Moedict API."
  :prefix "moedict"
  :link '(url-link "http://github.com/kuanyui/moedict.el"))

(defgroup moedict-faces nil
  "Faces used in Moedict-mode"
  :group 'moedict
  :group 'faces)

(defface moedict-title
  '((((class color) (background light))
     (:foreground "#ff8700" :bold t :height 1.2))
    (((class color) (background dark))
     (:foreground "#ffa722" :bold t :height 1.2)))
  "Face for title. ex:"
  :group 'moedict-faces)

(defface moedict-stroke-count
  '((((class color) (background light))
     (:foreground "#787878"))
    (((class color) (background dark))
     (:foreground "#c1c1c1")))
  "Face for stroke-count."
  :group 'moedict-faces)

(defface moedict-radical
  '((((class color) (background light))
     (:foreground "#ffffff" :background "#a40000"))
    (((class color) (background dark))
     (:foreground "#ffffff" :background "#a40000")))
  "Face for character's radical."
  :group 'moedict-faces)

(defface moedict-non-radical-stroke-count
  '((((class color) (background light))
     (:inherit moedict-stroke-count))
    (((class color) (background dark))
     (:inherit moedict-stroke-count)))
  "Face for non-radical stroke-count."
  :group 'moedict-faces)

(defface moedict-bopomofo
  '((((class color) (background light))
     (:foreground "#008700" :background "#d7ff87"))
    (((class color) (background dark))
     (:foreground "#a1db00" :background "#5a5a5a")))
  "Face for bopomofo （注音符號）."
  :group 'moedict-faces)

(defface moedict-bopomofo2
  '((((class color) (background light))
     (:inherit moedict-bopomofo))
    (((class color) (background dark))
     (:inherit moedict-bopomofo)))
  "Face for bopomofo2 （注音二式）."
  :group 'moedict-faces)

(defface moedict-pinyin
  '((((class color) (background light))
     (:inherit moedict-bopomofo))
    (((class color) (background dark))
     (:inherit moedict-bopomofo)))
  "Face for pinyin （拼音）."
  :group 'moedict-faces)

(defface moedict-type
  '((((class color) (background light))
     (:foreground "#ffffd7" :background "#525252"))
    (((class color) (background dark))
     (:foreground "#525252" :background "#c1c1c1")))
  "Face for type. ex: [動]、[名]"
  :group 'moedict-faces)

(defface moedict-quote
  '((((class color) (background light))
     (:foreground "#ff4ea3" :slant italic))
    (((class color) (background dark))
     (:foreground "#ff6fa5" :slant italic)))
  "Face for quote."
  :group 'moedict-faces)

(defface moedict-def
  '((((class color) (background light))
     (:foreground "#1f5bff"))
    (((class color) (background dark))
     (:foreground "#6faaff")))
  "Face for definitions."
  :group 'moedict-faces)

(defface moedict-example
  '((((class color) (background light))
     (:foreground "#525252"))
    (((class color) (background dark))
     (:foreground "#cdcdcd")))
  "Face for example. ex: Example"
  :group 'moedict)

(defface moedict-link
  '((((class color) (background light))
     (:foreground "#00a775"))
    (((class color) (background dark))
     (:foreground "#00d7af")))
  "Face for link. ex:「見...等條」"
  :group 'moedict)

(defface moedict-synonyms
  '((((class color) (background light))
     (:foreground "#9a08ff"))
    (((class color) (background dark))
     (:foreground "#aa71ff")))
  "Face for synonyms."
  :group 'moedict)

(defface moedict-antonyms
  '((((class color) (background light))
     (:foreground "#9a08ff"))
    (((class color) (background dark))
     (:foreground "#aa71ff")))
  "Face for antonyms."
  :group 'moedict)

(defface moedict-syn/antonyms-tag
  '((((class color) (background light))
     (:foreground "#ffffff" :background "#9a08ff"))
    (((class color) (background dark))
     (:foreground "#7008a0" :background "#eeaeff")))
  "Face for syn/antonyms-tag. ex: [同]"
  :group 'moedict)


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



(defun moedict-concat-with-newline (&rest args)
  "Ignore nil, seperator is \\n."
  (mapconcat #'identity
             (remove-if #'null args)
             "\n"))

(defun moedict-mapconcat-with-2-newlines (list)
  "Ignore nil, seperator is \\n\\n."
  (mapconcat #'identity
             (remove-if #'null list)
             "\n\n"))

(defun moedict--replace-null-with-nil (list)
  "replace all :null in (two-level) list with nil"
  (mapcar (lambda (x)
            (mapcar (lambda (y) (if (eq y :null) nil y))
                    x))
          list))

(defun moedict-match-positions (regexp string &optional subexp-depth)
  "Get all matched REGEXP position in a STRING.
SUBEXP-DEPTH is 0 by default."
  (if (null subexp-depth)
      (setq subexp-depth 0))
  (let ((pos 0) result)
    (while (and (string-match regexp string pos)
                (< pos (length string)))
      (let ((m (match-end subexp-depth)))
        (push (cons (match-beginning subexp-depth) (match-end subexp-depth)) result)
        (setq pos m)))
    (nreverse result)))

;; ======================================================
;; Render
;; ======================================================

(defmacro moedict--render-type ()
  `(let ((aaa (moedict--get-column row 'type)))
     (if aaa
         (concat " " (propertize (format "[%s]" aaa) 'face 'moedict-type))
       "")))

(defmacro moedict--render-def ()
  `(let* ((def (moedict--get-column row 'def))
          (example (moedict--get-column row 'example))
          (quote (moedict--get-column row 'quote))
          (quote (if (stringp quote) (replace-regexp-in-string "," "\n        " quote)))
          (link (moedict--get-column row 'link))
          (synonyms (moedict--get-column row 'synonyms))
          (antonyms (moedict--get-column row 'antonyms)))

     (when link
       (add-text-properties 0 (length link) '(face moedict-link) link)
       (mapc (lambda (begin-end)
               (add-face-text-property (car begin-end) (cdr begin-end) '(underline t) t link))
             (moedict-match-positions "「\\(.+?\\)」" link 1)
             ))

     (moedict-concat-with-newline
      (if def      (format "    %s" (propertize def 'face 'moedict-def)))
      (if example  (format "        %s" (propertize example 'face 'moedict-example)))
      (if quote    (format "        %s" (propertize quote 'face 'moedict-quote)))
      (if link     (format "        %s" link))
      (if synonyms (format "            %s %s"
                           moedict-synonyms-tag (propertize synonyms 'face 'moedict-synonyms)))
      (if antonyms (format "            %s %s"
                           moedict-antonyms-tag (propertize antonyms 'face 'moedict-antonyms))))))


(defun moedict--render-rows (rows)
  "ROWS is the query result retrieved from `moedict-query-vocabulary',
Return value is rendered string."
  (moedict-mapconcat-with-2-newlines
   (cons
    ;; car
    (if (eq :null (moedict--get-column (car rows) 'radical))
        nil
      (format "%s + %s = %s"
              (propertize (moedict--get-column (car rows) 'radical) 'face 'moedict-radical)
              (moedict--get-column (car rows) 'non_radical_stroke_count)
              (moedict--get-column (car rows) 'stroke_count)))
    ;; cdr
    (let ( bopomofo type )
      (mapcar
       (lambda (row)
         (cond ((not (equal (moedict--get-column row 'bopomofo) bopomofo))
                (setq bopomofo (moedict--get-column row 'bopomofo))
                (setq type (moedict--get-column row 'type))
                (format "%s %s %s %s%s%s\n\n%s"
                        (propertize (moedict--get-column row 'title) 'face 'moedict-title)
                        (propertize (moedict--get-column row 'bopomofo) 'face 'moedict-bopomofo)
                        (propertize (moedict--get-column row 'pinyin) 'face 'moedict-pinyin)
                        (propertize (moedict--get-column row 'bopomofo2) 'face 'moedict-bopomofo2)
                        (if type "\n\n" "") ; Because some vocabulary have no type
                        (moedict--render-type)
                        (moedict--render-def)))
               ((not (equal (moedict--get-column row 'type) type))
                (setq type (moedict--get-column row 'type))
                (concat (moedict--render-type)
                        (if type "\n\n" "") ; Because some vocabulary have no type
                        (moedict--render-def)))
               (t
                (moedict--render-def))))
       (moedict--replace-null-with-nil rows))
      ))))

(defun moedict-render (vocabulary)
  "Return rendered string"
  (moedict--render-rows (moedict-query-vocabulary vocabulary)))

;; ======================================================
;; UI
;; ======================================================

(defun moedict-message (string)
  (message (format "[萌典] %s" string)))

(defun moedict-lookup-and-show-in-buffer (vocabulary)
  ""
  (moedict-message "查詢中...")
  (let ((rendered-result (moedict-render vocabulary)))
    (with-temp-buffer-window moedict-buffer-name nil nil)
    (with-selected-window (get-buffer-window moedict-buffer-name)
      (moedict-mode)
      (let (buffer-read-only)
        (insert rendered-result))
      (goto-char (point-min))))
  (moedict-message "查詢完成。"))

(defun moedict (&optional init-input)
  (interactive)
  (if (null
       (helm :sources
             (helm-build-sync-source "請選擇您欲查詢的單字："
               :candidates (lambda () (moedict-get-candidates-list helm-pattern))
               :volatile t
               :candidate-number-limit moedict-candidates-limit
               :action #'moedict-lookup-and-show-in-buffer
               :requires-pattern t
               )
             :input (or "" init-input)
             :buffer moedict-candidate-buffer-name
             :prompt moedict-prompt))
      (moedict-message "找不到你輸入的這個單字喔！")))

;; ======================================================
;; Interactive Commands
;; ======================================================
(defun moedict-point-at-underline-p (&optional point)
  (if (null point) (setq point (point)))
  (let ((face (get-text-property point 'face)))
    (and face (listp face) (or (member '(underline t) face)
                               (equal  '(underline t) face))
         )))

(defun moedict-try-to-get-vocabulary-at-point ()
  (let ((pos (point)))
    (cond ((moedict-point-at-underline-p)
           (if (null (moedict-point-at-underline-p (1- pos)))
               (setq pos (1+ pos)))
           (buffer-substring-no-properties (previous-property-change pos)
                                           (next-property-change pos)))
          (t
           ""))))

(defun moedict:enter ()
  (interactive)
  (let ((vocabulary (moedict-try-to-get-vocabulary-at-point)))
    (if (moedict-point-at-underline-p)
        (moedict-lookup-and-show-in-buffer vocabulary)
      (progn
        (moedict vocabulary)
        ))))

(defun moedict:tab ()
  (interactive)
  (let ((pos (point)))
    ;; If already on underline
    (if (moedict-point-at-underline-p pos)
        (setq pos (next-property-change pos)))
    (while (and (not (moedict-point-at-underline-p pos)) ;if current point have no underline
                (next-property-change pos))
      (setq pos (next-property-change pos)))
    (goto-char pos)))

(defun moedict:shift+tab ()
  (interactive)
  (let ((pos (point)))
    ;; If already on underline
    (if (moedict-point-at-underline-p pos)
        (setq pos (previous-property-change pos)))
    (while (and (not (moedict-point-at-underline-p pos)) ;if current point have no underline
                (previous-property-change pos))
      (setq pos (previous-property-change pos)))
    (goto-char pos)))


(provide 'moedict)
;;; moedict+.el ends here
