;;; moedict.el --- Moe Dict ("萌典", a Chinese dictionary) client for Emacs -*- lexical-binding: t; -*-


;; Author: ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Keywords: tools, dictionary
;; Package-Requires: ((emacs "24.3") (helm "1.9.1") (esqlite "0.3.1"))
;; X-URL: https://github.com/kuanyui/moedict.el
;; Version: {{VERSION}}

;;
;; Copyright (C) 2014, 2016 ono hiroko
;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;;; Commentary:

;; M-x `moedict' to use. M-x `moedict/help' to see a complete how to use.

;; Because the answer of Universe is 42, and the line number of this
;; file *MUST* be 689, I cannot write a complete README in here.
;;
;; To read more info, please visit https://github.com/kuanyui/moedict.el

;;; Code:

;; ======================================================
;; Variables
;; ======================================================

(require 'esqlite)
(require 'helm)
(require 'org-table)
(require 'cl)
(require 'url)

(defconst moedict-files-directory (file-name-directory load-file-name))

(defvar moedict-dictionary-file-path (concat moedict-files-directory "dict.sqlite3")
  "萌典 sqlite3 字典檔的檔案路令")

(defvar moedict-dictionary-xz-file-path (concat moedict-files-directory "dict.sqlite3.xz")
  "萌典 sqlite3.xz 的路徑")

(defvar moedict-dictionary-source-url
  "https://raw.githubusercontent.com/kuanyui/moedict.el/master/dict.sqlite3.xz"
  "萌典字典檔的 .xz 下載連結")

(defvar moedict-prompt "萌典：")
(defvar moedict-buffer-name "*[萌典] 查詢結果*")
(defvar moedict-help-buffer-name "*[萌典] 幫助視窗*")
(defvar moedict-candidate-buffer-name "*[萌典] 候選字*")
(defvar moedict-history-buffer-name "*[萌典] 查詢歷史*")
(defvar moedict-synonyms-tag (propertize "同" 'face 'moedict-syn/antonyms-tag))
(defvar moedict-antonyms-tag (propertize "反" 'face 'moedict-syn/antonyms-tag))
(defvar moedict-punctuations "[ \n。，！？；：．「」『』（）、【】《》〈〉—]")
(defvar moedict--history nil "History list of current moedict buffer. DON'T SETQ ME.")
(defvar moedict--current-vocabulary nil
  "History list of current moedict buffer. DON'T SETQ ME YOU IDIOT")

(defcustom moedict-candidates-limit 200
  "查詢時所列出的候選字最大數量"
  :type 'integer :group 'moedict)

(defcustom moedict-try-to-get-vocabulary-max-length 4
    "嘗試猜測字彙時，猜測字彙的最大長度"
    :type 'integer :group 'moedict)

(defcustom moedict-mode-hook nil
  "Normal hook run when entering moedict-mode."
  :type 'hook :group 'moedict)

(defvar moedict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "Q")         'moedict/exit)
    (define-key map (kbd "SPC")       'moedict)
    (define-key map (kbd "l")         'moedict)
    (define-key map (kbd "h")         'moedict/help)
    (define-key map (kbd "?")         'moedict/help)
    (define-key map (kbd "<enter>")   'moedict:enter)
    (define-key map (kbd "RET")       'moedict:enter)
    (define-key map (kbd "<tab>")     'moedict:tab)
    (define-key map (kbd "TAB")       'moedict:tab)
    (define-key map (kbd "<backtab>") 'moedict:shift+tab)
    (define-key map (kbd "H")         'moedict/history-show-list)
    (define-key map (kbd "[")         'moedict/history-previous)
    (define-key map (kbd "q")         'moedict/history-previous)
    (define-key map (kbd "^")         'moedict/history-previous)
    (define-key map (kbd "C-c C-b")   'moedict/history-previous)
    (define-key map (kbd "]")         'moedict/history-next)
    (define-key map (kbd "C-c C-f")   'moedict/history-next)
    (define-key map (kbd "o")         'moedict/open-website)
    map)
  "Keymap for Moedict major mode.")

(define-derived-mode moedict-mode nil "萌典"
  "Moedict (萌典) Client for Emacs"
  (set (make-local-variable 'buffer-read-only) t))

(defgroup moedict nil
  "Moedict (萌典) Client for Emacs"
  :prefix "moedict" :link '(url-link "http://github.com/kuanyui/moedict.el"))

(defgroup moedict-faces nil
  "Faces used in `moedict-mode'"
  :group 'moedict :group 'faces)

(defface moedict-title
  '((((class color) (background light)) (:foreground "#ff8700" :bold t :height 1.2))
    (((class color) (background dark)) (:foreground "#ffa722" :bold t :height 1.2)))
  "Face for title. ex:"
  :group 'moedict-faces)

(defface moedict-stroke-count
  '((((class color) (background light)) (:foreground "#787878"))
    (((class color) (background dark)) (:foreground "#c1c1c1")))
  "Face for stroke-count."
  :group 'moedict-faces)

(defface moedict-radical
  '((((class color) (background light)) (:foreground "#ffffff" :background "#a40000"))
    (((class color) (background dark)) (:foreground "#ffffff" :background "#a40000")))
  "Face for character's radical."
  :group 'moedict-faces)

(defface moedict-non-radical-stroke-count
  '((((class color) (background light)) (:inherit moedict-stroke-count))
    (((class color) (background dark)) (:inherit moedict-stroke-count)))
  "Face for non-radical stroke-count."
  :group 'moedict-faces)

(defface moedict-bopomofo
  '((((class color) (background light)) (:foreground "#008700" :background "#d7ff87"))
    (((class color) (background dark)) (:foreground "#a1db00" :background "#5a5a5a")))
  "Face for bopomofo （注音符號）."
  :group 'moedict-faces)

(defface moedict-bopomofo2
  '((((class color) (background light)) (:inherit moedict-bopomofo))
    (((class color) (background dark)) (:inherit moedict-bopomofo)))
  "Face for bopomofo2 （注音二式）."
  :group 'moedict-faces)

(defface moedict-pinyin
  '((((class color) (background light)) (:inherit moedict-bopomofo))
    (((class color) (background dark)) (:inherit moedict-bopomofo)))
  "Face for pinyin （拼音）."
  :group 'moedict-faces)

(defface moedict-type
  '((((class color) (background light)) (:foreground "#ffffd7" :background "#525252"))
    (((class color) (background dark)) (:foreground "#525252" :background "#c1c1c1")))
  "Face for type. ex: [動]、[名]"
  :group 'moedict-faces)

(defface moedict-quote
  '((((class color) (background light)) (:foreground "#ff4ea3" :slant italic))
    (((class color) (background dark)) (:foreground "#ff6fa5" :slant italic)))
  "Face for quote."
  :group 'moedict-faces)

(defface moedict-def
  '((((class color) (background light)) (:foreground "#1f5bff"))
    (((class color) (background dark)) (:foreground "#6faaff")))
  "Face for definitions."
  :group 'moedict-faces)

(defface moedict-example
  '((((class color) (background light)) (:foreground "#525252"))
    (((class color) (background dark)) (:foreground "#cdcdcd")))
  "Face for example. ex: Example"
  :group 'moedict-faces)

(defface moedict-link
  '((((class color) (background light)) (:foreground "#00a775"))
    (((class color) (background dark)) (:foreground "#00d7af")))
  "Face for link. ex:「見...等條」"
  :group 'moedict-faces)

(defface moedict-synonyms
  '((((class color) (background light)) (:foreground "#9a08ff"))
    (((class color) (background dark)) (:foreground "#aa71ff")))
  "Face for synonyms."
  :group 'moedict-faces)

(defface moedict-antonyms
  '((((class color) (background light)) (:foreground "#9a08ff"))
    (((class color) (background dark)) (:foreground "#aa71ff")))
  "Face for antonyms."
  :group 'moedict-faces)

(defface moedict-syn/antonyms-tag
  '((((class color) (background light)) (:foreground "#ffffff" :background "#9a08ff"))
    (((class color) (background dark)) (:foreground "#7008a0" :background "#eeaeff")))
  "Face for syn/antonyms-tag. ex: [同]"
  :group 'moedict-faces)

;; ======================================================
;; Download Dictionary File
;; ======================================================

(defun moedict-check-dictionary-file ()
  "If dictionary file is not existed, download & uncompress it with xz."
  (interactive)
  (if (not (file-exists-p moedict-dictionary-file-path))
      (moedict-message "字典檔似乎尚未下載或解壓縮，請執行 M-x moedict-install-dictionary （可能會花上一段時間）")))

(defun moedict-install-dictionary ()
  (interactive)
  (if (not (executable-find "xz"))
      (moedict-message "您的系統上沒有發現xz這個指令，請安裝後再重試一次。
Command 'xz' not found on your system. Please install it then try again")
    (if (file-exists-p moedict-dictionary-xz-file-path)
        (progn (shell-command (format "xz -kdfvv %s.xz" moedict-dictionary-file-path))
               (moedict-message "字典檔設定完成！您現在已經可以使用 M-x moedict"))
      (moedict-download-dictionary-file-then-uncompress))))

(defun moedict-download-dictionary-file-then-uncompress ()
  (url-copy-file moedict-dictionary-source-url moedict-dictionary-file-path)
  (moedict-install-dictionary))

;; ======================================================
;; Query
;; ======================================================

(defmacro moedict-query (sql-query-string)
  `(progn
     (esqlite-read moedict-dictionary-file-path ,sql-query-string)))

(defmacro moedict-query-with-limit (sql-query-string)
  `(progn
     (esqlite-read moedict-dictionary-file-path
                   (format "%s LIMIT %s" ,sql-query-string ,moedict-candidates-limit))))

(defun moedict-get-candidates-list (string)
  (cl-remove-if
   (lambda (x) (string-prefix-p "{" x))
   (mapcon #'car (moedict-query-with-limit
                  (format "SELECT title FROM entries WHERE title LIKE %s"
                          (esqlite-format-text (concat string "%")))))))

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

(defun moedict-if-a-vocabulary-exactly-exist (vocabulary)
  (moedict-query
   (format "SELECT 1 FROM entries WHERE title = %s;" (esqlite-format-text vocabulary))))

;; ======================================================
;; Low-level & Internal Tools
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
   row))

(defun moedict-mapconcat-with-newline (list)
  "Ignore nil, seperator is \\n."
  (mapconcat #'identity
             (remove-if #'null list)
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
             (moedict-match-positions "「\\(.+?\\)」" link 1)))

     (moedict-mapconcat-with-newline
      (list
       (if def      (format "    %s" (propertize def 'face 'moedict-def)))
       (if example  (format "        %s" (propertize example 'face 'moedict-example)))
       (if quote    (format "        %s" (propertize quote 'face 'moedict-quote)))
       (if link     (format "        %s" link))
       (if synonyms (format "            %s %s" moedict-synonyms-tag (propertize synonyms 'face 'moedict-synonyms)))
       (if antonyms (format "            %s %s" moedict-antonyms-tag (propertize antonyms 'face 'moedict-antonyms)))))))

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
               (t (moedict--render-def))))
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

(defun moedict-lookup-and-show-in-buffer (vocabulary &optional no-push-to-history)
  "Hello"
  (when (stringp vocabulary)
    (moedict-message "查詢中...")
    (if no-push-to-history
        'pass
      (moedict-history-push vocabulary))
    (let ((rendered-result (moedict-render vocabulary)))
      (with-temp-buffer-window moedict-buffer-name nil nil)
      (with-selected-window (get-buffer-window moedict-buffer-name)
        (moedict-mode)
        (let (buffer-read-only)
          (insert rendered-result))
        (goto-char (point-min))))
    (moedict-message "完成～")))

(defun moedict (&optional init-input)
  "查萌典。"
  (interactive)
  (moedict-check-dictionary-file)
  (if (null
       (helm :sources (helm-build-sync-source "[萌典] 請輸入您欲查詢的單字："
			:candidates (lambda () (moedict-get-candidates-list helm-pattern))
			:volatile t
			:candidate-number-limit moedict-candidates-limit
			:action #'moedict-lookup-and-show-in-buffer
			:requires-pattern t)
             :input (or init-input "")
             :buffer moedict-candidate-buffer-name
             :prompt moedict-prompt))
      (moedict-message "找不到結果，取消～")))

(defun moedict/region (begin end)
  "用萌典查詢選取範圍內的文字。"
  (interactive "r")
  (moedict-check-dictionary-file)
  (if (not (region-active-p))
      (moedict-message "請先反白選取您欲查詢的單字後，再執行此命令！")
    (moedict (buffer-substring-no-properties begin end))))

(defun moedict/smart ()
  "功能同 `moedict-lookup-region' ，但會自動檢查目前的選取狀態，
如果處於選取狀態就查詢選取範圍內的字串，否則就直接呼叫 `moedict'"
  (interactive)
  (moedict-check-dictionary-file)
  (if (region-active-p)
      (moedict (buffer-substring-no-properties (region-beginning) (region-end)))
    (moedict)))

(defun moedict/last-vocabulary ()
  "開啟萌典查詢界面，並以目前條目為預設輸入"
  (interactive)
  (moedict-check-dictionary-file)
  (moedict moedict--current-vocabulary))

;; ======================================================
;; Tools for Interactive Commands
;; ======================================================

(defun moedict-point-at-underline-p (&optional point)
  (if (null point) (setq point (point)))
  (let ((face (get-text-property point 'face)))
    (and face (listp face) (or (member '(underline t) face)
                               (equal  '(underline t) face)))))

(defun moedict-try-to-get-vocabulary-at-point ()
  (let ((pos (point))
        begin end)
    (if (moedict-point-at-underline-p)
        ;; [1] if at an underlined vocabulary.
        (progn (if (null (moedict-point-at-underline-p (1- pos)))
                   (setq pos (1+ pos)))
               (buffer-substring-no-properties (previous-property-change pos)
                                               (next-property-change pos)))
      ;; If cursor is not at an underlined vocabulary,
      ;; guess vocabulary according `moedict-punctuations' &
      ;; `moedict-try-to-get-vocabulary-max-length'
      (save-excursion
        (search-backward-regexp moedict-punctuations nil t 1)
        (if (not (bolp)) (right-char 1))
        (setq begin (point))
        (search-forward-regexp moedict-punctuations nil t 1)
        (setq end (1- (point)))
        (if (<= (- end begin) moedict-try-to-get-vocabulary-max-length)
            (format "%s" (buffer-substring-no-properties begin end)) ; [2] got guessed vocabulary according punctuation
          (moedict-try-to-get-single-char-at-point pos)))))) ; [3] Try to get single character at point

(defun moedict-try-to-get-single-char-at-point (&optional pos)
  (if (null pos) (setq pos (point)))
  (if (moedict-if-a-valid-chinese-character (char-after pos))
      (char-to-string (char-after pos))
    ""))

(defun moedict-if-a-valid-chinese-character (char)
  "Not include punctuation."
  (if (characterp char)
      (setq char (char-to-string char)))
  (and (string-match (rx (category chinese)) char)
       (not (string-match moedict-punctuations char))))

;; ======================================================
;; Commands for Keys
;; ======================================================

(defun moedict:enter ()
  (interactive)
  (if (region-active-p)
      (moedict (buffer-substring-no-properties (region-beginning) (region-end)))
    (let ((vocabulary (moedict-try-to-get-vocabulary-at-point)))
      (if (and (moedict-point-at-underline-p)
               (moedict-if-a-vocabulary-exactly-exist vocabulary))
          (moedict-lookup-and-show-in-buffer vocabulary)
        (moedict vocabulary)))))

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

;; ======================================================
;; Misc Commands :: 有的沒的命令
;; ======================================================

(defun moedict/exit ()
  "Kill all moedict buffers."
  (interactive)
  (mapc (lambda (x)
          (when (get-buffer x)
            (bury-buffer x)
            (kill-buffer x)))
        (list moedict-buffer-name
              moedict-history-buffer-name
              moedict-help-buffer-name
              moedict-candidate-buffer-name)))

(defun moedict/open-website ()
  (interactive)
  (if moedict--current-vocabulary
      (browse-url-default-browser (format "https://www.moedict.tw/%s" moedict--current-vocabulary))
    (browse-url-default-browser "https://www.moedict.tw/"))
  (moedict-message "開啟網頁版中..."))

(defun moedict/help ()
  (interactive)
  (with-temp-buffer-window moedict-help-buffer-name nil nil)
  (with-selected-window (get-buffer-window moedict-help-buffer-name)
    (help-mode)
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (insert (with-temp-buffer
                (insert (moedict-get-help-string))
                (org-table-align)
                (buffer-string))
              "\n")))
  (switch-to-buffer-other-window moedict-help-buffer-name))

(defun moedict-get-help-string ()
  (concat
   (propertize "* 萌え萌えキュン的萌典說明書\n" 'face 'bold)
   "以下按鍵可以在萌典的 buffer 中使用：\n
| 函數名稱 | 按鍵 | 描述 |
|----------|------|------|\n"
   (mapconcat
    (lambda (x)
      (format "| %s | %s | %s |"
              (propertize (symbol-name (car x)) 'face 'font-lock-keyword-face) ;command name
              (mapconcat            ;key-binding
               (lambda (a)
                 (propertize (key-description a) 'face 'font-lock-constant-face))
               (where-is-internal (car x) moedict-mode-map) ", ")  ; (key-binding list)
              (cdr x)))                   ;description
    '((moedict/help              . "開啟目前這個說明書")
      (moedict                   . "開啟萌典查詢界面")
      (moedict/exit              . "關掉所有萌典相關視窗跟buffer")
      (moedict:enter             . "智慧動作鍵（自動猜測您想查詢的東西）")
      (moedict/last-vocabulary   . "開啟萌典查詢界面，並以目前條目為預設輸入")
      (moedict/history-show-list . "開啟查詢歷史清單")
      (moedict/history-next      . "跳到下一查詢歷史")
      (moedict/history-previous  . "跳到上一查詢歷史")
      (moedict/history-clean     . "清除查詢歷史")
      (moedict:tab               . "往下跳到連結")
      (moedict:shift+tab         . "往上跳到連結")
      (moedict/open-website      . "開啟目前條目的網頁版界面"))
    "\n")))

;; ======================================================
;; History :: 查詢歷史
;; ======================================================

(defun moedict/history-show-list ()
  (interactive)
  (if (= (length moedict--history) 0)
      (moedict-message "目前歷史紀錄是空的喔")
    (if (null (helm :sources (helm-build-sync-source "請選擇單字："
                               :candidates moedict--history
                               :volatile t
                               :action (lambda (x) (moedict-lookup-and-show-in-buffer x)
                                         (kill-buffer moedict-history-buffer-name)))
                    :buffer moedict-history-buffer-name
                    :prompt moedict-prompt))
        (moedict-message "取消動作！"))))

(defun moedict/history-previous ()
  (interactive)
  (if (= (length moedict--history) 0)
      (moedict-message "目前歷史紀錄是空的喔")
    (let ((previous (moedict-history-get-previous-vocabulary)))
      (if previous
          (progn (moedict-lookup-and-show-in-buffer previous :no-push-history)
                 (moedict-set-current-vocabulary-to previous))
        (moedict-message "沒有更舊的歷史了！")))))

(defun moedict/history-next ()
  (interactive)
  (if (= (length moedict--history) 0)
      (moedict-message "目前歷史紀錄是空的喔")
    (let ((next (moedict-history-get-next-vocabulary)))
      (if next
          (progn (moedict-lookup-and-show-in-buffer next :no-push-history)
                 (moedict-set-current-vocabulary-to next))
        (moedict-message "已經是最新的項目！")))))

(defun moedict/history-clean ()
  (interactive)
  (if (y-or-n-p "確定要清除歷史紀錄嗎？")
      (progn (setq moedict--history (list (or moedict--current-vocabulary "")))
             (moedict-message "清除啦～"))
    (moedict-message "不清除～")))

(defun moedict-history-push (vocabulary)
  "1. Move `moedict--current-vocabulary' to the first item in `moedict--history',
2. Remove the existed same vocabulary in `moedict--history',
3. Push the VOCABULARY into `moedict--history' as the first item"
  (setq moedict--history (delete moedict--current-vocabulary moedict--history))
  (push moedict--current-vocabulary moedict--history)
  (setq moedict--current-vocabulary vocabulary)
  (setq moedict--history (delete vocabulary moedict--history))
  (push vocabulary moedict--history))

(defun moedict-history-get-previous-vocabulary ()
  "Get previous vocabulary.
Return value is string or nil"
  (cadr (member moedict--current-vocabulary moedict--history)))

(defun moedict-history-get-next-vocabulary ()
  "Get next vocabulary in history.
Return value is string or nil"
  (let* ((n (1- (- (length moedict--history)
                   (length (member moedict--current-vocabulary moedict--history))))))
    (if (>= n 0)
        (nth n moedict--history)
      nil)))

(defun moedict-set-current-vocabulary-to (value)
  (setq moedict--current-vocabulary value))

(provide 'moedict)

;; The ultimate answer of life, Universe, and everything is `42',
;; the ultimate answer of Taiwan is `689'.

;;; moedict.el ends here at line 689.
