;;; moedict.el --- Looking up Chinese vocabulary via moedict API.

;; Author: kuanyui <azazabc123@gmail.com>
;; URL: https://github.com/kuanyui/moedict.el
;; Created: 20140130
;; Keywords: dictionary
;; Compatibility: Emacs 24.3 and above

;; This file is NOT a part of GNU Emacs

;;; License:

;; Revised BSD License
;;
;; Copyright (c) 2014, kuanyui
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <organization> nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL KUANYUI BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Screenshot and more information:
;;     http://github.com/kuanyui/moedict.el
;;
;; Moedict-mode is a major mode for looking up Traditional Chinese
;; vocabulary in Emacs via moedict API (萌典, http://moedict.tw/).

;; == Installation ==
;;
;;       (add-to-list 'load-path "~/path/to/moedict/")
;;       (require 'moedict)
;;       (global-set-key (kbd "C-c m l") 'moedict-lookup)
;;       (global-set-key (kbd "C-c m r") 'moedict-lookup-region)
;;
;; If you use Un*x-like OS and `curl` is available, we suggest using
;; it instead of `url.el' to avoid some strange GnuTLS problems:
;;
;;       (setq moedict-use-curl t)
;;
;; == Usage ==
;;
;; `moedict-lookup': Look up vocabulary via minibuffer.
;; `moedict-lookup-region': Look up vocabulary under the selected region.
;;
;; If under *moedict* buffer:
;;
;; ================= Basic =================
;; l             `moedict-lookup'
;; r             `moedict-lookup-region'
;; ================ History ================
;; C-c C-b , [   `moedict-history-backward'
;; C-c C-f , ]   `moedict-history-forward'
;; C-c D         `moedict-history-clean'
;; ============== Navigation ===============
;; TAB           Move cursor to next word.
;; Shift-TAB     Move cursor to previous word.
;; ================= Misc ==================
;; o             `moedict-open-website-of-this-entry'
;; q             Close/bury the buffer.
;; h             Display help.

;;; Code:

(require 'json)
(require 'url)
;; 該如何判斷GnuTLS -19 error出現？且出現的話重試？（懶得鳥你，用curl了）
;;

(defcustom moedict-mode-hook nil
  "Normal hook run when entering moedict-mode."
  :type 'hook
  :group 'moedict)

(defvar moedict-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "l") 'moedict-lookup)
    (define-key map (kbd "r") 'moedict-lookup-region)
    (define-key map (kbd "<tab>") 'moedict-cursor-forward-word)
    (define-key map (kbd "<backtab>") 'moedict-cursor-backward-word)
    (define-key map (kbd "o") 'moedict-open-website-of-this-entry)
    (define-key map (kbd "C-c C-b") 'moedict-history-backward)
    (define-key map (kbd "C-c C-f") 'moedict-history-forward)
    (define-key map (kbd "[") 'moedict-history-backward)
    (define-key map (kbd "]") 'moedict-history-forward)
    (define-key map (kbd "C-c D") 'moedict-history-clean)
    map)
  "Keymap for Moedict major mode.")

;; moedict-history格式（數字為moedict-history-n、即nthcdr所用的值）：
;; ("最新0" "1" "2" "3" ...)
(defvar moedict-history nil
  "History list of current moedict buffer.")
(defvar moedict-history-n 0
  "Record current position in moedict history list")
;; [FIXME] 以上兩者應該改成*moedict*內的local variable，但我做不出來，好像每次 (with-temp-buffer-window ...)都會改掉local variable.暫時先定義一個function讓人可以清除history
(defun moedict-history-clean ()
  "Clear all history of moedict."
  (interactive)
  (if (yes-or-no-p "Really clear all history?")
      (progn
        (setq moedict-history nil)
        (setq moedict-history-n 0)
        (message "Done."))
    (message "Canceled.")))

(defvar moedict-use-curl nil
  "If non-nil, call shell command `curl` to fetch json data.
If nil, use `url.el' to do this.

If `curl` installed on your system, it's recommended to setq this to t,
because `url-retrieve' occurs GnuTLS error very often in our some testing.")

(if (and (executable-find "curl")
         (not (equal system-type 'ms-dos))
         (not (equal system-type 'windows-nt)))
    (setq moedict-use-curl t))

(define-derived-mode moedict-mode nil "MoeDict"
  "Major mode for looking up Chinese vocabulary via Moedict API."
  (set (make-local-variable 'buffer-read-only) t))

(defgroup moedict nil
  "Major mode for looking up Chinese vocabulary via Moedict API."
  :prefix "moedict-"
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

;; ===========================================================
(defvar moedict-characters-pattern "[^ㄅㄆㄇㄈㄉㄊㄋㄌㄍㄎㄏㄐㄑㄒㄓㄔㄕㄖㄗㄘㄙㄧㄨㄩㄚㄛㄜㄝㄞㄟㄠㄡㄢㄣㄤㄥㄦˊˇˋ。，！？；：．「」『』（）、【】《》〈〉— [:ascii:]āáǎàēéěèīíǐìōóǒòūúǔù]+")

(defun moedict-cursor-forward-word ()
  (interactive)
  (if (not (re-search-forward moedict-characters-pattern nil t))
      (message "End of buffer.")))

(defun moedict-cursor-backward-word ()
  (interactive)
  (backward-word 2)
  (re-search-forward moedict-characters-pattern nil t))

(defun moedict-history-backward ()
  (interactive)
  (if (not (equal (buffer-name) "*moedict*"))
      (message "Please run this command in *moedict* buffer.")
    (if (or (equal (length moedict-history) (1+ moedict-history-n))
            (<= (length moedict-history) 1))
        (message "There's no older item in history.")
      (let (buffer-read-only)         ;Unlock buffer-read-only
        (delete-region (point-min) (point-max))
        (setq moedict-history-n (1+ moedict-history-n))
        (insert (nth moedict-history-n moedict-history))
        (beginning-of-buffer)
        (message "Backward!")))))

(defun moedict-history-forward ()
  (interactive)
  (if (not (equal (buffer-name) "*moedict*"))
      (message "Please run this command in *moedict* buffer.")
    (if (<= moedict-history-n 0)
        (message "There's no newer item in history.")
      (let (buffer-read-only)
        (delete-region (point-min) (point-max))
        (setq moedict-history-n (1- moedict-history-n))
        (insert (nth moedict-history-n moedict-history))
        (beginning-of-buffer)
        (message "Forward!")))))

(defun moedict-open-website-of-this-entry ()
  (interactive)
  (if moedict-history
      (progn (string-match "^\\([^ ]+\\)" (car moedict-history))
             (browse-url-default-browser (concat "https://www.moedict.tw/" (match-string-no-properties 1 (car moedict-history)))))
    (message "歷史紀錄是空的欸；請先查詢任一字詞。")))

(defun moedict-lookup (&optional begin end)
  "Look up Chinese vocabulary with moedict."
  (interactive "r")
  (let* ((user-input (if mark-active
                         (buffer-substring-no-properties begin end)
                       (read-from-minibuffer "萌典：")))
        (parsed-finale (moedict-run-parser (format "%s" user-input))))
    (if (equal parsed-finale 'failed)
        (message "查詢失敗，可能無此字詞。")
      (progn (with-temp-buffer-window "*moedict*" nil nil)
             (if (not (equal (buffer-name) "*moedict*"))
                 (switch-to-buffer-other-window "*moedict*"))
             (if (not (equal major-mode 'moedict-mode))
                 (moedict-mode))
             ;; 每次有新查詢就把forward的資料清空，只留下cdr
             (setq moedict-history
                   (nthcdr moedict-history-n moedict-history))
             (setq moedict-history-n 0)
             (push parsed-finale moedict-history)
             (let (buffer-read-only)
               (insert parsed-finale)
               (goto-char (point-min)))))))
;; [FIXME] 加上沒有網路時的curl錯誤訊息判斷？「Could not resolve host」

(defun moedict-lookup-region (begin end)
  (interactive "r")
  (if (region-active-p)
      (let* ((region-input (format "%s" (buffer-substring-no-properties begin end)))
             (parsed-finale (moedict-run-parser region-input)))
        (if (equal parsed-finale 'failed)
            (message "查詢失敗，可能無此字詞。")
          (progn (with-temp-buffer-window "*moedict*" nil nil)
                 (if (not (equal (buffer-name) "*moedict*"))
                     (switch-to-buffer-other-window "*moedict*"))
                 (if (not (equal major-mode 'moedict-mode))
                     (moedict-mode))
                 (setq moedict-history
                       (nthcdr moedict-history-n moedict-history))
                 (setq moedict-history-n 0)
                 (push parsed-finale moedict-history)
                 (let (buffer-read-only)
                   (insert parsed-finale)
                   (goto-char (point-min))))))
    (let ((mark-even-if-inactive t))
      (set-mark-command nil)
      (message (concat "使用方向鍵移動游標來選取欲查詢的字詞，完成後請再"
                       (if (eq major-mode 'moedict)
                           "按一次 r 以查詢該字詞"
                         "執行一次 `moedict-lookup-region' 以查詢該字詞")))
      )))
;; [FIXME] 自動改變按鍵指示

(defun moedict-retrieve-json (word)
  "Get JSON and return the parsed list of the word.
When variable `moedict-use-curl' is t or non-nil, call shell command `curl'
instead of `url.el' to avoid some strang error when fetching json data."
    (if (null moedict-use-curl)         ;判斷是否使用curl，nil為不使用
        (with-current-buffer
            (url-retrieve-synchronously
             (format "https://www.moedict.tw/uni/%s.json" word))
          (set-buffer-multibyte t)
          (if (string-match "404 Not Found" (buffer-string))
              'failed
            (progn
              (re-search-backward "\n\n")
              (delete-region (point-min) (point))
              (json-read-from-string (buffer-string)))))
    (let (JSON-DATA)
      (setq JSON-DATA
            (shell-command-to-string
             (format "curl https://www.moedict.tw/uni/%s.json 2>/dev/null" word)))
      (if (string-match "404 Not Found" JSON-DATA)
          'failed
        (json-read-from-string JSON-DATA)))))

(defun vector-to-list (input)
  "A tools to covert vector to list, hence `dolist' available.
e.g. [a b c] => (a b c)"
  (mapcar (lambda (x) x) input))

(defun moedict-run-parser (word)
  "透過 moedict-retrieve-json 抓出資料後，此function開始處理這堆玩意。"
  (let (FINALE JSON-DATA)
    (setq JSON-DATA (moedict-retrieve-json word))
    (if (equal JSON-DATA 'failed)
        'failed
      (progn
        (moedict-run-title JSON-DATA)
        (let (buffer-read-only)
          (format "%s" FINALE))))))

(defun moedict-run-title (parsed-json)
  "Do not use this seperately.
處理title、radical + stroke-count，然後把 heteronyms 送給 moedict-run-heteronyms"
  (let (title radical stroke_count non_radical_stroke_count heteronyms)
    (when (setq title (cdr (assoc 'title parsed-json)))
      (progn (put-text-property 0 (length title) 'face 'moedict-title title)))
    (if (setq radical (cdr (assoc 'radical parsed-json)))
        (progn (put-text-property 0 (length radical) 'face 'moedict-radical radical)
               (setq FINALE (format "%s" radical))

               (setq non_radical_stroke_count (format "%s" (cdr (assoc 'non_radical_stroke_count parsed-json))))
               (put-text-property 0 (length non_radical_stroke_count) 'face 'moedict-stroke-count non_radical_stroke_count)
               (setq FINALE (format "%s + %s" FINALE non_radical_stroke_count))

               (setq stroke_count (format "%s" (cdr (assoc 'stroke_count parsed-json))))
               (put-text-property 0 (length stroke_count) 'face 'moedict-stroke-count stroke_count)
               (setq FINALE (format "%s = %s\n\n" FINALE stroke_count))))

    (when (setq heteronyms (cdr (assoc 'heteronyms parsed-json)))
      (setq FINALE (format "%s" (concat FINALE
                                        (moedict-run-heteronyms heteronyms)))))))

(defun moedict-run-heteronyms (heteronyms)
  "Do not use this seperately.
輸入為heteronyms的cdr (形式是vector)。此function會把vector轉換成list後，用dolist一項項送給moedict-run-heteronym"
  (let (HETERONYMS)
      (dolist (x (vector-to-list heteronyms))
        (moedict-run-heteronym x))
      (format "%s" HETERONYMS)))

(defun moedict-run-heteronym (heteronym)
  "Do not use this seperately.
輸入為heteronyms的cdr中的小項目（單個heteronym），為list，如((pinyin . liao) (definitions . ...))
因為輸出存在 HETERONYMS，請透過moedict-run-heteronyms來呼叫此function"
  (let (bopomofo pinyin bopomofo2 HETERONYM)
    (setq HETERONYM (format "%s" (concat HETERONYM title))) ;;總之先加上title
    (when (setq bopomofo (cdr (assoc 'bopomofo heteronym)))
      (progn (put-text-property 0 (length bopomofo) 'face 'moedict-bopomofo bopomofo)
             (setq HETERONYM (format "%s %s" HETERONYM bopomofo))))
    (when (setq pinyin (cdr (assoc 'pinyin heteronym)))
      (progn (put-text-property 0 (length pinyin) 'face 'moedict-pinyin pinyin)
             (setq HETERONYM (format "%s %s" HETERONYM pinyin))))
    (when (setq bopomofo2 (cdr (assoc 'bopomofo2 heteronym)))
      (progn (put-text-property 0 (length bopomofo2) 'face 'moedict-bopomofo2 bopomofo2)
             (setq HETERONYM (format "%s %s" HETERONYM bopomofo2))))
    (setq HETERONYM
          (format "%s%s" HETERONYM
                  (moedict-run-definitions (cdr (assoc 'definitions heteronym)))))
    (setq HETERONYMS (format "%s\n\n" (concat HETERONYMS HETERONYM)))))

(defun moedict-run-definitions (definitions)
  "Do not use this seperately.
輸入為vector(definitions的cdr)。此function會把vector轉換成list後，用 dolist 一項項送給 moedict-run-definition，最後得到的結果全部存到 DEFINITIONS 集合起來"
  (let (DEFINITIONS last-type) ;DEFINITIONS是用來存整個definitions的cdr的最後輸出
    (dolist (x (vector-to-list definitions))
      (moedict-run-definition x))
    (format "%s" DEFINITIONS)))

(defun moedict-run-definition (definition)
  "Do not use this seperately.
輸入需為一個list，如:((type . \"名\") (def . \"羊\"))"
  (let (type def example quote synonyms antonyms link)
  ;; 如果type跟上個(last-type)重複，就不要再次插入type
    (when (setq type (cdr (assoc 'type definition)))
      (if (not (equal type last-type)) ;如果type跟上一個不同（才需要放type）
          (progn
            (setq last-type type)       ;更新last-type
            ;; 因為沒處理過的last-type已經setq好了，就可以大膽改type的顏色和值，不用擔心之後equal失敗問題
            (setq type (format "[%s]" type))
            (put-text-property 0 (length type) 'face 'moedict-type type)
            (setq DEFINITIONS (format "%s" (concat DEFINITIONS "\n\n " type))))))
    (if (and (setq def (cdr (assoc 'def definition)))
             (> (length def) 0))
        ;; 不然一個 heteronym 沒有def時，link會多出不必要的空行，例如「混」
        (progn (put-text-property 0 (length def) 'face 'moedict-def def)
               (setq DEFINITIONS (format "%s" (concat DEFINITIONS "\n\n    " def))))
      (setq DEFINITIONS (format "%s" (concat DEFINITIONS "\n"))))

    ;; example的cdr是vector
    (when (setq example (cdr (assoc 'example definition)))
      (dolist (x (vector-to-list example))
        (put-text-property 0 (length x) 'face 'moedict-example x)
        (setq DEFINITIONS (format "%s\n        %s" DEFINITIONS x))))
    ;; quote的cdr是vector
    (when (setq quote (cdr (assoc 'quote definition)))
      (dolist (x (vector-to-list quote))
        (put-text-property 0 (length x) 'face 'moedict-quote x)
        (setq DEFINITIONS (format "%s\n        %s" DEFINITIONS x))))
    (if (setq synonyms (cdr (assoc 'synonyms definition)))
        (progn (setq synonyms(concat
                               (propertize "同" 'face 'moedict-syn/antonyms-tag) " " synonyms))
               (put-text-property 2 (length synonyms) 'face 'moedict-synonyms synonyms)
               (setq DEFINITIONS (format "%s\n            %s" DEFINITIONS synonyms))))
    (if (setq antonyms (cdr (assoc 'antonyms definition)))
        (progn (setq antonyms (concat
                               (propertize "反" 'face 'moedict-syn/antonyms-tag) " " antonyms))
               (put-text-property 2 (length antonyms) 'face 'moedict-antonyms antonyms)
               (setq DEFINITIONS (format "%s\n            %s" DEFINITIONS antonyms))))
    ;; link的cdr是vector
    (when (setq link (cdr (assoc 'link definition)))
      (dolist (x (vector-to-list link))
        (put-text-property 0 (length x) 'face 'moedict-link x)
        (setq DEFINITIONS (format "%s\n        %s" DEFINITIONS x))))))

(provide 'moedict)
