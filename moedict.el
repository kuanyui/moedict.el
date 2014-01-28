(require 'json)
(require 'url)

;;(defun moedict-retrieve-json (word)
;;  "Get JSON and return the parsed list of the word."
;;    (with-current-buffer
;;  (let ((url-request-method "GET")
;;        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
;;    (url-retrieve-synchronously
;;     (format "https://www.moedict.tw/uni/%s.json" word))
;;    (set-buffer-multibyte t)
;;    (re-search-backward "\n\n")
;;    (delete-region (point-min) (point))
;;    (json-read-from-string (buffer-string)))))
  ;; 上面沒問題了不要再改了

(defcustom moedict-mode-hook nil
  "Normal hook run when entering moedict-mode."
  :type 'hook
  :group nil)

(defvar moedict-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "l") 'moedict-lookup)
    (define-key map (kbd "r") 'moedict-lookup-region)
    map)
  "Keymap for Moedict major mode.")

;; (defvar moedict-lookup-ring nil
;;   "History of moedict-lookup.")

(define-derived-mode moedict-mode nil "MoeDict"
  "Major mode for looking up Chinese vocabulary via Moedict API."
  (set (make-local-variable 'buffer-read-only) t))

(defgroup moedict nil
  "Major mode for looking up Chinese vocabulary via Moedict API."
  :prefix "moedict-"
  :link '(url-link "http://github.com/kuanyui/moedict.el"))

(defun moedict-lookup ()
  (interactive)
  (let ((user-input (read-from-minibuffer "萌典：")))
    (if (stringp user-input)
        (with-temp-buffer-window "*moedict*" nil nil
                                 (let (buffer-read-only)
                                   (insert (moedict-run-parser user-input)))
                                 (moedict-mode)
                                 (switch-to-buffer-other-window "*moedict*"))
      (message "Input should be a string."))))

(defun moedict-lookup-region (begin end)
  (interactive "r")
  (if (region-active-p)
      (let* ((user-input (format "%s" (buffer-substring-no-properties begin end))))
        (with-temp-buffer-window "*moedict*" nil nil
                                 (let (buffer-read-only)
                                   (insert (moedict-run-parser user-input)))
                                 (moedict-mode)))
    (let* ((mark-even-if-inactive t))
      (set-mark-command nil)
      (message "r again to finish."))))

(defgroup moedict-faces nil
  "Faces used in Moedict-mode"
  :group 'moedict
  :group 'faces)

(defface moedict-title
  '((((class color)) (:foreground "#ff8700" :background nil :bold t)))
  "Face for title. ex:"
  :group 'moedict-faces)

(defface moedict-stroke-count
  '((((class color)) (:foreground "#787878" :background nil)))
  "Face for stroke-count."
  :group 'moedict-faces)

(defface moedict-radical
  '((((class color)) (:foreground "#ffffff" :background "#787878")))
  "Face for character's radical."
  :group 'moedict-faces)

(defface moedict-non-radical-stroke-count
  '((((class color)) (:inherit moedict-stroke-count)))
  "Face for non-radical stroke-count."
  :group 'moedict-faces)

(defface moedict-bopomofo
  '((((class color)) (:foreground "#008700" :background "#d7ff87")))
  "Face for bopomofo （注音符號）."
  :group 'moedict-faces)

(defface moedict-bopomofo2
  '((((class color)) (:inherit moedict-bopomofo)))
  "Face for bopomofo2 （注音二式）."
  :group 'moedict-faces)

(defface moedict-pinyin
  '((((class color)) (:inherit moedict-bopomofo)))
  "Face for pinyin （拼音）."
  :group 'moedict-faces)

(defface moedict-type
  '((((class color)) (:foreground "#ffffd7" :background "#525252")))
  "Face for type. ex: [動]、[名]"
  :group 'moedict-faces)

(defface moedict-quote
  '((((class color)) (:foreground "#ff4ea3" :background nil :slant italic)))
  "Face for quote."
  :group 'moedict-faces)

(defface moedict-def
  '((((class color)) (:foreground "#1f5bff" :background nil)))
  "Face for definitions."
  :group 'moedict-faces)

(defface moedict-example
  '((((class color)) (:foreground "#525252" :background nil)))
  "Face for example. ex: Example"
  :group 'moedict)

(defface moedict-link
  '((((class color)) (:foreground "#00d7af" :background nil)))
  "Face for link. ex:「見...等條」"
  :group 'moedict)

(defface moedict-synonyms
  '((((class color)) (:foreground "#9a08ff" :background nil)))
  "Face for synonyms."
  :group 'moedict)

(defface moedict-antonyms
  '((((class color)) (:foreground "#9a08ff" :background nil)))
  "Face for antonyms."
  :group 'moedict)

(defface moedict-syn/antonyms-tag
  '((((class color)) (:foreground "#ffffff" :background "#9a08ff")))
  "Face for syn/antonyms-tag. ex: [同]"
  :group 'moedict)



;; =================================================================
;; defface結束
;; =======================================================Face for defface結束)
(require 'url)

(defun moedict-retrieve-json (word)
  "Get JSON and return the parsed list of the word."
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://www.moedict.tw/uni/%s.json" word))
    (set-buffer-multibyte t)
    (re-search-backward "\n\n")
    (delete-region (point-min) (point))
    (json-read-from-string (buffer-string))))

(defun vector-to-list (input)
  "A tools to covert vector to list, hence `dolist' available.
e.g. [a b c] => (a b c)"
  (mapcar (lambda (x) x) input))

;;  (with-output-to-temp-buffer)
;;  (with-temp-buffer-window)
;;  (switch-to-buffer)
;;  (set-buffer)

(defun moedict-run-parser (word)
  "目前暫時會insert，要不要改成不會insert?已改掉。"
  (let (FINALE)
    (moedict-run-title (moedict-retrieve-json word))
;;    (moedict-run-title word)            ;測試用，用variable
    (let (buffer-read-only)
      (format "%s" FINALE))))

(defun moedict-run-title (parsed-json)
  ""
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
  "輸入為heteronyms的cdr (形式是vector)。此function會把vector轉換成list後，用dolist一項項送給moedict-run-heteronym"
  (let (HETERONYMS)
      (dolist (x (vector-to-list heteronyms))
        (moedict-run-heteronym x))
      (format "%s" HETERONYMS)))

(defun moedict-run-heteronym (heteronym)
  "輸入為heteronyms的cdr中的小項目（單個heteronym），為list，如((pinyin . liao) (definitions . ...))
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
          (format "%s%s\n\n" HETERONYM
                  (moedict-run-definitions (cdr (assoc 'definitions heteronym)))))
    (setq HETERONYMS (format "%s" (concat HETERONYMS HETERONYM)))))

(defun moedict-run-definitions (definitions)
  "輸入為vector(definitions的cdr)。此function會把vector轉換成list後，用 dolist 一項項送給 moedict-run-definition"
  (let (DEFINITIONS last-type) ;DEFINITIONS是用來存整個definitions的cdr的最後輸出
    (dolist (x (vector-to-list definitions))
      (moedict-run-definition x))
    (format "%s" DEFINITIONS)))

(defun moedict-run-definition (definition)
  "輸入需為一個list，如:((type . \"名\") (def . \"羊\"))"
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
    (when (setq def (cdr (assoc 'def definition)))
      (progn (put-text-property 0 (length def) 'face 'moedict-def def)
             (setq DEFINITIONS (format "%s" (concat DEFINITIONS "\n\n    " def)))))
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
        (setq DEFINITIONS (format "%s     %s" DEFINITIONS x))))))
