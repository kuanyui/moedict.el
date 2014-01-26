(require 'json)
(require 'url)

(defun moedict-retrieve-json (word)
  "Get JSON and return the parsed list of the word."
    (with-current-buffer
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (url-retrieve-synchronously
     (format "https://www.moedict.tw/uni/%s.json" word))
    (set-buffer-multibyte t)
    (re-search-backward "\n\n")
    (delete-region (point-min) (point))
    (json-read-from-string (buffer-string)))))
  ;; 上面沒問題了不要再改了

;; (defgroup markdown nil
;;   "Major mode for editing text files in Markdown format."
;;   :prefix "markdown-"
;;   :group 'wp
;;   :link '(url-link "http://jblevins.org/projects/markdown-mode/"))

(defgroup moedict-faces nil
  "Faces used in Moedict Mode"
  :group 'moedict
  :group 'faces)

(defface moedict-title
  '((((class color)) (:foreground "#ff8700" :background nil :bold t)))
  "字的title"
  :group 'moedict-faces)

(defface moedict-stroke-count
  '((((class color)) (:foreground "#787878" :background nil)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-radical
  '((((class color)) (:foreground "#ffffff" :background "#787878")))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-non-radical-stroke-count
  '((((class color)) (:inherit moedict-stroke-count)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-bopomofo
  '((((class color)) (:foreground "#008700" :background "#d7ff87")))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-bopomofo2
  '((((class color)) (:inherit moedict-bopomofo)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-pinyin
  '((((class color)) (:inherit moedict-bopomofo)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-type
  '((((class color)) (:foreground "#ffffd7" :background "#525252")))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-quote
  '((((class color)) (:foreground "#ff4ea3" :background nil :italic t)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-def
  '((((class color)) (:foreground "#1f5bff" :background nil)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-example
  '((((class color)) (:foreground "#525252" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-link
  '((((class color)) (:foreground "#00d7af" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-synonyms
  '((((class color)) (:foreground "#9a08ff" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-antonyms
  '((((class color)) (:foreground "#9a08ff" :background nil)))
  "This comment is necessary"
  :group 'moedict)

;; =================================================================
;; defface結束
;; =================================================================
(require 'json)
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
  "[a b c] => (a b c)"
  (mapcar (lambda (x) x) input))


(defun moedict-run-parser (word)
  (let (FINALE)
;;    (moedict-run-title (moedict-retrieve-json word))
    (moedict-run-title word)            ;測試用，用variable
    (insert (format "%s" FINALE))))

(defun moedict-run-title (parsed-json)
  ""
  (let (title radical stroke_count non_radical_stroke_count heteronyms)
    (when (setq title (cdr (assoc 'title parsed-json)))
      (progn (put-text-property 0 (length title) 'face 'moedict-title title)))
    (if (setq radical (cdr (assoc 'radical parsed-json)))
        (progn (put-text-property 0 (length radical) 'face 'moedict-radical radical)
               (setq FINALE (format "%s" radical))
               (setq stroke_count (format "%s" (cdr (assoc 'stroke_count parsed-json))))
               (put-text-property 0 (length stroke_count) 'face 'moedict-stroke-count stroke_count)
               (setq FINALE (format "%s + %s" FINALE stroke_count))
               (setq non_radical_stroke_count (format "%s" (cdr (assoc 'non_radical_stroke_count parsed-json))))
               (put-text-property 0 (length non_radical_stroke_count) 'face 'moedict-stroke-count non_radical_stroke_count)
               (setq FINALE (format "%s = %s" FINALE non_radical_stroke_count))))
    (when (setq heteronyms (cdr (assoc 'heteronyms parsed-json)))
      (setq FINALE (format "%s" (concat FINALE
                                        "\n\n"
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
          (format "\n%s%s" HETERONYM
                  (moedict-run-definitions (cdr (assoc 'definitions heteronym)))))
    (setq HETERONYMS (format "%s" (concat HETERONYMS "\n" HETERONYM)))))

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
    ;; quote的cdr是是vector
    (when (setq quote (cdr (assoc 'quote definition)))
      (dolist (x (vector-to-list quote))
        (put-text-property 0 (length x) 'face 'moedict-quote x)
        (setq DEFINITIONS (format "%s\n        %s" DEFINITIONS x))))
    (when (setq synonyms (cdr (assoc 'synonyms definition)))
      (progn (put-text-property 0 (length synonyms) 'face 'moedict-synonyms synonyms)
             (setq DEFINITIONS (format "%s\n            %s" DEFINITIONS synonyms))))
    (when (setq antonyms (cdr (assoc 'antonyms definition)))
      (progn (put-text-property 0 (length antonyms) 'face 'moedict-antonyms antonyms)
             (setq DEFINITIONS (format "%s\n            %s" DEFINITIONS antonyms))))
    (when (setq link (cdr (assoc 'link definition)))
      (progn (put-text-property 0 (length link) 'face 'moedict-link link)
             (setq DEFINITIONS (format "%s\n            %s" DEFINITIONS link))))))
