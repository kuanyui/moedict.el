(defun moedict-retrieve-json (word)
  "Get JSON and return the parsed list of the word."
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://www.moedict.tw/uni/%s.json" word))
    (set-buffer-multibyte t)
    (re-search-backward "\n\n")
    (delete-region (point-min) (point))
    (json-read-from-string (buffer-string))))

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
  '((((class color)) (:foreground "#c0c0c0" :background nil)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-radical
  '((((class color)) (:foreground "#ffffff" :background "#c0c0c0")))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-non-radical-stroke-count
  '((((class color)) (:inherit moedict-stroke-count)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-bopomofo
  '((((class color)) (:foreground "#6c0099" :background nil)))
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
  '((((class color)) (:foreground "#ffffd7" :background "#525252" :bold t)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-quote
  '((((class color)) (:foreground "#ff4ea3" :background nil :italic t)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-def
  '((((class color)) (:foreground "#5f87af" :background nil)))
  "This comment is necessary"
  :group 'moedict-faces)

(defface moedict-example
  '((((class color)) (:foreground "#ff8700" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-link
  '((((class color)) (:foreground "#00af00" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-synonyms
  '((((class color)) (:foreground "#00d7af" :background nil)))
  "This comment is necessary"
  :group 'moedict)

(defface moedict-antonyms
  '((((class color)) (:foreground "#cc0000" :background nil)))
  "This comment is necessary"
  :group 'moedict)

;; =================================================================
;; defface結束
;; =================================================================

(defun vector-to-list (input)
  (mapcar (lambda (x) x) input))

(defun moedict-run-definitions (definitions)
  "輸入為vector。此function會把vector轉換成list後，用 dolist 一項項送給 moedict-run-definition"
  (let* (DEFINITIONS last-type) ;DEFINITIONS是用來存整個definitions的cdr的最後輸出
    (dolist (x (vector-to-list definitions))
      (moedict-run-definition x))
    (insert DEFINITIONS)))

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
            (setq DEFINITIONS (format "%s\n\n  %s" DEFINITIONS type)))))

    (when (setq def (cdr (assoc 'def definition)))
      (progn (put-text-property 0 (length def) 'face 'moedict-def def)
             (setq DEFINITIONS (format "%s\n\n    %s" DEFINITIONS def))))

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
