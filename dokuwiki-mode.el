(defgroup dokuwiki nil
  "Guide key bidings."
  :group 'text
  :prefix "dokuwiki"
  :tag "DokuWiki"
  :link '(url-link "https://www.dokuwiki.org/dokuwiki"))

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-mode-hook nil
  "dokuwiki-mode-hook.")

(defvar dokuwiki-smiley-list
  '("8-)" "8-O" ":-(" ":-)" "=) " ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME")
  "Smiley list in DokuWiki.")

;;;; Faces
(defface dokuwiki-code '((t (:inherit shadow)))
  "DokuWiki face for code."
  :group 'dokuwiki)

(defface dokuwiki-list '((t (:inherit font-lock-type-face)))
  "DokuWiki face for list."
  :group 'dokuwiki)

(defface dokuwiki-verbatim '((t (:inherit shadow)))
  "DokuWiki face for text as is."
  :group 'dokuwiki)

(defface dokuwiki-headline-1 '((t (:inherit outline-1)))
  "DokuWiki face for level 1 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-2 '((t (:inherit outline-2)))
  "DokuWiki face for level 2 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-3 '((t (:inherit outline-3)))
  "DokuWiki face for level 3 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-4 '((t (:inherit outline-4)))
  "DokuWiki face for level 4 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-5 '((t (:inherit outline-5)))
  "DokuWiki face for level 5 headline."
  :group 'dokuwiki)

(defface dokuwiki-link '((t (:inherit link)))
  "DokuWiki face for link."
  :group 'dokuwiki)

(defface dokuwiki-smiley '((t (:inherit font-lock-constant-face)))
  "DokuWiki face for smiley."
  :group 'dokuwiki)

(defvar dokuwiki-font-lock-keywords
  `(
   ;; bold
   ("\\*\\*.+?\\*\\*" (0 'bold append))
   ;; italic
   ("//.+?//" . (0 'italic append))
   ;; underline
   ("__.+?__" . (0 'underline append))
   ;; monospace
   ("''.+?''" (0 'dokuwiki-code append))
   ;; verbatim
   ("%%.+?%%" (0 'dokuwiki-code t))
   ;; headline
   (" ?======.+======[ \t]*$" (0 'dokuwiki-headline-1))
   (" ?=====.+=====[ \t]*$" (0 'dokuwiki-headline-2))
   (" ?====.+====[ \t]*$" (0 'dokuwiki-headline-3))
   (" ?===.+===[ \t]*$" (0 'dokuwiki-headline-4))
   (" ?==.+==[ \t]*$" (0 'dokuwiki-headline-5))
   ;; link
   ("\\[\\[[^|].+?\\(?:\\(|\\)\\(.*?\\)\\)?\\]\\]"
    (0 'dokuwiki-link) (1 'dokuwiki-code t t)
    (2 'font-lock-string-face t t) (2 'underline append t))
   ;; linebreak
   ("\\\\\\\\\\s-+" (0 'dokuwiki-code t))
   ;; list
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" "\\([-*]\\).*$" nil nil (1 'dokuwiki-list))
   ;; code block
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" dokuwiki-code-block-search
     nil nil (0 'dokuwiki-code t))
   ;; smiley
   ,@(mapcar #'(lambda (smiley)
                 (list (concat "\\W\\(" (regexp-quote smiley) "\\)\\W")
                       1 'dokuwiki-smiley))
             dokuwiki-smiley-list)
   ))

(defun dokuwiki-code-block-search (limit)
  (if (not (looking-at "[-*]"))
      (re-search-forward ".*$" limit t)))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  )

(provide 'dokuwiki-mode)
