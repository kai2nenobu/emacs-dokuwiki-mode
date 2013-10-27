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

;;;; Faces
(defface dokuwiki-code '((t (:inherit shadow)))
  "DokuWiki face for code."
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

(defvar dokuwiki-font-lock-keywords
  (list
   ;; bold
   '("\\*\\*.+\\*\\*" . 'font-lock-keyword-face)
   ;; italic
   '("//.+//" . 'font-lock-keyword-face)
   ;; underline
   '("__.+__" . 'font-lock-keyword-face)
   ;; monospace
   '("''.+''" . 'font-lock-keyword-face)
   ;; headline
   '("[ \t]?======.+======[ \t]*$" . 'outline-1)
   '("[ \t]?=====.+=====[ \t]*$" . 'outline-2)
   '("[ \t]?====.+====[ \t]*$" . 'outline-3)
   '("[ \t]?===.+===[ \t]*$" . 'outline-4)
   '("[ \t]?==.+==[ \t]*$" . 'outline-5)
   ;; link
   '("\\[\\[[^]]+\\]\\]" . 'font-lock-keyword-face)
   ;; linebreak
   '("\\\\\\\\\\s-+" . 'font-lock-keyword-face)
   ))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  )

(provide 'dokuwiki-mode)
