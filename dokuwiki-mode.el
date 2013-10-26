(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-mode-hook nil
  "dokuwiki-mode-hook.")

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
