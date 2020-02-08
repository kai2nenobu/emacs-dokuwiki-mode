;;; dokuwiki-mode.el --- Major mode for DokuWiki document

;; Copyright (C)  2013-2017 Tsunenobu Kai

;; Author: Tsunenobu Kai <kai2nenobu@gmail.com>
;; URL: https://github.com/kai2nenobu/emacs-dokuwiki-mode
;; Version: 0.1.1
;; Keywords: hypermedia text DokuWiki

;; This file is not part of GNU Emacs.

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

(defgroup dokuwiki-mode nil
  "Major mode for DokuWiki document."
  :group 'text
  :group 'dokuwiki
  :tag "DokuWiki"
  :link '(url-link "https://www.dokuwiki.org/dokuwiki"))

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
    (define-key map (kbd "C-c C-u") 'outline-up-heading)
    (define-key map (kbd "C-c C-@") 'outline-mark-subtree)
    (define-key map (kbd "C-c C-t 1") 'dokuwiki-insert-header-1)
    (define-key map (kbd "C-c C-t 2") 'dokuwiki-insert-header-2)
    (define-key map (kbd "C-c C-t 3") 'dokuwiki-insert-header-3)
    (define-key map (kbd "C-c C-t 4") 'dokuwiki-insert-header-4)
    (define-key map (kbd "C-c C-t 5") 'dokuwiki-insert-header-5)
    (define-key map (kbd "C-c C-t 6") 'dokuwiki-insert-header-6)
    (define-key map (kbd "C-c C-t 8") 'dokuwiki-insert-header-current-level)
    (define-key map (kbd "C-c C-t 9") 'dokuwiki-insert-header-down-level)
    (define-key map (kbd "C-c C-t 0") 'dokuwiki-insert-header-up-level)
    (define-key map (kbd "C-c C-t b") 'dokuwiki-insert-bold)
    (define-key map (kbd "C-c C-t i") 'dokuwiki-insert-italic)
    (define-key map (kbd "C-c C-t u") 'dokuwiki-insert-underline)
    (define-key map (kbd "C-c C-t d") 'dokuwiki-insert-deleteline)
    (define-key map (kbd "C-c C-t m") 'dokuwiki-insert-code)
    (define-key map (kbd "C-c C-t c") 'dokuwiki-insert-code-block)
    (define-key map (kbd "C-c C-t o") 'dokuwiki-insert-code-file)

    (define-key map (kbd "C-c C-t l") 'dokuwiki-insert-link)
    (define-key map (kbd "C-c C-t f") 'dokuwiki-insert-footnote)
    (define-key map (kbd "M-RET") 'dokuwiki-insert-list)
    (define-key map (kbd "C-c C-t -") 'dokuwiki-insert-number-list)
    (define-key map (kbd "C-c C-t q") 'dokuwiki-insert-quote)
    (define-key map (kbd "C-c C-t r") 'dokuwiki-insert-rss)
    (define-key map (kbd "C-c C-t h") 'dokuwiki-insert-hr)
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-smiley-list
  '("8-)" "8-O" ":-(" ":-)" "=) " ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME")
  "Smiley list in DokuWiki.")

(defvar dokuwiki-outline-regexp " ?\\(=\\{2,6\\}\\)"
  "Regexp which indicates headline in DokuWiki.
See also `outline-regexp'.")

;;;; Faces
(defface dokuwiki-box '((t (:box t)))
  "Face enabled box property")

(defface dokuwiki-code '((t (:inherit shadow)))
  "DokuWiki face for code."
  :group 'dokuwiki)

(defface dokuwiki-list '((t (:inherit font-lock-type-face)))
  "DokuWiki face for list."
  :group 'dokuwiki)

(defface dokuwiki-verbatim '((t (:inherit shadow)))
  "DokuWiki face for text as is."
  :group 'dokuwiki)

(defface dokuwiki-footnote '((t (:inherit font-lock-builtin-face)))
  "DokuWiki face for footnote."
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

(defface dokuwiki-image '((t (:inherit font-lock-variable-name-face)))
  "DokuWiki face for image."
  :group 'dokuwiki)

(defface dokuwiki-table '((t (:inherit font-lock-function-name-face)))
  "DokuWiki face for table."
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
   ("''\\(.+?\\)''" (0 'dokuwiki-code append) (1 'dokuwiki-box append))
   ;; verbatim
   ("%%.+?%%" (0 'dokuwiki-code t))
   ;; footnote
   ("((.+?))" (0 'dokuwiki-footnote))
   ;; headline
   (" ?======.+======[ \t]*$" (0 'dokuwiki-headline-1))
   (" ?=====.+=====[ \t]*$" (0 'dokuwiki-headline-2))
   (" ?====.+====[ \t]*$" (0 'dokuwiki-headline-3))
   (" ?===.+===[ \t]*$" (0 'dokuwiki-headline-4))
   (" ?==.+==[ \t]*$" (0 'dokuwiki-headline-5))
   ;; link
   ("\\[\\[[^|]+?\\(?:\\(|\\)\\(.*?\\)\\)?\\]\\]"
    (0 'dokuwiki-link) (1 'dokuwiki-code t t)
    (2 'font-lock-string-face t t) (2 'underline append t))
   ("https?://\\(\\([-_.!~*'()a-zA-Z0-9;?:@&=+$,%#]+\\)/?\\)+" (0 'dokuwiki-link))
   ;; image
   ("{{[^|]+?\\(|\\(.*?\\)\\)?}}"
    (0 'dokuwiki-image t)
    (1 'dokuwiki-code t t) (2 'font-lock-string-face t t))
   ;; table
   ("^[ \t]*[|^].*$" (0 'dokuwiki-table))
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

(defun dokuwiki-outline-level ()
  "Compute a header's nesting level in `dokuwiki-mode'.
See also `outline-level'."
  (when (looking-at outline-regexp)
    (let ((const 7)
          (headline (match-string 1)))
      (- const (length headline)))))

(defun dokuwiki-outline-level-for-insert-header ()
  "Return outline level. If not have level"
(save-excursion
   (end-of-line)
   (if (re-search-backward "^=+" nil t)
       (- (match-end 0) (match-beginning 0))
     0)))
;; DEBUG: strange regexp

;;;; Work with `outline-magic'
(eval-after-load "outline-magic"
  '(progn
     (define-key dokuwiki-mode-map (kbd "TAB") 'outline-cycle)
     (define-key dokuwiki-mode-map (kbd "<S-tab>")
       '(lambda () (interactive) (outline-cycle '(4))))
     (define-key dokuwiki-mode-map (kbd "<M-S-right>") 'outline-demote)
     (define-key dokuwiki-mode-map (kbd "<M-S-left>") 'outline-promote)
     (define-key dokuwiki-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
     (define-key dokuwiki-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
     (add-hook 'dokuwiki-mode-hook 'dokuwiki-outline-magic-hook)
     ;; Enable outline-magic features in `dokuwiki-mode' buffers
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (eq major-mode 'dokuwiki-mode) (dokuwiki-outline-magic-hook))))
     ))

(defun dokuwiki-outline-magic-hook ()
  "Hook to configure `outline-magic'."
  (set (make-local-variable 'outline-promotion-headings)
       '(("======" . 1) ("=====" . 2) ("====" . 3) ("===" . 4) ("==" . 5)))
  (set (make-local-variable 'outline-cycle-emulate-tab) t))

(defun dokuwiki-insert-header (&optional level text setext)
  "This code is derived from markdown-insert-header in markdown-mode.el
  "
  ;; TODO: clear dependence of markdown-mode
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (use-region-p)
        ;; Active region
        (setq text (delete-and-extract-region (region-beginning) (region-end)))
      ;; No active region
      ;; (markdown-remove-header)
      (setq text (delete-and-extract-region
                  (line-beginning-position) (line-end-position)))
      (when (and setext (string-match-p "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (markdown-compress-whitespace-string text)))
  ;; Insertion with given text
  (markdown-ensure-blank-line-before)
  (let (hdr)
    (cond (setext
           (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
           (insert text "\n" hdr))
          (t
           (setq hdr (make-string level ?=))
           (insert hdr " " text)
           (when (null markdown-asymmetric-header) (insert " " hdr)))))
  (markdown-ensure-blank-line-after)
  ;; Leave point at end of text
  (cond (setext
         (backward-char (1+ (string-width text))))
        ((null markdown-asymmetric-header)
         (backward-char (1+ level)))))

(defun dokuwiki-insert-header-1 ()
(interactive)
(dokuwiki-insert-header 1))

(defun dokuwiki-insert-header-2 ()
(interactive)
(dokuwiki-insert-header 2))

(defun dokuwiki-insert-header-3 ()
(interactive)
(dokuwiki-insert-header 3))

(defun dokuwiki-insert-header-4 ()
(interactive)
(dokuwiki-insert-header 4))

(defun dokuwiki-insert-header-5 ()
(interactive)
(dokuwiki-insert-header 5))

(defun dokuwiki-insert-header-6 ()
(interactive)
(dokuwiki-insert-header 6))

(defun dokuwiki-insert-header-current-level ()
  (interactive)
  (let ((current-level (dokuwiki-outline-level-for-insert-header)))
    (if (= current-level 0) (dokuwiki-insert-header 6)
      (dokuwiki-insert-header current-level))))

(defun dokuwiki-insert-header-up-level ()
  (interactive)
  (let ((current-level (dokuwiki-outline-level-for-insert-header)))
    (if (= current-level 0) (dokuwiki-insert-header 6)
      (dokuwiki-insert-header (+ current-level 1)))))

(defun dokuwiki-insert-header-down-level ()
  (interactive)
  (let ((current-level (dokuwiki-outline-level-for-insert-header)))
    (if (= current-level 0) (dokuwiki-insert-header 6)
    (dokuwiki-insert-header (- current-level 1)))))

(defun dokuwiki-insert-base (before after)
    (if (use-region-p)
        ;; Active region
        (let ((bounds (markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       markdown-regex-bold 2 4)))
          (markdown-wrap-or-insert before after nil (car bounds) (cdr bounds)))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at markdown-regex-bold)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert before after 'word nil nil))))

(defun dokuwiki-insert-bold ()
  (interactive)
  (dokuwiki-insert-base "**" "**"))

(defun dokuwiki-insert-italic ()
  (interactive)
  (dokuwiki-insert-base "//" "//"))

(defun dokuwiki-insert-underline ()
  (interactive)
  (dokuwiki-insert-base "__" "__"))

(defun dokuwiki-insert-code ()
  (interactive)
  (dokuwiki-insert-base "''" "''"))

(defun dokuwiki-insert-code-block ()
  (interactive)
  (dokuwiki-insert-base "<code>\n" "\n</code>"))

(defun dokuwiki-insert-code-file ()
  (interactive)
  (dokuwiki-insert-base "<file lang file>\n" "\n</file>"))

(defun dokuwiki-insert-deleteline ()
  (interactive)
  (dokuwiki-insert-base "<del>" "</del>"))

(defun dokuwiki-insert-link ()
  (interactive)
  (dokuwiki-insert-base "[[" "]]"))

(defun dokuwiki-insert-footnote ()
  (interactive)
  (dokuwiki-insert-base "((" "))"))

(defun dokuwiki-insert-number-list ()
  (interactive)
  (dokuwiki-insert-base "  - " ""))

(defun dokuwiki-insert-list ()
  ;; TODO: need to rewrite
  (interactive)
  (dokuwiki-insert-base "  * " ""))

(defun dokuwiki-insert-quote ()
  (interactive)
  (dokuwiki-insert-base "> " ""))

(defun dokuwiki-insert-rss ()
  (interactive)
  (dokuwiki-insert-base "{{rss>" " 10 author date 1h}}"))

(defun dokuwiki-insert-hr ()
  ;; TODO: add case of active region.
  (interactive)
  (insert "------"))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki document."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  (set (make-local-variable 'outline-regexp) dokuwiki-outline-regexp)
  (set (make-local-variable 'outline-level) 'dokuwiki-outline-level)
  (outline-minor-mode 1)
  )

(provide 'dokuwiki-mode)

;;; dokuwiki-mode.el ends here
