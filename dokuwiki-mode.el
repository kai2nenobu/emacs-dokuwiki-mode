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

(defvar dokuwiki-regex-blank-line
  "^[[:blank:]]*$"
  "Regular expression that matches a blank line.")

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
  "Return outline level. "
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
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (use-region-p)
        ;; Active region
        (setq text (delete-and-extract-region (region-beginning) (region-end)))
      ;; No active region
      (setq text (delete-and-extract-region
                  (line-beginning-position) (line-end-position)))
      (when (and setext (string-match-p "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (dokuwiki-compress-whitespace-string text)))
  ;; Insertion with given text
  (dokuwiki-ensure-blank-line-before)
  (let (hdr)
    (cond (setext
           (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
           (insert text "\n" hdr))
          (t
           (setq hdr (make-string level ?=))
           (insert hdr " " text)
           (insert " " hdr))))
  (dokuwiki-ensure-blank-line-after)
  ;; Leave point at end of text
  (cond (setext
         (backward-char (1+ (string-width text))))
        (
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
  (dokuwiki-wrap-or-insert before after 'word nil nil))

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

;; (defun dokuwiki-insert-list ()
;;   (interactive)
;;   (let (mark search-limit-point)
;;     ;; get search-limit-point
;;     (save-excursion
;;       (progn
;; 	(end-of-line)
;; 	(setq search-limit-point (point))
;; 	))
;;     ;; search order or non-order list
;;     (save-excursion
;;       (if (dokuwiki-cur-line-blank-p)
;; 	  (forward-line -1)
;; 	)
;;       (beginning-of-line)
;;       (if (search-forward "  -" search-limit-point t)
;; 	     (setq mark "- ")
;; 		(setq mark "* ")))
;; 	;; indent
;; 	(setq indent 2)
;; 	(setq new-indent (make-string indent 32))
;;     ;; insert
;;   (progn
;;     (unless (dokuwiki-cur-line-blank-p)
;;       (insert "\n"))
;;     (insert (concat new-indent mark)))))

;; インデント保持はされてるけど、リストマークが保持されていない。*になる。なんで。書いてあるのと違う...記法が微妙に違うためか。
(defun dokuwiki-insert-list (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (let (bounds cur-indent marker indent new-indent new-loc)
    (save-match-data
      ;; Look for a list item on current or previous non-blank line
	  ;; 各種代入
      (save-excursion
        (while (and (not (setq bounds (markdown-cur-list-item-bounds))) ; boundsはlist-itemの各種情報、現在行で
                    (not (bobp))
                    (markdown-cur-line-blank-p))
          (forward-line -1))) ; 辿って、一番最後に使われたmarkerがあると止まる

      (when bounds ; boundsがあるとき...
        (cond ((save-excursion
                 (skip-chars-backward " \t") ; インデントを飛ばして移動
                 (looking-at-p markdown-regex-list)) ; listをサーチ
               (beginning-of-line)
               (insert "\n")
               (forward-line -1)) ; カーソルを戻すバージョン？
              ((not (markdown-cur-line-blank-p)) ; 空白でないときはnewline？(違いがわからなん)
               (newline)))
        (setq new-loc (point)))

	  ;; 次の非空白行のリストでboundsを定義する
      ;; Look ahead for a list item on next non-blank line
      (unless bounds
        (save-excursion
          (while (and (null bounds)
                      (not (eobp)) ; バッファの最後ではない
                      (markdown-cur-line-blank-p)) ; 空行である
            (forward-line)				; 次の行に進む…*while内。
            (setq bounds (markdown-cur-list-item-bounds)))) ; boundsが定義されると抜ける
        (when bounds
          (setq new-loc (point))
          (unless (markdown-cur-line-blank-p) ; 空白でないとき、新しい行。
            (newline))))

      (if (not bounds) ; boundsがないとき。インデントなどがあり大きくなっている。ここじゃないときはインデント使わないってことだよな…。new-locがあるので、上のwhenかunlessのどちらかは確実に評価されることになる。
          ;; When not in a list, start a new unordered one
          (progn
            (unless (markdown-cur-line-blank-p)
              (insert "\n"))
            (insert markdown-unordered-list-item-prefix))
        ;; Compute indentation and marker for new list item
        (setq cur-indent (nth 2 bounds))
        (setq marker (nth 4 bounds))	; markerは何？ -- * とか + とかか。マーク。
        (when (nth 5 bounds)
          (setq marker
                (concat marker
                        (replace-regexp-in-string "[Xx]" " " (nth 5 bounds)))))

        ;; If current item is a GFM checkbox, insert new unchecked checkbox.
        (cond

         ;; Indent: increment indentation by 4, use same marker.
         ((= arg 16) (setq indent (+ cur-indent 4)))

         ;; Same level: keep current indentation and marker.
         (t (setq indent cur-indent)))

        (setq new-indent (make-string indent 32)) ; レベルに応じたインデント数で空白を作る
        (goto-char new-loc)
        (cond
         ;; Unordered list, GFM task list, or ordered list with hash mark
         ((string-match-p "[\\*\\+-]\\|#\\." marker)
          (insert new-indent marker)))) ; マークがあってない。どれも*になる…。→markdownと文法が異なるから？でも同じのでやってもできず。
      ;; Propertize the newly inserted list item now
      (markdown-syntax-propertize-list-items (point-at-bol) (point-at-eol)))))

(defun dokuwiki-insert-quote ()
  (interactive)
  (dokuwiki-insert-base "> " ""))

(defun dokuwiki-insert-rss ()
  (interactive)
  (dokuwiki-insert-base "{{rss>" " 10 author date 1h}}"))

(defun dokuwiki-insert-hr ()
  (interactive)
  (when (looking-at-p "------")
    (delete-region (match-beginning 0) (match-end 0)))
  (insert "------")
  (beginning-of-line))

;;; Tools -----------------------------------------------------------

(defun dokuwiki-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at-p dokuwiki-regex-blank-line)))

(defun dokuwiki-compress-whitespace-string (str)
   "Compress whitespace in STR and return result.
Leading and trailing whitespace is removed.  Sequences of multiple
spaces, tabs, and newlines are replaced with single spaces. Derived from dokuwiki.el"
  (replace-regexp-in-string "\\(^[ \t\n]+\\|[ \t\n]+$\\)" ""
				     (replace-regexp-in-string "[ \t\n]+" " " str)))

(defun dokuwiki-ensure-blank-line-before ()
  "If previous line is not already blank, insert a blank line before point."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp) (looking-back "\n\\s-*\n" nil)) (insert "\n")))

(defun dokuwiki-ensure-blank-line-after ()
  "If following line is not already blank, insert a blank line after point.
Return the point where it was originally."
  (save-excursion
    (unless (eolp) (insert "\n"))
    (unless (or (eobp) (looking-at-p "\n\\s-*\n")) (insert "\n"))))

(defun dokuwiki-wrap-or-insert (s1 s2 &optional thing beg end)
  "Insert the strings S1 and S2, wrapping around region or THING.
If a region is specified by the optional BEG and END arguments,
wrap the strings S1 and S2 around that region.
If there is an active region, wrap the strings S1 and S2 around
the region.  If there is not an active region but the point is at
THING, wrap that thing (which defaults to word).  Otherwise, just
insert S1 and S2 and place the point in between.  Return the
bounds of the entire wrapped string, or nil if nothing was wrapped
and S1 and S2 were only inserted."
  (let (a b bounds new-point)
    (cond
     ;; Given region
     ((and beg end)
      (setq a beg
            b end
            new-point (+ (point) (length s1))))
     ;; Active region
     ((use-region-p)
      (setq a (region-beginning)
            b (region-end)
            new-point (+ (point) (length s1))))
     ;; Thing (word) at point
	 ;; TODO: dependancy!!
     ((setq bounds (bounds-of-thing-at-point (or thing 'word)))
      (setq a (car bounds)
            b (cdr bounds)
            new-point (+ (point) (length s1))))
     ;; No active region and no word
     (t
      (setq a (point)
            b (point))))
    (goto-char b)
    (insert s2)
    (goto-char a)
    (insert s1)
    (when new-point (goto-char new-point))
    (if (= a b)
        nil
      (setq b (+ b (length s1) (length s2)))
      (cons a b))))

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
