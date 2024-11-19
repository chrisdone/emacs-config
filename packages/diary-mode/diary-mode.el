(define-derived-mode diary-mode
  text-mode "Diary"
  "Major mode for writing a daily work diary.
 \\{diary-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(diary-keywords t nil nil))
  (display-line-numbers-mode -1)
  (jumpto-address-mode 1)
  (auto-fill-mode -1)
  (set (make-local-variable 'page-delimiter) "^[0-9]+ [A-Z][a-z]+ [0-9]+$"))

(defgroup diary-faces nil
 "Faces for diary-mode.")

(defface diary-heading-face
  '((((class color) (background dark))
     (:foreground "#fff" :background "#333" :box (:line-width (5 . 2) :color "#333" :style flat-butotn) :height 200 :family "Arial"))
    (((class color) (background light))
     (:foreground "#000" :background "#eee" :box (:line-width (5 . 2) :color "#eee" :style flat-butotn) :height 200 :family "Arial")))
  "Headings."
  :group 'diary-faces)

(defface diary-done-prefix-face
  '((((class color) (background dark))
     (:foreground "#fff" :bold t))
    (((class color) (background light))
     (:foreground "#000" :bold t)))
  "Done prefix for items."
  :group 'diary-faces)

(defface diary-meet-prefix-face
  '((((class color) (background dark))
     (:foreground "#65c9ef" :bold t))
    (((class color) (background light))
     (:foreground "#1a6e8e" :bold t)))
  "Meet prefix for items."
  :group 'diary-faces)

(defface diary-reference-face
  '((t (:height 100)))
  "References to links highlighted."
  :group 'diary-faces)

(defconst diary-keywords
  `(("^[0-9]+ [A-Z][a-z]+ [0-9]+$" . 'diary-heading-face)
    ("^[A-Z].*$" . 'diary-heading-face)
    ("^ *• Done " . 'diary-done-prefix-face)
    ("^ *• Meet " . 'diary-meet-prefix-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.diary\\'" . diary-mode))

(defun diary-insert-done ()
  (interactive)
  (insert "• Done "))

(defun diary-insert-day ()
  (interactive)
  (insert (format-time-string "\n%-d %b %Y\n" (current-time)))
  (insert "\n"))

(define-key diary-mode-map (kbd "M-RET") 'diary-dwim-done)

(defun diary-dwim-done ()
  (interactive)
  (if (or (= (line-beginning-position) (line-end-position))
          (looking-back "^• "))
      (progn (delete-region (line-beginning-position) (line-end-position))
             (diary-insert-done))
      (if (> (point) (line-beginning-position))
          (progn (goto-char (line-end-position))
                 (insert "\n")
                 (diary-insert-done))
        (goto-char (line-beginning-position))
        (open-line 1)
        (diary-insert-done))))

(defun diary-dwim-newline ()
  (interactive)
  (if (save-excursion (back-to-indentation) (looking-at "• "))
      (progn (call-interactively 'newline)
             (insert "• "))
    (call-interactively 'newline)))

(define-key diary-mode-map (kbd "RET") 'diary-dwim-newline)
(define-key diary-mode-map (kbd "SPC") 'diary-dwim-space)
(define-key diary-mode-map (kbd "TAB") 'diary-dwim-tab)
(define-key diary-mode-map (kbd "<backtab>") 'diary-dwim-backtab)
(define-key diary-mode-map (kbd "C-c C-t") 'diary-mark-done)

(defun diary-mark-done ()
  "Similar behavior to org-mode."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (when (and (looking-at "• ")
               (not (looking-at "• Done ")))
     (forward-char 1)
     (cycle-spacing 1)
     (insert "Done "))))

(defun diary-dwim-space ()
  (interactive)
  (when (looking-back "\\*")
    (delete-char -1)
    (insert "•"))
  (call-interactively 'self-insert-command))

(defun diary-dwim-tab ()
  (interactive)
  (indent-rigidly
   (line-beginning-position)
   (line-end-position)
   2))

(defun diary-dwim-backtab ()
  (interactive)
  (indent-rigidly
   (line-beginning-position)
   (line-end-position)
   -2))

(provide 'diary-mode)
