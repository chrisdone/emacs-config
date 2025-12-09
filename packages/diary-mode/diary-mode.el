(define-derived-mode diary-mode
  text-mode "Diary"
  "Major mode for writing a daily work diary.
 \\{diary-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(diary-keywords t nil nil))
  (display-line-numbers-mode -1)
  (jumpto-address-mode 1)
  (auto-fill-mode -1)
  (set (make-local-variable 'paragraph-start) "|[ 	]*$\\|[ ]*• "))

(defgroup diary-faces nil
 "Faces for diary-mode.")

(defface diary-heading-face
  '((((class color) (background dark))
     (:foreground "#fff" :background "#333" :box (:line-width (5 . 2) :color "#333" :style flat-button) :height 200 :family "Arial"))
    (((class color) (background light))
     (:foreground "#000" :background "#eee" :box (:line-width (5 . 2) :color "#eee" :style flat-button) :height 200 :family "Arial")))
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
(defface diary-note-prefix-face
  '((((class color) (background dark))
     (:foreground "#888" :bold t))
    (((class color) (background light))
     (:foreground "#1a6e8e" :bold t)))
  "Meet prefix for items."
  :group 'diary-faces)

(defface diary-p1-face
  '((((class color) (background dark))
     (:foreground "#C48C8C" :bold t))
    (((class color) (background light))
     (:foreground "#1a6e8e" :bold t)))
  "Meet prefix for items."
  :group 'diary-faces)
(defface diary-p2-face
  '((((class color) (background dark))
     (:foreground "#F0DFAF" :bold t))
    (((class color) (background light))
     (:foreground "#1a6e8e" :bold t)))
  "Meet prefix for items."
  :group 'diary-faces)
(defface diary-p3-face
  '((((class color) (background dark))
     (:foreground "#7F9F7F" :bold t))
    (((class color) (background light))
     (:foreground "#1a6e8e" :bold t)))
  "Meet prefix for items."
  :group 'diary-faces)

(defface diary-reference-face
  '((t (:height 100)))
  "References to links highlighted."
  :group 'diary-faces)

(defconst diary-heading-regex
  "^[0-9]+ [A-Z][a-z]+ [0-9]+$")

(defconst diary-keywords
  `((,diary-heading-regex . 'diary-heading-face)
    ("^[A-Z].*$" . 'diary-heading-face)
    ("^ *• Done " . 'diary-done-prefix-face)
    ("^ *• p1 [a-z][a-z][a-z]: " . 'diary-p1-face)
    ("^ *• p2 [a-z][a-z][a-z]:" . 'diary-p2-face)
    ("^ *• p3 [a-z][a-z][a-z]:" . 'diary-p3-face)
    ("^ *• p1 " . 'diary-p1-face)
    ("^ *• p2" . 'diary-p2-face)
    ("^ *• p3" . 'diary-p3-face)
    ("^ *• Meet " . 'diary-meet-prefix-face)
    ("^ *• Note " . 'diary-note-prefix-face)))

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
  (let* ((keywords (list "Done" "Meet" "Note"))
         (keywords-ring (ring-convert-sequence-to-ring keywords))
         (word-regexp (regexp-opt keywords t)))
    (save-excursion
      (back-to-indentation)
      (search-forward-regexp "• " (+ (point) 2))
      (if (looking-at word-regexp)
          (let* ((word (match-string 1))
                 (index (ring-member keywords-ring word))
                 (next (ring-ref keywords-ring (1+ index))))
            (delete-region (point) (save-excursion (forward-word 1) (point)))
            (insert next))
        (insert "Done")))))

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

(defun diary-export-md ()
  (interactive)
  (let ((string (buffer-substring (region-beginning)
                                     (region-end))))
  (with-current-buffer "export.md"
    (erase-buffer)
    (insert
          (replace-regexp-in-string
        "* Done" "* **Done** "
        (replace-regexp-in-string
         "* Meet" "* **Meet** "
         (replace-regexp-in-string "•" "*" string))))
    (save-buffer)))
  )

(defun diary-insert-day ()
  (interactive)
  (let ((date (format-time-string "\n%-d %b %Y\n" (current-time))))
    (goto-char (point-min))
    (if (search-forward date nil t 1)
        (end-of-line)
      (forward-line 1)
      (delete-blank-lines)
      (insert date "\n")
      (save-excursion (insert "\n\n")))))

(defun diary-goto-previous-heading-exclusive ()
  "Jump to the end of the line at the previous heading."
  (interactive)
  (search-backward-regexp diary-heading-regex nil t 1)
  (end-of-line))

(defun diary-goto-next-heading-exclusive ()
  "Jump to the beginning of the line at the next heading."
  (interactive)
  (search-forward-regexp diary-heading-regex nil t 1)
  (beginning-of-line))

(defun diary-break-out-lines ()
  (interactive)
  "Insert exactly one blank line between the two current lines."
  (insert "\n"))

(defun diary-space-out-headings ()
  (save-excursion
    (diary-goto-previous-heading-exclusive)
    (diary-break-out-lines))
  (save-excursion
    (diary-goto-next-heading-exclusive)
    (diary-break-out-lines)))

(provide 'diary-mode)
