(define-derived-mode diary-mode
   text-mode "Diary"
   "Major mode for writing a daily work diary.
 \\{diary-mode-map}"
   (set (make-local-variable 'font-lock-defaults) '(diary-keywords t nil nil)))

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
  '((t (:bold t)))
  "Done prefix for items.."
  :group 'diary-faces)

(defface diary-reference-face
  '((t (:height 100)))
  "References to links highlighted."
  :group 'diary-faces)

(defconst diary-keywords
  `(("^[0-9]+ [A-Z][a-z]+ [0-9]+$" . 'diary-heading-face)
    ("^[A-Z].*$" . 'diary-heading-face)
    ("^• Done " . 'diary-done-prefix-face)
    ("\\[^[0-9]+\\]" . 'diary-reference-face)
    ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.diary\\'" . diary-mode))

(provide 'diary-mode)
