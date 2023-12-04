;;; art-mode.el --- Language support for the Brossa programming language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Csongor Kiss
;;
;; Author: Csongor Kiss <http://github.csom/kcsongor>
;; Maintainer: Csongor Kiss
;; Created: May 29, 2020
;; Modified: May 29, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((s) (dash))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Language support for the Brossa programming language
;;
;;; Code:
;;;

(eval-when-compile
  (require 'dash)
  (require 's))

(defvar art-keywords nil
  "Art language keywords.")
(defvar art-macros nil
  "Art language macros.")
(defvar art-builtins nil
  "Art language builtins.")

(setq art-keywords
      (append '("@derived"
                "@hidden"
                "@adjustable")
              (-map (lambda (b) (concat "\\_<" b "\\_>"))
                    '("condition"
                      "with"
                      "if"
                      "then"
                      "else"
                      "then"
                      "when"
                      "has"
                      "for any"
                      "let"
                      "table"
                      "and"
                      "or"
                      "type"
                      "group"
                      ))))

(setq art-macros
      (-map (lambda (b) (concat "\\_<" (s-snake-case b) "\\_>"))
            '("ListKeyArgs"
              "Explain"
              "ShortExplain"
              "Optional"
              "Datapoint"
              "Invalid"
              "Document"
              "DocumentPartFromHtml"
              "DocumentPartFromStaticPdf")))

(setq art-builtins
      (-map (lambda (b) (concat "\\_<" (s-snake-case b) "\\_>"))
            '("ColumnList"
              "VerticalLookupList"
              "VerticalLookupFirst"
              "RemoveDuplicates"
              "Get"
              "LinearInterpolateTableColumns"
              "SumList"
              "MinList"
              "OrList"
              "LaxParseBool"
              "YesNoBool"
              "Months"
              "Years"
              "Days"
              "NumYears"
              "Period"
              "Min"
              "Max"
              "FlooredAt"
              "CappedAt"
              "Not"
              "FindClosest"
              "Abs"
              "Round2"
              "Elem"
              "Length"
              "Null"
              "Some"
              "Concat")))

(defvar art-font-lock
      `(("//.*$" . font-lock-comment-face)
        ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
        (,(s-join "\\|" art-keywords) . font-lock-keyword-face)
        (,(s-join "\\|" art-macros) . font-lock-constant-face)
        (,(s-join "\\|" art-builtins) . font-lock-builtin-face)
        ("\\<\\*?\\[?[A-Z][a-z]*\\]?\\>" . font-lock-type-face)
        ("\\w\\(#\\w+\\)" . (1 font-lock-builtin-face))))


;;;###autoload
(define-derived-mode art-mode prog-mode "Brossa"
  (setq-local font-lock-defaults '(art-font-lock t nil nil))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "// ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.art\\'" . art-mode))

(defun art--disable-highlight-numbers ()
  "The highlight-numbers package interferes with our syntax rules.
Disable it, if it's available."
  (if (featurep 'highlight-numbers)
      (highlight-numbers-mode -1)))

(add-hook 'art-mode-hook 'art--disable-highlight-numbers)


(provide 'art-mode)
;;; art-mode.el ends here
