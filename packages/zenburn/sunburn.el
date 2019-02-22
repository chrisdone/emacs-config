;;; sunburn.el --- a dark on light theme using zenburn's codebase

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2003, 2004, 2005, 2006  Daniel Brockman
;; Copyright (C) 2009  Adrian C., Bastien Guerry

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'color-theme)

;; Confirmed
(defvar sunburn-fg "#444444")
(defvar sunburn-bg "#fefefe")
(defvar sunburn-bg-1 "#f9f9f9")
(defvar sunburn-bg+1 "#eeeeee")
(defvar sunburn-bg+2 "#e8e8e8")
(defvar sunburn-grey "#666666")
(defvar sunburn-grey+1 "#999999")
(defvar sunburn-red+1 "#8f4e8b")
(defvar sunburn-red   "#8f4e8b")
(defvar sunburn-red-1 "#8f4e8b")
(defvar sunburn-red-2 "#8f4e8b")
(defvar sunburn-red-3 "#8f4e8b")
(defvar sunburn-red-4 "#8f4e8b")
(defvar sunburn-orange "#8f684e")
(defvar sunburn-yellow "#c3a043")
(defvar sunburn-yellow-1 "#c3a043")
(defvar sunburn-yellow-2 "#cfb56e")
(defvar sunburn-green-1 "#366354")
(defvar sunburn-green   "#397460")
(defvar sunburn-green+1 "#366354")
(defvar sunburn-green+2 "#397460")
(defvar sunburn-green+3 "#366354")
(defvar sunburn-green+4 "#397460")
(defvar sunburn-cyan "#20a6ab")
(defvar sunburn-blue+1 "#4F4371")
(defvar sunburn-blue "#2e659c")
(defvar sunburn-blue-1 "#4F4371")
(defvar sunburn-blue-2 "#2e659c")
(defvar sunburn-blue-3 "#2e659c")
(defvar sunburn-blue-4 "#4c7073")
(defvar sunburn-blue-5 "#366060")
(defvar sunburn-magenta "#8f4e8b")
(defvar sunburn-primary-3-foreground "#4F4371")
(defvar sunburn-primary-4-foreground sunburn-red+1)
(defvar sunburn-primary-5-foreground sunburn-blue+1)
(defvar sunburn-highlight-damp-foreground "#397460")
(defvar sunburn-highlight-damp-background "#f7f7f7")
(defvar sunburn-highlight-alerting-foreground sunburn-red)
(defvar sunburn-highlight-alerting-background sunburn-highlight-damp-background)
(defvar sunburn-highlight-subtle-background "#f7f7f7")
(defvar sunburn-lowlight-1-foreground "#8f4e8b")
(defvar sunburn-lowlight-2-foreground "#366354")
(defvar sunburn-term-dark-gray-foreground "#366354")
(defvar sunburn-term-light-blue-foreground sunburn-blue+1)
(defvar sunburn-term-light-cyan-foreground sunburn-cyan)
(defvar sunburn-term-light-green-foreground sunburn-highlight-damp-foreground)
(defvar sunburn-term-light-magenta-foreground sunburn-magenta)
(defvar sunburn-term-light-red-foreground sunburn-red+1)
(defvar sunburn-term-light-yellow-foreground sunburn-yellow)
(defvar sunburn-term-white-foreground "#ffffff")
(defvar sunburn-term-black-foreground "#222222")
(defvar sunburn-term-dark-blue-foreground sunburn-blue+1)
(defvar sunburn-term-dark-cyan-foreground sunburn-cyan)
(defvar sunburn-term-dark-green-foreground sunburn-green)
(defvar sunburn-term-dark-magenta-foreground sunburn-magenta)
(defvar sunburn-term-dark-red-foreground sunburn-red)
(defvar sunburn-term-dark-yellow-foreground sunburn-orange)
(defvar sunburn-mode-line-foreground sunburn-green)
(defvar sunburn-mode-line-background sunburn-highlight-subtle-background)
(defvar sunburn-hover-highlight-foreground sunburn-yellow)
(defvar sunburn-active-region-background sunburn-highlight-subtle-background)
(defvar sunburn-archived-foreground sunburn-blue+1)
(defvar sunburn-hidden-foreground sunburn-green)
(defvar sunburn-grid-foreground sunburn-orange)
(defvar sunburn-new-foreground sunburn-red)
(defvar sunburn-indent-foreground sunburn-magenta)
(defvar sunburn-top-foreground sunburn-green)
(defvar sunburn-diff-add-background "#DFD")
(defvar sunburn-diff-del-background "#FDD")
(defvar sunburn-hunk-header-foreground sunburn-blue)
(defvar sunburn-none-foreground sunburn-grey+1)
(defvar sunburn-button-background sunburn-highlight-subtle-background)
(defvar sunburn-button-foreground sunburn-fg)
(defvar sunburn-bright-foreground sunburn-green)
(defvar sunburn-highlight-background "#eeeeee")

(eval-after-load 'term
  '(setq ansi-term-color-vector
     (vector 'unspecified sunburn-bg
       sunburn-red sunburn-green
       sunburn-yellow sunburn-blue+1
       sunburn-magenta sunburn-cyan)))

(defvar font-lock-pseudo-keyword-face 'font-lock-pseudo-keyword-face)
(defvar font-lock-operator-face 'font-lock-operator-face)

(defun sunburn-format-spec-works-p ()
  (and (fboundp 'format-spec)
    (= (next-property-change
	 0 (format-spec #("<%x>" 0 4 (face (:weight bold)))
	     '((?x . "foo"))) 4) 4)))

(defun sunburn-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
	;; Quoted percent sign.
	((eq (char-after) ?%)
	  (delete-char 1))
	;; Valid format spec.
	((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
	  (let* ((num (match-string 1))
		  (spec (string-to-char (match-string 2)))
		  (val (cdr (assq spec specification))))
	    (unless val
	      (error "Invalid format character: %s" spec))
	    (let ((text (format (concat "%" num "s") val)))
	      (insert-and-inherit text)
	      ;; Delete the specifier body.
	      (delete-region (+ (match-beginning 0) (length text))
		(+ (match-end 0) (length text)))
	      ;; Delete the percent sign.
	      (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
	;; Signal an error on bogus format strings.
	(t
	  (error "Invalid format string"))))
    (buffer-string)))

(defun sunburn-define-format-spec ()
  (interactive)
  (fset 'format-spec #'sunburn-format-spec))

(unless (sunburn-format-spec-works-p)
  (sunburn-define-format-spec))

(eval-after-load 'format-spec
  '(unless (sunburn-format-spec-works-p)
     (sunburn-define-format-spec)))

(setq-default mode-line-buffer-identification
  (list (propertize "%12b" 'face
	  (list :weight 'bold
	    :foreground sunburn-blue+1))))
(setq-default mode-line-frame-identification "")
(setq-default erc-mode-line-format
  (concat (propertize "%t" 'face
	    (list :weight 'bold
	      :foreground sunburn-blue+1))
    " %a"))


(setq gnus-logo-colors `(,sunburn-bg+2 ,sunburn-bg+1)
  gnus-mode-line-image-cache
  '(image :type xpm :ascent center :data sunburn-fg))

(defun sunburn-make-face-alias-clauses (alias-symbols)
  (let (clauses)
    (dolist (alias-symbol alias-symbols clauses)
      (let ((alias-name (symbol-name alias-symbol)))
        (if (not (string-match "-face" alias-name))
	  (error "Invalid face alias: %s" alias-name)
          (let ((target-name (replace-regexp-in-string
			       ".*\\(-face\\)" ""
			       alias-name nil nil 1)))
            (push `(,(intern alias-name)
		     ((t (:inherit ,(intern target-name)))))
	      clauses)))))))

;;;###autoload
(defun color-theme-sunburn ()
  "Just some alien fruit salad to keep you in the zone."
  (interactive)
  (color-theme-install
    (append
      (list 'color-theme-sunburn
	`((background-color . ,sunburn-bg)
	   (background-mode . light)
	   (border-color . ,sunburn-bg)
	   (foreground-color . ,sunburn-fg)
	   (mouse-color . ,sunburn-fg))
	`((emms-mode-line-icon-color . ,sunburn-fg)
	   (goto-address-mail-face . italic)
	   (goto-address-mail-mouse-face . secondary-selection)
	   (goto-address-url-face . bold)
	   (goto-address-url-mouse-face . hover-highlight)
	   (help-highlight-face . hover-highlight)
	   (imaxima-label-color . ,sunburn-green)
	   (imaxima-equation-color . ,sunburn-fg)
	   (list-matching-lines-face . bold)
	   (view-highlight-face . hover-highlight)
	   (widget-mouse-face . hover-highlight))

	'(bold ((t (:weight bold))))
	'(bold-italic ((t (:italic t :weight bold))))
	`(default ((t (:background ,sunburn-bg :foreground ,sunburn-fg))))
	'(fixed-pitch ((t (:weight bold))))
	'(italic ((t (:slant italic))))
	'(underline ((t (:underline t))))
	;; '(variable-pitch ((t (:font "-*-utopia-regular-r-*-*-12-*-*-*-*-*-*-*"))))


	`(sunburn-background-1 ((t (:background ,sunburn-bg+1))))
	`(sunburn-background-2 ((t (:background ,sunburn-bg+2))))

	`(sunburn-primary-1 ((t (:foreground ,sunburn-blue :weight normal))))
	`(sunburn-primary-2 ((t (:foreground ,sunburn-orange :weight normal))))
	`(sunburn-primary-3 ((t (:foreground ,sunburn-primary-3-foreground :weight normal))))
	`(sunburn-primary-4 ((t (:foreground ,sunburn-primary-4-foreground :weight normal))))
	`(sunburn-primary-5 ((t (:foreground ,sunburn-primary-5-foreground :weight normal))))

	`(sunburn-highlight-damp
	   ((t (:foreground ,sunburn-highlight-damp-foreground
                            :background ,sunburn-highlight-damp-background))))
	`(sunburn-highlight-alerting
	   ((t (:foreground ,sunburn-highlight-alerting-foreground
                            :background ,sunburn-highlight-alerting-background))))
	`(sunburn-highlight-subtle
	   ((t (:background ,sunburn-highlight-subtle-background))))

	`(sunburn-lowlight-1 ((t (:foreground ,sunburn-lowlight-1-foreground))))
	`(sunburn-lowlight-2 ((t (:foreground ,sunburn-lowlight-2-foreground))))

	`(sunburn-yellow ((t (:foreground ,sunburn-yellow))))
	`(sunburn-orange ((t (:foreground ,sunburn-orange))))
	`(sunburn-red ((t (:foreground ,sunburn-red))))
	`(sunburn-green-1 ((t (:foreground ,sunburn-green-1))))
	`(sunburn-green ((t (:foreground ,sunburn-green))))
	`(sunburn-green+1 ((t (:foreground ,sunburn-green+1))))
	`(sunburn-green+2 ((t (:foreground ,sunburn-green+2))))
	`(sunburn-green+3 ((t (:foreground ,sunburn-green+3))))
	`(sunburn-green+4 ((t (:foreground ,sunburn-green+4))))
	`(sunburn-blue ((t (:foreground ,sunburn-blue))))
	`(sunburn-blue-1 ((t (:foreground ,sunburn-blue-1))))
	`(sunburn-blue-2 ((t (:foreground ,sunburn-blue-2))))
	`(sunburn-blue-3 ((t (:foreground ,sunburn-blue-3))))
	`(sunburn-blue-4 ((t (:foreground ,sunburn-blue-4))))
        `(sunburn-grey ((t (:foreground ,sunburn-grey))))

	'(sunburn-title ((t (:inherit variable-pitch :weight bold))))

	'(font-lock-builtin
	   ((t (:inherit sunburn-green))))
	'(font-lock-comment
	   ((t (:inherit sunburn-grey))))
	'(font-lock-comment-delimiter
	   ((t (:inherit sunburn-lowlight-2))))
	'(font-lock-constant
	   ((t (:inherit sunburn-primary-4))))
	'(font-lock-doc
	   ((t (:inherit sunburn-green+1))))
	`(font-lock-doc-string
	   ((t (:foreground ,sunburn-blue+1))))
	`(font-lock-function-name
	   ((t (:foreground ,sunburn-blue))))
	'(font-lock-keyword
          ((t (:inherit sunburn-primary-1))))
	'(font-lock-negation-char
	   ((t (:inherit sunburn-primary-1))))
	'(font-lock-preprocessor
	   ((t (:inherit sunburn-blue))))
	'(font-lock-string
	   ((t (:inherit sunburn-green))))
	'(font-lock-type
	   ((t (:inherit sunburn-primary-3))))
	`(font-lock-variable-name
	   ((t (:foreground ,sunburn-blue+1))))
        `(ats-font-lock-keyword-face
          ((t (:inherit sunburn-primary-1))))
        `(ats-font-lock-static-face
          ((t (:inherit sunburn-primary-2))))
	'(font-lock-warning
	   ((t (:inherit sunburn-highlight-damp))))
	'(font-lock-error
	   ((t (:inherit sunburn-highlight-alerting))))

	`(fixme-face ((t (:foreground ,sunburn-fg :background ,sunburn-bg
                                      :weight bold :box nil)))) ; Colours taken from vim ":hl Todo"

	`(semantic-tag-boundary-face ((t (:overline ,sunburn-bg+2)))) ; sunburn-bg+2
	`(semantic-decoration-on-unparsed-includes
	  ((t (:foreground ,sunburn-highlight-damp-foreground :background ,sunburn-highlight-damp-background)))) ; sunburn-highlight-damp

	`(font-lock-pseudo-keyword
	   ((t (:inherit sunburn-primary-2))))
	`(font-lock-operator
	   ((t (:inherit sunburn-primary-3))))

	`(term-default-bg ((t (nil))))
	`(term-default-bg-inv ((t (nil))))
	`(term-default-fg ((t (nil))))
	`(term-default-fg-inv ((t (nil))))
	`(term-invisible ((t (nil)))) ;; FIXME: Security risk?
	`(term-invisible-inv  ((t (nil))))
	`(term-bold ((t (:weight bold))))
	`(term-underline ((t (:underline t))))

        `(gmail-search-mode-from-face ((t (:inherit sunburn-blue-1))))
        `(gmail-search-mode-subject-face ((t (:inherit sunburn-red))))
        `(gmail-search-mode-subject-unread-face ((t (:foreground ,sunburn-red :weight bold))))
        `(gmail-search-mode-snippet-face ((t (:inherit sunburn-green+1))))
        `(gmail-search-mode-date-face ((t (:inherit sunburn-orange))))
        `(gmail-search-mode-labels-face ((t (:background "#f8f8f8" :foreground "#999"))))
        `(gmail-search-mode-query-face ((t (:foreground "#999"))))
        `(gmail-thread-mode-read-face ((t (:background "#eeeeee"))))
        `(gmail-thread-mode-unread-face ((t (:background "#f5f5f5" :weight bold))))

	;; FIXME: Map these to ansi-term's faces (`term-red', etc.).
	`(sunburn-term-dark-gray      ((t (:foreground ,sunburn-term-dark-gray-foreground))))
	`(sunburn-term-light-blue     ((t (:foreground ,sunburn-term-light-blue-foreground))))
	`(sunburn-term-light-cyan     ((t (:foreground ,sunburn-term-light-cyan-foreground))))
	`(sunburn-term-light-green    ((t (:foreground ,sunburn-term-light-green-foreground))))
	`(sunburn-term-light-magenta  ((t (:foreground ,sunburn-term-light-magenta-foreground))))
	`(sunburn-term-light-red      ((t (:foreground ,sunburn-term-light-red-foreground))))
	`(sunburn-term-light-yellow   ((t (:foreground ,sunburn-term-light-yellow-foreground))))
	`(sunburn-term-white          ((t (:foreground ,sunburn-term-white-foreground))))

	`(sunburn-term-black          ((t (:foreground ,sunburn-term-black-foreground))))
	`(sunburn-term-dark-blue      ((t (:foreground ,sunburn-term-dark-blue-foreground))))
	`(sunburn-term-dark-cyan      ((t (:foreground ,sunburn-term-dark-cyan-foreground))))
	`(sunburn-term-dark-green     ((t (:foreground ,sunburn-term-dark-green-foreground))))
	`(sunburn-term-dark-magenta   ((t (:foreground ,sunburn-term-dark-magenta-foreground))))
	`(sunburn-term-dark-red       ((t (:foreground ,sunburn-term-dark-red-foreground))))
	`(sunburn-term-dark-yellow    ((t (:foreground ,sunburn-term-dark-yellow-foreground))))
	`(sunburn-term-light-gray     ((t (:foreground ,sunburn-fg))))

	`(plain-widget-button
	   ((t (:weight bold))))
	`(plain-widget-button-pressed
	   ((t (:inverse-video t))))
	`(plain-widget-documentation
	   ((t (:inherit font-lock-doc))))
	`(plain-widget-field
	   ((t (:background ,sunburn-bg+2))))
	`(plain-widget-inactive
	   ((t (:strike-through t))))
	`(plain-widget-single-line-field
	   ((t (:background ,sunburn-bg+2))))

	`(fancy-widget-button
	   ((t (:background ,sunburn-bg+1
		 :box (:line-width 1 :style released-button)))))
	`(fancy-widget-button-pressed
	   ((t (:background ,sunburn-bg+1
		 :box (:line-width 1 :style pressed-button)))))
	`(fancy-widget-button-highlight
	   ((t (:background ,sunburn-bg+1
		 :box (:line-width 1 :style released-button)))))
	`(fancy-widget-button-pressed-highlight
	   ((t (:background ,sunburn-bg+1
		 :box (:line-width 1 :style pressed-button)))))
	`(fancy-widget-documentation
	   ((t (:inherit font-lock-doc))))
	`(fancy-widget-field
	   ((t (:background ,sunburn-bg+2))))
	`(fancy-widget-inactive
	   ((t (:strike-through t))))
	`(fancy-widget-single-line-field
	   ((t (:background ,sunburn-bg+2))))

	`(widget-button
	   ((t (:inherit plain-widget-button))))
	`(widget-button-pressed
	   ((t (:inherit fancy-widget-button-pressed))))
	`(widget-button-highlight
	   ((t (:inherit fancy-widget-button-highlight))))
	`(widget-button-pressed-highlight
	   ((t (:inherit fancy-widget-button-pressed-highlight))))
	`(widget-documentation
	   ((t (:inherit fancy-widget-documentation))))
	`(widget-field
	   ((t (:inherit fancy-widget-field))))
	`(widget-inactive
	   ((t (:inherit fancy-widget-inactive))))
	`(widget-single-line-field
	   ((t (:inherit fancy-widget-single-line-field))))

	`(border ((t (:background ,sunburn-bg))))
	`(fringe ((t (:inherit sunburn-highlight-subtle))))
	`(header-line ((t (:inherit sunburn-highlight-damp
			    :box (:color ,sunburn-highlight-damp-background :line-width 1)))))
	`(mode-line ((t (:foreground ,sunburn-mode-line-foreground :background ,sunburn-mode-line-background
			  :box (:color ,sunburn-mode-line-background :line-width 1)))))
	`(mode-line-inactive ((t (:background ,sunburn-highlight-damp-background :foreground ,sunburn-highlight-damp-foreground
				   :box (:color ,sunburn-highlight-damp-background :line-width 1)))))
	`(minibuffer-prompt ((t (:foreground ,sunburn-green))))
	`(Buffer-menu-buffer ((t (:inherit sunburn-primary-1))))

	`(region ((t (:foreground nil :background ,sunburn-bg+2))))
	`(secondary-selection ((t (:foreground nil :background ,sunburn-highlight-subtle-background))))

	`(trailing-whitespace ((t (:inherit font-lock-warning))))
	`(highlight ((t (:underline t))))
	`(paren ((t (:inherit sunburn-lowlight-1))))
	`(show-paren-mismatch ((t (:inherit font-lock-warning))))
        `(show-paren-match ((t (:inherit sunburn-highlight-damp))))
	`(match ((t (:weight bold))))

	`(button ((t (:foreground ,sunburn-button-foreground
                                  :background ,sunburn-button-background
		       :weight bold :underline t))))

	`(hover-highlight ((t (:underline t :foreground ,sunburn-hover-highlight-foreground))))
	`(menu ((t nil)))
	`(mouse ((t (:inherit sunburn-foreground))))
	`(scroll-bar ((t (:background ,sunburn-bg+2))))
	`(tool-bar ((t (:background ,sunburn-bg+2))))

	`(ido-first-match ((t (:inherit sunburn-primary-1))))
	`(ido-only-match ((t (:inherit sunburn-primary-2))))
	`(ido-subdir ((t (:foreground ,sunburn-green))))

	`(icompletep-choices ((t (:foreground ,sunburn-fg)))) ; sunburn-fg
	`(icompletep-determined ((t (:foreground ,sunburn-green+1)))) ; sunburn-green+1
	`(icompletep-nb-candidates ((t (:foreground ,sunburn-green+3)))) ; sunburn-green+3
	`(icompletep-keys ((t (:foreground ,sunburn-red)))) ; sunburn-red

	`(isearch ((t (:foreground ,sunburn-term-white-foreground :background ,sunburn-term-dark-blue-foreground))))
	`(isearch-lazy-highlight
	   ((t (:foreground ,sunburn-term-white-foreground :background "#aaaaaa" :weight normal))))

	`(mtorus-highlight ((t (:inherit sunburn-highlight-bluish))))
	`(mtorus-notify-highlight ((t (:inherit sunburn-primary-1))))

	`(sunburn-active-region-background ((t (:foreground nil
						 :background ,sunburn-active-region-background))))
	`(which-func ((t (:inherit mode-line))))

	`(apt-utils-normal-package
	   ((t (:inherit sunburn-primary-1))))
	`(apt-utils-virtual-package
	   ((t (:inherit sunburn-primary-2))))
	`(apt-utils-field-keyword
	   ((t (:inherit font-lock-doc))))
	`(apt-utils-field-contents
	   ((t (:inherit font-lock-comment))))
	`(apt-utils-summary
	   ((t (:inherit bold))))
	`(apt-utils-description
	   ((t (:inherit default))))
	`(apt-utils-version
	   ((t (:inherit sunburn-blue))))
	`(apt-utils-broken
	   ((t (:inherit font-lock-warning))))

	`(breakpoint-enabled-bitmap ((t (:inherit sunburn-primary-1))))
	`(breakpoint-disabled-bitmap ((t (:inherit font-lock-comment))))

	`(calendar-today ((t (:underline nil :inherit sunburn-primary-2))))
	`(diary ((t (:underline nil :inherit sunburn-primary-1))))
	`(holiday ((t (:underline t :inherit sunburn-primary-4))))

	`(bongo-unfilled-seek-bar ((t (:background ,sunburn-lowlight-1-foreground))))

	`(change-log-date ((t (:inherit sunburn-blue))))

	`(comint-highlight-input ((t (:inherit sunburn-primary-1))))
	`(comint-highlight-prompt ((t (:inherit sunburn-primary-2))))

	`(compilation-info ((t (:inherit sunburn-primary-1))))
	`(compilation-warning ((t (:inherit font-lock-warning))))
	`(compilation-error ((t (:inherit font-lock-error))))

	;; TODO
	`(cua-rectangle ((t (:inherit region))))

	`(custom-button
	   ((t (:inherit fancy-widget-button))))
	`(custom-button-pressed
	   ((t (:inherit fancy-widget-button-pressed))))
	`(custom-changed
	   ((t (:inherit sunburn-blue))))
	`(custom-comment
	   ((t (:inherit font-lock-doc))))
	`(custom-comment-tag
	   ((t (:inherit font-lock-doc))))
	`(custom-documentation
	   ((t (:inherit font-lock-doc))))
	`(custom-link
	   ((t (:inherit sunburn-green :underline t))))
	`(custom-tag
	   ((t (:inherit sunburn-primary-2))))
	`(custom-group-tag
	   ((t (:inherit sunburn-primary-1))))
	`(custom-group-tag-1
	   ((t (:inherit sunburn-primary-4))))
	`(custom-invalid
	   ((t (:inherit font-lock-warning))))
	`(custom-modified
	   ((t (:inherit sunburn-primary-3))))
        `(custom-button-mouse
           ((t (:background ,sunburn-button-background
                :foreground ,sunburn-button-foreground
                :box (:line-width 1 :style released-button)))))
	`(custom-rogue
	   ((t (:inhrit font-lock-warning))))
	`(custom-saved
	   ((t (:underline t))))
	`(custom-set
	   ((t (:inverse-video t :inherit sunburn-blue))))
	`(custom-state
	   ((t (:inherit font-lock-comment))))
	`(custom-variable-button
	   ((t (:weight bold :underline t))))
	`(custom-variable-tag
	   ((t (:inherit sunburn-primary-2))))

	`(dictionary-button ((t (:inherit fancy-widget-button))))
	`(dictionary-reference ((t (:inherit sunburn-primary-1))))
	`(dictionary-word-entry ((t (:inherit font-lock-keyword))))

	`(diff-header ((t (:inherit sunburn-highlight-subtle))))
	`(diff-index ((t (:inherit bold))))
	`(diff-file-header ((t (:inherit bold))))
	`(diff-hunk-header ((t (:inherit sunburn-highlight-subtle))))

	`(diff-added ((t (:inherit sunburn-primary-3))))
	`(diff-removed ((t (:inherit sunburn-blue))))
	`(diff-context ((t (:inherit font-lock-comment))))
	`(diff-refine-change ((t (:inherit sunburn-background-2))))

	`(emms-pbi-song ((t (:foreground ,sunburn-green))))
	`(emms-pbi-current ((t (:inherit sunburn-primary-1))))
	`(emms-pbi-mark-marked ((t (:inherit sunburn-primary-2))))

	`(erc-action ((t (:inherit erc-default))))
	`(erc-bold ((t (:weight bold))))
	`(erc-current-nick ((t (:inherit sunburn-primary-3 :weight bold))))
        `(erc-my-nick ((t (:foreground ,sunburn-blue :weight bold))))
	`(erc-dangerous-host ((t (:inherit font-lock-warning))))
	`(erc-default ((t (:foreground ,sunburn-fg))))
	`(erc-direct-msg ((t (:inherit erc-default))))
	`(erc-error ((t (:inherit font-lock-warning))))
	`(erc-fool ((t (:inherit sunburn-lowlight-1))))
	`(erc-highlight ((t (:inherit hover-highlight))))
	`(erc-input ((t (:foreground ,sunburn-magenta))))
	`(erc-keyword ((t (:inherit sunburn-primary-1))))
	`(erc-nick-default ((t (:inherit sunburn-primary-1))))
	`(erc-nick-msg ((t (:inherit erc-default))))
	`(erc-notice ((t (:inherit sunburn-green))))
	`(erc-pal ((t (:inherit sunburn-primary-3))))
	`(erc-prompt ((t (:inherit sunburn-primary-2))))
	`(erc-timestamp ((t (:inherit sunburn-green+1))))
	`(erc-underline ((t (:inherit underline))))
        `(shm-quarantine-face ((t (:inherit font-lock-error))))
        `(shm-current-face ((t (:inherit sunburn-highlight-subtle))))

	`(circe-highlight-nick-face ((t (:inherit sunburn-primary-1))))
	`(circe-my-message-face ((t (:inherit sunburn-green))))
	`(circe-originator-face ((t (:inherit bold))))
	`(circe-prompt-face ((t (:inherit sunburn-primary-1))))
	`(circe-server-face ((t (:inherit font-lock-comment-face))))

	`(rcirc-my-nick ((t (:inherit sunburn-primary-1))))
	`(rcirc-other-nick ((t (:inherit bold))))
	`(rcirc-bright-nick ((t (:foreground ,sunburn-bright-foreground :inherit rcirc-other-nick))))
	`(rcirc-dim-nick ((t (:inherit font-lock-comment))))
	`(rcirc-nick-in-message ((t (:inherit bold))))
	`(rcirc-server ((t (:inherit font-lock-comment))))
	`(rcirc-server-prefix ((t (:inherit font-lock-comment-delimiter))))
	`(rcirc-timestamp ((t (:inherit font-lock-comment))))
	`(rcirc-prompt ((t (:inherit sunburn-primary-1))))
	`(rcirc-mode-line-nick ((t (:inherit sunburn-primary-1))))

	`(eshell-prompt ((t (:inherit sunburn-primary-1))))
	`(eshell-ls-archive ((t (:foreground ,sunburn-term-light-green-foreground :weight bold))))
	`(eshell-ls-backup ((t (:inherit font-lock-comment))))
	`(eshell-ls-clutter ((t (:inherit font-lock-comment))))
	`(eshell-ls-directory ((t (:foreground ,sunburn-blue+1 :weight bold))))
	`(eshell-ls-executable ((t (:foreground ,sunburn-red+1 :weight bold))))
	`(eshell-ls-unreadable ((t (:inherit sunburn-lowlight-1))))
	`(eshell-ls-missing ((t (:inherit font-lock-warning))))
	`(eshell-ls-product ((t (:inherit font-lock-doc))))
	`(eshell-ls-special ((t (:inherit sunburn-primary-1))))
	`(eshell-ls-symlink ((t (:foreground ,sunburn-cyan :weight bold))))

	`(highlight-current-line ((t (:inherit sunburn-highlight-subtle))))

	`(ibuffer-deletion ((t (:inherit sunburn-primary-2))))
	`(ibuffer-marked ((t (:inherit sunburn-primary-1))))
	`(ibuffer-special-buffer ((t (:inherit font-lock-doc))))
	`(ibuffer-help-buffer ((t (:inherit font-lock-comment))))

	`(message-cited-text ((t (:inherit font-lock-comment))))
	;;`(message-cited-text ((t (:foreground ,sunburn-blue))))
	`(message-header-name ((t (:inherit sunburn-green+1))))
	`(message-header-other ((t (:inherit sunburn-green))))
	`(message-header-to ((t (:inherit sunburn-primary-1))))
	`(message-header-from ((t (:inherit sunburn-primary-1))))
	`(message-header-cc ((t (:inherit sunburn-primary-1))))
	`(message-header-newsgroups ((t (:inherit sunburn-primary-1))))
	`(message-header-subject ((t (:inherit sunburn-primary-2))))
	`(message-header-xheader ((t (:inherit sunburn-green))))
	`(message-mml ((t (:inherit sunburn-primary-1))))
	`(message-separator ((t (:inherit font-lock-comment))))

	`(gnus-header-name ((t (:inherit message-header-name))))
	`(gnus-header-content ((t (:inherit message-header-other))))
	`(gnus-header-from ((t (:inherit message-header-from))))
	`(gnus-header-subject ((t (:inherit message-header-subject))))
	`(gnus-header-newsgroups ((t (:inherit message-header-other))))

	`(gnus-x-face ((t (:background ,sunburn-fg :foreground ,sunburn-bg))))
        `(evaporate-face ((t (:foreground ,sunburn-grey+1))))

	;; (gnus-cite-1 ((t (:inherit message-cited-text))))
	`(gnus-cite-1 ((t (:foreground ,sunburn-blue))))
	`(gnus-cite-2 ((t (:foreground ,sunburn-blue-1))))
	`(gnus-cite-3 ((t (:foreground ,sunburn-blue-2))))
	;;      (gnus-cite-4 ((t (:foreground ,sunburn-blue-3))))
	;;      (gnus-cite-5 ((t (:foreground ,sunburn-blue-4))))
	;;      (gnus-cite-6 ((t (:foreground ,sunburn-red-4))))
	;;      (gnus-cite-5 ((t (:foreground ,sunburn-red-3))))
	`(gnus-cite-4 ((t (:foreground ,sunburn-green+2))))
	`(gnus-cite-5 ((t (:foreground ,sunburn-green+1))))
	`(gnus-cite-6 ((t (:foreground ,sunburn-green))))
	`(gnus-cite-7 ((t (:foreground ,sunburn-red))))
	`(gnus-cite-8 ((t (:foreground ,sunburn-red-1))))
	`(gnus-cite-9 ((t (:foreground ,sunburn-red-2))))
	`(gnus-cite-10 ((t (:foreground ,sunburn-green-1))))
	`(gnus-cite-11 ((t (:foreground ,sunburn-green))))

	`(gnus-group-news-1-empty ((t (:foreground ,sunburn-green))))
	`(gnus-group-news-2-empty ((t (:foreground ,sunburn-green+3))))
	`(gnus-group-news-3-empty ((t (:foreground ,sunburn-green+1))))
	`(gnus-group-news-4-empty ((t (:foreground ,sunburn-blue-2))))
	`(gnus-group-news-5-empty ((t (:foreground ,sunburn-blue-3))))
	`(gnus-group-news-6-empty ((t (:inherit sunburn-lowlight-1))))
	`(gnus-group-news-low-empty ((t (:inherit sunburn-lowlight-1))))

	`(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
	`(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
	`(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
	`(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
	`(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
	`(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
	`(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))

	`(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
	`(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
	`(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
	`(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
	`(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
	`(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
	`(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))

	`(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
	`(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
	`(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
	`(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
	`(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
	`(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
	`(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))

	`(gnus-signature ((t (:foreground ,sunburn-green))))

	`(gnus-summary-selected
	   ((t (:inherit sunburn-primary-1))))
	`(gnus-summary-cancelled
	   ((t (:inherit sunburn-highlight-alerting))))

	`(gnus-summary-low-ticked
	   ((t (:inherit sunburn-primary-2))))
	`(gnus-summary-normal-ticked
	   ((t (:inherit sunburn-primary-2))))
	`(gnus-summary-high-ticked
	   ((t (:inherit sunburn-primary-2))))

	`(gnus-summary-low-unread
	   ((t (:inherit sunburn-foreground :weight normal))))
	`(gnus-summary-normal-unread
	   ((t (:inherit sunburn-foreground :weight normal))))
	`(gnus-summary-high-unread
	   ((t (:inherit sunburn-foreground :weight bold))))

	`(gnus-summary-low-read
	   ((t (:inherit sunburn-green :weight normal))))
	`(gnus-summary-normal-read
	   ((t (:inherit sunburn-green :weight normal))))
	`(gnus-summary-high-read
	   ((t (:inherit sunburn-green :weight bold))))

	`(gnus-summary-low-ancient
	   ((t (:inherit sunburn-blue :weight normal))))
	`(gnus-summary-normal-ancient
	   ((t (:inherit sunburn-blue :weight normal))))
	`(gnus-summary-high-ancient
	   ((t (:inherit sunburn-blue))))

	`(help-argument-name ((t (:weight bold))))

	;; See also the variable definitions at the top of this file
	`(imaxima-latex-error ((t (:inherit font-lock-warning))))

	`(info-xref ((t (:foreground ,sunburn-green :weight bold))))
	`(info-xref-visited ((t (:inherit info-xref :weight normal))))
	`(info-header-xref ((t (:inherit info-xref))))
	`(info-menu-star ((t (:foreground ,sunburn-orange :weight bold))))
	`(info-menu-5 ((t (:inherit info-menu-star))))
	`(info-node ((t (:weight bold))))
	`(info-header-node ((t (:weight normal))))

	`(jabber-roster-user-chatty
	   ((t (:inherit sunburn-primary-1))))
	`(jabber-roster-user-online
	   ((t (:inherit sunburn-primary-2))))
	`(jabber-roster-user-away
	   ((t (:inherit font-lock-doc))))
	`(jabber-roster-user-xa
	   ((t (:inherit font-lock-comment))))
	`(jabber-roster-user-offline
	   ((t (:inherit sunburn-lowlight-1))))
	`(jabber-roster-user-dnd
	   ((t (:inherit sunburn-primary-5))))
	`(jabber-roster-user-error
	   ((t (:inherit font-lock-warning))))

	`(jabber-title-small
	   ((t (:inherit sunburn-title :height 1.2))))
	`(jabber-title-medium
	   ((t (:inherit jabber-title-small :height 1.2))))
	`(jabber-title-large
	   ((t (:inherit jabber-title-medium :height 1.2))))

	`(jabber-chat-prompt-local
	   ((t (:inherit sunburn-primary-1))))
	`(jabber-chat-prompt-foreign
	   ((t (:inherit sunburn-primary-2))))

	`(jabber-rare-time-face
	   ((t (:inherit sunburn-green+1))))

	`(jde-java-font-lock-modifier
	   ((t (:inherit sunburn-primary-2))))
	`(jde-java-font-lock-doc-tag
	   ((t (:inherit sunburn-primary-1))))
	`(jde-java-font-lock-constant
	   ((t (:inherit font-lock-constant))))
	`(jde-java-font-lock-package
	   ((t (:inherit sunburn-primary-3))))
	`(jde-java-font-lock-number
	   ((t (:inherit font-lock-constant))))
	`(jde-java-font-lock-operator
	   ((t (:inherit font-lock-keyword))))
	`(jde-java-font-lock-link
	   ((t (:inherit sunburn-primary-5 :underline t))))

	`(keywiz-right ((t (:inherit sunburn-primary-1))))
	`(keywiz-wrong ((t (:inherit font-lock-warning))))
	`(keywiz-command ((t (:inherit sunburn-primary-2))))

	`(font-latex-bold ((t (:inherit bold))))
	`(font-latex-warning ((t (:inherit font-lock-warning))))
	`(font-latex-sedate ((t (:inherit sunburn-primary-1))))
	`(font-latex-title-4 ((t (:inherit sunburn-title))))

	`(makefile-space ((t (:inherit font-lock-warning))))
	`(makefile-shell ((t (nil))))
	;; This does not work very well because everything that's highlighted
	;; inside the shell region will get its own box.
	;; (makefile-shell ((t (:background ,sunburn-bg+1
	;;                           :box (:line-width 2 :color ,sunburn-bg+1)))))

	`(nxml-delimited-data ((t (:inherit font-lock-string))))
	`(nxml-name ((t (:inherit sunburn-primary-1))))
	`(nxml-ref ((t (:inherit sunburn-primary-5))))
	`(nxml-delimiter ((t (:inherit default))))
	`(nxml-text ((t (:inherit default))))

	`(nxml-comment-content
	   ((t (:inherit font-lock-comment))))
	`(nxml-comment-delimiter
	   ((t (:inherit nxml-comment-content))))
	`(nxml-processing-instruction-target
	   ((t (:inherit sunburn-primary-2))))
	`(nxml-processing-instruction-delimiter
	   ((t (:inherit nxml-processing-instruction-target))))
	`(nxml-processing-instruction-content
	   ((t (:inherit nxml-processing-instruction-target))))
	`(nxml-cdata-section-CDATA
	   ((t (:inherit sunburn-primary-4))))
	`(nxml-cdata-section-delimiter
	   ((t (:inherit nxml-cdata-section-CDATA))))
	`(nxml-cdata-section-content
	   ((t (:inherit nxml-text))))
	`(nxml-entity-ref-name
	   ((t (:inherit sunburn-primary-5))))
	`(nxml-entity-ref-delimiter
	   ((t (:inherit nxml-entity-ref-name))))
	`(nxml-char-ref-number
	   ((t (:inherit nxml-entity-ref-name))))
	`(nxml-char-ref-delimiter
	   ((t (:inherit nxml-entity-ref-delimiter))))

	`(nxml-tag-delimiter ((t (:inherit default))))
	`(nxml-tag-slash ((t (:inherit default))))
	`(nxml-element-local-name ((t (:inherit sunburn-primary-1))))
	`(nxml-element-prefix ((t (:inherit default))))
	`(nxml-element-colon ((t (:inherit default))))

	`(nxml-attribute-local-name
	   ((t (:inherit sunburn-primary-3))))
	`(nxml-namespace-attribute-prefix
	   ((t (:inherit nxml-attribute-local-name))))
	`(nxml-attribute-value
	   ((t (:inherit font-lock-string))))
	`(nxml-attribute-value-delimiter
	   ((t (:inherit nxml-attribute-value))))
	`(nxml-attribute-prefix
	   ((t (:inherit default))))
	`(nxml-namespace-attribute-xmlns
	   ((t (:inherit nxml-attribute-prefix))))
	`(nxml-attribute-colon
	   ((t (:inherit default))))
	`(nxml-namespace-attribute-colon
	   ((t (:inherit nxml-attribute-colon))))

	`(org-agenda-date-today ((t (:foreground ,sunburn-bright-foreground
				      :slant italic :weight bold))) t)       ;; white
	`(org-agenda-structure ((t (:inherit font-lock-comment-face))))  ;; sunburn-green
	`(org-archived ((t (:foreground ,sunburn-archived-foreground))))                    ;; sunburn-bg slight lighter
	`(org-column ((t (:height 98 :family "DejaVu Sans Mono"))))      ;; n/a
	`(org-checkbox ((t (:background ,sunburn-bg+2 :foreground ,sunburn-bright-foreground    ;; sunburn-fg on sunburn-bg+2
			     :box (:line-width 1 :style released-button))))) ;;   - turn checkboxes into buttons
	`(org-date ((t (:foreground ,sunburn-blue :underline t))))           ;; sunburn-blue
	`(org-deadline-announce ((t (:foreground ,sunburn-red-1))))           ;; sunburn-red-1
	`(org-done ((t (:bold t :weight bold :foreground ,sunburn-green+3))))   ;; sunburn-green+3
	`(org-formula ((t (:foreground ,sunburn-green+1))))                     ;; sunburn-green+1
	`(org-headline-done ((t (:foreground ,sunburn-green+3))))               ;; sunburn-green+3
	`(org-hide ((t (:foreground ,sunburn-hidden-foreground))))                        ;; sunburn-bg slight darker
	`(org-level-1 ((t (:foreground ,sunburn-orange))))                     ;; sunburn-orange
	`(org-level-2 ((t (:foreground ,sunburn-green))))                     ;; sunburn-green
	`(org-level-3 ((t (:foreground ,sunburn-blue))))                     ;; sunburn-blue
	`(org-level-4 ((t (:foreground ,sunburn-cyan))))                     ;; sunburn-cyan
	`(org-level-5 ((t (:foreground ,sunburn-blue-1))))                     ;; sunburn-blue-1
	`(org-level-6 ((t (:foreground ,sunburn-blue-2))))                     ;; sunburn-blue-2
	`(org-level-7 ((t (:foreground ,sunburn-blue-3))))                     ;; sunburn-blue-3
	`(org-level-8 ((t (:foreground ,sunburn-blue-4))))                     ;; sunburn-blue-4
	`(org-link ((t (:foreground ,sunburn-green+1 :underline t))))           ;; sunburn-green+1
					;`(org-priority faces                                            TODO
	`(org-scheduled ((t (:foreground ,sunburn-green+4))))                   ;; sunburn-green+4
	`(org-scheduled-previously ((t (:foreground ,sunburn-red-4))))        ;; sunburn-red-4
	`(org-scheduled-today ((t (:foreground ,sunburn-blue+1))))             ;; sunburn-blue+1
	`(org-special-keyword ((t (:foreground ,sunburn-green-1))))             ;; sunburn-green-1
	`(org-table ((t (:foreground ,sunburn-green+2))))                       ;; sunburn-green+2
	`(org-tag ((t (:bold t :weight bold))))                          ;; n/a
	`(org-time-grid ((t (:foreground ,sunburn-grid-foreground))))                   ;; sunburn-orange slight lighter
	`(org-todo ((t (:bold t :foreground ,sunburn-red :weight bold))))   ;; sunburn-red
	`(org-upcoming-deadline ((t (:inherit font-lock-keyword-face)))) ;; sunburn-fg
	`(org-warning ((t (:bold t :foreground ,sunburn-red :weight bold))));; sunburn-red

	;; TODO
	`(outline-8 ((t (:inherit default))))
	`(outline-7 ((t (:inherit outline-8 :height 1.0))))
	`(outline-6 ((t (:inherit outline-7 :height 1.0))))
	`(outline-5 ((t (:inherit outline-6 :height 1.0))))
	`(outline-4 ((t (:inherit outline-5 :height 1.0))))
	`(outline-3 ((t (:inherit outline-4 :height 1.0))))
	`(outline-2 ((t (:inherit outline-3 :height 1.0))))
	`(outline-1 ((t (:inherit outline-2 :height 1.0))))

	`(setnu-line-number ((t (:inherit sunburn-lowlight-2))))

	`(speedbar-button ((t (:inherit sunburn-primary-1))))
	`(speedbar-file ((t (:inherit sunburn-primary-2))))
	`(speedbar-directory ((t (:inherit sunburn-primary-5))))
	`(speedbar-tag ((t (:inherit font-lock-function-name))))
	`(speedbar-highlight ((t (:underline t))))

	`(strokes-char ((t (:inherit font-lock-keyword))))

	`(todoo-item-header
	   ((t (:inherit sunburn-primary-1))))
	`(todoo-item-assigned-header
	   ((t (:inherit sunburn-primary-2))))
	`(todoo-sub-item-header
	   ((t (:foreground ,sunburn-green))))

	`(tuareg-font-lock-governing
	   ((t (:inherit sunburn-primary-2))))
	`(tuareg-font-lock-interactive-error
	   ((t (:inherit font-lock-warning))))
	`(tuareg-font-lock-interactive-output
	   ((t (:inherit sunburn-primary-3))))
	`(tuareg-font-lock-operator
	   ((t (:inherit font-lock-operator))))

	`(w3m-form-button
	   ((t (:inherit widget-button))))
	`(w3m-form-button-pressed
	   ((t (:inherit widget-button-pressed))))
	`(w3m-form-button-mouse
	   ((t (:inherit widget-button-pressed))))
	`(w3m-tab-unselected
	   ((t (:box (:line-width 1 :style released-button)))))
	`(w3m-tab-selected
	   ((t (:box (:line-width 1 :style pressed-button)))))
	`(w3m-tab-unselected-retrieving
	   ((t (:inherit (w3m-tab-unselected widget-inactive)))))
	`(w3m-tab-selected-retrieving
	   ((t (:inherit (w3m-tab-selected widget-inactive)))))
	`(w3m-tab-background
	   ((t (:inherit sunburn-highlight-subtle))))
	`(w3m-anchor
	   ((t (:inherit sunburn-primary-1))))
	`(w3m-arrived-anchor
	   ((t (:inherit sunburn-primary-2))))
	`(w3m-image
	   ((t (:inherit sunburn-primary-3))))
	`(w3m-form
	   ((t (:inherit widget-field))))

	`(hl-line ((t (:background ,sunburn-bg-1))))

	`(magit-section-title ((t (:inherit sunburn-primary-1))))
	`(magit-branch ((t (:inherit sunburn-primary-2))))

	`(flyspell-duplicate ((t (:inherit sunburn-primary-1))))
	`(flyspell-incorrect ((t (:inherit font-lock-warning))))

	`(elscreen-tab-other-screen ((t ((:foreground ,sunburn-fg
					   :background ,sunburn-green-1)))))
	`(elscreen-tab-current-screen ((t (:foreground ,sunburn-blue+1
					    :background ,sunburn-mode-line-background))))

	`(wl-highlight-message-headers ((t (:inherit sunburn-red+1))))
	`(wl-highlight-message-header-contents ((t (:inherit sunburn-green))))
	`(wl-highlight-message-important-header-contents
	   ((t (:inherit sunburn-green))))
	`(wl-highlight-message-important-header-contents2
	   ((t (:inherit sunburn-blue))))
	`(wl-highlight-message-unimportant-header-contents
	   ((t (:inherit sunburn-term-dark-gray))))   ;; reuse term
	`(wl-highlight-message-citation-header ((t (:inherit sunburn-red))))

	`(wl-highlight-message-cited-text-1 ((t (:inherit sunburn-green))))
	`(wl-highlight-message-cited-text-2 ((t (:inherit sunburn-blue))))
	`(wl-highlight-message-cited-text-3 ((t (:foreground ,sunburn-archived-foreground))))
	`(wl-highlight-message-cited-text-4 ((t (:inherit sunburn-green))))

	`(wl-highlight-message-signature ((t (:inherit sunburn-green))))

	`(wl-highlight-summary-answered ((t (:inherit sunburn-fg))))
	`(wl-highlight-summary-new ((t (:foreground ,sunburn-new-foreground))))

	`(wl-highlight-summary-displaying ((t (:underline t
						:foreground ,sunburn-green+1))))

	`(wl-highlight-thread-indent ((t (:foreground ,sunburn-indent-foreground))))
	`(wl-highlight-summary-thread-top ((t (:foreground ,sunburn-top-foreground))))

	`(wl-highlight-summary-normal ((t (:inherit sunburn-fg))))

	`(wl-highlight-folder-zero ((t (:inherit sunburn-fg))))
	`(wl-highlight-folder-few ((t (:inherit sunburn-red+1))))
	`(wl-highlight-folder-many ((t (:inherit sunburn-red+1))))
	`(wl-highlight-folder-unread ((t (:foreground ,sunburn-new-foreground))))

	`(wl-highlight-folder-path ((t (:inherit sunburn-orange))))

	`(twitter-time-stamp ((t (:foreground ,sunburn-orange :background ,sunburn-mode-line-background))))
	`(twitter-user-name ((t (:foreground ,sunburn-mode-line-foreground :background ,sunburn-mode-line-background))))
	`(twitter-header ((t (:foreground ,sunburn-orange :background ,sunburn-mode-line-background))))

	`(rpm-spec-dir ((t (:inherit sunburn-green))))
	`(rpm-spec-doc ((t (:inherit sunburn-green))))
	`(rpm-spec-ghost ((t (:inherit sunburn-red))))
	`(rpm-spec-macro ((t (:inherit sunburn-green))))
	`(rpm-spec-obsolete-tag ((t (:inherit sunburn-red))))
	`(rpm-spec-package ((t (:inherit sunburn-red))))
	`(rpm-spec-section ((t (:inherit sunburn-green))))
	`(rpm-spec-tag ((t (:inherit sunburn-blue))))
	`(rpm-spec-var ((t (:inherit sunburn-red))))

	`(mew-face-header-subject ((t (:inherit sunburn-orange))))
	`(mew-face-header-from ((t (:inherit sunburn-green))))
	`(mew-face-header-date ((t (:inherit sunburn-green))))
	`(mew-face-header-to ((t (:inherit sunburn-red))))
	`(mew-face-header-key ((t (:inherit sunburn-green))))
	`(mew-face-header-private ((t (:inherit sunburn-green))))
	`(mew-face-header-important ((t (:inherit sunburn-blue))))
	`(mew-face-header-marginal ((t (:inherit sunburn-term-dark-gray))))
	`(mew-face-header-warning ((t (:inherit sunburn-red))))
	`(mew-face-header-xmew ((t (:inherit sunburn-green))))
	`(mew-face-header-xmew-bad ((t (:inherit sunburn-red))))
	`(mew-face-body-url ((t (:inherit sunburn-orange))))
	`(mew-face-body-comment ((t (:inherit sunburn-term-dark-gray))))
	`(mew-face-body-cite1 ((t (:inherit sunburn-green))))
	`(mew-face-body-cite2 ((t (:inherit sunburn-blue))))
	`(mew-face-body-cite3 ((t (:inherit sunburn-orange))))
	`(mew-face-body-cite4 ((t (:inherit sunburn-green))))
	`(mew-face-body-cite5 ((t (:inherit sunburn-red))))
	`(mew-face-mark-review ((t (:inherit sunburn-blue))))
	`(mew-face-mark-escape ((t (:inherit sunburn-green))))
	`(mew-face-mark-delete ((t (:inherit sunburn-red))))
	`(mew-face-mark-unlink ((t (:inherit sunburn-green))))
	`(mew-face-mark-refile ((t (:inherit sunburn-green))))
	`(mew-face-mark-unread ((t (:inherit sunburn-red-2))))
	`(mew-face-eof-message ((t (:inherit sunburn-green))))
	`(mew-face-eof-part ((t (:inherit sunburn-green))))

        `(slime-repl-inputed-output-face ((t (:inherit font-lock-warning))))
        `(slime-highlight-edits-face ((t (:background ,sunburn-highlight-background))))
        `(magit-item-highlight ((t (:background ,sunburn-highlight-background))))
        `(magit-diff-add ((t (:foreground ,sunburn-green))))
        `(magit-diff-del ((t (:foreground ,sunburn-red))))
        `(magit-diff-file-header ((t (:foreground ,sunburn-term-white-foreground))))
        `(magit-diff-hunk-header ((t (:inherit magit-header) (:foreground ,sunburn-hunk-header-foreground))))
        `(magit-diff-none ((t (:foreground ,sunburn-none-foreground))))
        )

      (sunburn-make-face-alias-clauses
	`(Buffer-menu-buffer-face
	   apt-utils-broken-face
	   apt-utils-description-face
	   apt-utils-field-contents-face
	   apt-utils-field-keyword-face
	   apt-utils-normal-package-face
	   apt-utils-summary-face
	   apt-utils-version-face
	   apt-utils-virtual-package-face
	   breakpoint-disabled-bitmap-face
	   breakpoint-enabled-bitmap-face
	   calendar-today-face
	   change-log-date-face
	   compilation-info-face
	   compilation-warning-face
	   cua-rectangle-face
	   custom-button-face
	   custom-button-pressed-face
	   custom-changed-face
	   custom-comment-face
	   custom-comment-tag-face
	   custom-documentation-face
	   custom-face-tag-face
	   custom-group-tag-face
	   custom-group-tag-face-1
	   custom-invalid-face
	   custom-modified-face
	   custom-rogue-face
	   custom-saved-face
	   custom-set-face
	   custom-state-face
	   custom-variable-button-face
	   custom-variable-tag-face
	   diary-face
	   dictionary-button-face
	   dictionary-reference-face
	   dictionary-word-entry-face
	   diff-added-face
	   diff-context-face
	   diff-file-header-face
	   diff-header-face
	   diff-hunk-header-face
	   diff-index-face
	   diff-removed-face
	   diff-refine-change-face
	   emms-pbi-current-face
	   emms-pbi-mark-marked-face
	   emms-pbi-song-face
	   erc-action-face
	   erc-bold-face
	   erc-current-nick-face
	   erc-dangerous-host-face
	   erc-default-face
	   erc-direct-msg-face
	   erc-error-face
	   erc-fool-face
	   erc-highlight-face
	   erc-input-face
	   erc-keyword-face
	   erc-nick-default-face
	   erc-nick-msg-face
	   erc-notice-face
	   erc-pal-face
	   erc-prompt-face
	   erc-timestamp-face
	   erc-underline-face
	   eshell-ls-archive-face
	   eshell-ls-backup-face
	   eshell-ls-clutter-face
	   eshell-ls-directory-face
	   eshell-ls-executable-face
	   eshell-ls-missing-face
	   eshell-ls-product-face
	   eshell-ls-special-face
	   eshell-ls-symlink-face
	   eshell-ls-unreadable-face
	   eshell-prompt-face
	   fancy-widget-button-face
	   fancy-widget-button-highlight-face
	   fancy-widget-button-pressed-face
	   fancy-widget-button-pressed-highlight-face
	   fancy-widget-documentation-face
	   fancy-widget-field-face
	   fancy-widget-inactive-face
	   fancy-widget-single-line-field-face
	   font-latex-bold-face
	   font-latex-sedate-face
	   font-latex-title-4-face
	   font-latex-warning-face
	   font-lock-builtin-face
	   font-lock-comment-delimiter-face
	   font-lock-comment-face
	   font-lock-constant-face
	   font-lock-doc-face
	   font-lock-doc-string-face
	   font-lock-function-name-face
	   font-lock-keyword-face
	   font-lock-negation-char-face
	   font-lock-operator-face
	   font-lock-preprocessor-face
	   font-lock-pseudo-keyword-face
	   font-lock-string-face
	   font-lock-type-face
	   font-lock-variable-name-face
	   font-lock-warning-face
	   gnus-cite-face-1
	   gnus-cite-face-10
	   gnus-cite-face-11
	   gnus-cite-face-2
	   gnus-cite-face-3
	   gnus-cite-face-4
	   gnus-cite-face-5
	   gnus-cite-face-6
	   gnus-cite-face-7
	   gnus-cite-face-8
	   gnus-cite-face-9
	   gnus-group-mail-1-empty-face
	   gnus-group-mail-2-empty-face
	   gnus-group-mail-3-empty-face
	   gnus-group-mail-3-face
	   gnus-group-news-1-empty-face
	   gnus-group-news-2-empty-face
	   gnus-group-news-3-empty-face
	   gnus-header-content-face
	   gnus-header-from-face
	   gnus-header-name-face
	   gnus-header-newsgroups-face
	   gnus-header-subject-face
	   gnus-signature-face
	   gnus-summary-cancelled-face
	   gnus-summary-high-ancient-face
	   gnus-summary-high-read-face
	   gnus-summary-high-ticked-face
	   gnus-summary-high-unread-face
	   gnus-summary-low-ancient-face
	   gnus-summary-low-read-face
	   gnus-summary-low-ticked-face
	   gnus-summary-low-unread-face
	   gnus-summary-normal-ancient-face
	   gnus-summary-normal-read-face
	   gnus-summary-normal-ticked-face
	   gnus-summary-normal-unread-face
	   gnus-summary-selected-face
	   highlight-current-line-face
	   holiday-face
	   ibuffer-deletion-face
	   ibuffer-help-buffer-face
	   ibuffer-marked-face
	   ibuffer-special-buffer-face
	   ido-first-match-face
	   ido-only-match-face
	   ido-subdir-face
	   imaxima-latex-error-face
	   isearch-lazy-highlight-face
	   jde-java-font-lock-constant-face
	   jde-java-font-lock-doc-tag-face
	   jde-java-font-lock-link-face
	   jde-java-font-lock-modifier-face
	   jde-java-font-lock-number-face
	   jde-java-font-lock-operator-face
	   jde-java-font-lock-package-face
	   keywiz-command-face
	   keywiz-right-face
	   keywiz-wrong-face
	   makefile-shell-face
	   makefile-space-face
	   message-cited-text-face
	   message-header-cc-face
	   message-header-from-face
	   message-header-name-face
	   message-header-newsgroups-face
	   message-header-other-face
	   message-header-subject-face
	   message-header-to-face
	   message-header-xheader-face
	   message-mml-face
	   message-separator-face
	   mtorus-highlight-face
	   mtorus-notify-highlight-face
	   nxml-attribute-colon-face
	   nxml-attribute-local-name-face
	   nxml-attribute-prefix-face
	   nxml-attribute-value-delimiter-face
	   nxml-attribute-value-face
	   nxml-cdata-section-CDATA-face
	   nxml-cdata-section-content-face
	   nxml-cdata-section-delimiter-face
	   nxml-char-ref-delimiter-face
	   nxml-char-ref-number-face
	   nxml-comment-content-face
	   nxml-comment-delimiter-face
	   nxml-delimited-data-face
	   nxml-delimiter-face
	   nxml-element-colon-face
	   nxml-element-local-name-face
	   nxml-element-prefix-face
	   nxml-entity-ref-delimiter-face
	   nxml-entity-ref-name-face
	   nxml-name-face
	   nxml-namespace-attribute-colon-face
	   nxml-namespace-attribute-prefix-face
	   nxml-namespace-attribute-xmlns-face
	   nxml-processing-instruction-content-face
	   nxml-processing-instruction-delimiter-face
	   nxml-processing-instruction-target-face
	   nxml-ref-face
	   nxml-tag-delimiter-face
	   nxml-tag-slash-face
	   nxml-text-face
	   org-agenda-date-today-face
	   org-agenda-structure-face
	   org-archived-face
	   org-column-face
					;org-checkbox-face
	   org-date-face
	   org-deadline-announce-face
	   org-done-face
	   org-formula-face
	   org-headline-done-face
	   org-hide-face
	   org-level-1-face
	   org-level-2-face
	   org-level-3-face
	   org-level-4-face
	   org-level-5-face
	   org-level-6-face
	   org-level-7-face
	   org-level-8-face
	   org-link-face
					;org-priority-face
	   org-scheduled-face
	   org-scheduled-previously-face
	   org-scheduled-today-face
	   org-special-keyword-face
	   org-table-face
	   org-tag-face
	   org-time-grid-face
	   org-todo-face
	   org-upcoming-deadline-face
	   org-warning-face
	   paren-face
	   plain-widget-button-face
	   plain-widget-button-pressed-face
	   plain-widget-documentation-face
	   plain-widget-field-face
	   plain-widget-inactive-face
	   plain-widget-single-line-field-face
	   rpm-spec-dir-face
	   rpm-spec-doc-face
	   rpm-spec-ghost-face
	   rpm-spec-macro-face
	   rpm-spec-obsolete-tag-face
	   rpm-spec-package-face
	   rpm-spec-section-face
	   rpm-spec-tag-face
	   rpm-spec-var-face
	   setnu-line-number-face
	   show-paren-match-face
	   show-paren-mismatch-face
	   speedbar-button-face
	   speedbar-directory-face
	   speedbar-file-face
	   speedbar-highlight-face
	   speedbar-tag-face
	   strokes-char-face
	   todoo-item-assigned-header-face
	   todoo-item-header-face
	   todoo-sub-item-header-face
	   tuareg-font-lock-governing-face
	   tuareg-font-lock-interactive-error-face
	   tuareg-font-lock-interactive-output-face
	   tuareg-font-lock-operator-face
	   w3m-anchor-face
	   w3m-arrived-anchor-face
	   w3m-form-button-face
	   w3m-form-button-mouse-face
	   w3m-form-button-pressed-face
	   w3m-form-face
	   w3m-image-face
	   w3m-tab-background-face
	   w3m-tab-selected-face
	   w3m-tab-selected-retrieving-face
	   w3m-tab-unselected-face
	   w3m-tab-unselected-retrieving-face
	   widget-button-face
	   widget-button-highlight-face
	   widget-button-pressed-face
	   widget-button-pressed-highlight-face
	   widget-documentation-face
	   widget-field-face
	   widget-inactive-face
	   widget-single-line-field-face
	   flyspell-duplicate-face
	   flyspell-incorrect-face
	   wl-highlight-message-headers-face
	   wl-highlight-message-header-contents-face
	   wl-highlight-message-important-header-contents-face
	   wl-highlight-message-important-header-contents2-face
	   wl-highlight-message-unimportant-header-contents-face
	   wl-highlight-message-citation-header-face
	   wl-highlight-message-cited-text-1-face
	   wl-highlight-message-cited-text-2-face
	   wl-highlight-message-cited-text-3-face
	   wl-highlight-message-cited-text-4-face
	   wl-highlight-message-signature-face
	   wl-highlight-summary-answered-face
	   wl-highlight-summary-new-face
	   wl-highlight-summary-displaying-face
	   wl-highlight-thread-indent-face
	   wl-highlight-summary-thread-top-face
	   wl-highlight-summary-normal-face
	   wl-highlight-folder-zero-face
	   wl-highlight-folder-few-face
	   wl-highlight-folder-many-face
	   wl-highlight-folder-unread-face
	   wl-highlight-folder-path-face
	   elscreen-tab-background-face
	   elscreen-tab-control-face
	   elscreen-tab-current-screen-face
	   elscreen-tab-other-screen-face
	   identica-uri-face
	   twitter-time-stamp-face
	   twitter-user-name-face
	   twitter-header-face))
      )))

(defalias 'sunburn #'color-theme-sunburn)

(provide 'sunburn)

;; Local Variables:
;; time-stamp-format: "%:y-%02m-%02d %02H:%02M"
;; time-stamp-start: "Updated: "
;; time-stamp-end: "$"
;; End:

;;; sunburn.el ends here.
