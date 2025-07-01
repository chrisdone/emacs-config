;; 2024-03-07 modified by chris done for diary-mode to display markdown link syntax as rich text links

;;; jumpto-addr.el --- click to browse URL or to send to e-mail address  -*- lexical-binding: t; -*-

;; Copyright (C) 1995, 2000-2023 Free Software Foundation, Inc.

;; Author: Eric Ding <ericding@alum.mit.edu>
;; Maintainer: emacs-devel@gnu.org
;; Created: 15 Aug 1995
;; Keywords: www, mouse, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.  By default, we bind to
;; the [mouse-2] and the [C-c return] key sequences.

;; INSTALLATION
;;
;; To use jumpto-address in a particular mode (this example uses
;; the fictional rich-text-mode), add this to your init file:
;;
;; (add-hook 'rich-text-mode-hook 'jumpto-address)
;;
;; The mouse click method is bound to [mouse-2] on highlighted URLs or
;; e-mail addresses only; it functions normally everywhere else.  To bind
;; another mouse click to the function, add the following to your .emacs
;; (for example):
;;
;; (setq jumpto-address-highlight-keymap
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m [S-mouse-2] 'jumpto-address-at-point)
;;     m))
;;

;; Known bugs/features:
;; * jumpto-address-mail-regexp only catches foo@bar.org style addressing,
;;   not stuff like X.400 addresses, etc.
;; * regexp also catches Message-Id line, since it is in the format of
;;   an Internet e-mail address (like Compuserve addresses)
;; * If the buffer is fontified after jumpto-address-fontify is run
;;   (say, using font-lock-fontify-buffer), then font-lock faces will
;;   override jumpto-address faces.

;;; Code:

(require 'seq)
(require 'thingatpt)
(autoload 'browse-url-url-at-point "browse-url")

(defgroup jumpto-address nil
  "Click to browse URL or to send to e-mail address."
  :group 'mouse
  :group 'comm)


;; I don't expect users to want fontify'ing without highlighting.
(defcustom jumpto-address-fontify-p t
  "Non-nil means URLs and e-mail addresses in buffer are fontified.
But only if `jumpto-address-highlight-p' is also non-nil."
  :type 'boolean)

(defcustom jumpto-address-highlight-p t
  "Non-nil means URLs and e-mail addresses in buffer are highlighted."
  :type 'boolean)

(defcustom jumpto-address-fontify-maximum-size 30000
  "Maximum size of file in which to fontify and/or highlight URLs.
A value of t means there is no limit--fontify regardless of the size."
  :type '(choice (integer :tag "Maximum size") (const :tag "No limit" t)))

(defvar jumpto-address-mail-regexp
  ;; Actually pretty much any char could appear in the username part.  -stef
  "[-a-zA-Z0-9=._+]+@\\([-a-zA-Z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression probably matching an e-mail address.")

(defvar jumpto-address-uri-schemes-ignored
  ;; By default we exclude `mailto:' (email addresses are matched
  ;; by `jumpto-address-mail-regexp') and also `data:', as it is not
  ;; terribly useful to follow those URIs, and leaving them causes
  ;; `use Data::Dumper;' to be fontified oddly in Perl files.
  '("mailto:" "data:")
  "List of URI schemes to exclude from `jumpto-address-uri-schemes'.

Customizations to this variable made after jumpto-addr is loaded
will have no effect.")

(defvar jumpto-address-uri-schemes
  ;; We use `thing-at-point-uri-schemes', with a few exclusions,
  ;; as listed in `jumpto-address-uri-schemes-ignored'.
  (seq-reduce (lambda (accum elt) (delete elt accum))
              jumpto-address-uri-schemes-ignored
              (copy-sequence thing-at-point-uri-schemes))
  "List of URI schemes matched by `jumpto-address-url-regexp'.

Customizations to this variable made after jumpto-addr is loaded
will have no effect.")

(defvar jumpto-address-url-regexp
  (concat "\\<"
          (regexp-opt jumpto-address-uri-schemes t)
          thing-at-point-url-path-regexp)
  "A regular expression probably matching a URL.")

(defvar jumpto-address-highlight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<mouse-2>") #'jumpto-address-at-point)
    (define-key m (kbd "C-c RET") #'jumpto-address-at-point)
    (define-key m (kbd "C-c C-o") #'jumpto-address-at-point)
    m)
  "Keymap to hold jumpto-addr's mouse key defs under highlighted URLs.")

(defun jumpto-address-context-menu (menu click)
  "Populate MENU with `jumpto-address' commands at CLICK."
  (when (mouse-posn-property (event-start click) 'jumpto-address)
    (define-key menu [jumpto-address-separator] menu-bar-separator)
    (define-key menu [jumpto-address-at-mouse]
      '(menu-item "Follow Link" jumpto-address-at-mouse
                  :help "Follow a link where you click")))
  menu)

(defcustom jumpto-address-url-face 'link
  "Face to use for URLs."
  :type 'face)

(defcustom jumpto-address-url-mouse-face 'highlight
  "Face to use for URLs when the mouse is on them."
  :type 'face)

(defcustom jumpto-address-mail-face 'italic
  "Face to use for e-mail addresses."
  :type 'face)

(defcustom jumpto-address-mail-mouse-face 'secondary-selection
  "Face to use for e-mail addresses when the mouse is on them."
  :type 'face)

(defun jumpto-address-unfontify (start end)
  "Remove `jumpto-address' fontification from the given region."
  (dolist (overlay (overlays-in start end))
    (if (overlay-get overlay 'jumpto-address)
	(delete-overlay overlay))))

(defvar jumpto-address-prog-mode)

(defun jumpto-address-fontify (&optional start end)
  "Fontify the URLs and e-mail addresses in the current buffer.
This function implements `jumpto-address-highlight-p'
and `jumpto-address-fontify-p'."
  ;; Clean up from any previous go.
  (jumpto-address-unfontify (or start (point-min)) (or end (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (when (or (eq t jumpto-address-fontify-maximum-size)
	      (< (- (or end (point-max)) (point))
                 jumpto-address-fontify-maximum-size))
      (while (re-search-forward "\\[\\(\\(.\\|\n\\)*?\\)\\](\\(.*?\\))"
                                ;; jumpto-address-url-regexp
                                end t)
	(let* ((s (match-beginning 0))
	       (e (match-end 0))
               (title (match-string 1))
               (url (match-string 2))
	       this-overlay)
	  (when (or (not jumpto-address-prog-mode)
		    ;; This tests for both comment and string
		    ;; syntax.
		    (nth 8 (syntax-ppss)))
	    (setq this-overlay (make-overlay s e))
	    (and jumpto-address-fontify-p
		 (overlay-put this-overlay 'face jumpto-address-url-face))
	    (overlay-put this-overlay 'display title)
            (put-text-property s e 'jumpto-url url)
            (overlay-put this-overlay 'evaporate t)
	    (overlay-put this-overlay
			 'mouse-face jumpto-address-url-mouse-face)
	    (overlay-put this-overlay 'follow-link t)
	    (overlay-put this-overlay
			 'help-echo "mouse-2, C-c RET: follow URL")
	    (overlay-put this-overlay
			 'keymap jumpto-address-highlight-keymap)
	    (overlay-put this-overlay 'jumpto-address t))))
      (goto-char (or start (point-min)))
      (while (re-search-forward jumpto-address-mail-regexp end t)
	(let* ((s (match-beginning 0))
	       (e (match-end 0))
	       this-overlay)
	  (when (or (not jumpto-address-prog-mode)
		    ;; This tests for both comment and string
		    ;; syntax.
		    (nth 8 (syntax-ppss)))
	    (setq this-overlay (make-overlay s e))
	    (and jumpto-address-fontify-p
		 (overlay-put this-overlay 'face jumpto-address-mail-face))
	    (overlay-put this-overlay 'evaporate t)
	    (overlay-put this-overlay 'mouse-face
			 jumpto-address-mail-mouse-face)
	    (overlay-put this-overlay 'follow-link t)
	    (overlay-put this-overlay
			 'help-echo "mouse-2, C-c RET: mail this address")
	    (overlay-put this-overlay
			 'keymap jumpto-address-highlight-keymap)
	    (overlay-put this-overlay 'jumpto-address t)))))))

(defun jumpto-address-fontify-region (start end)
  "Fontify URLs and e-mail addresses in the given region."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
          (end-line (progn (goto-char end) (line-end-position))))
      (jumpto-address-fontify beg-line end-line))))

;; code to find and goto addresses; much of this has been blatantly
;; snarfed from browse-url.el

;;;###autoload
(defun jumpto-address-at-point (&optional event)
  "Compose a new message to the e-mail address or open URL at point.

Compose message to address at point.  See documentation for
`jumpto-address-find-address-at-point'.

If no e-mail address is found at point, open the URL at or before
point using `browse-url'.  With a prefix argument, open the URL
using `browse-url-secondary-browser-function' instead."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (if (get-text-property (point) 'jumpto-url)
        (progn ;; (message "Got jumpto-url: %S" (get-text-property (point) 'jumpto-url))
               (if (string-match "^https?://" (get-text-property (point) 'jumpto-url))
                   (browse-url (get-text-property (point) 'jumpto-url))
                 (find-file-other-window (get-text-property (point) 'jumpto-url))))
        (let ((address (save-excursion (jumpto-address-find-address-at-point))))
          (if (and address
                   (save-excursion
                     (goto-char (previous-single-char-property-change
                                 (point) 'jumpto-address nil
                                 (line-beginning-position)))
                     (not (looking-at jumpto-address-url-regexp))))
              (compose-mail address)
            (if-let ((url (browse-url-url-at-point)))
                (browse-url-button-open-url url)
              (error "No e-mail address or URL found")))))))

(defun jumpto-address-find-address-at-point ()
  "Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return nil."
  (re-search-backward "[^-_A-Za-z0-9.@]" (line-beginning-position) 'lim)
  (if (or (looking-at jumpto-address-mail-regexp)	; already at start
	  (and (re-search-forward jumpto-address-mail-regexp
				  (line-end-position) 'lim)
	       (goto-char (match-beginning 0))))
      (match-string-no-properties 0)))

(defun jumpto-address-at-mouse (click)
  "Send to the e-mail address or load the URL at mouse click."
  (interactive "e")
  (jumpto-address-at-point click))

;;;###autoload
(defun jumpto-address ()
  "Sets up jumpto-address functionality in the current buffer.
Allows user to use mouse/keyboard command to click to go to a URL
or to send e-mail.
By default, jumpto-address binds `jumpto-address-at-point' to mouse-2 and C-c RET
only on URLs and e-mail addresses.

Also fontifies the buffer appropriately (see `jumpto-address-fontify-p' and
`jumpto-address-highlight-p' for more information)."
  (interactive)
  (if jumpto-address-highlight-p
      (jumpto-address-fontify)))
;;;###autoload(put 'jumpto-address 'safe-local-eval-function t)

;;;###autoload
(define-minor-mode jumpto-address-mode
  "Minor mode to buttonize URLs and e-mail addresses in the current buffer."
  :lighter ""
  (cond
   (jumpto-address-mode
    (jit-lock-register #'jumpto-address-fontify-region)
    (add-hook 'context-menu-functions 'jumpto-address-context-menu 10 t))
   (t
    (jit-lock-unregister #'jumpto-address-fontify-region)
    (save-restriction
      (widen)
      (jumpto-address-unfontify (point-min) (point-max)))
    (remove-hook 'context-menu-functions 'jumpto-address-context-menu t))))

(defun jumpto-addr-mode--turn-on ()
  (when (not jumpto-address-mode)
    (jumpto-address-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-jumpto-address-mode
  jumpto-address-mode jumpto-addr-mode--turn-on
  :version "28.1")

;;;###autoload
(define-minor-mode jumpto-address-prog-mode
  "Like `jumpto-address-mode', but only for comments and strings."
  :lighter ""
  (if jumpto-address-prog-mode
      (jit-lock-register #'jumpto-address-fontify-region)
    (jit-lock-unregister #'jumpto-address-fontify-region)
    (save-restriction
      (widen)
      (jumpto-address-unfontify (point-min) (point-max)))))

(provide 'jumpto-addr)

;;; jumpto-addr.el ends here
