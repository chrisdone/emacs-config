(setq notmuch-tag-formats
      `(("unread" (propertize tag 'face '(:foreground ,zenburn-red)))
        ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
      notmuch-show-logo nil
      notmuch-hello-thousands-separator ","
      notmuch-search-oldest-first nil
      notmuch-search-line-faces
      `(("unread" :weight bold)
        ("flagged" :foreground ,zenburn-blue)))

(setq notmuch-tag-formats
      `(("unread" (propertize tag 'face '(:foreground ,sunburn-red)))
        ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
      notmuch-show-logo nil
      notmuch-hello-thousands-separator ","
      notmuch-search-oldest-first nil
      notmuch-search-line-faces
      `(("unread" :weight bold)
        ("flagged" :foreground ,sunburn-blue)))

(defun notmuch-mark-deleted ()
  "Mark this email as deleted."
  (interactive)
  (notmuch-show-add-tag (list "+deleted"))
  (notmuch-kill-this-buffer)
  (notmuch-refresh-this-buffer))

(define-key notmuch-show-mode-map (kbd "d") 'notmuch-mark-deleted)
(define-key notmuch-show-mode-map (kbd "RET") 'goto-address-at-point)
(define-key notmuch-show-mode-map (kbd "TAB") 'notmuch-show-toggle-message)
(define-key notmuch-search-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
(define-key notmuch-hello-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
