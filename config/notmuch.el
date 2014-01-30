(setq notmuch-tag-formats
      `(("unread" (propertize tag 'face '(:foreground ,zenburn-red)))
        ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
      notmuch-show-logo nil
      notmuch-hello-thousands-separator ","
      notmuch-search-oldest-first nil
      notmuch-search-line-faces
      `(("unread" :weight bold)
        ("flagged" :foreground ,zenburn-blue)))
