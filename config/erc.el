
;; Requirements

(require 'erc)


;; Functions

(defun my-erc-hook ()
  "Disable ERC crap and set ignores."
  (erc-fill-disable)
  (setq erc-ignore-list '("cntrational" "selpa`i"))
  (setq erc-ignore-reply-list '("cntrational" "selpa`i")))

(defun erc-insert-emoticon ()
  "Prompt to insert an emoticon."
  (interactive)
  (let* ((category (ido-completing-read "Category: " (mapcar 'car *japanese-emoticons*)))
         (choices (assoc category *japanese-emoticons*)))
    (when choices (insert (ido-completing-read "Emoticon: " (cdr choices))))))


;; Keybindings

(define-key erc-mode-map (kbd "C-c e") 'erc-insert-emoticon)


;; Hooks

(add-hook 'erc-mode-hook 'my-erc-hook)
(add-hook 'erc-mode-hook 'erc-fill-disable)


;; Faces

(custom-set-faces
 '(erc-my-nick-face ((t (:foreground "#dca3a3" :weight bold)))))


;; Mode settings

(setq erc-max-buffer-size 10000)
(erc-truncate-mode 1)
