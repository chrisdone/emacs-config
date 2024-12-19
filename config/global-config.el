
;; Overrides

(setq
 isearch-lazy-count t
 lazy-count-prefix-format nil
 lazy-count-suffix-format "   (%s/%s)"
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 suggest-key-bindings nil
 ido-auto-merge-delay-time 99999
 require-final-newline t
 frame-resize-pixelwise t
 explicit-shell-file-name "/bin/bash"
 tags-revert-without-query 1
 async-shell-command-buffer 'new-buffer)


;; Defaults

(set-default 'tags-case-fold-search nil)
(setq-default indent-tabs-mode nil)
(setq-default auto-fill-function nil)


;; Puts

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Fsets

(fset 'yes-or-no-p 'y-or-n-p)

(defvar usr1-hooks (list))


;; Faces

(custom-set-faces
 '(default ((t (:family "Ubuntu Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(link ((t (:foreground "#7f9f7f" :underline t)))))

(custom-set-faces
 `(visual-replace-delete-match
   ((t (:strike-through t :background ,zenburn-red :foreground "black")))))
