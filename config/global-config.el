
;; Overrides

(setq
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 suggest-key-bindings nil
 ido-auto-merge-delay-time 99999
 require-final-newline t
 frame-resize-pixelwise t
 explicit-shell-file-name "/bin/bash"
 tags-revert-without-query 1)


;; Defaults

(set-default 'tags-case-fold-search nil)
(setq-default indent-tabs-mode nil)


;; Puts

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Fsets

(fset 'yes-or-no-p 'y-or-n-p)
