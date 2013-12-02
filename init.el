
;; Standard libraries needed

(require 'cl)


;; Packages and configs to load

(defvar packages
  '(color-theme
    zenburn
    god-mode
    paredit
    haskell-mode
    smex
    magit
    notify
    turbo-devel-mode
    hsenv
    goto-last-change))

(defvar configs
  '("global"
    "god"
    "haskell"
    "erc"
    "magit"))


;; Load packages

(loop for name in packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       "packages/"
                                       (symbol-name name)))
                  (require name))))

(add-to-list 'load-path
             (concat (file-name-directory load-file-name)
                     "packages/"
                     "structured-haskell-mode/elisp"))

(require 'structured-haskell-mode)


;; Global/standard Emacs configuration

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "config/"
                       name ".el")))


;; Mode initializations

(smex-initialize)
(zenburn)
(god-mode)
(turn-on-haskell-simple-indent)
(load "haskell-mode-autoloads.el")
