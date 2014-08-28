
;; Standard libraries needed

(require 'cl)


;; Packages and configs to load

(defvar packages
  '(color-theme
    zenburn
    sunburn
    god-mode
    paredit
    haskell-mode
    smex
    magit
    notify
    turbo-devel-mode
    hsenv
    goto-last-change
    markdown-mode
    ace-jump-mode
    dash
    elisp-slime-nav
    lpaste
    echo-keys
    column-marker
    align-by-current-symbol
    rainbow-mode
    lua-mode
    ag
    notmuch
    w3m
    goto-last-point
    github-urls
    s
    hamlet-mode
    service
    number))

(defvar configs
  '("global"
    "god"
    "haskell"
    "erc"
    "magit"
    "notmuch"
    "email"
    "lisp"
    "w3m"
    "markdown"))


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

(add-to-list 'load-path
             (concat (file-name-directory load-file-name)
                     "packages/"
                     "ghc-server/elisp"))

(add-to-list 'load-path
             (concat (file-name-directory load-file-name)
                     "packages/"
                     "hindent/elisp"))

(require 'shm)
(require 'hindent)
(require 'ghc)
(require 'shm-case-split)
(require 'w3m-haddock)


;; Global/standard Emacs configuration

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "config/"
                       name ".el")))


;; Mode initializations

(smex-initialize)
(sunburn)
(god-mode)
(goto-last-point-mode)
(turn-on-haskell-simple-indent)
(load "haskell-mode-autoloads.el")
