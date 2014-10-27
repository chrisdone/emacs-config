
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
    goto-last-change
    ace-jump-mode
    markdown-mode
    dash
    elisp-slime-nav
    lpaste
    echo-keys
    align-by-current-symbol
    ag
    w3m
    goto-last-point
    github-urls
    s
    hamlet-mode
    number
    hide-region
    ats-mode
    multiple-cursors
    projects-mode)
  "Packages whose location follows the
  packages/package-name/package-name.el format.")

(defvar custom-load-paths
  '("structured-haskell-mode/elisp"
    "hindent/elisp")
  "Custom load paths that don't follow the normal
  package-name/module-name.el format.")

(defvar configs
  '("global"
    "god"
    "haskell"
    "erc"
    "magit"
    "email"
    "lisp"
    "w3m"
    "markdown"
    "org")
  "Configuration files that follow the config/foo.el file path
  format.")


;; Load packages

(loop for name in packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       "packages/"
                                       (symbol-name name)))
                  (require name))))

(loop for location in custom-load-paths
      do (add-to-list 'load-path
             (concat (file-name-directory load-file-name)
                     "packages/"
                     location)))

(require 'shm)
(require 'hindent)
(require 'shm-case-split)
(require 'shm-reformat)
(require 'w3m-haddock)


;; Emacs configurations

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
