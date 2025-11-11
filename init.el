
;; Standard libraries needed

(require 'cl)


;; Packages and configs to load

;; Commented out are candidates for removal
(defvar packages
  '(god-mode
    smex
    string-inflection
    git-link
    magit
    color-theme
    zenburn
    sunburn
    inheritenv
    envrc
    ag
    swiper
    watchexec
    quickjump
    hindent
    h98-mode
    ts-mode
    paredit
    markdown-mode
    s ;; for markdown-toc
    dash ;; for markdown-toc
    markdown-toc
    company
    hell-mode
    hiedb
    intero
    hcl-mode
    art-mode
    yaml-mode
    haskell-navigate-imports
    page-break-lines
    portal
    visual-replace)
  "Packages whose location follows the
  packages/package-name/package-name.el format.")

(defvar emacs-bin-path
  (concat
   (file-name-directory (or load-file-name
                            (buffer-file-name)))
   (if (string= (shell-command-to-string "uname -m") "aarch64\n")
       "bin/arm64"
     "bin/amd64")))

(defvar custom-load-paths
  '("git-modes"
    "diary-mode")
  "Custom load paths that don't follow the normal
  package-name/module-name.el format.")

(defvar configs
  '("global-macros"
    "global-functions"
    "global-config"
    "tab-bar-config"
    "god"
    "dired"
    "git"
    "macos"
    "envrc"
    "shell"
    "ivy"
    "art-customization"
    "haskell-functions"
    "haskell-customization"
    "paredit-functions"
    "lisp-functions"
    "paredit-keys"
    "haskell-keys"
    "hooks"
    "autoload"
    "portals"
    ;; Global-keys needs to be at the end to override all other major
    ;; modes.
    "global-keys")
  "Configuration files that follow the config/foo.el file path
  format.")


;; Load packages

(loop for location in custom-load-paths
      do (add-to-list 'load-path
             (concat (file-name-directory (or load-file-name
                                              (buffer-file-name)))
                     "packages/"
                     location)))

(loop for name in packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       "packages/"
                                       (symbol-name name)))
                  (require name))))


;; Custom require calls

(require 'counsel)
(require 'magit-blame)
(require 'eglot)
(require 'jumpto-addr)
(require 'diary-mode)


;; Emacs configurations

(loop for name in configs
      do (load (concat (file-name-directory (or load-file-name
                                                buffer-file-name))
                       "config/"
                       name ".el")))


;; Mode initializations

(progn (load "zenburn") (zenburn))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(god-mode-all)
(smex-initialize)
(global-display-line-numbers-mode)
(ido-mode)
(global-font-lock-mode)
(show-paren-mode)
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(transient-mark-mode)
(delete-selection-mode)
(envrc-global-mode)
(tab-bar-mode)
(global-page-break-lines-mode)
(toggle-truncate-lines -1)


;; Disable auto-revert mode
(magit-auto-revert-mode -1)


;; Extra setups

(set-auto-saves)


;; Binaries

;; Adds binaries to the PATH and exec-path, so that they're available
;; to Emacs and all shells within Emacs.
(let ((bin-dir emacs-bin-path))
  (add-to-list 'exec-path bin-dir)
  (setenv "PATH" (concat bin-dir ":" (getenv "PATH"))))
