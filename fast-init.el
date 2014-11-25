
;; Start-up mode

(defvar fast-startup nil)


;; Standard libraries needed

(require 'cl)


;; Packages and configs to load

(defvar packages
  '(god-mode
    smex
    goto-last-change
    ace-jump-mode
    goto-last-point
    resmacro)
  "Packages whose location follows the
  packages/package-name/package-name.el format.")

(defvar configs
  '("global"
    "god")
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


;; Emacs configurations

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "config/"
                       name ".el")))


;; Mode initializations

(smex-initialize)
(god-mode)
(goto-last-point-mode)
