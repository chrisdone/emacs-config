;;; purescript-mode.el ---

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'haskell-mode)

(define-derived-mode purescript-mode
  haskell-mode "Purescript"
  "Major mode for purescript."
  (setq case-fold-search t))

(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))

(define-key purescript-mode-map [f5]
  (lambda ()
    (interactive)
    (compile "cd ../; sh check.sh")))

(define-key purescript-mode-map [f9]
  (lambda ()
    (interactive)
    (compile "cd ../; sh build.sh")))

(add-hook 'purescript-mode-hook 'flycheck-mode)
(add-hook 'purescript-mode-hook 'company-mode)

(provide 'purescript-mode)
