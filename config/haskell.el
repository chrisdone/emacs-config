
;; Requirements

(require 'haskell-mode)
(require 'hindent)
(require 'haskell-process)
                                        ;(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell)
(require 'intero)
(require 'haskell-font-lock)
(require 'haskell-debug)
(require 'sgml-mode)
(require 'css-mode)
(require 'ghci-script-mode)


;; Functions

(defun shm-repl-tab ()
  "TAB completion or jumping."
  (interactive)
  (unless (shm/jump-to-slot)
    (call-interactively 'haskell-interactive-mode-tab)))

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (unless (= (line-beginning-position)
             (line-end-position))
    (shm/backward-paragraph))
  (unless (= (line-beginning-position)
             (line-end-position))
    (save-excursion (insert "\n")))
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defvar haskell-process-use-ghci nil)

(defun haskell-process-cabal-build-and-restart ()
  "Build and restart the Cabal project."
  (interactive)
  (intero-devel-reload))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     ;; Use ag
     (t (ag-files sym
                  "\\.hs$"
                  (haskell-session-current-dir (haskell-session)))))))

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)
    (god-mode)))

(defun shm-comma-god ()
  (interactive)
  (if god-local-mode
      (call-interactively 'god-mode-self-insert)
    (call-interactively 'shm/comma)))

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if god-local-mode
      (call-interactively 'god-mode-self-insert)
    (progn
      (let ((ident (haskell-ident-at-point)))
        (when ident
          (and interactive-haskell-mode
               (haskell-process-do-try-type ident))))
      (call-interactively 'shm/space))))

(defun shm/insert-putstrln ()
  "Insert a putStrLn."
  (interactive)
  (let ((name
         (save-excursion
           (goto-char (car (shm-decl-points)))
           (buffer-substring-no-properties
            (point)
            (1- (search-forward " "))))))
    (insert
     (format "putStrLn \"%s:%s:%d\""
             (file-name-nondirectory (buffer-file-name))
             name
             (line-number-at-pos)))))


;; Mode settings

(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-ghci '())
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(hindent-style "chris-done"))

(setq haskell-complete-module-preferred
      '("Data.ByteString"
        "Data.ByteString.Lazy"
        "Data.Conduit"
        "Data.Function"
        "Data.List"
        "Data.Map"
        "Data.Maybe"
        "Data.Monoid"
        "Data.Text"
        "Data.Ord"))

(setq haskell-session-default-modules
      '("Control.Monad.Reader"
        "Data.Text"
        "Control.Monad.Logger"))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(setq haskell-process-path-ghci
      "ghci-ng")

(setq haskell-process-args-ghci '("-ferror-spans"))

(setq haskell-process-args-cabal-repl
      '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))

(setq haskell-process-generate-tags nil)

(defvar haskell-quals
  '(("L" . "Data.ByteString.Lazy")
    ("S" . "Data.ByteString")
    ("Csv" . "Data.Csv")
    ("Seq" . "Data.Sequence")
    ("T" . "Data.Text")
    ("V" . "Data.Vector")
    ("LT" . "Data.Text.Lazy")
    ("M" . "Data.Map")
    ("HM" . "Data.HashMap")
    ("Set" . "Data.Set")
    ("List" . "Data.List")
    ("RIO" . "RIO")))

(setq haskell-import-mapping
      '(("Data.Attoparsec.Char8" . "import qualified Data.Attoparsec.Char8 as Atto8
")
        ("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
        ("Data.Text.Encoding" . "import qualified Data.Text.Encoding as T
")
        ("Data.Text.Lazy.Encoding" . "import qualified Data.Text.Lazy.Encoding as LT
")
        ("Data.Text.Lazy.Builder" . "import qualified Data.Text.Lazy.Builder as LT
")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
        ("Data.Text.IO" . "import qualified Data.Text.IO as T
")
        ("Data.Text.Lazy.IO" . "import qualified Data.Text.Lazy.IO as LT
")
        ("Data.ByteString" . "import qualified Data.ByteString as S
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Char8" . "import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
        ("Data.ByteString.Lazy.Builder" . "import qualified Data.ByteString.Lazy.Builder as SB
")
        ("Data.ByteString.Builder" . "import qualified Data.ByteString.Builder as SB
")
        ("Data.ByteString.Lazy.Char8" . "import qualified Data.ByteString.Lazy.Char8 as L8
")
        ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("Data.HashMap" . "import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
")
        ("Data.IntMap" . "import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
")
        ("Data.StrMap" . "import Data.StrMap as StrMap
import Data.StrMap (StrMap)
")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("RIO" . "import qualified RIO
import RIO (RIO, glog, GLogFunc, HasGLogFunc(..))
")
        ("Data.Set" . "import qualified Data.Set as Set
import Data.Set (Set)
")
        ("Data.List" . "import qualified Data.List as List
")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
        ("Data.Vector.Storable" . "import qualified Data.Vector.Storable as SV
import Data.Vector (Vector)
")
        ("Data.List.NonEmpty" . "import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
")
        ("Data.Conduit.List" . "import qualified Data.Conduit.List as CL
")
        ("Data.Conduit.Binary" . "import qualified Data.Conduit.Binary as CB
")
        ("Data.Sequence" . "import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
")
        ("Data.Csv" . "import qualified Data.Csv as Csv
")))

(setq haskell-language-extensions '("-XMagicHash" "-XUnboxedTuples" "-XRecursiveDo" "-XTypeApplications"))
(setq shm-language-extensions '("-XMagicHash" "-XUnboxedTuples" "-XRecursiveDo" "-XTypeApplications"))


;; Add hook

                                        ;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
(remove-hook 'haskell-mode-hook 'stack-mode)
(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'w3m-display-hook 'w3m-haddock-display)

(remove-hook 'haskell-mode-hook 'intero-mode)
(remove-hook 'haskell-mode-hook 'my-intero-mode)

(defun my-intero-mode ()
  (interactive)
  (if (eq major-mode 'purescript-mode)
      (call-interactively 'psc-ide-mode)
    (call-interactively 'intero-mode-whitelist)))


;; Keybindings

(define-key intero-mode-map (kbd "C-`") 'my-flycheck-list-errors)
(defvar my-error-list-toggle nil)
(defun my-flycheck-list-errors ()
  (interactive)
  (if my-error-list-toggle
      (call-interactively 'flycheck-list-errors)
    (if (get-buffer "*compilation*")
        (display-buffer "*compilation*")
      (call-interactively 'flycheck-list-errors)))
  (setq my-error-list-toggle (not my-error-list-toggle)))

(define-key highlight-uses-mode-map (kbd "C-t") 'highlight-uses-mode-replace)

(define-key ghci-script-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key ghci-script-mode-map (kbd "C-c C-l") 'ghci-script-mode-load)
(define-key ghci-script-mode-map [f5] 'ghci-script-mode-load)
(define-key ghci-script-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key ghci-script-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key interactive-haskell-mode-map [f5] 'haskell-process-load-or-reload)
;; (define-key interactive-haskell-mode-map [f12] 'turbo-devel-reload)
(define-key interactive-haskell-mode-map [f12] nil)
;; (define-key interactive-haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key interactive-haskell-mode-map [f12] nil)
(define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

;; (define-key html-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key html-mode-map [f12] nil)
(define-key html-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key html-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

;; (define-key css-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key css-mode-map [f12] nil)
(define-key css-mode-map (kbd "C-`") 'haskell-interactive-bring)

(define-key purescript-mode-map (kbd "C-`") 'flycheck-list-errors)

(define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
(define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
(define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
(define-key haskell-mode-map (kbd "<space>") 'haskell-mode-contextual-space)

(define-key haskell-cabal-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-cabal-mode-map [f11] 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-cabal-mode-map [f12] nil)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

(define-key haskell-interactive-mode-map (kbd "C-c C-v") 'haskell-interactive-toggle-print-mode)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map [f9] 'haskell-interactive-mode-visit-error)
(define-key haskell-interactive-mode-map [f11] 'haskell-process-cabal-build)
;; (define-key haskell-interactive-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key haskell-interactive-mode-map [f12] nil)
(define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
(define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
(define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key shm-repl-map (kbd "TAB") 'shm-repl-tab)
(define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
(define-key shm-map (kbd ",") 'shm-comma-god)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
(define-key shm-map (kbd "SPC") 'shm-contextual-space)
(define-key shm-map (kbd "C-\\") 'shm/goto-last-point)
(define-key shm-map (kbd "C-c C-f") 'shm-fold-toggle-decl)
(define-key shm-map (kbd "C-c i") 'shm-reformat-decl)

;; (define-key ide-backend-mode-map [f5] 'ide-backend-mode-load)
;; (setq ide-backend-mode-cmd "cabal")

(defun haskell-insert-operator ()
  (interactive)
  (insert ()))

(defun haskell-process-all-types ()
  "List all types in a grep-mode buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                 (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)))))


;; for ghcjs
(when nil
  (defun haskell-process-compile-ghcjs ()
    (interactive)
    (save-buffer)
    (haskell-process-file-loadish
     (format "!sh check.sh %s"
             (buffer-file-name))
     nil
     (current-buffer)))
  (define-key interactive-haskell-mode-map [f5] 'haskell-process-compile-ghcjs)
  (defun haskell-process-build-ghcjs ()
    (interactive)
    (save-buffer)
    (haskell-process-file-loadish
     (format "!ghcjs -O2 %s"
             (buffer-file-name))
     nil
     (current-buffer)))
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-compile-ghcjs))

(defun haskell-process-toggle-import-suggestions ()
  (interactive)
  (setq haskell-process-suggest-remove-import-lines (not haskell-process-suggest-remove-import-lines))
  (message "Import suggestions are now %s." (if haskell-process-suggest-remove-import-lines
                                                "enabled"
                                              "disabled")))

(defvar haskell-stack-commands
  '("build"
    "update"
    "test"
    "bench"
    "install")
  "Stack commands.")

;;;###autoload
(defun haskell-process-stack-build ()
  "Build the Stack project."
  (interactive)
  (haskell-process-do-stack "build")
  (haskell-process-add-cabal-autogen))

;;;###autoload
(defun haskell-process-stack (p)
  "Prompts for a Stack command to run."
  (interactive "P")
  (if p
      (haskell-process-do-stack
       (read-from-minibuffer "Stack command (e.g. install): "))
    (haskell-process-do-stack
     (funcall haskell-completing-read-function "Stack command: "
              (append haskell-stack-commands
                      (list "build --ghc-options=-fforce-recomp")
                      (list "build --ghc-options=-O0"))))))

(defun haskell-process-do-stack (command)
  "Run a Cabal command."
  (let ((process (haskell-interactive-process)))
    (cond
     ((let ((child (haskell-process-process process)))
        (not (equal 'run (process-status child))))
      (message "Process is not running, so running directly.")
      (shell-command (concat "stack " command)
                     (get-buffer-create "*haskell-process-log*")
                     (get-buffer-create "*haskell-process-log*"))
      (switch-to-buffer-other-window (get-buffer "*haskell-process-log*")))
     (t (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list (haskell-interactive-session) process command 0)

          :go
          (lambda (state)
            (haskell-process-send-string
             (cadr state)
             (format ":!stack %s"
                     (cl-caddr state))))

          :live
          (lambda (state buffer)
            (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                                 "\\1"
                                                 (cl-caddr state))))
              (cond ((or (string= cmd "build")
                         (string= cmd "install"))
                     (haskell-process-live-build (cadr state) buffer t))
                    (t
                     (haskell-process-cabal-live state buffer)))))

          :complete
          (lambda (state response)
            (let* ((process (cadr state))
                   (session (haskell-process-session process))
                   (message-count 0)
                   (cursor (haskell-process-response-cursor process)))
              (haskell-process-set-response-cursor process 0)
              (while (haskell-process-errors-warnings session process response)
                (setq message-count (1+ message-count)))
              (haskell-process-set-response-cursor process cursor)
              (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                                 (cl-caddr state)
                                 message-count)))
                (haskell-interactive-mode-echo session msg)
                (when (= message-count 0)
                  (haskell-interactive-mode-echo
                   session
                   "No compiler messages, dumping complete output:")
                  (haskell-interactive-mode-echo session response))
                (haskell-mode-message-line msg)
                (when (and haskell-notify-p
                           (fboundp 'notifications-notify))
                  (notifications-notify
                   :title (format "*%s*" (haskell-session-name (car state)))
                   :body msg
                   :app-name (cl-ecase (haskell-process-type)
                               ('ghci haskell-process-path-cabal)
                               ('cabal-repl haskell-process-path-cabal)
                               ('cabal-ghci haskell-process-path-cabal))
                   :app-icon haskell-process-logo)))))))))))

(define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-stack-build)
(define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-stack)

(setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun haskell-capitalize-module (m)
  ;; FIXME:
  (with-temp-buffer
    (insert m)
    (upcase-initials-region (point-min) (point-max))
    (buffer-string)))

(defun haskell-modules-list ()
  (let* ((stack-root (intero-project-root))
         (modules
          (split-string
           (concat (shell-command-to-string (format "for i in $(fd '.*.cabal' %s); do cabal-info --cabal-file $i exposed-modules; done" stack-root))
                   (shell-command-to-string "sqlite3 -noheader ~/.haskell-modules.sqlite3 'select module from mod order by uses desc, length(module) asc, module asc;' | tail -n +2"))
           "\n" t)))
    modules))

(defun haskell-fast-get-import (custom)
  (if custom
      (let ((chosen (read-from-minibuffer "Module: " "")))
        (if (string-match "^import" chosen)
            chosen
          (haskell-capitalize-module chosen)))
    (let ((module (haskell-capitalize-module
                   (haskell-complete-module-read
                    "Module: "
                    (append (mapcar #'car haskell-import-mapping)
                            (haskell-modules-list))))))
      (shell-command-to-string (format "sqlite3 ~/.haskell-modules.sqlite3 \"update mod set uses = uses + 1 where module = '%s'\";" module))
      module)))

;; [bat,exa,fd,procs,ytop,rg] $ sqlite3 ~/.haskell-modules.sqlite3
;; SQLite version 3.22.0 2018-01-22 18:45:57
;; Enter ".help" for usage hints.
;; create table mod (module text not null unique, uses int not null default 0);
;; sqlite>
;;
;; sqlite> .mode csv
;; sqlite> .import .haskell-modules.hs mod

(defun haskell-fast-add-import (custom)
  "Add an import to the import list.  Sorts and aligns imports,
unless `haskell-stylish-on-save' is set, in which case we defer
to stylish-haskell."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (haskell-navigate-imports)
    (let* ((chosen (haskell-fast-get-import custom))
           (module (let ((mapping (assoc chosen haskell-import-mapping)))
                     (if mapping
                         (cdr mapping)
                       (if (string-match "^import" chosen)
                           (concat chosen "\n")
                         (concat "import " chosen "\n"))))))
      (insert module))
    (haskell-sort-imports)
    (haskell-align-imports)))

(define-key haskell-mode-map (kbd "C-i") 'haskell-fast-add-import)
(define-key shm-map (kbd "C-i") 'haskell-fast-add-import)
(define-key shm-map (kbd "<tab>") 'shm/tab)

(add-hook 'purescript-mode 'psc-ide-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression watching support for intero

(defvar intero-watch-expression "parseFunction \"\\\\x -> if 5>=x then 0 else 1\"")
(defun intero-watch-expression (string)
  (interactive "sEnter an expression: ")
  (setq intero-watch-expression string)
  (flycheck-buffer))
(defun intero-watch-expression-hook ()
  (interactive)
  (when intero-watch-expression
    (run-with-idle-timer
     0.0
     nil
     (lambda ()
       (when (eq major-mode 'haskell-mode)
         (let* ((result
                 (replace-regexp-in-string "\n$" ""  (intero-fontify-expression
                                                      (intero-blocking-call 'backend intero-watch-expression))))
                (msg
                 (format
                  "> %s\n\n%s"
                  (intero-fontify-expression intero-watch-expression)
                  result)))
           (unless (string-match "<interactive>:[0-9]+:[0-9]+: Not in scope: " result)
             (with-current-buffer (get-buffer-create "*Intero-Watch*")
               (erase-buffer)
               (insert msg)
               (goto-char (point-min))))))))))
(remove-hook 'flycheck-after-syntax-check-hook 'intero-watch-expression-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-key intero-mode-map (kbd "C-?") 'intero-uses-at)
(define-key intero-mode-map (kbd "M-/") 'intero-special-expand)

(defun intero-special-expand ()
  "Expand holes or do regular dabbrev"
  (interactive)
  (if (intero-grab-hole)
      (call-interactively 'company-complete)
    (call-interactively 'dabbrev-expand)))


(global-set-key [home] (lambda () (interactive)))
(global-set-key [prior] (lambda () (interactive)))

(define-key flycheck-mode-map (kbd "C-v C-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "C-v C-h") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "C-v C-v") 'flycheck-buffer)

(defun haskell-trace-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert "|])")
    (goto-char beg)
    (insert "$(tracing [|")))

(setq flycheck-display-errors-function nil)

(setq intero--flycheck-multiple-files-support t)
(define-key intero-mode-map (kbd "C-c C-c")
  'my-intero-compile)

(defun my-intero-compile ()
  (interactive)
  (when (intero-buffer-p 'backend)
    (let ((targets (buffer-local-value 'intero-targets (intero-buffer 'backend))))
      (call-interactively 'compile))))

(define-key haskell-mode-map (kbd "C-c C-d") 'stack-doc)

(defun psc-reformat-decl ()
  "Re-format the current declaration.

The declaration is parsed and pretty printed.  Comments are
preserved, although placement may be funky."
  (interactive)
  (let ((start-end (hindent-decl-points))
        (origin (point)))
    (when start-end
      (let ((beg (car start-end))
            (end (cdr start-end)))
        (let* ((string (buffer-substring-no-properties beg end))
               (out (with-temp-buffer
                      (insert
                       (replace-regexp-in-string
                        "^instance \\([a-zA-Z0-9_']+\\) :: " "instance \\1 => "
                        (replace-regexp-in-string
                         "\\([a-zA-Z0-9_']+\\): *\\([\n]*\\)"
                         "\\1PSCHINDENTHACK =\\2"
                         string)))
                      (hindent-reformat-region (point-min) (point-max) t)
                      (replace-regexp-in-string "\n$" "" (buffer-string)))))
          (delete-region beg end)
          (insert (replace-regexp-in-string
                   "instance \\([a-zA-Z0-9]+\\) => "
                   "instance \\1 :: "
                   (replace-regexp-in-string
                    "PSCHINDENTHACK ="
                    ":"
                    out)))
          (goto-char origin))))))

(defun haskell-early-reformat-decl ()
  "Re-format the current declaration.

The declaration is parsed and pretty printed.  Comments are
preserved, although placement may be funky."
  (interactive)
  (let ((start-end (hindent-decl-points))
        (origin (point)))
    (when start-end
      (let ((beg (car start-end))
            (end (cdr start-end)))
        (let* ((string (buffer-substring-no-properties beg end))
               (out (with-temp-buffer
                      (insert
                       (replace-regexp-in-string
                        "\\?$"
                        "EARLY_QMARK"
                        string))
                      (hindent-reformat-region (point-min) (point-max) t)
                      (replace-regexp-in-string "\n$" "" (buffer-string)))))
          (delete-region beg end)
          (insert (replace-regexp-in-string
                   "[\n ]*EARLY_QMARK$"
                   "?"
                   out))
          (goto-char origin))))))

(define-key purescript-mode-map (kbd "C-c i") 'psc-reformat-decl)
(define-key psc-ide-mode-map (kbd "C-c i") 'psc-reformat-decl)

(define-key psc-ide-mode-map (kbd "C-i") 'haskell-fast-add-import)
(define-key purescript-mode-map (kbd "C-i") 'haskell-fast-add-import)

(define-key psc-ide-mode-map (kbd "C-c C-r")
  'psc-ide-flycheck-insert-suggestion)

(defun haskell-sig ()
  (interactive)
  (save-window-excursion
    (paredit-wrap-sexp)
    (forward-char -1)
    (forward-sexp)
    (forward-char -1)
    (insert " :: _")
    (flycheck-buffer)
    (flycheck-list-errors)
    (read-event "Hit ENTER to finish.")
    (undo)))

(defun haskell-escape-region (beg end)
  (interactive "r")
  (replace-regexp "\"" "\\\\\"" nil beg end))

(defun haskell-copy-module-name ()
  "Guess the current module name of the buffer."
  (interactive)
  (kill-new (haskell-guess-module-name)))

(defun haskell-copy-imports ()
  (interactive)
  (save-excursion
    (let* ((start (progn (goto-char (point-min))
                         (search-forward-regexp "^import")
                         (line-beginning-position)))
           (end (progn (goto-char (point-max))
                       (search-backward-regexp "^import")
                       (line-end-position)))
           (string (buffer-substring start end)))
      (kill-new string)
      (message "Copied %d lines of imports." (length (split-string string "\n"))))))

(defun haskell-rename ()
  )

(defun haskell-add-early ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "{-# OPTIONS -F -pgmF=early #-}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ormolu ()
  "Autoformat the buffer with ormolu."
  (interactive)
  (let ((point (point)))
    (save-buffer)
    (call-process-region
     (point-min)
     (point-max)
     "/opt/ormolu-0.1.4.1"
     t
     t
     t

     ;; copied from brossa/treefmt.toml#L6
     "--ghc-opt" "-XBangPatterns"
     "--ghc-opt" "-XNumericUnderscores"
     "--ghc-opt" "-XOverloadedLabels"
     "--ghc-opt" "-XPatternSynonyms"
     "--ghc-opt" "-XTypeApplications"
     "--mode" "stdout"
     ;; "--check-idempotence" aoyig
     ;;

     ;; Does not work on 0.1.4.1
     ;; "--stdin-input-file"
     (buffer-file-name))
    (save-buffer)
    (goto-char point)))

(defun hindent-reformat-decl-via ()
  (interactive)
  (let ((hindent-process-path
         (ido-completing-read "Formatter: "
                              (list "/opt/hindent-5.3.1" "/opt/hindent-ormolu"))))
    (call-interactively 'hindent-reformat-decl)))

(defun hindent-reformat-region-via ()
  (interactive)
  (let ((hindent-process-path
         (ido-completing-read "Formatter: "
                              (list "/opt/hindent-5.3.1" "/opt/hindent-ormolu"))))
    (call-interactively 'hindent-reformat-region)))

(defun hindent/reformat-decl (u)
  (interactive "P")
  (if u
      (call-interactively 'hindent-reformat-decl-via)
    (call-interactively 'hindent-reformat-decl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code integrates the trivial hiedb.el with intero.el
;;
;; The final aim would be to move intero's code for highlighting uses
;; and pretty printing into hiedb, perhaps. But it's nice that
;; hiedb.el is so trivial.
;;

(define-key intero-mode-map (kbd "M-.") 'hiedb-goto-def)
(define-key intero-mode-map (kbd "C-c C-t") 'my-hiedb-intero-show-type)
(define-key intero-mode-map (kbd "C-?") 'my-hiedb-intero-uses-at)
(defun my-hiedb-intero-show-type ()
  (interactive)
  (message
   "%s" (intero-fontify-expression (hiedb-call-by-point 'hiedb-point-types))))

(defun my-hiedb-intero-uses-at ()
  (interactive)
  (let ((highlighted nil)
        (thing (intero-thing-at-point))
        (externals nil))
    (cl-loop
     for use in (hiedb-call-by-point 'hiedb-point-refs)
     do (let ((loaded-file (hiedb-module-filepath (plist-get use :module))))
          (if (string= loaded-file (buffer-file-name))
              (let ((start (save-excursion (goto-char (point-min))
                                           (forward-line (1- (plist-get use :line)))
                                           (forward-char (plist-get use :column))
                                           (point))))
                (when (string= loaded-file (buffer-file-name (current-buffer)))
                  (unless highlighted
                    (intero-highlight-uses-mode))
                  (setq highlighted t)
                  (intero-highlight-uses-mode-highlight
                   start
                   (save-excursion (goto-char (point-min))
                                   (forward-line (1- (plist-get use :end-line)))
                                   (forward-char (plist-get use :end-column))
                                   (point))
                   (= start (car thing)))))
            (setq externals (cons (append use (list :file loaded-file)) externals)))))
    (when externals
      (switch-to-buffer-other-window
       (with-current-buffer (get-buffer-create "*Haskell-uses*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Call sites for %s\n\n" thing))
           (save-excursion
             (cl-loop for use in externals
                      do (if (file-exists-p (plist-get use :file))
                             (insert (format "%s:%d: here.\n"
                                             (plist-get use :file)
                                             (plist-get use :line)
                                             ))
                           (insert (format "%s - unknown module location.\n"
                                           (plist-get use :module))))))
           (grep-mode)
           (current-buffer)))))))
