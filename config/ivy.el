(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(let ((completing-read-function 'ivy-completing-read))
  (call-interactively 'project-search))

(defun ls-files ()
  (interactive)
  (let ((default-directory (car (split-string (shell-command-to-string "git rev-parse --show-toplevel")))))
    (find-file
     (ivy-completing-read
      "Find File: "
      (split-string (shell-command-to-string "git ls-files") "\n")))))

(defun rg ()
  (interactive)
  (counsel-rg (ag/dwim-at-point)))

(provide 'ivy)
