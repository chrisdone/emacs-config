(define-key ats-mode-map (kbd "M-;") 'my-ats-comment)
(define-key ats-mode-map (kbd "<f5>") 'recompile)

(defun my-ats-comment ()
  (interactive)
  (if (= (line-beginning-position)
         (line-end-position))
      (progn (insert "(*  *)")
             (forward-char -3))
    (call-interactively 'comment-dwim-line)))

(defun ats-insert-template ()
  (interactive)
  (when (= (point-min)
           (point-max))
    (insert "(*  *)

(* Demo of *)
val _ =
  begin print! (,\"\\n\");
  end

implement main () = ()")
 (goto-char (point-min))
 (forward-char 3)))

(add-hook 'ats-mode-hook 'ats-insert-template)
