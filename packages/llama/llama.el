(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MacBook-specific server starting

(defun llama-start ()
  "Starts or restarts the server in the *llama-server* buffer."
  (interactive)
  (let ((ip (shell-command-to-string "/home/chris/Work/chrisdone-artificial/utm/host-ip.hell")))
    (let ((default-directory (concat "/ssh:" ip ":")))
      (shell (get-buffer-create "*llama-server*"))
      (unless (= (point-min) (point-max))
        (while (not (memq 'comint-highlight-prompt (get-text-property (1- (point-max)) 'face)))
          (comint-interrupt-subjob)
          (message "Waiting for job to terminate ...")
          (sit-for 0.5)))
      (erase-buffer)
      (insert (mapconcat
               'identity
               (list "Downloads/llama/bin/llama-server"
                     "--model"
                     "UTM-Shared/LLMs/Llama-3.2-3B-Instruct-uncensored.Q4_K_M.gguf"
                     "--port 8080")
               " "))
      (call-interactively 'comint-send-input)
      (goto-char (point-max)))))
