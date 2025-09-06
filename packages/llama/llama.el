(require 'cl-lib)

;; Examples
;;
;; Completion example:
;;
;; (llama-insert-tokens (make-llama-complete-stream :prompt "e=mc2" :n-predict 300))
;;
;; Chat example:
;;
;; (llama-insert-tokens
;;  (make-llama-chat-stream
;;   :n-predict 300
;;   :messages
;;   (vector
;;    (list :role "system"
;;          :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests.")
;;    (list :role "user"
;;          :content "Write a factorial function in Haskell"))))

(defun llama-chat-region ()
  "Query the LLM, with current region appended to the end of the
prompt, and get the output in *llama-output* buffer."
  (interactive)
  (let ((prompt
         (concat
          (read-from-minibuffer "Prompt: ")
          "\n\n"
          (buffer-substring
           (region-beginning)
           (region-end)))))
    (switch-to-buffer-other-window
     (get-buffer-create "*llama-output*"))
    (markdown-mode)
    (goto-char (point-max))
    (insert "\n\n> " prompt "\n\n")
    (llama-insert-tokens
     (make-llama-chat-stream
      :messages
      (vector
       (list :role "system"
             :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests.")
       (list :role "user"
             :content prompt))))))

(defun llama-chat ()
  "Query the LLM and get the output in *llama-output* buffer."
  (interactive)
  (let ((prompt (read-from-minibuffer "Prompt: ")))
    (switch-to-buffer-other-window
     (get-buffer-create "*llama-output*"))
    (markdown-mode)
    (goto-char (point-max))
    (insert "\n\n> " prompt "\n\n")
    (llama-insert-tokens
     (make-llama-chat-stream
      :n-predict 300
      :messages
      (vector
       (list :role "system"
             :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests.")
       (list :role "user"
             :content prompt))))))

(defun llama-insert-tokens (stream)
  "Insert all tokens from STREAM into the current buffer."
  (llama-map
   :func (lambda (object) (insert (llama-get-token object)))
   :stream stream))

(defun llama-message-objects (stream)
  "Call MESSAGE on all objects from STREAM into the current buffer."
  (llama-map
   :func (lambda (object) (message "%S" object))
   :stream stream))

(defun llama-get-token (object)
  (apply 'concat
         (mapcar (lambda (choice)
                   (let ((content (or (plist-get choice :text)
                                      (let ((delta (plist-get choice :delta)))
                                        (plist-get delta :content)))))
                     (if (stringp content)
                         content
                       "")))
                 (plist-get object :choices))))

(cl-defun llama-map (&key func stream)
  "Simply apply a FUNC to all objects in the stream."
  (llama-fold-sse-json
   :fold (lambda (func object) (funcall func object) func)
   :accum func
   :stream stream))

(cl-defun llama-fold-sse-json (&key fold accum stream)
  "Fold over the SSE JSON blobs output with F(ACCUM,object) over STREAM."
  (funcall
   stream
   (lambda (state chunk)
     (with-current-buffer (plist-get state :buffer)
       (save-excursion
         (goto-char (point-max))
         (insert chunk))
       (cl-loop
        for message = (when (search-forward "data: " nil t 1)
                        (condition-case nil (json-parse-buffer :object-type 'plist)
                          (json-end-of-file nil)
                          (json-parse-error nil)))
        while message
        do (plist-put state :acc
                      (with-current-buffer (plist-get state :original-buffer)
                        (funcall (plist-get state :func)
                                 (plist-get state :acc)
                                 message)))))
     state)
   (list :buffer (generate-new-buffer "*llama-fold-sse*")
         :acc accum
         :func fold
         :original-buffer (current-buffer))))

(defun llama-debug-raw-stream (stream)
  "Print everything from the stream via MESSAGE."
  (funcall
   stream
   (lambda (_ chunk)
     (message "%s" chunk))
   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source streams

(cl-defun make-llama-complete-stream (&key prompt n-predict)
  "Make a SUSPENDED RAW complete stream against the llama server with PROMPT."
  (make-llama-stream
   "/v1/completions"
   (list :n_predict n-predict
         :stream t
         :prompt prompt)))

(cl-defun make-llama-chat-stream (&key messages n-predict)
  "Make a SUSPENDED RAW chat stream against the llama server with MESSAGES."
  (make-llama-stream
   "/v1/chat/completions"
   (list :n_predict n-predict
         :stream t
         :messages messages)))

(defun make-llama-stream (path config)
  "Make a SUSPENDED RAW stream against the llama server with PROMPT."
  (lexical-let ((path path)
                (config config))
    (lambda (func acc)
      (let* ((json-body (json-encode config))
             (content-length (number-to-string (string-bytes json-body)))
             (proc (make-network-process
                    :name "llama-stream"
                    :buffer "*llama-stream*"
                    :host (shell-command-to-string "/home/chris/Work/chrisdone-artificial/utm/host-ip.hell")
                    :service 8080
                    :nowait t)))
        (process-put proc :func func)
        (process-put proc :acc acc)
        (set-process-filter proc
                            (lambda (proc chunk)
                              (process-put proc :acc
                                           (funcall (process-get proc :func)
                                                    (process-get proc :acc)
                                                    chunk))))
        (process-send-string
         proc
         (format "POST %s HTTP/1.1\r\n\
Content-Type: application/json\r\n\
Content-Length: %s\r\n\
Connection: close\r\n\
\r\n%s"
                 path content-length json-body))))))

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
