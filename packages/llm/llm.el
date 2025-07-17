(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")

;; (llm-chat-to-buffer
;;  (list :model "llama3.1:8b"
;;        :messages
;;        (vector
;;         (list :role "system"
;;               :content "You are an assistant that consults Emacs documentation via a reliable tool. When the tool returns documentation, treat its contents as authoritative and do not question or reinterpret it. Do not offer alternative interpretations or suggestions that contradict the tool output")
;;         (list :role "user"
;;               :content "does emacs lisp form save-excursion affect mark state after emacs 25.1?"))
;;        :tools
;;        (vector
;;         (list
;;          :type "function"
;;          :function
;;          (list
;;           :name "describe-function"
;;           :description "Get detailed information about a Lisp function including its signature, documentation, and examples"
;;           :parameters
;;           (list :type "object"
;;                 :properties
;;                 (list :function_name
;;                       (list :type "string"
;;                             :description "The name of the Lisp function to describe"))
;;                 :required (vector "function_name")))))
;;        :stream t
;;        :options (list :think nil))
;;  (get-buffer "*scratch*"))

(defun llm-generate-region-to-buffer ()
  (interactive)
  (llm-generate-to-buffer
   (list :model "deepseek-r1:7b"
         :prompt (buffer-substring (region-beginning) (region-end))
         :stream t
         :options (list :think nil))
   (current-buffer)))

(defun llm-generate-region-to-scratch ()
  (interactive)
  (llm-generate-to-buffer
   (list :model "deepseek-r1:7b"
         :prompt (buffer-substring (region-beginning) (region-end))
         :stream t
         :options (list :think nil))
   (get-buffer "*scratch*")))

(defun llm-generate-to-scratch (string)
  (interactive "sPrompt: ")
  (llm-generate-to-buffer
   (list :model "deepseek-r1:7b"
         :prompt string
         :stream t
         :options (list :think nil))
   (get-buffer "*scratch*")))

(defun llm-generate-to-buffer (config buffer)
  (let ((process (llm-make-process "/api/generate" config)))
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-typewriter-callback)))

(defun llm-chat-to-buffer (config buffer)
  (let ((process (llm-make-process "/api/chat" config)))
    (process-put process :original-config config)
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-chat-callback)))

(defun llm-make-process (endpoint config)
  (message "llm-make-process: %S" config)
  (make-process
   :name "llm"
   :buffer (generate-new-buffer "*llm-stream*")
   :command (list "curl"
                  (concat "http://" (funcall llm-host-port) endpoint)
                  "--no-buffer"
                  "--silent"
                  "-d" (json-encode config))
   :connection-type 'pipe
   :filter 'llm-process-filter))

(defun llm-typewriter-callback (process message)
  (let ((typewriter-buffer (process-get process :typewriter-buffer)))
    (when typewriter-buffer
      (with-current-buffer typewriter-buffer
        (goto-char (point-max))
        (insert (plist-get message :response))))))

(defun llm-chat-callback (process message)
  (let ((typewriter-buffer (process-get process :typewriter-buffer)))
    (when typewriter-buffer
      (with-current-buffer typewriter-buffer
        (goto-char (point-max))
        (if (plist-get message :message)
            (if (plist-get (plist-get message :message) :content)
                (insert (plist-get (plist-get message :message) :content))
              (insert (format "%S\n" (plist-get message :message))))
            (insert (format "%S\n" message))))))
  (let ((message (plist-get message :message)))
    (when message
      (process-put process :messages
                   (vconcat (vector message) (process-get process :messages)))))
  (when (eq t (plist-get message :done))
    (let ((tool-calls
           (apply 'vconcat
                  (mapcar
                   (lambda (message)
                     (plist-get message :tool_calls))
                   (cl-remove-if-not
                    (lambda (message)
                      (plist-get message :tool_calls))
                    (process-get process :messages))))))
      (if (> (length tool-calls) 0)
          (progn
            (llm-add-messages (process-get process :original-config)
                              (process-get process :messages))
            (llm-add-tool-messages (process-get process :original-config)
                                   tool-calls)
            (message "Continuing the conversation with tool call results.")
            (llm-chat-to-buffer (process-get process :original-config)
                                (get-buffer "*scratch*"))
            )
        (message "Interaction complete.")))))

(defun llm-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (save-excursion (insert string))
    (cl-loop for message = (condition-case nil (json-parse-buffer :object-type 'plist)
                             (json-end-of-file nil))
             while message
             do (funcall (process-get process :callback) process message))))

(defun llm-add-messages (config messages)
  (plist-put config :messages
             (vconcat (plist-get config :messages)
                      messages)))

(defun llm-add-tool-messages (config tool-calls)
  (plist-put config :messages
             (vconcat
              (plist-get config :messages)
              (remove-if-not
               'identity
               (mapcar (lambda (tool-call)
                         (let ((function (plist-get tool-call :function)))
                           (when function
                             (list :role "tool"
                                   :tool_call_id (plist-get function :id) ; doesn't seem to exist?
                                   :name (plist-get function :name)
                                   :content
                                   (if (string= (plist-get function :name) "describe-function")
                                       (with-temp-buffer
                                         (let ((standard-output (current-buffer)))
                                           (describe-function-1
                                            (intern
                                             (plist-get (plist-get function :arguments)
                                                        :function_name)))
                                           (buffer-string)))
                                     "")))))
                       tool-calls)))))
