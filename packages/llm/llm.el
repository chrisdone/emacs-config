(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")

;; (progn
;;   (with-current-buffer (get-buffer "*scratch*") (erase-buffer))
;;   (llm-chat-to-buffer
;;    (list :model "llama3.1:8b"
;;          :messages
;;          (vector
;;           (list :role "user"
;;                 :content "Tell me about Canada."))
;;          :format (list
;;                   :type "object"
;;                   :properties (list
;;                                :name (list :type "string")
;;                                :capital (list :type "string")
;;                                :languages (list
;;                                            :type "array"
;;                                            :items (list
;;                                                    :type "string"))))
;;          :stream t
;;          )
;;    (get-buffer "*scratch*")))

;; (progn
;;   (with-current-buffer (get-buffer "*scratch*") (erase-buffer))
;;   (llm-chat-to-buffer
;;    (list :model "llama3.1:8b"
;;          :messages
;;          (vector
;;           (list :role "system"
;;                 :content "use tools before recommending emacs lisp function")
;;           (list :role "user"
;;                 :content "what emacs lisp function restores buffer point after doing some work?"))
;;          :tools
;;          (vector
;;           (list
;;            :type "function"
;;            :function
;;            (list
;;             :name "describe-function"
;;             :description "Get detailed information about a Lisp function including its signature, documentation, and examples"
;;             :parameters
;;             (list :type "object"
;;                   :properties
;;                   (list :function_name
;;                         (list :type "string"
;;                               :description "The name of the Lisp function to describe"))
;;                   :required (vector "function_name")))))
;;          :stream t
;;          ;; :options (list :think nil)
;;          )
;;    (get-buffer "*scratch*")))

(defun llm-text-region-to-buffer ()
  (interactive)
  (llm-generate-to-buffer
   (list :model "llama3.2:3b-text-q2_K"
         :prompt (buffer-substring (region-beginning) (region-end))
         :stream t
         :options (list :think nil))
   (current-buffer)))

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
   (list :model "llama3.2:3b-text-q2_K"
         :prompt string
         :stream t
         :options (list :think nil))
   (get-buffer "*scratch*")))

(defun llm-generate-to-buffer (config buffer)
  (let ((process (llm-make-process "/api/generate" config)))
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-typewriter-callback)))

(defun llm-chat-to-buffer (config buffer)
  (llm-dribble "[control] Starting process")
  (let ((process (llm-make-process "/api/chat" config)))
    (process-put process :original-config config)
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-chat-callback)))

(defun llm-make-process (endpoint config)
  (let ((args (list "curl"
                    ;; "--max-time" "5"
                    (concat "http://" (funcall llm-host-port) endpoint)
                    "--no-buffer"
                    "--silent"
                    "-d" (json-encode config))))
    (llm-dribble "[outgoing] %s"
             (json-encode config))
    (make-process
     :name "llm"
     :buffer (generate-new-buffer "*llm-stream*")
     :command args
     :connection-type 'pipe
     :filter 'llm-process-filter
     :sentinel 'llm-process-sentinel
     :stderr (get-buffer-create "*llm-dribble*")
     )))

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
              (insert (format "[huh] %S\n" (plist-get message :message))))
          (insert (format "[msg] %S\n" message))))))
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

            (llm-chat-to-buffer (process-get process :original-config)
                                (get-buffer "*scratch*"))
            )
        ))))

(defun llm-process-sentinel (process status)
  (llm-dribble "[sentinel] %s = %s" process status)
  (kill-buffer (process-buffer process)))

(defun llm-process-filter (process string)
  (llm-dribble "[incoming] %s" string)
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
                                   (cond
                                    ((string= (plist-get function :name) "describe-function")
                                     (with-temp-buffer
                                       (let ((standard-output (current-buffer)))
                                         (let ((name (intern
                                                      (plist-get (plist-get function :arguments)
                                                                 :function_name))))
                                           (when (fboundp name)
                                             (progn (llm-dribble "[TOOL USE] describe-function: %s" name)
                                                    (describe-function-1 name))))
                                         (buffer-string))))
                                    (t
                                     ;; spurious; warn?
                                     (llm-dribble "[warn] spurious function: %s"
                                                  (plist-get function :name))
                                     ""))))))
                       tool-calls)))))

(defun llm-dribble (f &rest args)
  (with-current-buffer (get-buffer-create "*llm-dribble*")
    (save-excursion
      (goto-char (point-max))
      (insert (apply 'format (cons f args)) "\n"))))
