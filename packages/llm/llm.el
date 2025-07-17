(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")

(llm-generate-to-scratch "random animal")

(llm-chat-to-buffer
 (list :model "deepseek-r1:7b"
       :messages (vector
                  (list :role "user"
                        :content "random quote"))
       :stream t
       :options (list :think nil))
   (get-buffer "*scratch*"))

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
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-chat-callback)))

(defun llm-make-process (endpoint config)
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
        (insert (format "%S\n" message))))))

(defun llm-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (save-excursion (insert string))
    (cl-loop for message = (condition-case nil (json-parse-buffer :object-type 'plist)
                             (json-end-of-file nil))
             while message
             do (funcall (process-get process :callback) process message))))
