(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")

(defun llm-generate-region-to-buffer ()
  (interactive)
  (llm-generate-to-buffer
   (list :model "deepseek-r1:7b"
         :prompt (buffer-substring (region-beginning) (region-end))
         :stream t
         :options (list :think nil))
   (current-buffer)))

(defun llm-generate-to-buffer (config buffer)
  (let ((process (llm-make-process config)))
    (process-put process :typewriter-buffer buffer)
    (process-put process :callback 'llm-typewriter-callback)))

(defun llm-make-process (config)
  (make-process
   :name "llm"
   :buffer (generate-new-buffer "*llm-stream*")
   :command (list "curl"
                  (concat "http://" (funcall llm-host-port) "/api/generate")
                  "--no-buffer"
                  "--silent"
                  "-d" (json-encode config))
   :connection-type 'pipe
   :filter 'llm-process-filter))

(defun llm-typewriter-callback (process chunk)
  (let ((typewriter-buffer (process-get process :typewriter-buffer)))
    (when typewriter-buffer
      (with-current-buffer typewriter-buffer
        (goto-char (point-max))
        (insert (plist-get message :response))))))

(defun llm-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (save-excursion (insert string))
    (cl-loop for message = (condition-case nil (json-parse-buffer :object-type 'plist)
                             (json-end-of-file nil))
             while message
             do (funcall (process-get process :callback)
                         process
                         (plist-get message :response)))))
