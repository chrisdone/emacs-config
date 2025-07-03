(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")

(make-process
 :name "llm"
 :buffer (generate-new-buffer "*llm-stream*")
 :command (list "curl" (concat "http://" (funcall llm-host-port) "/api/generate") "--no-buffer"  "-d" "{\"model\": \"deepseek-r1:7b\", \"prompt\": \"random quote\", \"options\": {\"num_predict\":20}  }, \"stream\": true}" "--silent")
 :connection-type 'pipe
 :filter 'llm-process-filter)

(defun llm-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (save-excursion (insert string))
    (cl-loop for message = (condition-case nil (json-parse-buffer :object-type 'plist)
                             (json-end-of-file nil))
             while message
             do (message "%s" (plist-get message :response)))))
