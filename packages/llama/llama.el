(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples

;;
;; Synchronous to string:
;; (llama-tokens-to-string (make-llama-complete-stream :prompt "e=mc2" :n-predict 10))

;; Completion example:
;;
;; (llama-insert-tokens (make-llama-complete-stream :prompt "e=mc2" :n-predict 10))

;; Chat example:
;;
;; (llama-insert-tokens
;;  (make-llama-chat-stream
;;   :n-predict 300
;;   :grammar "root ::= \"yes\" | \"no\""
;;   :messages
;;   (vector
;;    (list :role "system"
;;          :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests. You must only answer with 'bob' or 'brian'.")
;;    (list :role "user"
;;          :content "Is the Earth flat?"))))

;; (llama-insert-tokens
;;  (make-llama-chat-stream
;;   :n-predict 300
;;   :grammar "root ::= statement+
;; statement ::= expr \".\n\"
;; expr ::= term \"(\" exprlist \")\" | term
;; exprlist ::= expr (\",\" expr)*
;; term ::= [a-zA-Z][a-zA-Z0-9_]*
;; "
;;   :messages
;;   (vector
;;    (list :role "system"
;;          :content "You are a translator that converts English statements into Prolog/Datalog relations.

;; Rules:
;; - Always output valid Prolog/Datalog syntax.
;; - Facts must end with a period.
;; - Predicates and constants must be lowercase, words separated by underscores.
;; - Variables must begin with an uppercase letter.
;; - Translate adjectives, verbs and relationships into predicates.
;; - Generalize universal statements using variables.
;; - Encode attributes and relations as facts or rules.
;; - Do not output explanations or natural language—only Prolog/Datalog relations.
;; - Do not use the @< operator.

;; Examples:

;; Input: Socrates is a man. All men are mortal.
;; Output:
;; man(socrates).
;; mortal(X) :- man(X).

;; Input: John loves Mary. Every person who loves someone is happy.
;; Output:
;; loves(john, mary).
;; happy(X) :- loves(X, Y).")
;;    (list :role "user"
;;          :content "Tumbler Ridge is a district municipality in the foothills of the B.C. Rockies in northeastern British Columbia, Canada, and a member municipality of the Peace River Regional District. With a population of 2,399 in 2021, the municipality encompasses an area of 1,558 km2 (602 sq mi). Located near the confluence of the Murray River and Flatbed Creek and the intersection of Highways 52 and 29, it is part of the Peace River South provincial electoral district and the Prince George—Peace River—Northern Rockies federal riding. It is a planned community, with the housing and infrastructure built simultaneously in 1981 by the provincial government to service the coal industry. After dinosaur footprints and fossils were discovered in the municipality, along with fossils of Triassic fishes and Cretaceous plants, the Peace Region Paleontology Research Center opened in 2003. The study of the area led to a recognition of its geological importance and listing in the UNESCO Global Geopark Network."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stream consumers and conduits

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
   :stream stream
   :end 'identity))

(defun llama-chat-on-buffer-to-string (prompt)
  "Run a query against the current buffer, returning the output as a string."
  (llama-tokens-to-string
   (make-llama-chat-stream
    :messages
    (vector
     (list :role "system"
           :content "You are an AI assistant. Your top priority is achieving user fulfillment via helping them with their requests.")
     (list :role "user"
           :content (concat prompt "\n\n" (buffer-string)))))))

(defun llama-tokens-to-string (stream)
  "Output all tokens from STREAM into a string synchronously."
  (lexical-let ((output nil))
    (llama-fold-sse-json
     :fold (lambda (acc token)
             (cons (llama-get-token token) acc))
     :accum (list)
     :stream stream
     :end (lambda (list) (setq output (apply 'concat (reverse list)))))
    (while (eq nil output)
      (sit-for 0.1))
    output))

(cl-defun llama-fold-sse-json (&key fold accum stream end)
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
   (lambda (state)
     (kill-buffer (plist-get state :buffer))
     (funcall (plist-get state :end)
              (plist-get state :acc)))
   (list :buffer (generate-new-buffer "*llama-fold-sse*")
         :acc accum
         :func fold
         :end end
         :original-buffer (current-buffer))))

(defun llama-debug-raw-stream (stream)
  "Print everything from the stream via MESSAGE."
  (funcall
   stream
   (lambda (_ chunk)
     (message "%s" chunk))
   nil
   'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source streams

(cl-defun make-llama-complete-stream (&key prompt n-predict grammar)
  "Make a SUSPENDED RAW complete stream against the llama server with PROMPT."
  (make-llama-stream
   "/v1/completions"
   (list :n_predict n-predict
         :stream t
         :grammar grammar
         :prompt prompt)))

(cl-defun make-llama-chat-stream (&key messages n-predict grammar)
  "Make a SUSPENDED RAW chat stream against the llama server with MESSAGES."
  (make-llama-stream
   "/v1/chat/completions"
   (list :n_predict n-predict
         :stream t
         :grammar grammar
         :messages messages)))

(defun make-llama-stream (path config)
  "Make a SUSPENDED RAW stream against the llama server with PROMPT."
  (lexical-let ((path path)
                (config config))
    (lambda (func end acc)
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
        (process-put proc :end end)
        (set-process-sentinel proc
                              (lambda (proc event)
                                (unless (string= "open\n" event)
                                  (funcall (process-get proc :end)
                                           (process-get proc :acc))
                                  (kill-buffer (process-buffer proc)))))
        (set-process-filter proc
                            (lambda (proc chunk)
                              (process-put proc :acc
                                           (funcall (process-get proc :func)
                                                    (process-get proc :acc)
                                                    chunk))))
        ;; (with-current-buffer (get-buffer-create "*scratch*")
        ;;   (insert json-body))
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
                     "--host 0.0.0.0"
                     "--port 8080")
               " "))
      (call-interactively 'comint-send-input)
      (goto-char (point-max)))))
