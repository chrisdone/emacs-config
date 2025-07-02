;; (llm-query-sync-response "one line answer: ollama default port")

(defvar llm-host-port 'llm-default-host-port)

(defun llm-default-host-port ()
  "localhost:6379")


;; It's a sequence of messages that could be piped to an Emacs buffer,
;; and then a JSON parser could walk across it, consuming messages,
;; but retaining position. Each message would trigger a callback event
;; handler, until the final one indicating closed. A synchronous call
;; built on the async one would be a good test.
;;
;; Multiple simultaneous requests seem to work too.

;; IMPORTANT: --no-buffer

;; Streaming protocol example:
;;
;; chris@Chriss-MacBook-Pro ~ % curl http://localhost:8080/api/generate --no-buffer  -d '{"model": "deepseek-r1:7b", "prompt": "is haskell the best programming language?", "options": {"num_predict":10}  }, "stream": true}' --silent
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.607244Z","response":"\u003cthink\u003e","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.622323Z","response":"\n","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.635501Z","response":"Okay","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.648725Z","response":",","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.662088Z","response":" so","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.675341Z","response":" I","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.688821Z","response":"'m","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.702438Z","response":" trying","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.716038Z","response":" to","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.729821Z","response":" figure","done":false}
;; {"model":"deepseek-r1:7b","created_at":"2025-07-02T10:39:37.729825Z","response":"","done":true,"done_reason":"length","context":[151644,285,702,74,613,279,1850,15473,4128,30,151645,151648,198,32313,11,773,358,2776,4460,311,7071],"total_duration":320742791,"load_duration":29775416,"prompt_eval_count":12,"prompt_eval_duration":167150291,"eval_count":10,"eval_duration":123079584}
