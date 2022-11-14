(defvar server)
(progn
  (delete-process server)
  (let ((coding-system-for-read 'binary))
    (setq server
          (make-network-process
           :name "image-server"
           :buffer "*image-server*"
           :family 'ipv4
           :service 9009
           :sentinel 'image-server-sentinel
           :filter 'image-server-filter
           :coding 'binary
           :server 't)))
  )

(defun image-server-sentinel (proc msg)
  (let ((buffer-name (concat " *image-server-" (process-name proc) "*")))
    (if (string-match "^open" msg)
        (progn
          (process-put proc 'start-time (current-time))
          (process-put
           proc
           'buffer
           (with-current-buffer (get-buffer-create buffer-name)
             (erase-buffer)
             (toggle-enable-multibyte-characters -1)
             (current-buffer))))
      (progn (message "image-server: receive time: %.0fms"
                      (* 1000 (float-time (time-since (process-get proc 'start-time)))))
             (kill-buffer (get-buffer-create buffer-name))
             (with-current-buffer (get-buffer-create "*image-output*")
               (let ((img (create-image (process-get proc 'payload) 'jpeg t)))
                 (insert-image img)))))))

(defun image-server-filter (proc string)
  (with-current-buffer (process-get proc 'buffer)
    (let ((inhibit-modification-hooks t))
     (insert string)
     (save-excursion
       (goto-char (point-min))
       (cond
        ((not (process-get proc 'boundary))
         (let ((point (search-forward "\r\n\r\n" nil t 1)))
           (when point
             (process-put proc 'boundary point)
             (goto-char (point-min))
             (search-forward "Content-Length: " point nil 1)
             (process-put proc
                          'content-length
                          (string-to-number (buffer-substring (point) (line-end-position)))))))
        (t
         (let ((boundary (process-get proc 'boundary))
               (content-length (process-get proc 'content-length)))
           (when (>= (point-max) (+ boundary content-length))
             (process-put proc 'payload
                          (buffer-substring-no-properties boundary content-length))
             (delete-process proc)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarks

;; Currently in use

;; (benchmark-run-compiled 100
;;   (length
;;    (with-temp-buffer
;;      (cl-loop for i from 0 to 10000
;;               do (insert "hello world hello worldhello worldhello worldhello worldhello world"))
;;      (buffer-string)))
;;   )

;; Map concat

;; (benchmark-run-compiled 100
;;   (length
;;    (let ((buffer (list)))
;;      (cl-loop for i from 0 to 10000
;;               do (setq buffer (cons "hello world hello worldhello worldhello worldhello worldhello world" buffer)))
;;      (mapconcat 'identity (reverse buffer) ""))))

;; setcdr + mapconcat

;; (benchmark-run-compiled 100
;;   (length
;;    (let* ((buffer (list ""))
;;           (pointer buffer))
;;      (cl-loop for i from 0 to 10000
;;               do (setq
;;                   pointer
;;                   (setcdr pointer
;;                           (list "hello world hello worldhello worldhello worldhello worldhello world"))))
;;      (mapconcat 'identity buffer ""))))

;; setcdr + apply 'concat

;; (benchmark-run-compiled 100
;;   (length
;;    (let* ((buffer (list ""))
;;           (pointer buffer))
;;      (cl-loop for i from 0 to 10000
;;               do (setq
;;                   pointer
;;                   (setcdr pointer
;;                           (list "hello world hello worldhello worldhello worldhello worldhello world"))))
;;      (apply #'concat buffer))))

(provide 'test)
