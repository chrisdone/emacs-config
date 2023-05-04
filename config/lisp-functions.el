(defun byte-compile-reload-dir ()
  "Byte-compile and reload everything."
  (interactive)
  (let ((byte-compile-warnings '(free-vars unresolved callargs redefine make-local mapcar constants suspicious)))
    (loop for file in (directory-files (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       nil
                                       "^[a-z0-9-]+\\.el$")
          do (byte-recompile-file file t 0 t))))
