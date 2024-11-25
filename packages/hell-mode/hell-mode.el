(require 'company)

(defun company-hell-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-hell-backend))
    (prefix (and (eq major-mode 'hell-mode)
                 (or (and (looking-back hell-ident-regex 1)
                          (buffer-substring
                           (save-excursion
                             (when (re-search-backward hell-ident-regex
                                                       (line-beginning-position)
                                                       t
                                                       1)
                               (point)))
                           (point)))
                     (company-grab-symbol))))
    (candidates
     (cl-remove-if-not
      (lambda (c)
        (or (string-match
             (concat "^"
                     (mapconcat (lambda (x)
                                  (concat "\\(" (regexp-quote x) "\\)"))
                                (split-string arg (regexp-quote ".") t " ")
                                ".*?"))
             c)
            (string-prefix-p arg c)))
      hell-completions))))

(defconst hell-ident-regex "[A-Z][a-zA-Z0-9']*\\.[a-zA-Z'0-9]+")

(add-to-list 'company-backends 'company-hell-backend)

(define-derived-mode hell-mode
  prog-mode "Hell"
  "Major mode for Hell.
 \\{hell-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(hell-keywords t nil nil))
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "-- ")
  (setq-local comment-end "")
  (company-mode))

(defconst hell-keywords
  `((" -- .*$" . font-lock-comment-face)
    ("{-.*-}" . font-lock-comment-face)
    (" --$" . font-lock-comment-face)
    ("^--.*" . font-lock-comment-face)
    ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
    ("\\<\\*?\\[?[A-Z][a-zA-Z0-9_']*\\]?\\>" . font-lock-type-face)
    (,(concat "\\<" (regexp-opt '("if" "case" "of" "then" "else" "data" "do" "let" "$" ".")) "\\>") . font-lock-keyword-face)
    ("^[a-zA-Z_'0-9]+ " . font-lock-function-name-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hell\\'" . hell-mode))

(defconst hell-completions
  '(
    "Async.concurrently"
    "Async.pooledForConcurrently"
    "Async.pooledForConcurrently_"
    "Async.pooledMapConcurrently"
    "Async.pooledMapConcurrently_"
    "Async.race"
    "Bool.False"
    "Bool.True"
    "Bool.bool"
    "Bool.not"
    "ByteString.getContents"
    "ByteString.hGet"
    "ByteString.hPutStr"
    "ByteString.interact"
    "ByteString.readFile"
    "ByteString.readProcess"
    "ByteString.readProcessStdout_"
    "ByteString.readProcess_"
    "ByteString.writeFile"
    "Concurrent.threadDelay"
    "Directory.copyFile"
    "Directory.createDirectory"
    "Directory.createDirectoryIfMissing"
    "Directory.getCurrentDirectory"
    "Directory.listDirectory"
    "Directory.removeFile"
    "Directory.renameFile"
    "Directory.setCurrentDirectory"
    "Double.eq"
    "Double.fromInt"
    "Double.plus"
    "Double.show"
    "Double.subtract"
    "Either.Left"
    "Either.Right"
    "Either.either"
    "Environment.getArgs"
    "Environment.getEnv"
    "Environment.getEnvironment"
    "Eq.eq"
    "Error.error"
    "Function.fix"
    "Function.id"
    "IO.BlockBuffering"
    "IO.LineBuffering"
    "IO.NoBuffering"
    "IO.forM_"
    "IO.hSetBuffering"
    "IO.mapM_"
    "IO.print"
    "IO.pure"
    "IO.stderr"
    "IO.stdin"
    "IO.stdout"
    "Int.eq"
    "Int.plus"
    "Int.show"
    "Int.subtract"
    "Json.Array"
    "Json.Bool"
    "Json.Null"
    "Json.Number"
    "Json.Object"
    "Json.String"
    "Json.decode"
    "Json.encode"
    "Json.value"
    "List.and"
    "List.concat"
    "List.cons"
    "List.drop"
    "List.filter"
    "List.foldl'"
    "List.groupBy"
    "List.iterate'"
    "List.length"
    "List.lookup"
    "List.map"
    "List.nil"
    "List.or"
    "List.reverse"
    "List.sort"
    "List.sortOn"
    "List.take"
    "List.zip"
    "List.zipWith"
    "Map.adjust"
    "Map.all"
    "Map.any"
    "Map.delete"
    "Map.filter"
    "Map.filterWithKey"
    "Map.fromList"
    "Map.insert"
    "Map.insertWith"
    "Map.lookup"
    "Map.map"
    "Map.singleton"
    "Map.size"
    "Map.toList"
    "Map.unionWith"
    "Maybe.Just"
    "Maybe.Nothing"
    "Maybe.listToMaybe"
    "Maybe.mapMaybe"
    "Maybe.maybe"
    "Monad.bind"
    "Monad.forM"
    "Monad.forM_"
    "Monad.mapM"
    "Monad.mapM_"
    "Monad.return"
    "Monad.then"
    "Monad.when"
    "Ord.gt"
    "Ord.lt"
    "Process.proc"
    "Process.runProcess"
    "Process.runProcess_"
    "Process.setEnv"
    "Record.cons"
    "Record.get"
    "Record.modify"
    "Record.nil"
    "Record.set"
    "Set.delete"
    "Set.fromList"
    "Set.insert"
    "Set.member"
    "Set.singleton"
    "Set.size"
    "Set.toList"
    "Set.union"
    "Show.show"
    "Tagged.Tagged"
    "Text.appendFile"
    "Text.breakOn"
    "Text.concat"
    "Text.decodeUtf8"
    "Text.drop"
    "Text.dropEnd"
    "Text.encodeUtf8"
    "Text.eq"
    "Text.getContents"
    "Text.getLine"
    "Text.hPutStr"
    "Text.interact"
    "Text.intercalate"
    "Text.isInfixOf"
    "Text.isPrefixOf"
    "Text.isSuffixOf"
    "Text.length"
    "Text.lines"
    "Text.putStr"
    "Text.putStrLn"
    "Text.readFile"
    "Text.readProcess"
    "Text.readProcessStdout_"
    "Text.readProcess_"
    "Text.replace"
    "Text.reverse"
    "Text.setStdin"
    "Text.splitOn"
    "Text.strip"
    "Text.stripPrefix"
    "Text.stripSuffix"
    "Text.take"
    "Text.takeEnd"
    "Text.toLower"
    "Text.toUpper"
    "Text.unlines"
    "Text.unwords"
    "Text.words"
    "Text.writeFile"
    "Timeout.timeout"
    "Vector.fromList"
    "Vector.toList"
    ))

(provide 'hell-mode)
