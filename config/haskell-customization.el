(defvar-local hasktags-directories (list ".")
  "Where to find Haskell source to generate tags from.")

(defvar-local hasktags-path ".tags"
  "Where to write the TAGS file.")

(defvar-local hiedb-directories (list ".")
  "Where to find Haskell source to generate database from.")

(defvar-local hiedb-path ""
  "Where to write the hiedb file.")

(defcustom haskell-suggestion-buffers
  (list "*compilation*" "*ghci*" "*sh:server*")
  "Buffers from which one can pull suggestions.")
