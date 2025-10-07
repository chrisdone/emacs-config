;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions

(defvar-local intero-extensions nil)

(defun intero-extensions ()
  "Get extensions for the current project's GHC."
  (or intero-extensions
      (setq intero-extensions
            (cl-remove-if-not
             (lambda (str) (let ((case-fold-search nil))
                             (string-match "^[A-Z][A-Za-z0-9]+$" str)))
             (split-string
              "Haskell2010
GHC2021
Unsafe
Trustworthy
Safe
AllowAmbiguousTypes
AlternativeLayoutRule
AlternativeLayoutRuleTransitional
Arrows
AutoDeriveTypeable
BangPatterns
BinaryLiterals
CApiFFI
CPP
CUSKs
ConstrainedClassMethods
ConstraintKinds
DataKinds
DatatypeContexts
DefaultSignatures
DeriveAnyClass
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveLift
DeriveTraversable
DerivingStrategies
DerivingVia
DisambiguateRecordFields
DoAndIfThenElse
BlockArguments
DoRec
DuplicateRecordFields
FieldSelectors
EmptyCase
EmptyDataDecls
EmptyDataDeriving
ExistentialQuantification
ExplicitForAll
ExplicitNamespaces
ExtendedDefaultRules
ExtendedLiterals
FlexibleContexts
FlexibleInstances
ForeignFunctionInterface
FunctionalDependencies
GADTSyntax
GADTs
GHCForeignImportPrim
GeneralizedNewtypeDeriving
GeneralisedNewtypeDeriving
ImplicitParams
ImplicitPrelude
ImportQualifiedPost
ImpredicativeTypes
IncoherentInstances
TypeFamilyDependencies
InstanceSigs
ApplicativeDo
InterruptibleFFI
KindSignatures
LambdaCase
LexicalNegation
LiberalTypeSynonyms
LinearTypes
MagicHash
MonadComprehensions
MonoLocalBinds
DeepSubsumption
MonomorphismRestriction
MultiParamTypeClasses
MultiWayIf
NumericUnderscores
NPlusKPatterns
NamedFieldPuns
NamedWildCards
NegativeLiterals
HexFloatLiterals
NullaryTypeClasses
NumDecimals
OverlappingInstances
OverloadedLabels
OverloadedLists
OverloadedStrings
PackageImports
ParallelArrays
ParallelListComp
PartialTypeSignatures
PatternGuards
PatternSignatures
PatternSynonyms
PolyKinds
PolymorphicComponents
QuantifiedConstraints
PostfixOperators
QuasiQuotes
QualifiedDo
Rank2Types
RankNTypes
RebindableSyntax
OverloadedRecordDot
OverloadedRecordUpdate
RecordPuns
RecordWildCards
RecursiveDo
RelaxedLayout
RelaxedPolyRec
RoleAnnotations
ScopedTypeVariables
StandaloneDeriving
StarIsType
StaticPointers
Strict
StrictData
TemplateHaskell
TemplateHaskellQuotes
StandaloneKindSignatures
TraditionalRecordSyntax
TransformListComp
TupleSections
TypeAbstractions
TypeApplications
TypeData
TypeInType
TypeFamilies
TypeOperators
TypeSynonymInstances
UnboxedTuples
UnboxedSums
UndecidableInstances
UndecidableSuperClasses
UnicodeSyntax
UnliftedDatatypes
UnliftedFFITypes
UnliftedNewtypes
ViewPatterns")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing errors/warnings/splices

(defconst intero-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6: Warning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.")

(defun intero-parse-error (string)
  "Parse the line number from the error in STRING."
  (save-match-data
    (when (string-match (mapconcat #'car intero-error-regexp-alist "\\|")
                        string)
      (let ((string3 (match-string 3 string))
            (string5 (match-string 5 string)))
        (list :file (match-string 1 string)
              :line (string-to-number (match-string 2 string))
              :col (string-to-number (match-string 4 string))
              :line2 (when string3
                       (string-to-number string3))
              :col2 (when string5
                      (string-to-number string5)))))))

(defun intero-parse-errors-warnings-splices (string)
  "Parse GHC errors and warnings in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((messages (list))
          (found-error-as-warning nil))
      (while (search-forward-regexp
              (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                      "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
              nil t 1)
        (let* ((local-file (match-string 1))
               (file (expand-file-name local-file default-directory))
               (location-raw (match-string 2))
               (msg (replace-regexp-in-string
                     "[\n\r ]*|$"
                     ""
                     (match-string 3))) ;; Replace gross bullet points.
               (type (cond ((string-match "^Warning:" msg)
                            (setq msg (replace-regexp-in-string "^Warning: *" "" msg))
                            (if (string-match-p
                                 (rx bol
                                     "["
                                     (or "-Wdeferred-type-errors"
                                         "-Wdeferred-out-of-scope-variables"
                                         "-Wtyped-holes")
                                     "]")
                                 msg)
                                (progn (setq found-error-as-warning t)
                                       'error)
                              'warning))
                           ((string-match-p "^Splicing " msg) 'splice)
                           (t                                 'error)))
               (location (intero-parse-error
                          (concat local-file ":" location-raw ": x")))
               (line (plist-get location :line))
               (column (plist-get location :col)))
          (setq messages
                (cons (list
                       :line line
                       :column column
                       :type type
                       :msg msg
                       :filename file)
                      messages)))
        (forward-line -1))
      (delete-dups
       (if found-error-as-warning
           (cl-remove-if (lambda (msg) (eq 'warning (plist-get msg :type))) messages)
         messages)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting suggestions

(defvar-local intero-suggestions nil)

(defun intero-collect-compiler-messages (msgs)
  "Collect information from compiler MSGS.
This may update in-place the MSGS objects to hint that
suggestions are available."
  (let ((intero-suggestions nil))
    (let ((extension-regex (concat "-?X?" (regexp-opt (remove-if (lambda (e) (or (string= e "Strict")
                                                                                 (string= e "Unsafe")))
                                                                 (intero-extensions)) t)))
          (quoted-symbol-regex "[‘`‛]\\([^ ]+\\)['’]"))
      (cl-loop
       for msg in msgs
       do (let ((text (plist-get msg :msg))
                (note nil))
            ;; Messages of this format
            ;; No module named ‘L’ is imported.
            (let ((start 0))
              (while (let ((case-fold-search nil))
                       (string-match "No module named [‘`‛]\\([^ ]+\\)['’] is imported" text start))
                (let* ((qual (match-string 1 text))
                       (mapped (assoc qual haskell-quals)))
                  (when mapped
                    (let ((unmapped (assoc (cdr mapped) haskell-import-mapping)))
                      (when unmapped
                        (setq note t)
                        (add-to-list 'intero-suggestions
                                     (list :msg msg
                                           :type 'add-mapped-import
                                           :import-string (cdr unmapped)))))))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;;
            ;;     • Constructor ‘Assert’ does not have the required strict field(s): assertName,
            ;; assertDoc, assertExpression,
            ;; assertSection
            ;; (let ((start 0))
            ;;   (while (or
            ;;           (string-match "does not have the required strict field.*?:[\n\t\r ]" text start)
            ;;           (string-match "Fields of .*? not initialised:[\n\t\r ]" text start))
            ;;     (let* ((match-end (match-end 0))
            ;;            (fields
            ;;             (let ((reached-end nil))
            ;;               (mapcar
            ;;                (lambda (field)
            ;;                  (with-temp-buffer
            ;;                    (insert field)
            ;;                    (goto-char (point-min))
            ;;                    (intero-ident-at-point)))
            ;;                (cl-remove-if
            ;;                 (lambda (field)
            ;;                   (or reached-end
            ;;                       (when (string-match "[\r\n]" field)
            ;;                         (setq reached-end t)
            ;;                         nil)))
            ;;                 (split-string
            ;;                  (substring text match-end)
            ;;                  "[\n\t\r ]*,[\n\t\r ]*" t))))))
            ;;       (setq note t)
            ;;       (add-to-list
            ;;        'intero-suggestions
            ;;        (list :msg msg
                            ;; :type 'add-missing-fields
            ;;              :fields fields
            ;;              :line (plist-get msg :line)
            ;;              :column (plist-get msg :column)))
            ;;       (setq start (min (length text) (1+ match-end))))))

            ;; Messages of this format:
            ;;
            ;; Can't make a derived instance of ‘Functor X’:
            ;;       You need DeriveFunctor to derive an instance for this class
            ;;       Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
            ;;       In the newtype declaration for ‘X’
            (let ((start 0))
              (while (let ((case-fold-search nil))
                       (string-match extension-regex text start))
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-extension
                                   :extension (match-string 1 text)))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;;
            ;; Could not find module ‘Language.Haskell.TH’
            ;;     It is a member of the hidden package ‘template-haskell’.
            ;;     Use -v to see a list of the files searched for....
            (let ((start 0))
              (while (string-match "It is a member of the hidden package [‘`‛]\\([^ ]+\\)['’]" text start)
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-package
                                   :package (match-string 1 text)))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;; Expected type: String
            ;; Actual type: Data.Text.Internal.Builder.Builder
            (let ((start 0))
              (while (or (string-match
                          "Expected type: String" text start)
                         (string-match
                          "Actual type: String" text start)
                         (string-match
                          "Actual type: \\[Char\\]" text start)
                         (string-match
                          "Expected type: \\[Char\\]" text start))
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-extension
                                   :extension "OverloadedStrings"))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;;
            ;; Defaulting the following constraint(s) to type ‘Integer’
            ;;   (Num a0) arising from the literal ‘1’
            ;; In the expression: 2
            ;; In an equation for ‘x'’: x' = 2
            (let ((start 0))
              (while (string-match
                      " Defaulting the following constraint" text start)
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-ghc-option
                                   :option "-fno-warn-type-defaults"))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;;
            ;;     This binding for ‘x’ shadows the existing binding
            (let ((start 0))
              (while (string-match
                      " This binding for ‘\\(.*\\)’ shadows the existing binding" text start)
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-ghc-option
                                   :option "-fno-warn-name-shadowing"))
                (setq start (min (length text) (1+ (match-end 0))))))
            ;; Messages of this format:
            ;; Perhaps you want to add ‘foo’ to the import list
            ;; in the import of ‘Blah’
            ;; (/path/to/thing:19
            (when (string-match "Perhaps you want to add [‘`‛]\\([^ ]+\\)['’][\n ]+to[\n ]+the[\n ]+import[\n ]+list[\n ]+in[\n ]+the[\n ]+import[\n ]+of[\n ]+[‘`‛]\\([^ ]+\\)['’][\n ]+(\\([^ ]+\\):(?\\([0-9]+\\)[:,]"
                                text)
              (let ((ident (match-string 1 text))
                    (module (match-string 2 text))
                    (file (match-string 3 text))
                    (line (string-to-number (match-string 4 text))))
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-to-import
                                   :module module
                                   :ident ident
                                   :line line))))
            ;; Messages of this format:
            ;;
            ;; The import of ‘Control.Monad’ is redundant
            ;;   except perhaps to import instances from ‘Control.Monad’
            ;; To import instances alone, use: import Control.Monad()... (intero)
            (when (string-match
                   " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant"
                   text)
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :msg msg
                                 :type 'remove-import
                                 :module (match-string 2 text)
                                 :line (plist-get msg :line))))
            ;; Messages of this format:
            ;;
            ;; Not in scope: ‘putStrn’
            ;; Perhaps you meant one of these:
            ;;   ‘putStr’ (imported from Prelude),
            ;;   ‘putStrLn’ (imported from Prelude)
            ;;
            ;; Or this format:
            ;;
            ;; error:
            ;;    • Variable not in scope: lopSetup :: [Statement Exp']
            ;;    • Perhaps you meant ‘loopSetup’ (line 437)
            (when (string-match
                   "[Nn]ot in scope: \\(data constructor \\|type constructor or class \\)?[‘`‛]?\\([^'’ ]+\\).*\n.*Perhaps you meant"
                   text)
              (let ((typo (match-string 2 text))
                    (start (min (length text) (1+ (match-end 0)))))
                (while (string-match quoted-symbol-regex text start)
                  (setq note t)
                  (add-to-list 'intero-suggestions
                               (list :msg msg
                                     :type 'fix-typo
                                     :typo typo
                                     :replacement (match-string 1 text)
                                     :column (plist-get msg :column)
                                     :line (plist-get msg :line)))
                  (setq start (min (length text) (1+ (match-end 0)))))))
            ;; Messages of this format:
            ;;
            ;;     Top-level binding with no type signature: main :: IO ()
            (when (string-match
                   "Top-level binding with no type signature:"
                   text)
              (let ((start (min (length text) (match-end 0))))
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'add-signature
                                   :signature (mapconcat #'identity (split-string (substring text start)) " ")
                                   :line (plist-get msg :line)))))
            ;; Messages of this format:
            (when (string-match "The import of [‘`‛]\\(.+?\\)[’`'][\n ]+from[\n ]+module[\n ]+[‘`‛]\\(.+?\\)[’`'][\n ]+is[\n ]+redundant" text)
              (let ((module (match-string 2 text))
                    (idents (split-string (match-string 1 text) "," t "[ \n]+")))
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :msg msg
                                   :type 'redundant-import-item
                                   :idents idents
                                   :line (plist-get msg :line)
                                   :module module))))
            ;; Messages of this format:
            ;;
            ;;     Redundant constraints: (Arith var, Bitwise var)
            ;; Or
            ;;     Redundant constraint: Arith var
            ;; Or
            ;;     Redundant constraints: (Arith var,
            ;;                             Bitwise var,
            ;;                             Functor var,
            ;;                             Applicative var,
            ;;                             Monad var)
            ;; (when (string-match "Redundant constraints?: " text)
            ;;   (let* ((redundant-start (match-end 0))
            ;;          (parts (with-temp-buffer
            ;;                  (insert (substring text redundant-start))
            ;;                  (goto-char (point-min))
            ;;                  ;; A lone unparenthesized constraint might
            ;;                  ;; be multiple sexps.
            ;;                  (while (not (eq (point) (point-at-eol)))
            ;;                    (forward-sexp))
            ;;                  (let ((redundant-end (point)))
            ;;                    (search-forward-regexp ".*\n.*In the ")
            ;;                    (cons (buffer-substring (point-min) redundant-end)
            ;;                          (buffer-substring (match-end 0) (point-max)))))))
            ;;     (setq note t)
            ;;     (add-to-list
            ;;      'intero-suggestions
            ;;      (let ((rest (cdr parts))
            ;;            (redundant (let ((raw (car parts)))
            ;;                         (if (eq (string-to-char raw) ?\()
            ;;                             (substring raw 1 (1- (length raw)))
            ;;                           raw))))
            ;;        (list :type 'redundant-constraint
            ;;              :redundancies (mapcar #'string-trim
            ;;                                    (intero-parse-comma-list redundant))
            ;;              :signature (mapconcat #'identity (split-string rest) " ")
            ;;              :line (plist-get msg :line))))))
            )))
    intero-suggestions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiswitch

(defvar intero-multiswitch-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
    (define-key map (kbd "C-c C-k") 'abort-recursive-edit)
    (define-key map (kbd "C-g")     'abort-recursive-edit)
    map))

(defun intero-multiswitch (title options)
  "Displaying TITLE, read multiple flags from a list of OPTIONS.
Each option is a plist of (:key :default :title) wherein:
  :key should be something comparable with EQUAL
  :title should be a string
  :default (boolean) specifies the default checkedness"
  (let ((available-width (window-total-width)))
    (save-window-excursion
      (with-temp-buffer
        (rename-buffer (generate-new-buffer-name "multiswitch"))
        (widget-insert (concat title "\n\n"))
        (widget-insert (propertize "Select options with RET, hit " 'face 'font-lock-comment-face))
        (widget-create 'push-button :notify
                       (lambda (&rest ignore)
                         (exit-recursive-edit))
                       "C-c C-c")
        (widget-insert (propertize " to apply these choices, or hit " 'face 'font-lock-comment-face))
        (widget-create 'push-button :notify
                       (lambda (&rest ignore)
                         (abort-recursive-edit))
                       "C-c C-k")
        (widget-insert (propertize " to cancel.\n\n" 'face 'font-lock-comment-face))
        (let* ((me (current-buffer))
               (choices (mapcar (lambda (option)
                                  (append option (list :value (plist-get option :default))))
                                options)))
          (cl-loop for option in choices
                   do (widget-create
                       'toggle
                       :notify (lambda (widget &rest ignore)
                                 (setq choices
                                       (mapcar (lambda (choice)
                                                 (if (equal (plist-get choice :key)
                                                            (plist-get (cdr widget) :key))
                                                     (plist-put choice :value (plist-get (cdr widget) :value))
                                                   choice))
                                               choices)))
                       :on (concat "[x] " (plist-get option :title))
                       :off (concat "[ ] " (plist-get option :title))
                       :value (plist-get option :default)
                       :key (plist-get option :key)))
          (let ((lines (line-number-at-pos)))
            (select-window (split-window-below))
            (switch-to-buffer me)
            (goto-char (point-min)))
          (use-local-map intero-multiswitch-keymap)
          (widget-setup)
          (recursive-edit)
          (kill-buffer me)
          (mapcar (lambda (choice)
                    (plist-get choice :key))
                  (cl-remove-if-not (lambda (choice)
                                      (plist-get choice :value))
                                    choices)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply suggestions

(defun intero-apply-suggestions (intero-suggestions)
  "Prompt and apply the suggestions."
  (if (null intero-suggestions)
      (message "No suggestions to apply")
    (let ((to-apply
           (intero-multiswitch
            (format "There are %d suggestions to apply:" (length intero-suggestions))
            (cl-remove-if-not
             #'identity
             (mapcar
              (lambda (suggestion)
                (cl-case (plist-get suggestion :type)
                  (add-to-import
                   (list :key suggestion
                         :title (format "Add ‘%s’ to import of ‘%s’"
                                        (plist-get suggestion :ident)
                                        (plist-get suggestion :module))
                         :default t))
                  (add-mapped-import
                   (list :key suggestion
                         :default t
                         :title
                         (format "Add qualified import text:\n%s\n"
                                 (plist-get suggestion :import-string))))
                  (add-missing-fields
                   (list :key suggestion
                         :default t
                         :title
                         (format "Add missing fields to record: %s"
                                 (mapconcat (lambda (ident)
                                              (concat "‘" ident "’"))
                                            (plist-get suggestion :fields)
                                            ", "))))
                  (redundant-import-item
                   (list :key suggestion
                         :title
                         (format "Remove redundant imports %s from import of ‘%s’"
                                 (mapconcat (lambda (ident)
                                              (concat "‘" ident "’"))
                                            (plist-get suggestion :idents) ", ")
                                 (plist-get suggestion :module))
                         :default t))
                  (add-extension
                   (list :key suggestion
                         :title (concat "Add {-# LANGUAGE "
                                        (plist-get suggestion :extension)
                                        " #-}")
                         :default (not (string= "OverloadedStrings" (plist-get suggestion :extension)))))
                  (add-ghc-option
                   (list :key suggestion
                         :title (concat "Add {-# OPTIONS_GHC "
                                        (plist-get suggestion :option)
                                        " #-}")
                         :default (not
                                   (string=
                                    (plist-get suggestion :option)
                                    "-fno-warn-name-shadowing"))))
                  (add-package
                   (list :key suggestion
                         :title (concat "Enable package: " (plist-get suggestion :package))
                         :default t))
                  (remove-import
                   (list :key suggestion
                         :title (concat "Remove: import "
                                        (plist-get suggestion :module))
                         :default t))
                  (fix-typo
                   (list :key suggestion
                         :title (concat "Replace ‘"
                                        (plist-get suggestion :typo)
                                        "’ with ‘"
                                        (plist-get suggestion :replacement)
                                        "’")
                         :default (null (cdr intero-suggestions))))
                  (add-signature
                   (list :key suggestion
                         :title (concat "Add signature: "
                                        (plist-get suggestion :signature))
                         :default t))
                  (redundant-constraint
                   (list :key suggestion
                         :title (concat
                                 "Remove redundant constraints: "
                                 (string-join (plist-get suggestion :redundancies)
                                              ", ")
                                 "\n    from the "
                                 (plist-get suggestion :signature))
                         :default nil))))
              intero-suggestions)))))
      (if (null to-apply)
          (message "No changes selected to apply.")
        (let ((sorted (sort to-apply
                            (lambda (lt gt)
                              (let ((lt-line   (or (plist-get lt :line)   0))
                                    (lt-column (or (plist-get lt :column) 0))
                                    (gt-line   (or (plist-get gt :line)   0))
                                    (gt-column (or (plist-get gt :column) 0)))
                                (or (> lt-line gt-line)
                                    (and (= lt-line gt-line)
                                         (> lt-column gt-column)))))))
              (added-imports nil))
          ;; # Changes unrelated to the buffer

          ;; commented out
          ;; Tue 29 Nov 2022 15:00:22 GMT
          ;; (cl-loop
          ;;  for suggestion in sorted
          ;;  do (cl-case (plist-get suggestion :type)
          ;;       (add-package
          ;;        (intero-add-package (plist-get suggestion :package)))))
          ;; # Changes that do not increase/decrease line numbers
          ;;

          ;; Update in-place suggestions
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-to-import
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (when (and (search-forward (plist-get suggestion :module) nil t 1)
                              (search-forward "(" nil t 1))
                     (insert (if (string-match-p "^[_a-zA-Z]" (plist-get suggestion :ident))
                                 (plist-get suggestion :ident)
                               (concat "(" (plist-get suggestion :ident) ")")))
                     (unless (looking-at-p "[:space:]*)")
                       (insert ", ")))))
                (redundant-import-item
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (let* ((case-fold-search nil)
                          (start (search-forward "(" nil t 1))
                          (end (or (save-excursion
                                     (when (search-forward-regexp "\n[^ \t]" nil t 1)
                                       (1- (point))))
                                   (line-end-position)))
                          (regex
                           (concat
                            "\\("
                            (mapconcat
                             (lambda (ident)
                               (if (string-match-p "^[_a-zA-Z]" ident)
                                   (concat "\\<" (regexp-quote ident) "\\> ?" "\\("(regexp-quote "(..)") "\\)?")
                                 (concat "(" (regexp-quote ident) ")")))
                             (plist-get suggestion :idents)
                             "\\|")
                            "\\)"))
                          (string (buffer-substring start end)))
                     (delete-region start end)
                     (insert
                      (replace-regexp-in-string
                       ",[\n ]*)" ")"
                       (replace-regexp-in-string
                        "^[\n ,]*" ""
                        (replace-regexp-in-string
                         "[\n ,]*,[\n ,]*" ", "
                         (replace-regexp-in-string
                          ",[\n ]*)" ")"
                          (replace-regexp-in-string
                           regex ""
                           string)))))
                      (make-string (1- (length (split-string string "\n" t))) 10)))))
                (fix-typo
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (move-to-column (- (plist-get suggestion :column) 1))
                   (delete-char (length (plist-get suggestion :typo)))
                   (insert (plist-get suggestion :replacement))))
                (add-missing-fields
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (move-to-column (- (plist-get suggestion :column) 1))
                   (search-forward "{")
                   (unless (looking-at "}")
                     (save-excursion (insert ", ")))
                   (insert (mapconcat (lambda (field) (concat field " = _"))
                                      (plist-get suggestion :fields)
                                      ", "))))))
          ;; # Changes that do increase/decrease line numbers
          ;;
          ;; Remove redundant constraints
          ;; commented out Tue 29 Nov 2022 15:00:46 GMT

          ;; (cl-loop
          ;;  for suggestion in sorted
          ;;  do (cl-case (plist-get suggestion :type)
          ;;       (redundant-constraint
          ;;        (save-excursion
          ;;          (goto-char (point-min))
          ;;          (forward-line (1- (plist-get suggestion :line)))
          ;;          (search-forward-regexp "[[:alnum:][:space:]\n]*=>")
          ;;          (backward-sexp 2)
          ;;          (let ((start (1+ (point))))
          ;;            (forward-sexp)
          ;;            (let* ((end (1- (point)))
          ;;                   (constraints (intero-parse-comma-list
          ;;                                 (buffer-substring start end)))
          ;;                   (nonredundant
          ;;                    (cl-loop for r in (plist-get suggestion :redundancies)
          ;;                             with nonredundant = constraints
          ;;                             do (setq nonredundant (delete r nonredundant))
          ;;                             finally return nonredundant)))
          ;;              (goto-char start)
          ;;              (delete-char (- end start))
          ;;              (insert (string-join nonredundant ", "))))))))

          ;; Add a type signature to a top-level binding.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-signature
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (insert (plist-get suggestion :signature))
                   (insert "\n")))))

          ;; Remove import lines from the file. May remove more than one
          ;; line per import.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (remove-import
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (delete-region (line-beginning-position)
                                  (or (when (search-forward-regexp "\n[^ \t]" nil t 1)
                                        (1- (point)))
                                      (line-end-position)))))))

          ;; Adds imports to the import list
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-mapped-import
                 (save-excursion
                   (goto-char (point-max))
                   (haskell-navigate-imports)
                   (insert (plist-get suggestion :import-string))
                   (setq added-imports t)))))

          ;; Add extensions to the top of the file
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-extension
                 (save-excursion
                   (goto-char (point-min))
                   (intero-skip-shebangs)
                   (insert "{-# LANGUAGE "
                           (plist-get suggestion :extension)
                           " #-}\n")))
                (add-ghc-option
                 (save-excursion
                   (goto-char (point-min))
                   (intero-skip-shebangs)
                   (insert "{-# OPTIONS_GHC "
                           (plist-get suggestion :option)
                           " #-}\n")))))

          ;; Commented out in migration
          ;;Tue 29 Nov 2022 14:59:50 GMT
          ;; (when added-imports
          ;;   (save-excursion
          ;;     (haskell-navigate-imports)
          ;;     (haskell-sort-imports)
          ;;     (haskell-align-imports)))
          )))))

(defun intero-skip-shebangs ()
  "Skip #! and -- shebangs used in Haskell scripts."
  (when (looking-at-p "#!") (forward-line 1))
  (when (looking-at-p "-- stack ") (forward-line 1)))

(defvar haskell-import-mapping nil)
(defvar haskell-quals nil)

(provide 'intero)
