# About

hamlet-mode is an Emacs major mode for editing files written in
[Hamlet](http://hackage.haskell.org/package/hamlet), a Haskell compile-time HTML
templating engine. Currently it only provides syntax highlighting.

# Installation

If you have Emacs >= 24, you can conveniently install `hamlet-mode` via the package
in [MELPA](http://melpa.milkbox.net/).

To install manually, simply put `hamlet-mode.el` wherever you want, load it, and `(require
'hamlet-mode)`.

If you want to highlight quasiquoted Hamlet, install `mmm-mode` and do something like this:

```lisp
(require 'mmm-vars)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(mmm-add-classes
 '((hamlet-quasiquote
    :submode hamlet-mode
    :delimiter-mode nil
    :front "\\[x?hamlet|"
    :back "|\\]")))
(mmm-add-mode-ext-class 'haskell-mode nil 'hamlet-quasiquote)
```
