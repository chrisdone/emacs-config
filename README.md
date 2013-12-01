# Emacs configuration

To use

    $ git submodule init
    $ git submodule update
    $ cd packages/haskell-mode; make
    $ cd packages/structured-haskell-mode; cabal install

Then

    $ emacs -Q -l config.el

or add this to your `.emacs`

    (load "/path-to/chrisdone-emacs/config.el")

and run Emacs as normal.
