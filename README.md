# Emacs configuration

To use

    $ git submodule init
    $ git submodule update
    $ cd packages/haskell-mode; make
    $ cd packages/structured-haskell-mode; cabal install
    $ cabal install hasktags

### Use without affecting existing Emacs configuration

This is perfect for the “just looking” use-case.

    $ emacs -Q -l init.el

### Use by modifying `.emacs`

Put only this in your `.emacs`

    (load "/path-to/chrisdone-emacs/init.el")

and run Emacs as normal.

### Use by checking out as `~/.emacs.d/`

Check out this project or symlink it as `~/.emacs.d/` and then run
Emacs as normal.
