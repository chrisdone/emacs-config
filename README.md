# Emacs configuration

To use

    $ git submodule init
    $ git submodule update
    $ cd packages/haskell-mode; make
    $ cd packages/structured-haskell-mode; cabal install; cd elisp; make
    $ cabal install hasktags

Optionally, for experimental type of subexpression:

    $ git clone https://github.com/bennofs/hdevtools.git && cd hdevtools && cabal install

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

### My software versions

    $ emacs --version
    GNU Emacs 24.3.1
    Copyright (C) 2013 Free Software Foundation, Inc.
    GNU Emacs comes with ABSOLUTELY NO WARRANTY.
    You may redistribute copies of Emacs
    under the terms of the GNU General Public License.
    For more information about these matters, see the file named COPYING.

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.6.2
    $
