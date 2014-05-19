# Emacs configuration

To use

    $ git submodule init
    $ git submodule update
    $ cd packages/haskell-mode; make
    $ cd packages/structured-haskell-mode; cabal install; cd elisp; make
    $ cabal install hasktags hoogle
    $ hoogle data

Optionally, for [`:present <exp>` support in the REPL](https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-REPL#presentations):

    $ cabal install present

Optionally, for experimental type of subexpression:

    $ git clone https://github.com/bennofs/hdevtools.git && cd hdevtools && cabal install

Optionally, for browsing haddocks inside Emacs:

    $ sudo apt-get install w3m

### I'm just here to see your Haskell config

Then you want to look inside
[config/haskell.el](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el)
and steal anything that looks interesting.

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

### Weird stuff you might want to disable

You might want to comment out the following things in [init.el](https://github.com/chrisdone/chrisdone-emacs/blob/master/init.el).

God-mode is a modal input mode for Emacs like Vim:

    (god-mode)

If you prefer a dark terminal, comment out this:

    (sunburn)

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
