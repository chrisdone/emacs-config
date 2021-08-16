# emacs-config

My entire Emacs configuration including copies of all library
dependencies checked into this repo.

* Get Emacs 27.2: http://ftp.snt.utwente.nl/pub/software/gnu/emacs/emacs-27.2.tar.xz

Build flags:

./configure --without-compress-install --with-x --without-threads \
  --without-modules --without-toolkit-scroll-bars --with-cairo \
  --with-x-toolkit=no --with-xpm=ifavailable --with-jpeg=ifavailable \
  --with-gif=ifavailable --with-tiff=ifavailable
