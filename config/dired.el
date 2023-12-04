(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$"
      dired-listing-switches "-alh --group-directories-first -t -o --time-style=long-iso")
