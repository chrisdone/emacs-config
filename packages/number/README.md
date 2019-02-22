# number

Do trivial arithmetic on the numbers at point. Attempts to preserve
padding when it can. Examples:

`M-x number/add 1 RET`

    1 -> 2
    05 -> 06
    6.30 -> 7.30
    07.30 -> 08.30
    -08.30 -> -07.30

`M-x number/pad 2 RET`

    5 -> 05

`M-x number/pad 2 RET 6 RET`

    3.141 -> 03.141000

The "guessing" where the number is isn't yet quite awesome, e.g. it
doesn't know that the `05` in `2014-05-01` is a month and not,
e.g. the number `-05`. But you can use the region to explicitly denote
the start and end of the number.

The following keybindings might be nice to use:

``` lisp
(global-set-key (kbd "C-c C-+") 'number/add)
(global-set-key (kbd "C-c C--") 'number/sub)
(global-set-key (kbd "C-c C-*") 'number/multiply)
(global-set-key (kbd "C-c C-/") 'number/divide)
(global-set-key (kbd "C-c C-0") 'number/pad)
(global-set-key (kbd "C-c C-=") 'number/eval)
```
