[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

Sirthias Theme for Emacs
==============

Sirthias Theme for Emacs developed by Alexander Ivanov [@4lex1v](https://twitter.com/4lex1v) and inspired by Mathias Doenitz [@sirthias](https://twitter.com/sirthias) slides to his talks on the [Spray](http://spray.io) Toolkit, after whom this theme was named.

Originaly this theme was designed to work with Scala, Haskell and Lisp-like languages, but after a month of active usage it's more or less ready. Though it's still under development, so it's quite possible that some colors might be slightly adjusted. Other parts of Emacs are not well styled yet, i'm working on them.

Examples
==============

Scala
---
![scala1](https://cloud.githubusercontent.com/assets/1001122/5550800/3f209416-8bc0-11e4-91ab-a21d0e1b77ed.png)
![scala2](https://cloud.githubusercontent.com/assets/1001122/5550807/5e335ef6-8bc0-11e4-9f41-30315e3fbaeb.png)

Haskell
---
![haskell1](https://cloud.githubusercontent.com/assets/1001122/5550809/6687dc8a-8bc0-11e4-8afb-df741d7469c7.png)
![haskell2](https://cloud.githubusercontent.com/assets/1001122/5550813/77b8a4da-8bc0-11e4-9155-d838a51c0ef5.png)

Emacs-lisp
---
![lisp2](https://cloud.githubusercontent.com/assets/1001122/5550816/80e09d60-8bc0-11e4-87a6-70d2d8afae87.png)

JavaScript
---
![js](https://cloud.githubusercontent.com/assets/1001122/5550818/8c1a2548-8bc0-11e4-9608-5a5b840fcfac.png)

Installation
==============

Currently this theme is not accessible on Melpa or other Emacs repository. To install it manually you need to copy `sirthias-theme` into you emacs configuration folder and load it in your configuration file, e.g:

- put [sirthias-theme.el](https://github.com/4lex1v/sirthias-theme/blob/master/sirthias-theme.el) into `~/.emacs.d/themes`
- add folder to your theme load path:
```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```
- load theme with `M-x load-theme`

A bit more complex way, but still the same available [here](https://github.com/4lex1v/emacs/blob/master/configs/ui.el)

Bugs & Improvements
==============

If you liked this theme and have ideas how to make it better, please don't hesitate to fork it and open a PR!

Thanks!
Alexander

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
