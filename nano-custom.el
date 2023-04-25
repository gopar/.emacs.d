(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
;; (setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(package-initialize)

;; Customize default emacs
(load-file "~/.emacs.d/lisp/myemacs/myemacs.el")

(load-file "~/.emacs.d/lisp/myemacs/pair-programming.el")

(load-file "~/.emacs.d/lisp/myemacs/projectile.el")

(load-file "~/.emacs.d/lisp/myemacs/org.el")

(load-file "~/.emacs.d/lisp/myemacs/ide-features.el")

(load-file "~/.emacs.d/lisp/myemacs/tree-sitter.el")

(load-file "~/.emacs.d/lisp/myemacs/langs.el")

(load-file "~/.emacs.d/lisp/myemacs/misc.el")

(load-file "~/.emacs.d/lisp/myemacs/magit.el")

(load-file "~/.emacs.d/lisp/myemacs/electric.el")

(load-file "~/.emacs.d/lisp/myemacs/eshell.el")
