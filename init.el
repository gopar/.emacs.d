(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
;; (setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(package-initialize)

;; Have to set it up here, otherwise it won't take effect in org mode doc
(setq inhibit-startup-echo-area-message user-login-name)
(setq use-package-always-ensure nil)

;; (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(load (locate-user-emacs-file "README.el"))

;; Mac only
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; Make it slightly transparent
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
