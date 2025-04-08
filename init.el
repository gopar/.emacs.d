(require 'package)
(setq use-package-verbose t)
(setq use-package-always-ensure nil)

(use-package gcmh
  :ensure t
  :demand t)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Have to set it up here, otherwise it won't take effect in org mode doc
(setq inhibit-startup-echo-area-message user-login-name)

(load (locate-user-emacs-file "README.el"))

;; Mac only
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
