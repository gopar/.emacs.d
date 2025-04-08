(setopt use-package-verbose t
        use-package-always-ensure nil)

(use-package gcmh
  :ensure t
  :demand t)

(use-package package
  :ensure nil
  :defer t
  :commands (list-packages package-install)
  :custom
  (package-enable-at-startup nil)
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("org" . "https://orgmode.org/elpa/")))
  :config
  (package-initialize))

;; Have to set it up here, otherwise it won't take effect in org mode doc
(setopt inhibit-startup-echo-area-message user-login-name)

(load (locate-user-emacs-file "README.el"))

;; Mac only
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setopt ns-use-proxy-icon nil)
