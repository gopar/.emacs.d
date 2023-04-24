(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ))
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(package-initialize)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Path to nano emacs modules (mandatory)
;; (add-to-list 'load-path "/Users/gopar/personal/nano-emacs")

;; ;; Default layout (optional)
;; (require 'nano-layout)

;; ;; Theming Command line options (this will cancel warning messages)
;; (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


;; ;; Customize support for 'emacs -q' (Optional)
;; ;; You can enable customizations by creating the nano-custom.el file
;; ;; with e.g. `touch nano-custom.el` in the folder containing this file.
;; (let* ((this-file  (or load-file-name (buffer-file-name)))
;;        (this-dir  (file-name-directory this-file))
;;        (custom-path  (concat this-dir "nano-custom.el")))
;;   (when (and (eq nil user-init-file)
;;              (eq nil custom-file)
;;              (file-exists-p custom-path))
;;     (setq user-init-file this-file)
;;     (setq custom-file custom-path)
;;     (load custom-file)))

;; ;; Theme
;; (require 'nano-faces)
;; (require 'nano-theme)
;; (require 'nano-theme-dark)
;; (require 'nano-theme-light)

;; (nano-theme-set-dark)
;; (call-interactively 'nano-refresh-theme)

;; ;; Nano default settings (optional)
;; (require 'nano-defaults)

;; ;; Nano session saving (optional)
;; (require 'nano-session)

;; ;; Nano header & mode lines (optional)
;; (require 'nano-modeline)

;; ;; Nano key bindings modification (optional)
;; (require 'nano-bindings)

;; ;; Compact layout (need to be loaded after nano-modeline)
;; (when (member "-compact" command-line-args)
;;   (require 'nano-compact))

;; ;; Nano counsel configuration (optional)
;; ;; Needs "counsel" package to be installed (M-x: package-install)
;; ;; (require 'nano-counsel)

;; ;; Welcome message (optional)
;; (let ((inhibit-message t))
;;   (message "Welcome to GNU Emacs / N Λ N O edition")
;;   (message (format "Initialization time: %s" (emacs-init-time))))

;; ;; Splash (optional)
;; (unless (member "-no-splash" command-line-args)
;;   (require 'nano-splash))

;; ;; Help (optional)
;; (unless (member "-no-help" command-line-args)
;;   (require 'nano-help))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure nil)

;; Have to set it up here, otherwise it won't take effect in org mode doc
(setq inhibit-startup-echo-area-message "gopar")
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
