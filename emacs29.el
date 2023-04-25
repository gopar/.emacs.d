;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/Users/gopar/personal/nano-emacs")

;; Default layout (optional)
(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
;; (let* ((this-file  (or load-file-name (buffer-file-name)))
;;        (this-dir  (file-name-directory this-file))
;;        (custom-path  (concat this-dir "nano-custom.el")))
;;   (when (and (eq nil user-init-file)
;;              (eq nil custom-file)
;;              (file-exists-p custom-path))
;;     (setq user-init-file this-file)
;;     (setq custom-file custom-path)
;;     (load custom-file)))
(when (file-exists-p (concat user-emacs-directory "nano-custom.el"))
  (load (concat user-emacs-directory "nano-custom.el")))

;; Theme
(setq nano-font-size 15)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)

(nano-theme-set-dark)
(call-interactively 'nano-refresh-theme)

;; Nano default settings (optional)
(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))
