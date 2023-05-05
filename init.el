(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ))
;; (setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure nil)

(if (string= emacs-version "29.0.90")
    ;; (load-file (concat user-emacs-directory "emacs29.el"))
    (org-babel-load-file (expand-file-name "~/.emacs.d/emacs29.org"))
  ;; Do the normal thing
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
  )
