(let ((default-directory user-emacs-directory)
      ;; (file-name-handler-alist nil)
      (gc-cons-percentage .9)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  (require 'package)

  (setq package-enable-at-startup nil
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")
                           ;; ("org" . "https://orgmode.org/elpa/")
                           ))
  (setq gnutls-algorithm-priority "normal:-vers-tls1.3")
  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq use-package-always-ensure nil)

  ;; For some reason shr is **SOMETIMES** being loaded when going through `org-babel-load-file'???
  ;; Sigh yet another thing I don't understand. Setting this here to avoid headaches
  ;; Update: Might be because of byte compiling the org doc. Had a similar issue. Investigate
  (setq shr-map
        (let ((map (make-sparse-keymap)))
          (define-key map "a" 'shr-show-alt-text)
          ;; (define-key map "i" 'shr-browse-image)
          (define-key map "z" 'shr-zoom-image)
          ;; (define-key map [?\t] 'shr-next-link)
          ;; (define-key map [?\M-\t] 'shr-previous-link)
          (define-key map [follow-link] 'mouse-face)
          (define-key map [mouse-2] 'shr-browse-url)
          (define-key map [C-down-mouse-1] 'shr-mouse-browse-url-new-window)
          ;; (define-key map "I" 'shr-insert-image)
          ;; (define-key map "w" 'shr-maybe-probe-and-copy-url) ;; Might keep this one
          ;; (define-key map "u" 'shr-maybe-probe-and-copy-url) ;; Might keep this one
          ;; (define-key map "v" 'shr-browse-url)
          ;; (define-key map "O" 'shr-save-contents)
          (define-key map (kbd "RET") 'shr-browse-url)
          map))

  ;; Have to set it up here, otherwise it won't take effect in org mode doc
  (setq inhibit-startup-echo-area-message "gopar")
  (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
  (garbage-collect))
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
