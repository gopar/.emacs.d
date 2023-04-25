(use-package rainbow-mode
  :ensure t
  ;; :diminish
  :hook (prog-mode . rainbow-mode))

(use-package alert
  :ensure t
  :custom
  (alert-default-style 'message)
  (alert-fade-time 5))

(use-package which-key
  ;; :diminish
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 3))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package corral
  :ensure t
  :bind (("M-9" . corral-parentheses-backward)
         ("M-0" . corral-parentheses-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward)
         ("M-\"" . corral-single-quotes-backward)
         ("M-'" . corral-single-quotes-forward)))

;; Works with themes except with nano? 
(use-package highlight-indentation
  :ensure t
  :diminish
  :hook ((prog-mode . highlight-indentation-mode)
         ;; (prog-mode . highlight-indentation-current-column-mode)
         ))

(use-package move-text
  :ensure t
  :init (move-text-default-bindings))

(use-package iedit
  :ensure t
  :bind (("C-c o" . gopar/iedit-dwim))
  :init
  (defun gopar/iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope.
With ARG, revert back to normal iedit."
    (interactive "P")
    (require 'iedit)
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))

(use-package expand-region
  :ensure t
  :bind (("C-\\" . er/expand-region)))

;; window management
(use-package windmove
  :ensure nil
  :init
  (windmove-default-keybindings))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(provide 'misc)

