(use-package projectile
  :ensure
  :load t
  :commands projectile-project-root
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :custom
  ;; (projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (projectile-ignored-projects '("~/.emacs.d/") "Never acknowledge these projects")
  (projectile-indexing-method 'hybrid)  ;; Not sure if this still needed?

  :config
  (projectile-global-mode))
  
(provide 'projectile)
