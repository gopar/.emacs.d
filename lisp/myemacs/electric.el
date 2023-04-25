(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed))

(provide 'electric)
