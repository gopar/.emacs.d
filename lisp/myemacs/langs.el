;; List of all languages currently in use
(use-package vue-mode
  :ensure
  :defer
  :mode "\\.vue\\'")

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :bind (:map ledger-mode-map
              ("C-c C-n" . ledger-navigate-next-uncleared)
              ("C-c C-b" . ledger-navigate-previous-uncleared))
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-report-use-native-highlighting nil)
  (ledger-accounts-file (expand-file-name "~/personal/finances/data/accounts.dat"))
  )

(use-package yaml-mode
  :ensure t)

(provide 'langs)
