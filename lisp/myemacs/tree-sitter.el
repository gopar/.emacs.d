(use-package tree-sitter
  :ensure nil
  :hook ((prog-mode . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(provide 'tree-sitter)
