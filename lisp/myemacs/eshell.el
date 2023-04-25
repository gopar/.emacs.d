(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-eshell)
  (setq venv-location '("~/work/color/local/virtualenv3/"
                        "~/personal/books"
                        "~/personal/arcade-traxx/traxx"
                        "~/work/fiagents/env/"
                        "~/personal/positron/venv/"
                        )))

(provide 'eshell)
