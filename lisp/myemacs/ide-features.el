(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package dabbrev
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-all-buffers nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  )
 
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-on-exact-match 'insert) ;; Insert when there's only one match
  (corfu-quit-no-match t)        ;; Quit when ther is no match
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary

  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-min-length 2)
  (setq cape-dabbrev-check-other-buffers 'some)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :bind ("C-c SPC" . cape-dabbrev)
)

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :after consult
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure
  :after projectile
  :bind (("C-s" . gopar/consult-line)
  ("C-c M-x" . consult-mode-command)
  ("C-x b" . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("M-y" . consult-yank-pop)
  ;; M-g bindings (goto-map)
  ("M-g M-g" . consult-goto-line)
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  :map minibuffer-local-map
  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ("M-r" . consult-history)
  :map projectile-command-map
  ("b" . consult-project-buffer)
  )

  :init
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<")
  (setq consult-line-start-from-top nil)

  (defun gopar/consult-line (&optional arg)
    "Start consult search with selected region if any.
If used with a prefix, it will search all buffers as well."
    (interactive "p")
    (let ((cmd (if current-prefix-arg '(lambda (arg) (consult-line-multi t arg)) 'consult-line)))
      (if (use-region-p)
        (let ((regionp (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (funcall cmd regionp))
      (funcall cmd "")))))

(use-package consult-ag
  :ensure
  :bind (:map projectile-command-map
              ("s s" . consult-ag)))

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ag)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers nil)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-.")))

(use-package marginalia
  :ensure
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package code-compass
  :ensure
  :config
  (code-compass-toggle-display-contributors))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'ag)
  (dumb-jump-force-searcher 'ag)
  (dumb-jump-selector 'completing-read)
  (dumb-jump-default-project "~/work")
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . subword-mode)
         ;; (prog-mode . which-function-mode) ;; TODO: Make it work with nano
         (prog-mode . (lambda () (setq-local fill-column 120)))))

(provide 'ide-features)

