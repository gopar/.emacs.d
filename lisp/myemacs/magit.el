(use-package magit
  :ensure t
  :commands magit-get-current-branch
  :demand t
  :bind ("C-x g" . magit)
  :hook (magit-mode . magit-wip-mode)
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-process-finish-apply-ansi-colors t)

  (defun magit/undo-last-commit (number-of-commits)
    "Undoes the latest commit or commits without loosing changes"
    (interactive "P")
    (let ((num (if (numberp number-of-commits)
                   number-of-commits
                 1)))
      (magit-reset-soft (format "HEAD^%d" num)))))

(use-package magit-todos
  :ensure
  :defer
  :hook (magit-mode . magit-todos-mode))

;; Part of magit
(use-package git-commit
  :after magit
  :hook (git-commit-setup . gopar/auto-insert-jira-ticket-in-commit-msg)
  :init
  (defun gopar/auto-insert-jira-ticket-in-commit-msg ()
    (let ((has-ticket-title (string-match "^[A-Z]+-[0-9]+" (magit-get-current-branch)))
          (has-ss-ticket (string-match "^[A-Za-Z]+/[A-Z]+-[0-9]+" (magit-get-current-branch)))
          (words (s-split-words (magit-get-current-branch))))
      (if has-ticket-title
          (insert (format "[%s-%s] " (car words) (car (cdr words)))))
      (if has-ss-ticket
          (insert (format "[%s-%s] " (nth 1 words) (nth 2 words)))))))

(use-package git-gutter
  :ensure t
  :diminish
  :hook (after-init . global-git-gutter-mode))

(provide 'magit)
