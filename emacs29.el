;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/Users/gopar/personal/nano-emacs")

;; Default layout (optional)
(require 'nano-layout)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(setq use-package-always-ensure nil)

;; Recommended to have this at the top
(setq load-prefer-newer t)
(setq gc-cons-percentage .6)

(use-package no-littering
  :ensure t)

;; Customize default emacs
(use-package emacs
  :ensure nil
  :defer
  :hook ((after-init . pending-delete-mode)
         (after-init . (lambda () (load-file custom-file)))
         ;; (after-init . toggle-frame-maximized)
         )
  :custom
  ;; flash the frame to represent a bell.
  (visible-bell t)
  ;; Sentences end with 1 space not 2
  (sentence-end-double-space nil)
  ;; make cursor the width of the character it is under
  ;; i.e. full width of a TAB
  (x-stretch-cursor t)
  ;; Stop cursor from going into minibuffer prompt text
  (minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (history-delete-duplicates t)
  ;; Completion stuff for consult
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (custom-file "~/.emacs.d/nano-ignoreme.el")

  :config
  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none)
    )
  (setq-default c-basic-offset 4
                c-default-style "linux"
                indent-tabs-mode nil
                fill-column 120
                tab-width 4)
  (prefer-coding-system 'utf-8)
  ;; Uppercase is same as lowercase
  (define-coding-system-alias 'UTF-8 'utf-8)
  ;; Enable some commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  ;; C-x n <key> useful stuff
  (put 'narrow-to-region 'disabled nil)

  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-x C-k RET" . nil)
         ("RET" . newline-and-indent)
         ("C-j" . newline)
         ("M-\\" . cycle-spacing)
         ("C-x \\" . align-regexp)
         ("C-x C-b" . ibuffer)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)
         ("C-S-k" . gopar/delete-line-backward)
         ("C-k" . gopar/delete-line)
         ("M-d" . gopar/delete-word)
         ("<M-backspace>" . gopar/backward-delete-word)
         ("M-e" . gopar/next-sentence)
         ("M-a" . gopar/last-sentence)
         (";" . gopar/easy-underscore)
         ("C-x k" . (lambda () (interactive) (kill-buffer)))
         ("C-x C-k" . (lambda () (interactive) (bury-buffer))))

  :init
  (defun gopar/easy-underscore (arg)
    "Convert all inputs of semicolon to an underscore.
If given ARG, then it will insert an acutal semicolon."
    (interactive "P")
    (if arg
        (insert ";")
      (insert "_")))

  (defun easy-camelcase (arg)
    (interactive "c")
    ;; arg is between a-z
    (cond ((and (>= arg 97) (<= arg 122))
           (insert (capitalize (char-to-string arg))))
          ;; If it's a new line
          ((= arg 13)
           (newline-and-indent))
          ((= arg 59)
           (insert ";"))
          ;; We probably meant a key command, so lets execute that
          (t (call-interactively
              (lookup-key (current-global-map) (char-to-string arg))))))

  (defun sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (completing-read "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  ;; Stolen from https://emacs.stackexchange.com/a/13096/8964
  (defun gopar/reload-dir-locals-for-current-buffer ()
    "Reload dir locals for the current buffer"
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))

  (defun gopar/delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (delete-region
     (point)
     (progn
       (forward-word arg)
       (point))))

  (defun gopar/backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (gopar/delete-word (- arg)))

  (defun gopar/delete-line ()
    "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
    (interactive)
    (delete-region
     (point)
     (progn (end-of-line 1) (point)))
    (delete-char 1))

  (defadvice gopar/delete-line (before kill-line-autoreindent activate)
    "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
    (when (and (eolp) (not (bolp)))
      (save-excursion
        (forward-char 1)
        (just-one-space 1))))

  (defun gopar/delete-line-backward ()
    "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
    (interactive)
    (let (p1 p2)
      (setq p1 (point))
      (beginning-of-line 1)
      (setq p2 (point))
      (delete-region p1 p2)))

  (defun gopar/next-sentence ()
    "Move point forward to the next sentence.
Start by moving to the next period, question mark or exclamation.
If this punctuation is followed by one or more whitespace
characters followed by a capital letter, or a '\', stop there. If
not, assume we're at an abbreviation of some sort and move to the
next potential sentence end"
    (interactive)
    (re-search-forward "[.?!]")
    (if (looking-at "[    \n]+[A-Z]\\|\\\\")
        nil
      (gopar/next-sentence)))

  (defun gopar/last-sentence ()
    "Does the same as 'gopar/next-sentence' except it goes in reverse"
    (interactive)
    (re-search-backward "[.?!][   \n]+[A-Z]\\|\\.\\\\" nil t)
    (forward-char))

  (defvar gopar-ansi-escape-re
    (rx (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ . ?~))))

  (defun gopar/nuke-ansi-escapes (beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gopar-ansi-escape-re end t)
        (replace-match "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar gopar-pair-programming nil)
(defun gopar/pair-programming ()
  "Poor mans minor mode for setting up things that i like to make pair programmikng easier."
  (interactive)
  (if gopar-pair-programming
      (progn
        ;; dont display line numbers
        (global-display-line-numbers-mode -1)
        ;; disable all themes change to a friendlier theme
        (mapcar 'disable-theme custom-enabled-themes)
        ;; (load-theme 'tao-yin)
        (setq gopar-pair-programming nil))

    (progn
      ;; display line numbers
      (global-display-line-numbers-mode)
      ;; disable all themes change to a friendlier theme
      (mapcar 'disable-theme custom-enabled-themes)
      (load-theme 'manoj-dark)
      (setq gopar-pair-programming t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure
  :load t
  :commands projectile-project-root
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :custom
  ;; (projectile-ignored-projects '("~/.emacs.d/") "Never acknowledge these projects")
  (projectile-indexing-method 'hybrid)  ;; Not sure if this still needed?
  (projectile-per-project-compilation-buffer t)
  :config
  (projectile-global-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/myemacs/org.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(use-package vertico-multiform
  :ensure nil
  :hook (after-init . vertico-multiform-mode)
  :init
  (setq vertico-multiform-commands
      '((consult-line (:not posframe))
        (gopar/consult-line (:not posframe))
        (consult-ag (:not posframe))
        (t posframe))))

;; just for looks
(use-package vertico-posframe
  :ensure t)

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
  (setq corfu-exclude-modes '(eshell-mode))
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
  ("b" . consult-project-buffer))

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
         ;; (prog-mode . which-function-mode) ;; TODO: Make it work with nano --> which-func-format
         (prog-mode . (lambda () (setq-local fill-column 120)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tree-sitter-langs
  :ensure t)

(use-package tree-sitter
  :ensure t
  :hook ((prog-mode . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of all languages currently in use
(use-package vue-mode
  :ensure
  :defer
  :bind (:map vue-mode-map
              (";" . easy-camelcase))
  :mode "\\.vue\\'")

(use-package typescript-mode
  :ensure t
  :bind (:map typescript-mode-map
              (";" . easy-camelcase))
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
  (ledger-accounts-file (expand-file-name "~/personal/finances/data/accounts.dat")))

(use-package yaml-mode
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package alert
  :ensure t
  :custom
  (alert-default-style 'message)
  (alert-fade-time 5))

(use-package which-key
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
  :hook ((minibuffer-setup . (lambda () (interactive) (windmove-mode -1)))
         (minibuffer-exit . windmove-mode))
  :init
  (windmove-default-keybindings))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(defun gopar/add-env-vars ()
  "Setup environment variables that I will need."
  (load-file "~/.emacs.d/etc/eshell/set_env.el")
  (setq-default eshell-path-env (getenv "PATH"))

  (setq exec-path (append exec-path
                          `("/usr/local/bin"
                            "/usr/bin"
                            "/usr/sbin"
                            "/sbin"
                            "/bin")
                          (split-string (getenv "PATH") ":"))))
(add-hook 'after-init-hook 'gopar/add-env-vars)

(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :defer
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Ibuffer Icons sets it's own local buffer format and overrides the =ibuffer-formats= variable.
;; So in order for ibuffer-vc to work, i have to include it in the icons-buffer format -_-
(use-package all-the-icons-ibuffer
  :ensure t
  :custom
  (all-the-icons-ibuffer-formats
        `((mark modified read-only locked vc-status-mini
          ;; Here you may adjust by replacing :right with :center or :left
          ;; According to taste, if you want the icon further from the name
          " " ,(if all-the-icons-ibuffer-icon
                   '(icon 2 2 :left :elide)
                 "")
          ,(if all-the-icons-ibuffer-icon
               (propertize " " 'display `(space :align-to 8))
             "")
          (name 18 18 :left :elide)
          " " (size-h 9 -1 :right)
          " " (mode+ 16 16 :left :elide)
          " " (vc-status 16 16 :left)
          " " vc-relative-file)
    (mark " " (name 16 -1) " " filename)))

  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; Quick recap of what =vc-status-mini=
;; https://github.com/purcell/ibuffer-vc/blob/master/ibuffer-vc.el#L204
(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer . (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-vc-status)
        ;; (ibuffer-do-sort-by-alphabetic)
        )
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-eshell)
  (setq venv-location '("~/personal/arcade-traxx/traxx"
                        "~/work/fiagents/env/")))

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("C-c C-p" . nil)
              ("C-c C-z" . run-python))
  :hook (python-mode . (lambda ()
                         (setq forward-sexp-function nil
                               python-forward-sexp-function nil)
                         (make-local-variable 'python-shell-virtualenv-root)
                         (setq completion-at-point '(cape-file cape-dabbrev python-completion-at-point))))
  :custom
  (python-shell-interpreter "python")
  (python-shell-interpreter-args "")
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))

(use-package eshell-git-prompt
  :after eshell
  :ensure t)

(use-package powerline-with-venv
  :ensure nil
  :after eshell-git-prompt
  :load-path "lisp/themes/powerline-with-venv"
  :config
  (add-to-list 'eshell-git-prompt-themes
               '(powerline-plus eshell-git-prompt-powerline-venv eshell-git-prompt-powerline-regexp))
  (eshell-git-prompt-use-theme 'powerline-plus))

(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell
  :ensure nil
  :hook (eshell-directory-change . gopar/sync-dir-in-buffer-name)
  :bind ("C-c >" . gopar/eshell-redirect-to-buffer)
  :custom
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt nil)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-input-filter 'gopar/eshell-input-filter)
  (eshell-cd-on-directory t)
  (eshell-list-files-after-cd nil)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil)
  :init
  (defun gopar/eshell-input-filter (input)
    "Do not save empty lines, commands that start with a space or 'l'/'ls'"
    (and
     (not (string-prefix-p "ls" input))
     (not (string= "l" input))
     (eshell-input-filter-default input)
     (eshell-input-filter-initial-space input)))

  (defun eshell/ff (&rest args)
    "Open files in emacs.
Stolen form aweshell"
    (if (null args)
        ;; If I just ran "emacs", I probably expect to be launching
        ;; Emacs, which is rather silly since I'm already in Emacs.
        ;; So just pretend to do what I ask.
        (bury-buffer)
      ;; We have to expand the file names or else naming a directory in an
      ;; argument causes later arguments to be looked for in that directory,
      ;; not the starting directory
      (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args)))))
    )

  (defun eshell/clear ()
    "Clear the eshell buffer.
This overrides the built in eshell/clear cmd in esh-mode."
    (interactive)
    (eshell/clear-scrollback))

  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell.
Similar to `cd =`"
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                   (completing-read "cd: " eshell-dirs)))))

  (defun eshell/jj ()
    "Jumpt to Root."
    (eshell/cd (projectile-project-root)))

  (defun eshell/cat (filename)
    "Like cat(1) but with syntax highlighting.
Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))

  (defun gopar/sync-dir-in-buffer-name ()
    "Update eshell buffer to show directory path.
Stolen from aweshell."
    (let* ((root (projectile-project-root))
           (root-name (projectile-project-name root)))
      (if root-name
          (rename-buffer (format "*eshell %s* %s" root-name (s-chop-prefix root default-directory)) t)
        (rename-buffer (format "*eshell %s*" default-directory) t))))

  (defun gopar/eshell-redirect-to-buffer (buffer)
    "Auto create command for redirecting to buffer."
    (interactive (list (read-buffer "Redirect to buffer: ")))
    (insert (format " >>> #<%s>" buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :hook (after-init . global-git-gutter-mode))


;; After adding or updating a snippet run:
;; =M-x yas-recompile-all=
;; =M-x yas-reload-all=
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (fundamental-mode . yas-minor-mode)
         (after-init . yas-reload-all))
  :bind (:map yas-minor-mode-map
              ("C-c C-SPC" . yas-insert-snippet)))

(use-package boolcase
  :load-path "lisp/modes/boolcase"
  :hook (python-mode . boolcase-mode))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     ;; (agenda . 5)
                     ))
   :config
  (dashboard-setup-startup-hook))

(use-package display-fill-column-indicator
  :ensure nil
  :hook (python-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alhoF --group-directories-first"))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package files
  :ensure nil
  :custom
  (insert-directory-program "gls") ; Will not work if system does not have GNU gls installed
   ;; Don't have backup
  (backup-inhibited t)
  ;; Don't save anything.
  (auto-save-default nil)
  ;; If file doesn't end with a newline on save, automatically add one.
  (require-final-newline t)
  :config
  (add-to-list 'auto-mode-alist '("Pipfile" . conf-toml-mode)))

(use-package replace
  :ensure nil
  :bind (("C-c C-o" . gopar/occur-definitions)
         :map occur-mode-map
         ("RET" . gopar/jump-to-defintion-and-kill-all-other-windows)
         ("<C-return>" . occur-mode-goto-occurrence))
  :init
  (defun gopar/occur-definitions ()
    "Show all the function/method/class definitions for the current language."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (occur "\(defun"))
     ((eq major-mode 'python-mode)
      (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
     ;; If no matching, then just do regular occur
     (t (call-interactively 'occur)))

    ;; Lets switch to that new occur buffer
    (let ((window (get-buffer-window "*Occur*")))
      (if window
          (select-window window)
        (switch-to-buffer "*Occur*"))))

  (defun gopar/jump-to-defintion-and-kill-all-other-windows ()
    (interactive)
    (occur-mode-goto-occurrence)
    (kill-buffer "*Occur*")
    (delete-other-windows)))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t)
  (compilation-buffer-name-function 'gopar/compilation-buffer-name-function)
  :hook (compilation-mode . hl-line-mode)
  :bind (:map compilation-mode-map
              ("y" . gopar/send-self)
              ("n" . gopar/send-self)
              ("RET" . gopar/send-self) ;; maybe M-RET?
              ("C-d" . gopar/send-self))
  :init
  (defun gopar/compilation-buffer-name-function (arg)
    "Rename buffer to whatever command was used.
eg. *python main.py*"
    (concat "*" compile-command "*"
            (if (projectile-project-p) (concat "<" (projectile-project-name) ">") ""))))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . gopar/colorize-compilation-buffer)
  :init
  (defun gopar/compilation-nuke-ansi-escapes ()
    (toggle-read-only)
    (gopar/nuke-ansi-escapes (point-min) (point-max))
    (toggle-read-only))

  ;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
  (defun gopar/colorize-compilation-buffer ()
    "Colorize the output from compile buffer"
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(use-package winner-mode
  :ensure nil
  :hook after-init
  :commands (winner-undo winnner-redo))

(use-package js-mode
  :bind (:map js-mode-map
              (";" . easy-camelcase)

              :map js-jsx-mode-map
              (";" . easy-camelcase))
  :custom
  (js-indent-level 2)
  (js-jsx-indent-level 2)
  :hook (js-mode . (lambda ()
                     (define-key js-mode-map (kbd ";") 'easy-camelcase)
                     (define-key js-jsx-mode-map (kbd ";") 'easy-camelcase))))

(use-package pulse
  :ensure nil
  :init
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))

(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "aspell")
  (ispell-personal-dictionary "~/.aspell.lang.pws")
  (ispell-dictionary nil)
  (ispell-local-dictionary nil)
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"
                       ;; "--run-together" "--run-together-limit=16"
                       "--camel-case")))

(use-package flyspell
  :ensure nil
  :hook ((prog-mode . flyspell-prog-mode)
         (org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-," . flyspell-goto-next-error)
              ("C-." . flyspell-auto-correct-word)))

(use-package browse-url
  :ensure nil
  :init
  (defun gopar/eww-advice-filter-args (args)
    "When using a search engine, sometimes the redirects don't work.
To work around this, we'll grab the url that we're targetting so that it doesn't
go through the search engine"
    (let (url path-and-query query is-ddg is-google param)
      (setq url (car args))
      (setq is-ddg (string-prefix-p "https://duckduckgo.com/l/?uddg=" url))
      (setq is-google (string-prefix-p "https://www.google.com/url?" url))

      (when (or is-ddg is-google)
        (setq url (url-generic-parse-url url))
        (setq path-and-query (url-path-and-query url))
        (setq query (cdr path-and-query))
        (setq param (if is-ddg "uddg" "q"))
        (setq url (car (cdr (assoc-string param (url-parse-query-string query))))))

      `(,url ,@(cdr args))))
  :custom
  ;; Emacs can't find browser binaries
  (browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
  ;; Neat trick to open that route to different places
  (browse-url-firefox-new-window-is-tab t)
  :config
  (put 'browse-url-handlers 'safe-local-variable (lambda (x) t))
  (advice-add 'browse-url :filter-args 'gopar/eww-advice-filter-args))

;; It may also be wise to raise gc-cons-threshold while the minibuffer is active,
;; so the GC doesn't slow down expensive commands (or completion frameworks, like
;; helm and ivy. The following is taken from doom-emacs

(use-package minibuffer
  :ensure nil
  :hook ((minibuffer-setup . defer-garbage-collection-h)
         (minibuffer-exit . restore-garbage-collection-h))
  :custom
  (completion-styles '(initials partial-completion flex))
  :init
  (defun defer-garbage-collection-h ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun restore-garbage-collection-h ()
    ;; Defer it so that commands launched immediately after will enjoy the
    ;; benefits.
    (run-at-time
     1 nil (lambda () (setq gc-cons-threshold 1600000)))))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package autorevert
  :ensure nil
  :custom
  ;; auto refresh files when changed from disk
  (global-auto-revert-mode t))

;; Do I need this???
(use-package simple
  :ensure nil
  :hook ((makefile-mode . indent-tabs-mode)
         (fundamental-mode . delete-selection-mode)
         (fundamental-mode . auto-fill-mode)
         (org-mode . auto-fill-mode)))

(use-package neotree
  :ensure t
  :bind ("<f5>" . neotree-toggle
         )
  :custom
  (neo-theme 'icons)
  (neo-smart-open t)
  ;; takes too long to update on first try
  ;; (neo-vc-integration '(face char))
  (neo-show-hidden-files t))

(use-package dizzee
  :ensure t
  :config
  (dz-defservice bfd-runserver "python"
                 :args ("manage.py" "runserver")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-flower "flower"
                 :args ("-A" "core" "--host=127.0.0.1" "--port=9002")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-bot-run "python"
                 :args ("manage.py" "bot" "run")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-celery-downloader-queue "celery"
                 :args ("-A" "core" "worker" "-n" "Downloader" "-Q" "Downloader" "--concurrency=8" "--purge" "-l" "info")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-celery-slow-downloader-queue "celery"
                 :args ("-A" "core" "worker" "-n" "SlowDownloader" "-Q" "SlowDownloader" "--concurrency=2" "--purge" "-l" "info")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-celery-diffbot-queue "celery"
                 :args ("-A" "core" "worker" "-n" "Diffbot" "-Q" "Diffbot" "--concurrency=8" "--purge" "-l" "info")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice bfd-celery-launcher-queue "celery"
                 :args ("-A" "core" "worker" "-n" "Launcher" "-Q" "Launcher" "--concurrency=8" "--purge" "-l" "info")
                 :cd "/Users/gopar/work/fiagents/")
  (dz-defservice-group bfd-celerys-flower-and-server (bfd-celery-diffbot-queue
                                                      bfd-celery-downloader-queue
                                                      bfd-celery-slow-downloader-queue
                                                      bfd-celery-launcher-queue
                                                      bfd-flower
                                                      bfd-runserver)))

(use-package string-inflection
  :ensure t
  :commands string-inflection-insert
  :bind (("C-;" . gopar/string-inflection-cycle-auto))
  :init
  (defun gopar/string-inflection-cycle-auto ()
    "Switching by major mode."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))

     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))

     ((or (eq major-mode 'js-mode)
          (eq major-mode 'vue-mode)
          (eq major-mode 'java-mode)
          (eq major-mode 'typescript-mode))
      (string-inflection-java-style-cycle))

     ((eq major-mode 'nxml-mode)
      (string-inflection-java-style-cycle))

     ((eq major-mode 'hy-mode)
      (string-inflection-kebab-case))

     (t
      (string-inflection-ruby-style-cycle)))))

(use-package boolcase
  :load-path "lisp/modes/boolcase"
  :hook (python-mode . boolcase-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of My Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Theme
(setq nano-font-size 15)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)

(nano-theme-set-dark)
(call-interactively 'nano-refresh-theme)

;; ;; Nano default settings (optional)
(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
;; (require 'nano-bindings)

;; Nano custom mode writier-mode
(require 'nano-writer)
