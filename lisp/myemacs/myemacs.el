(use-package emacs
  :ensure nil
  :defer
  :hook (after-init . pending-delete-mode)
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

(defun font-exists-p (font-name)
  (when (member font-name (font-family-list))
    t))

(defun num-of-monitors ()
  "Return the number of monitors the computer is currently connected to."
  (length (display-monitor-attributes-list)))

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

(provide 'myemacs)
