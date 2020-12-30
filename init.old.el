(require 'package)
;; (toggle-debug-on-error)
(custom-set-variables
 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Add custom modes and then require them
(let ((default-directory "~/.emacs.d/modes/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'boolcase)

;; ;; When I'm on Mac
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
  ;; Fix Ispell
  ;; (setq ispell-program-name "aspell")
  )

;; Add my path to emacs
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speed up movement apparently : https://emacs.stackexchange.com/a/28746/8964
(setq auto-window-vscroll nil)
;; no startup msg
(setq inhibit-startup-message t)
;; No bell noise, just blinking
(setq visible-bell t)
;; Make emacs fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; UTF-8 All Things!
(prefer-coding-system 'utf-8)
;; Uppercase is same as lowercase
(define-coding-system-alias 'UTF-8 'utf-8)
;; Hide tool bar, never use it
(tool-bar-mode -1)
;; Hide menu bar, never use it either
(menu-bar-mode -1)
;; Change yes or no to y or n, cause im lazy
(fset 'yes-or-no-p 'y-or-n-p)
;; auto refresh files when changed from disk
(global-auto-revert-mode t)
;; Show column number
(column-number-mode t)
;; Don't have backups, cause YOLO
(setq backup-inhibited t)
;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; keyboard scroll one line at a time
(setq scroll-step 1)
;; Don't suspend emacs, so ANNOYING
(global-unset-key (kbd "C-z"))
;; for all langs
(define-key global-map (kbd "C-c o") 'iedit-mode)
;; this saves typing
(define-key global-map (kbd "RET") 'newline-and-indent)
;; sometimes need this
(define-key global-map (kbd "C-j") 'newline)
;; For spacing problems
(global-set-key (kbd "M-\\") 'cycle-spacing)
;; Align lines to a symbol
(global-set-key (kbd "C-x \\") 'align-regexp)
;; Use ibuffer instead of default
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Global electric mode, for matching closing parens, ect.
(electric-pair-mode)
;; echo to screen faster, cause why not
(setq echo-keystrokes 0.1)
;; Clean whitespace on save, pretty freken awesome
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Aparently makefiles needs tabs, Booooooo
(add-hook 'makefile-mode 'indent-tabs-mode)
;; For super long lines
(global-so-long-mode 1)
;; Stop cursor from going into minibuffer prompt text
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt
                        face minibuffer-prompt)))

;; Exactly what it sounds for
(show-paren-mode t)

;; Delete selected text
(add-hook 'fundamental-mode 'delete-selection-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-indentation-mode +1)
            (flyspell-prog-mode)
            (compact-docstrings-mode +1)
            (subword-mode +1)
            (when (not (eq major-mode 'emacs-lisp-mode))
              (electric-operator-mode +1))
            ))

(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default indent-tabs-mode nil)
;; Default tab display is 4 spaces
(setq-default tab-width 4)
;; Don't save anything.
(setq auto-save-default nil)
;; show the function I'm in
;; (which-function-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
;; C-x n <key> useful stuff
(put 'narrow-to-region 'disabled nil)
;; Sentences end with 1 space not 2. DIS IS 'MERIKA
(setq sentence-end-double-space nil)
;; If file doesn't end with a newline on save, automatically add one.
(setq require-final-newline t)

;; Works pretty good w/ some themes. When it doesn't, the line is barley
;; visible but I can live with that.
(global-hl-line-mode)
;; show all todos
(global-hl-todo-mode)

;; Strongly dislike this keybinding
(global-unset-key (kbd "C-x C-k RET"))

;; Load newer bytecode over older
(setq load-prefer-newer t)

(put 'browse-url-browser-function 'safe-local-variable (lambda (x) t))
(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

;; Smoother scrolling. Stolen from
;; https://www.reddit.com/r/emacs/comments/fwmqc8/how_to_stop_emacs_from_half_scrolling_from_bottom/fmpc2k1
(setq scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)

(global-set-key (kbd "C-x k") '(lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "C-x C-k") '(lambda () (interactive) (bury-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(require 'iedit)


(defun easy-camelcase (arg)
  (interactive "c")
  ;; arg is between a-z
  (cond ((and (>= arg 97) (<= arg 122))
         (insert (capitalize (char-to-string arg))))
        ;; If it's a new line
        ((= arg 13)
         (insert 10))
        ;; We probably meant a key command, so lets execute that
        (t (call-interactively
            (lookup-key (current-global-map) (char-to-string arg))))))

(defun easy-underscore (arg)
  "Convert all inputs of semicolon to an underscore.
If given ARG, then it will insert an acutal semicolon."
  (interactive "P")
  (if arg
      (insert ";")
    (insert "_")))

(defun font-exists-p (font-name)
  (when (member font-name (font-family-list))
    t))

(defun num-of-monitors ()
  "Return the number of monitors the computer is currently connected to."
  (length (display-monitor-attributes-list)))


;; (require 's)
;; (defun remove-google-sdk-path ()
;;   (interactive)
;;   (let ((path-var (getenv "path")))
;;     (setq path-var (s-replace "/users/gopar/downloads/google-cloud-sdk/bin:" "" path-var))
;;     )
;;   )

;; (defun add-google-sdk-path ()
;;   (interactive)
;;   (let ((path-var (getenv "path")))
;;     (setq path-var (concat path-var ":/users/gopar/downloads/google-cloud-sdk/bin"))
;;     )
;;   )

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

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

;; (require 'iedit)

;; (defun iedit-dwim (arg)
;;   "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;;   (interactive "P")
;;   (if arg
;;       (iedit-mode)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         ;; this function determines the scope of `iedit-start'.
;;         (if iedit-mode
;;             (iedit-done)
;;           ;; `current-word' can of course be replaced by other
;;           ;; functions.
;;           (narrow-to-defun)
;;           (iedit-start (current-word) (point-min) (point-max)))))))

(defun my-next-sentence ()
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
    (my-next-sentence)))

(defun my-last-sentence ()
  "Does the same as 'my-next-sentence' except it goes in reverse"
  (interactive)
  (re-search-backward "[.?!][   \n]+[A-Z]\\|\\.\\\\" nil t)
  (forward-char))

;; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ;; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "M-e") 'my-next-sentence)
(global-set-key (kbd "M-a") 'my-last-sentence)

(define-key minibuffer-inactive-mode-map (kbd ";") 'easy-underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup proxy usage


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy
(require 'avy)
(global-set-key (kbd "M-g c") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prettify-symbols
;; (push '(">=" . ?≥) prettify-symbols-alist)
;; (push '("<=" . ?≤) prettify-symbols-alist)
;; (push '("->" . ?→) prettify-symbols-alist)

;; (global-prettify-symbols-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
(require 'org)
(require 'org-agenda)


;; https://stackoverflow.com/a/10091330/2178312
(defun zin/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))


;; Make sure we see syntax highlighting
(setq org-src-fontify-natively t
      ;; I dont use it for subs/super scripts
      org-use-sub-superscripts nil
      org-src-preserve-indentation nil
      ;; Don't ask if we already have an open Edit buffer
      org-src-ask-before-returning-to-edit-buffer nil
      ;; Where the org files live
      org-directory "~/.emacs.d/org/"
      ;; Where should notes go to?
      org-default-notes-file (concat org-directory "notes.org")
      ;; Should everything be hidden?
      org-startup-folded nil
      ;; If idle for more than 15 mins, resolve by asking what to do with clock
      org-clock-idle-time 15
      ;; Show more clocking history
      org-clock-history-length 10
      ;; Include running time in clock reports
      org-clock-report-include-clocking-task t
      org-clock-into-drawer "CLOCKING"
      ;; Setup default clocktable summary
      org-clock-clocktable-default-properties '(:maxlevel 2 :scope file :formula % :properties ("Effort" "Points") :sort (5 . ?t) :compact t :block today)
      ;; Show as utf-8 chars
      org-pretty-entities t
      ;; global Effort estimate values
      org-global-properties '(("EFFORT_ALL" .
                               ;; hotkeys when doing C-c C-x e
                               ;; 1   2    3    etc  etc
                               "0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 10:00")
                              ("POINTS_ALL" .
                               "1 2 3 5 8 13")
                              )
      ;; See down arrow instead of "..." when we have subtrees
      ;; org-ellipsis "⤵"
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm

      ;; Count all children TODO's not just direct ones
      org-hierarchical-todo-statistics nil
      ;; For agenda, only show work related stuff
      org-agenda-files (directory-files-recursively "~/.emacs.d/org/" "\.org$")
      org-edit-src-content-indentation 0
      org-refile-targets '((org-agenda-files :maxlevel .  2))
      ;; The right side of | indicates the DONE states
      org-todo-keywords '(
                          (sequence "CS-TODO(q)" "WAITING-ON-CUSTOMER(w" "|" "CS-DELEGATED(e!)" "CS-DONE{y@}")
                          (sequence "NEXT(n)" "IN-PROGRESS(i!)" "TODO(t)" "WAITING(w!)" "SOMEDAY(s!)" "PROJ(p)" "REPEAT(r)" "|" "DONE(d@)" "CANCELED(c@)" "DELEGATED(d@)")
                          )
      ;; Unchecked boxes will block switching the parent to DONE
      org-enforce-todo-checkbox-dependencies t
      ;; Don't allow TODO's to close without their dependencies done
      org-enforce-todo-dependencies t
      org-track-ordered-property-with-tag t
      org-agenda-dim-blocked-tasks t
      org-agenda-include-diary t
      ;; put timestamp when finished a todo
      org-log-done t
      ;; timestamp when we reschedule
      org-log-reschedule t
      ;; Indent the stars instead of piling them
      org-startup-indented t
      ;; catch invisible edit
      org-catch-invisible-edits 'error
      ;; dont create a bookmark when calling org-capture
      org-capture-bookmark nil
      ;; make TODO entries that cannot be marked as done b/c of unmarked children are shown in a dimmed font or even made invisible in agenda views
      org-agenda-dim-blocked-tasks t
      org-capture-templates '(
                              ("c" "Break" entry (file+headline "~/.emacs.d/org/captured-tasks.org"  "Tasks")
                               "* TODO %? :break:\n %U")
                              ("f" "Freetime" entry (file+headline "~/.emacs.d/org/captured-tasks.org"  "Tasks")
                               "* TODO %? :freetime:\n %U")
                              ;; ("w" "Task to do for work" entry (file+function "~/.emacs.d/org/work/weekly_agenda.org"  (lambda () (interactive) (goto-char (point-max)) (outline-back-to-heading)))
                              ;;  "* TODO %? :work:\n %U\n %^{EFFORT}p %^{POINTS}p")
                              )
      ;; shortcut for links [[SASB:1234][This points to SASB-1234 \o/]]
      org-link-abbrev-alist '(
                              ("SASB" . "https://datatheorem.atlassian.net/browse/SASB-%s")
                              ("POR" . "https://datatheorem.atlassian.net/browse/POR-%s")
                              ("CS" . "https://datatheorem.atlassian.net/browse/CS-%s")
                              ("CUSTSUP" . "https://datatheorem.atlassian.net/browse/CUSTSUP-%s")
                              ("API" . "https://datatheorem.atlassian.net/browse/API-%s")
                              ("OPS" . "https://datatheorem.atlassian.net/browse/OPS-%s")
                              )

      ;; For built in rss reading org-feed
      ;; org-feed-retrieve-method 'curl
      ;; Format for the feeds
      org-feed-default-template "\n* UNREAD %h\n  %U\n  %description\n  %a\n"
      ;; org-feed-file (concat org-directory "feed.org")
      ;; Only useful for property searching only but can slow down search :(
      org-use-property-inheritance t
      ;; Don't ask every time when I run a code block
      org-confirm-babel-evaluate nil
      ;; Save notes into log drawer
      org-log-into-drawer t
      ;; Org agenda
      org-agenda-custom-commands '(
                                   ("h" "Agenda and Home tasks"
                                    ((agenda "" ((org-agenda-skip-function '(zin/org-agenda-skip-tag "work"))))
                                     (tags-todo "freetime|break"))
                                    ((org-agenda-sorting-strategy '(priority-down))))
                                   ("w" "Agenda and Work tasks"
                                    ((agenda "" ((org-agenda-skip-function '(zin/org-agenda-skip-tag "work" 't))))
                                     (tags-todo "break"))
                                    ((org-agenda-sorting-strategy '(priority-down)))))
      )
(add-to-list 'org-agenda-files "~/Dropbox/Public/capture.org")


(require 'html2org)

(setq org-feed-alist
      '(
        ("r/orgmode" "https://www.reddit.com/r/orgmode/top.rss?t=week&limit=25" "~/.emacs.d/org/feed.org" "Org-Mode Entries"
         :parse-feed org-feed-parse-atom-feed :parse-entry org-feed-parse-atom-entry-gopar)

        ("r/Emacs" "https://www.reddit.com/r/emacs/top.rss?t=week&limit=25" "~/.emacs.d/org/feed.org" "Emacs Entries"
         :parse-feed org-feed-parse-atom-feed :parse-entry org-feed-parse-atom-entry-gopar)

        ("Howardism" "http://howardism.org/index.xml" "~/.emacs.d/org/feed.org" "Howardism")

        ("Life & Computing Science" "https://alhassy.github.io/rss.xml" "~/.emacs.d/org/feed.org" "Life & Computing Science")

        ("Remotive.io" "https://remotive.io/remote-jobs/software-dev/feed" "~/.emacs.d/org/feed.org" "Remotive.io")

        ;; ("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" "~/.emacs.d/org/feed.org" "slashdot")
        ("Pragmatic Emacs" "http://pragmaticemacs.com/feed/" "~/.emacs.d/org/feed.org" "Pragmatic Emacs Entries")
))


(defun org-feed-parse-atom-entry-gopar (entry)
  "Parse the `:item-full-text' as a sexp and create new properties."
  (let ((xml (car (read-from-string (plist-get entry :item-full-text)))))
    ;; Get first <link href='foo'/>.
    (setq entry (plist-put entry :link
                           (xml-get-attribute
                            (car (xml-get-children xml 'link))
                            'href)))
    ;; Add <title/> as :title.
    (setq entry (plist-put entry :title
                           (xml-substitute-special
                            (car (xml-node-children
                                  (car (xml-get-children xml 'title)))))))
    (let* ((content (car (xml-get-children xml 'content)))
           ;; (type (xml-get-attribute-or-nil content 'type))
           )
      (setq entry (plist-put entry :description
                                 (with-temp-buffer
                                   (insert (xml-substitute-special
                                  (car (xml-node-children content))))
                                   (html2org--shr (point-min) (point-max))))))
    entry))


;; (define-key global-map (kbd "C-c c") '(lambda () (interactive) (org-capture nil "b")))
(define-key global-map (kbd "C-c c") '(lambda () (interactive) (org-capture)))
(define-key global-map (kbd "C-c a") '(lambda () (interactive) (org-agenda)))
(define-key global-map (kbd "C-c j") '(lambda () (interactive) (org-clock-jump-to-current-clock)))
(define-key org-mode-map (kbd ";") 'easy-underscore)

(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook
          (lambda ()
            ;; (flyspell-mode t)
            (toggle-truncate-lines -1)
            (toggle-word-wrap 1)
            ;; (set (make-local-variable 'company-backends) '(company-dabbrev company-capf))
            (flycheck-mode -1)
            (add-pcomplete-to-capf)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sqlite . t)
   (python . t)
   (java . t)
   (emacs-lisp . t)
   (shell . t)
   (restclient . t)
   )
 )

;;;;;;;;;;;;;;;;;;;;
;; Copied from: https://emacs.stackexchange.com/a/43633/8964
;; For Running all TBLFM at once for a single table

(defun cmdbufmod-prepare (bufmod-list &optional start bound)
  "Prepare buffer for `cmdbufmod' with modifications BUFMOD-LIST.
See `cmdbufmod' for the format of BUFMOD-LIST.
If START is a buffer position the search for the regular expressions in BUFMOD-LIST
starts there. Otherwise it starts at `point-min'.
Optional BOUND limits the search.
Return the list of original text sections.
Each text section is a cons of an insertion marker and the old text
that needs to be restored there."
  (unless start (setq start (point-min)))
  (let (original-list)
    (save-excursion
      (dolist (bufmod bufmod-list)
    (let ((place (car bufmod))
          (newtext (cdr bufmod)))
      (goto-char start)
      (while (if (functionp place)
               (funcall place bound)
              (re-search-forward place bound t))
        (setq original-list
          (cons (cons (set-marker (make-marker) (match-beginning 0))
                  (match-string 0))
            original-list))
        (replace-match (propertize (if (functionp newtext)
                       (funcall newtext)
                     newtext)
                       'cmdbufmod t 'rear-nonsticky '(cmdbufmod)))))))
    original-list))

(defun cmdbufmod-cleanup (original-list)
  "Restore original text sections from ORIGINAL-LIST.
See the return value of `cmdbufmod-prepare' for the structure of ORIGINAL-LIST."
  (cl-loop for interval being the intervals property 'cmdbufmod
       if (get-text-property (car interval) 'cmdbufmod)
       do (delete-region (car interval) (cdr interval)))
  (cl-loop for original in original-list do
       (goto-char (car original))
       (insert (cdr original))))

(defun cmdbufmod (bufmod-list fun &rest args)
  "After applying BUFMOD-LIST to current buffer run FUN with ARGS like `apply'.
BUFMOD is a list of buffer modifications. Each buffer modification
is a cons \(PLACE . NEWTEXT).
PLACE can be a regular expression or a function.
If PLACE is a function it should search for the next place to be replaced
starting at point. It gets the search bound as an argument,
should set match-data like `re-search-forward',
and return non-nil if a match is found.
If PLACE is a regular expression it is treated like the function
\(lambda () (re-search-forward PLACE nil t))

NEWTEXT can be a replacement string or a function.
A function should return the string for `replace-match'."
  (let (original-list)
    (unwind-protect
        (progn
          (save-excursion
            (setq original-list (cmdbufmod-prepare bufmod-list)))
          (apply fun args))
      (save-excursion (cmdbufmod-cleanup original-list)))))

(defconst org-table-multi-tblfm-re "[[:space:]]*#\\+TBLFM:"
  "Regular expression identifying \"#+TBLFM:\" at the beginning of lines.
Don't include a leading carret here!")

(defun org-table-multi-tblfm-search (&optional bound)
  "Search for next \"#\\+TBLFM:\"-line that is preceded by another such line.
If BOUND is non-nil search stops there or at `point-max' otherwise.
The match-data is set to the match of \"[[:space:]]*\n[[:space:]]*#\\+TBLFM:[[:\" at the beginning of line."
  (interactive) ;; for testing
  (let ((re (concat "[[:space:]]*\n" org-table-multi-tblfm-re "[[:space:]]*"))
        found)
    (while (and (setq found (re-search-forward re bound t))
                (null
                 (save-excursion
                   (forward-line -1)
                   (looking-at-p org-table-multi-tblfm-re)))))
    found))

(defun org-table-multi-tblfm (oldfun &rest args)
  "Replace buffer local table formulas when calling OLDFUN with ARGS."
  (if (looking-at-p (concat "[[:space:]]*\n" org-table-multi-tblfm-re))
      (apply oldfun args)
    (cmdbufmod '((org-table-multi-tblfm-search . "::")) oldfun args)))

(advice-add 'org-table-recalculate :around #'org-table-multi-tblfm)
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-pomodoro
(global-set-key (kbd "<f12>") 'org-pomodoro)

(setq org-pomodoro-manual-break t
      org-pomodoro-short-break-length 20
      org-pomodoro-long-break-length 30
      org-pomodoro-length 60
      )


;; experimental
(defun gopar/save-window-config-and-show-work-agenda ()
    (interactive)
  (window-configuration-to-register ?`)
  (org-agenda nil "w")
  (delete-other-windows))

(defun gopar/load-window-config-and-close-work-agenda ()
  (interactive)
  (jump-to-register ?`))

;; Not sure if this will be called when overtime starts or will wait until overtime is over
(add-hook 'org-pomodoro-finished-hook
          'gopar/save-window-config-and-show-work-agenda)

(add-hook 'org-pomodoro-started-hook
          'gopar/load-window-config-and-close-work-agenda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; something i got ofreddit
;; https://www.reddit.com/r/emacs/comments/fosdi2/orgtableautoalignmode_minor_mode_for_auto/

(require 'subr-x)

(setq org-table-auto-align-in-progress nil)

(defun org-table-auto-align (begin end length)
  (save-match-data
    (unless (or org-table-auto-align-in-progress
                (not (org-at-table-p))
                (and (eq this-command 'org-self-insert-command)
                     (member (this-command-keys) '(" " "+" "|" "-"))))
      ;; uses zero-idle timer so the buffer content is settled after
      ;; the change, the cursor is moved, so we know what state we
      ;; have to restore after auto align
      (run-with-idle-timer
       0 nil
       (lambda ()
         (if (looking-back "| *\\([^|]+\\)")
             (let ((pos (string-trim-right (match-string 1))))
               (setq org-table-auto-align-in-progress t)
               (unwind-protect
                   (progn
                     (org-table-align)
                     (search-forward pos nil t))
                 (setq org-table-auto-align-in-progress nil)))))))))


(define-minor-mode org-table-auto-align-mode
  "A mode for aligning Org mode tables automatically as you type."
  :lighter " OrgTblAA"
  (if org-table-auto-align-mode
      (add-hook 'after-change-functions #'org-table-auto-align t t)
    (remove-hook 'after-change-functions #'org-table-auto-align t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(setq dired-listing-switches "-alh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move text
(move-text-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fill function stuff

(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell
(require 'flyspell)
;; ;; unbind flyspell key-binding
(define-key flyspell-mode-map (kbd "C-;") nil)
(setq flyspell-mode-line-string "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'string-inflection)

(global-set-key (kbd "C-;") 'my-string-inflection-cycle-auto)

(defun my-string-inflection-cycle-auto ()
  "Switching by major mode."
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for xml
   ((eq major-mode 'nxml-mode)
    (string-inflection-java-style-cycle))
   ;; for javascript
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpful
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(require 'markdown-mode)
(define-key markdown-mode-map (kbd ";") 'easy-underscore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compact-docstrings
;; Called in prog-mode-hook

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet
(require 'yasnippet)

(yas-global-mode +1)
(setq yas-snippet-dirs '("~/.emacs.d/mysnippets/"))
;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/mysnippets/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand-Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; Can overwrite characters that are highlighted. Thank God, for this
(pending-delete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp

(add-hook 'emacs-lisp-mode '(lambda ()
                              (enable-paredit-mode)
                              (electric-operator-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Corral for wrapping parens in non lisp modes
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-\"") 'corral-single-quotes-backward)
(global-set-key (kbd "M-'") 'corral-single-quotes-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation mode
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)

;; Make the compilation window automatically disappear - from enberg on #emacs
;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (and (null (string-match ".*exited abnormally.*" str)) (null (string-match ".*interrupt.*" str)))
;;             ;;no errors, make the compilation window go away in a few seconds
;;             (progn
;;               (run-at-time
;;                "2 sec" nil '(lambda ()
;;                               (let ((buffer (get-buffer-window "*compilation*")))
;;                                 (when buffer
;;                                   (quit-window nil buffer)))))
;;               (message "No Compilation Errors!")))))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Remove these dam files
;; (add-hook 'compilation-start-hook '(shell-command "find . -regex '\(.*__pycache__.*\|*.py[co]\)' -delete"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Occur mode
(define-key occur-mode-map (kbd "<C-return>")
  '(lambda () (interactive)
     (occur-mode-goto-occurrence)
     (kill-buffer "*Occur*")
     (delete-other-windows)))

(define-key occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)

(defun gopar-occur-definitions ()
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

(global-set-key (kbd "C-C C-o") 'gopar-occur-definitions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(setq vc-handled-backends nil)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Add an extra command where not to show mergers when logging
;; (magit-define-popup-switch 'magit-log-popup
;;   ?m "Omit merge commits" "--no-merge")

;; (magit-define-popup-option 'magit-commit-popup
;;   ?t "Insert file for commit msg" "--template=")


;; If the branch has a jira ticket, also add that on the commit message
(add-hook 'git-commit-setup-hook
          '(lambda ()
             (let ((has-ticket-title (string-match "^[A-Z]+-[0-9]+" (magit-get-current-branch)))
                   (words (s-split-words (magit-get-current-branch))))
               (if has-ticket-title
                   (insert (format "%s-%s " (car words) (car (cdr words))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra, pretty freken awesome
(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "window"
   ("j" windmove-left)
   ("k" windmove-down)
   ("i" windmove-up)
   ("l" windmove-right)
   ("b" balance-windows)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
    "ace")
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
    "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
    "horz")
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
    "swap")
   ("d" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
    "del")
   ("[" (lambda ()
          (interactive)
          (enlarge-window-horizontally 1)))
   ("]" (lambda ()
          (interactive)
          (shrink-window-horizontally 1)))
   ("{" (lambda ()
          (interactive)
          (enlarge-window 1)))
   ("}" (lambda ()
          (interactive)
          (enlarge-window -1)))
   ("o" delete-other-windows "1" :color blue)
   ("u" ace-maximize-window "a1" :color blue)
   ("q" nil "cancel")))

(global-set-key
 (kbd "C-M-l")
 (defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
(require 'helm-config)
(helm-mode)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action

;; Helm-fy everything! This package is awesome
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") #'helm-bookmarks)
(global-set-key (kbd "C-x m") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; For finding files
(define-key helm-find-files-map (kbd ";") '(lambda () (interactive) (insert "_")))

;; Use `helm-boring-file-regexp-list' to skip files when showing
(setq helm-ff-skip-boring-files t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm-Swoop
(require 'helm-swoop)
;; Keybindings
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; disable pre-input
(setq helm-swoop-pre-input-function (lambda () ""))
;; Match only for symbol
;; (setq helm-swoop-pre-input-function
;;       (lambda () (if (thing-at-point 'symbol) (format "\\_<%s\\_> " (thing-at-point 'symbol)) "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

(require 'projectile)
;; Yes, yes, everywhere please
(projectile-global-mode)

;; Lighter, make it more compact
(setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))

;; Ah the beaty in this
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-ignored-projects '("~/.emacs.d/"))

(require 'helm-ag)
(define-key helm-ag-map (kbd ";") 'easy-underscore)
(define-key helm-ag-edit-map (kbd ";") 'easy-underscore)
(setq helm-ag-use-grep-ignore-list t)
(add-to-list 'grep-find-ignored-directories '"dist")
(setq projectile-indexing-method 'hybrid)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(put 'projectile-project-run-cmd 'safe-local-variable (lambda (x) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rand-theme

(require 'rand-theme)
(setq rand-theme-unwanted '(light-blue tao))
;; (setq colorless-themes '(tao-yang tao-yin nordless constant-light purp-light basic commentary white anti-zenburn))
;; (setq night-themes '(tao-yin nordless purp foggy-night metalheart))
;; (setq rand-theme-wanted night-themes)
;; (setq rand-theme-wanted colorless-themes)
(global-set-key (kbd "C-z") 'rand-theme-iterate)
(global-set-key (kbd "C-S-z") 'rand-theme-iterate-backwards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter

(global-git-gutter-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dimish mode
;; get rid of dam things in modeline
(diminish 'helm-mode)
;; (diminish 'yas-minor-mode)
(diminish 'eldoc-mode)
(diminish 'lsp-mode)
(diminish 'boolcase-mode)
(diminish 'highlight-indentation-mode)
;; bleh
(diminish 'blacken-mode)
(diminish 'electric-operator-mode)
(diminish 'subword-mode)
(diminish 'git-gutter-mode)
(diminish 'global-git-gutter-mode)
(diminish 'compact-docstrings-mode)


(add-hook 'blacken-mode-hook '(lambda () (diminish 'blacken-mode)))
(add-hook 'electric-operator-mode-hook '(lambda () (diminish 'electric-operator-mode)))
(add-hook 'subword-mode-hook '(lambda () (diminish 'subword-mode)))
(add-hook 'global-git-gutter-mode-hook '(lambda () (diminish 'git-gutter-mode)))
(add-hook 'compact-docstrings-mode-hook '(lambda () (diminish 'compact-docstrings-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kotlin
(require 'kotlin-mode)

(define-key kotlin-mode-map (kbd ";") 'easy-underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode
(require 'cc-mode)

(defun newline-and-put-semicolon-if-needed ()
  "When wanting a new line, if the previous character was an underscore, convert it to a semiconlon.
Made this since I rebinding semicolon to always put an underscore since I spent most of my time in Python. "
  (interactive)
  (when (eq ?_ (char-before))
    (delete-char -1)
    (insert ";"))
  (newline-and-indent))

(define-key c-mode-map (kbd ";") 'easy-underscore)
(define-key c-mode-map (kbd "RET") 'newline-and-put-semicolon-if-needed)

(add-hook 'c-mode-hook 'lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
(require 'rjsx-mode)

(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(define-key rjsx-mode-map (kbd ";") 'easy-camelcase)

(put 'flycheck-javascript-eslint-executable 'safe-local-variable (lambda (x) t))

(add-hook 'rjsx-mode 'lsp)

(require 'json-mode)
(define-key json-mode-map (kbd ";") 'easy-underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prettier (js formatter)
(require 'prettier-js)
(put 'prettier-js-command 'safe-local-variable (lambda (x) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xml (android)
(require 'nxml-mode)

(define-key nxml-mode-map (kbd ";") 'easy-underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emmet mode

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yaml-mode)

(define-key yaml-mode-map (kbd ";") 'easy-underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dockerfile-mode)

(add-hook 'dockerfile-mode-hook '(lambda () (interactive) (electric-operator-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
(define-key python-mode-map (kbd ";") 'easy-underscore)
(add-hook 'before-save-hook 'py-isort-before-save)

(add-hook 'python-mode-hook
          '(lambda ()
             (boolcase-mode +1)
             (electric-indent-mode -1)
             (blacken-mode +1)
             ;; Dislike the default python nav s-exp
             ;; this makes it fallback to c-mode one
             ;; https://emacs.stackexchange.com/a/15244/8964
             (setq-local forward-sexp-function nil)
             ;; Annotate bby
             ;; (annotate-mode +1)
             (flycheck-mode +1)
             ))

(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shrface
(with-eval-after-load 'shr ; lazy load is very important, it can save you a lot of boot up time
  (require 'shrface)
  (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
  (shrface-trial) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
  (setq shrface-href-versatile t) ; enable versatile URL faces support
                                  ; (http/https/ftp/file/mailto/other), if
                                  ; `shrface-href-versatile' is nil, default
                                  ; face `shrface-href-face' would be used.
  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell

(require 'eshell)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun gopar-eshell-stuff ()
  (define-key eshell-hist-mode-map (kbd "C-c C-l") 'helm-eshell-history)
  (eshell/alias "l" "exa --long --classify --all --group --header --blocks --git $1")
  (eshell/alias "ff" "find-file $1")
  (eshell/alias "rmpyc" "find . -name *pyc -delete")
  (define-key eshell-mode-map (kbd ";") 'easy-underscore)
  ;; Remove random insertion of tabs grrrrrrr
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t)
  )

(add-hook 'eshell-mode-hook #'gopar-eshell-stuff)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; virtualenv

;; start moving into eshell instead of terminal \o/
(venv-initialize-eshell)

(setq eshell-prompt-function
      (lambda () (concat
             (if venv-current-name (concat "(" venv-current-name ") "))
             (abbreviate-file-name (eshell/pwd))
             (if (= (user-uid) 0) " # " " $ ")))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
(require 'lsp-mode)
;;(require 'lsp-clients)
(require 'lsp-ui)

(setq lsp-diagnostic-package :none)
(setq lsp-auto-guess-root t)

;; company
(require 'company)
(setq company-idle-delay .5)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase nil)
(setq company-lighter "")
(setq lsp-auto-require-clients nil)
(global-company-mode)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-mode-map (kbd "C-c SPC") 'company-complete)
(define-key company-mode-map (kbd "C-c C-SPC") 'company-yasnippet)

;; flycheck stuff
(require 'flycheck)
(setq-default flycheck-mode-line "")
(define-key flycheck-mode-map (kbd "C-c C-n") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "C-c C-p") #'flycheck-previous-error)

(put 'flycheck-python-flake8-executable 'safe-local-variable (lambda (x) t))
;; (put 'lsp-clients-flow-server 'safe-local-variable (lambda (x) t))

;; Make lsp use flycheck
(setq lsp-prefer-flymake nil)
(setq lsp-python-ms-parse-dot-env-enabled nil)
(setq-default flycheck-disabled-checkers '(python-pylint))

;; Stop emacs from saying this is a dangerous variable
;; (put 'flycheck-python-mypy-args 'safe-local-variable (lambda (x) t))
;; (put 'flycheck-python-mypy-executable 'safe-local-variable (lambda (x) t))

(require 'lsp-python-ms)

;; For debugging
;; (setq lsp-log-io t)
(add-hook 'hack-local-variables-hook
          (lambda ()
            (when (derived-mode-p 'python-mode 'kotlin-mode)
              (lsp)
              )
            ))

(flycheck-add-next-checker 'python-flake8 'python-mypy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that goes in the end :P
(find-file "~/.emacs.d/org/work/work.org")

;; Move the custom stuff to another file
(setq custom-file "~/.emacs.d/ignoreme.el")
(load custom-file t)
(load-theme 'tao-yin)

(progn
  (add-to-list 'default-frame-alist `(font . "Hack 13"))
  (set-face-attribute 'default t :font "Hack 13"))
