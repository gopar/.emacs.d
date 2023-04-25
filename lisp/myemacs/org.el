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

;; experimental
(defun gopar/save-window-config-and-show-work-agenda ()
  (interactive)
  (window-configuration-to-register ?`)
  (delete-other-windows)
  (org-save-all-org-buffers)
  (org-agenda nil "w"))

(defun gopar/load-window-config-and-close-work-agenda ()
  (interactive)
  (org-save-all-org-buffers)
  (jump-to-register ?`))

;; Originally from here: https://stackoverflow.com/a/59001859/2178312
(defun gopar/get-schedule-or-deadline-if-available ()
  (let ((scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point))))
    (if (not (or scheduled deadline))
        (format " ")
      "   ")))

(use-package org
  ;; :pin gnu
  :custom
  (fill-column 100)
  ;; Where the org files live
  (org-directory "~/.emacs.d/org/")
  ;; Where archives should go
  (org-archive-location (concat (expand-file-name "~/.emacs.d/org/private/org-roam/gtd/archives.org") "::"))
  ;; Make sure we see syntax highlighting
  (org-src-fontify-natively t)
  ;; I dont use it for subs/super scripts
  (org-use-sub-superscripts nil)
  ;; Should everything be hidden?
  (org-startup-folded 'content)
  (org-M-RET-may-split-line '((default . nil)))
  ;; hide stars except for leader star
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers nil)
  ;; Show as utf-8 chars
  (org-pretty-entities t)
  ;; put timestamp when finished a todo
  (org-log-done 'time)
  ;; timestamp when we reschedule
  (org-log-reschedule t)
  ;; Don't indent the stars
  (org-startup-indented nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  ;; Save notes into log drawer
  (org-log-into-drawer t)
  ;;
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  ;;
  (org-fontify-quote-and-verse-blocks t)
  ;; See down arrow instead of "..." when we have subtrees
  ;; (org-ellipsis "⤵")
  ;; catch invisible edit
  ( org-catch-invisible-edits 'error)
  ;; Only useful for property searching only but can slow down search
  (org-use-property-inheritance t)
  ;; Count all children TODO's not just direct ones
  (org-hierarchical-todo-statistics nil)
  ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Don't allow TODO's to close without their dependencies done
  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; Where should notes go to? Dont even use them tho
  (org-default-notes-file (concat org-directory "notes.org"))
  ;; List of default tags to choose from
  (org-tag-alist
   '(("break" . ?b) ;; Something i can do during my pomodoro break
     ("freetime" . ?f) ;; To do whenever i have time to kill
     ("emacs" . ?e) ;; emacs related project/task
     ("calls" . ?c) ;; involves calling humans
     ("moneyMaker" . ?m) ;; Things that potentially bring in money
     ("driving" . ?d) ;; Have to drive to X
     ("project" . ?p) ;; To let me know this is part of a project
     ("someday" . ?s) ;; Eventually i'll do this. I'll revisit this weekly thanks to GTD
     ("misc" . ?x) ;; Anything that doesn't fit these tags
     ("health" . ?h) ;; Health related things. Mucho important
     ("watch" . ?w) ;; Things to watch. Might never get to these sigh
     ("fun" . ?g) ;; FUN
     ))
  ;; The right side of | indicates the DONE states
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "WAITING(w@/@)" "|" "DONE(d)" "CANCELED(c@)" "DELEGATED(p@)")))
  ;; global Effort estimate values
  (org-global-properties
   '(("Effort_ALL" . "0:30 1:00 2:00 3:00 5:00 8:00 10:00")
     ("Points_ALL" . "1 2 3 5 8 13")))
  ;; Needed to allow helm to compute all refile options in buffer
  (org-outline-path-complete-in-steps nil)
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  ;; Repeat to previous todo state
  ;; If there was no todo state, then dont set a state
  (org-todo-repeat-to-state t)
  ;; Refile options
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; This worked ok, but lets try some more detail refiling
  ;; (org-refile-targets '((org-agenda-files :level .  1)))
  (org-refile-targets '(("~/.emacs.d/org/private/org-roam/gtd/gtd.org" :maxlevel . 3)
                        ("~/.emacs.d/org/private/org-roam/gtd/someday.org" :level . 1)
                        ("~/.emacs.d/org/private/org-roam/gtd/tickler.org" :maxlevel . 1)
                        ("~/.emacs.d/org/private/org-roam/gtd/repeat.org" :maxlevel . 1)
                        ))
  ;; Lets customize which modules we load up
  (org-modules '(ol-w3m
                 ol-bbdb
                 ol-bibtex
                 ol-docview
                 ol-gnus
                 ol-info
                 ol-irc
                 ol-mhe
                 ol-rmail
                 ol-eww
                 ;; Stuff I've enabled below
                 org-habit
                 ;; org-checklist
                 ))
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  :hook (org-mode . org-indent-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (sqlite . t)
     (python . t)
     (java . t)
     (emacs-lisp . t)
     (shell . t)))
  ;; Save history throughout sessions
  (org-clock-persistence-insinuate))

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org-clock
  :after org
  :custom
  ;; Save clock history accross emacs sessions (read var for required info)
  (org-clock-persist t)
  ;; If idle for more than 15 mins, resolve by asking what to do with clock
  (org-clock-idle-time 15)
  ;; Set clock in frame title, instead of mode line
  (org-clock-clocked-in-display 'frame-title)
  ;; Show more clocking history
  (org-clock-history-length 10)
  ;; Include running time in clock reports
  (org-clock-report-include-clocking-task t)
  ;; Put all clocking info int the "CLOCKING" drawer
  (org-clock-into-drawer "CLOCKING")
  ;; Setup default clocktable summary
  (org-clock-clocktable-default-properties
   '(:maxlevel 2 :scope file :formula % ;; :properties ("Effort" "Points")
               :sort (5 . ?t) :compact t :block today))
  :bind (:map global-map
              ("C-c j" . (lambda () (interactive) (org-clock-jump-to-current-clock)))
              :map org-mode-map
              ("C-c C-x r" . (lambda () (interactive) (org-clock-report)))))

(use-package org-agenda
  :after org
  :bind (("C-c a" . org-agenda))
  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-tags-column 'auto)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-files "~/.emacs.d/org/agenda-files.org")
  ;; (org-agenda-todo-ignore-scheduled 'future)
  ;; TODO entries that can't be marked as done b/c of children are shown as dimmed in agenda view
  (org-agenda-dim-blocked-tasks t)
  ;; Start the week view on whatever day im on
  (org-agenda-start-on-weekday nil)
  ;; How to identify stuck/non-stuck projects
  ;; Projects are identified by the 'project' tag and its always the first level
  ;; Next any of these todo keywords means it's not a stuck project
  ;; 3rd, theres no tags that I use to identify a stuck Project
  ;; Finally, theres no special text that signify a non-stuck project
  (org-stuck-projects
   '("+project+LEVEL=1"
     ("NEXT" "IN-PROGRESS" "WAITING" "DONE" "CANCELED" "DELEGATED")
     nil
     ""))
  (org-agenda-prefix-format
   '((agenda . " %-4e %i %-12:c%?-12t% s ")
     (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  ;; Lets define some custom cmds in agenda menu
  (org-agenda-custom-commands
   '(("h" "Agenda and Home tasks"
      ((agenda "" ((org-agenda-span 2)))
       (todo "WAITING|IN-PROGRESS")
       (todo "NEXT"))
      ((org-agenda-sorting-strategy '(habit-up priority-down category-up))))

     ("w" "Agenda and break|inbox tasks"
      ((agenda "" ((org-agenda-span 1)))
       (tags-todo "inbox|break"))
      ((org-agenda-sorting-strategy '(habit-up priority-down category-up))))

     ("i" "In-Progress Tasks"
      ((agenda "" ((org-agenda-skip-function '(zin/org-agenda-skip-tag "work"))))
       (todo "IN-PROGRESS|WAITING"))
      ((org-agenda-sorting-strategy '(habit-up priority-down category-up))))

     ("g" "Goals: 12 Week Year"
      ((agenda "")
       (todo "IN-PROGRESS|WAITING"))
      ((org-agenda-sorting-strategy '(habit-up priority-down category-up)) (org-agenda-tag-filter-preset '("+12WY"))))

     ("r" "Weekly Review"
      ((agenda "")
       (todo))
      ((org-agenda-sorting-strategy '(habit-up category-up priority-down ))
       (org-agenda-files "~/.emacs.d/org/weekly-reivew-agenda-files.org"))))))

(use-package org-capture
  :after org
  :bind (("C-c c" . org-capture))
  :custom
  ;; dont create a bookmark when calling org-capture
  (org-capture-bookmark nil)
  ;; also don't create bookmark in other things
  (org-bookmark-names-plist nil)
  (org-capture-templates
   '(
     ("c" "Inbox" entry (file "~/.emacs.d/org/private/org-roam/gtd/inbox.org")
      "* TODO %?\n:PROPERTIES:\n:DATE_ADDED: %u\n:END:")
     ("p" "Project" entry (file "~/.emacs.d/org/private/org-roam/gtd/gtd.org")
      "* %? [%] :project: \n:PROPERTIES: \n:TRIGGER: next-sibling todo!(NEXT) scheduled!(copy)\n:ORDERED: t \n:DATE_ADDED: %u\n:END:\n** TODO Add entry")
     ("t" "Tickler" entry (file "~/.emacs.d/org/private/org-roam/gtd/tickler.org")
      "* TODO %? \nSCHEDULED: %^{Schedule}t\n:PROPERTIES:\n:DATE_ADDED: %u\n:END:\n")
     ("k" "Contact" entry (file "~/.emacs.d/org/private/org-roam/references/contacts.org")
      "* %? \n%U
:PROPERTIES:
:EMAIL:
:PHONE:
:NICKNAME:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:Blog:
:END:"))))

(use-package ol
  :after org
  :custom
  (org-link-shell-confirm-function 'y-or-n-p)
  (org-link-elisp-confirm-function 'y-or-n-p))

(use-package org-src
  :after org
  :custom
  (org-src-preserve-indentation nil)
  ;; Don't ask if we already have an open Edit buffer
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-edit-src-content-indentation 0))

(use-package ob-core
  :after org
  :custom
  ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate nil))

(use-package org-habit
  :ensure nil
  :custom
  (org-habit-graph-column 45))

(use-package org-indent
  :ensure nil
  :diminish)

(use-package org-pomodoro
  :ensure t
  :after org
  :bind (("<f12>" . org-pomodoro))
  :hook ((org-pomodoro-started . gopar/load-window-config-and-close-work-agenda)
         (org-pomodoro-finished . gopar/save-window-config-and-show-work-agenda))
  :custom
  (org-pomodoro-manual-break t)
  (org-pomodoro-short-break-length 20)
  (org-pomodoro-long-break-length 30)
  (org-pomodoro-length 60)
  :init
  (defun gopar/home-pomodoro ()
    (interactive)
    (setq org-pomodoro-length 25
          org-pomodoro-short-break-length 5))

  (defun gopar/work-pomodoro ()
    (interactive)
    (setq org-pomodoro-length 60
          org-pomodoro-short-break-length 20)))

(use-package org-edna
  :ensure t
  :diminish
  :custom
  (org-edna-use-inheritance t)
  ;; Global minor mode, lets enable it once
  :hook (after-init . org-edna-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name "~/.emacs.d/org/private/org-roam"))
  (org-roam-db-location (expand-file-name "~/.emacs.d/org/private/org-roam.db"))
  (org-roam-tag-sources '(prop))
  (org-roam-db-update-method 'immediate)
  (org-roam-graph-viewer 'browse-url-firefox)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "./references/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-directory (expand-file-name "~/.emacs.d/org/private/journal/"))
  (org-roam-dailies-capture-templates
   `(("d" "daily" plain (file "/Users/gopar/.emacs.d/org/templates/dailies-daily.template")
      :target (file+head "daily/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("w" "weekly" plain (file "/Users/gopar/.emacs.d/org/templates/dailies-weekly.template")
      :target (file+head "weekly/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("m" "monthly" plain (file "/Users/gopar/.emacs.d/org/templates/dailies-monthly.template")
      :target (file+head "monthly/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  :bind (:map global-map
              (("C-c n i" . org-roam-node-insert)
               ("C-c n f" . org-roam-node-find)
               ("C-c n g" . org-roam-graph)
               ("C-c n n" . org-roam-capture)
               ("C-c n d" . org-roam-dailies-capture-today)
               ("C-c n s" . consult-org-roam-search)))
  :hook (after-init . org-roam-db-autosync-mode))

;; Belongs from the org-contrib pkg?
(use-package org-annotate-file
  :ensure nil
  :load-path "lisp/org"
  :custom
  (org-annotate-file-add-search t)
  :bind (:map python-mode-map ("C-c C-s" . gopar/org-annotate-file))
  :init
  (defun gopar/org-annotate-file (&optional arg)
    "Annotate current line.
When called with a prefix aurgument, it will open annotations file."
    (interactive "P")
    (require 'org-annotate-file)
    (let* ((root (projectile-project-root))
           (org-annotate-file-storage-file (format "%s.org-annotate.org" root)))
      (if arg
          (find-file org-annotate-file-storage-file)
        (org-annotate-file)))))

(provide 'org)
