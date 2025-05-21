;;; ewnium.el --- Vimium-like extensions for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Original Author <author@example.com>
;; Maintainer: Current Maintainer <maintainer@example.com>
;; Created: 2024
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (avy "0.5.0") (consult "0.16" nil))
;; Keywords: convenience, web
;; URL: https://github.com/yourusername/ewnium

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ewnium provides Vimium-like functionality for the EWW web browser in Emacs.
;; It adds convenient keybindings for navigation, link following, and buffer
;; management inspired by the popular Vimium browser extension.

;; Key features:
;; - Vim-style navigation keybindings
;; - Quick link following with avy
;; - Buffer management commands
;; - URL manipulation utilities
;; - Bookmark and history management

;;; Code:
(require 'cl-lib)
(require 'eww)
(require 'shr)
(require 'view)

(defgroup ewnium nil
  "Vimium-like extensions for EWW."
  :group 'eww)

(defcustom ewnium-rename-buffers t
  "Whether to rename EWW buffers based on page title."
  :type 'boolean
  :group 'ewnium)

(defcustom ewnium-scroll-step 1
  "Number of lines to scroll with j/k keys."
  :type 'integer
  :group 'ewnium)



(defun ewnium-open-eww-with-recent-kill-ring (&optional arg)
  "Open current EWW with most recent item in kill ring.
If prefix arg is passed, then open in new EWW buffer."
  (interactive "P")
  (let ((buffer (if (eq major-mode 'eww-mode)
                    (clone-buffer)
                  (generate-new-buffer "*eww*"))))
    (with-current-buffer buffer
      (eww-mode)
      (eww (current-kill 0)))))

(defun ewnium--rename-eww-buffer ()
  "Rename `eww-mode' buffer so sites open in new page.
Stolen from `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
  (let ((title (plist-get eww-data :title)))
    (when (eq major-mode 'eww-mode )
      (if title
          (rename-buffer title t)
        (rename-buffer "eww" t)))))

(defun ewnium--go-up-url-hierarchy ()
  "Go up the URL hierarchy."
  (interactive)
  (let ((url (url-generic-parse-url (eww-current-url))))
    (setq url (url-recreate-url
               (url-parse-make-urlobj
                (url-type url)
                (url-user url)
                (url-password url)
                (url-host url)
                (url-port url)
                (s-join "/" (butlast (s-split "/" (url-filename url)) 1))
                (url-target url)
                nil
                (url-fullness url))))
    (eww-browse-url url)))

(defun ewnium--go-to-root-url-hierarchy ()
  "Go to root of current URL hierarchy"
  (interactive)
  (let ((url (url-generic-parse-url (eww-current-url))))
    (setq url (url-recreate-url
               (url-parse-make-urlobj
                (url-type url)
                (url-user url)
                (url-password url)
                (url-host url)
                (url-port url)
                ""
                (url-target url)
                nil
                (url-fullness url))))
    (eww-browse-url url)))

(defun ewnium--get-list-of-buffers ()
  "Return a list of plist.
Each plist contains a :buffer, :title, :url and :type.

:BUFFER Points to the buffer we are referencing in the plist.

:URL The URL that the buffer is in.

:TITLE The title of the current url in said buffer.

:TYPE Symbol to let me know where its from.
Always set to 'buffer."
  (let (buffers-info)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eww-mode)
          (push (list :buffer buffer
                      :title (plist-get eww-data :title)
                      :url (plist-get eww-data :url)
                      :type 'buffer)
                buffers-info))))
    buffers-info))

(defun ewnium--get-list-of-bookmarks ()
  "Return a list of plists.
Each plist contains a :title, :url and :type.

:URL The URL of the saved bookmark.

:TITLE The page title of said URL bookmark.

:TYPE Symbol to let me know where its from.
Always set to 'bookmark."
  ;; Lets load bookmarks
  (eww-read-bookmarks)
  (if (not eww-bookmarks)
      '() ;; return empty list
    (let (bookmarks-info)
      (dolist (bookmark eww-bookmarks)
        (push (list :url (plist-get bookmark :url)
                    :title (plist-get bookmark :title)
                    :type 'bookmark)
              bookmarks-info))
      bookmarks-info)))

(defun ewnium--get-list-of-history ()
  "Return a list of plists.
Each plist contains a title, :url and :type.

:URL The URL of one point in time.

:TITLE The page title of said URL.

:TYPE Symbol to let me know where its from.
Always set to 'history."
  (let (history-list)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eww-mode)
          (dolist (history eww-history)
            (push (list :url (plist-get history :url)
                        :title (plist-get history :title)
                        :type 'history)
                  history-list)))))
    history-list))

(defun ewnium--get-list-of-history-bookmarks-and-buffers (history bookmarks buffers)
  "Return a list of plists"
  (let (options)
    (when history
      (dolist (entry (ewnium--get-list-of-history))
        (push entry options)))
    (when bookmarks
      (dolist (entry (ewnium--get-list-of-bookmarks))
        (push entry options)))
    (when buffers
      (dolist (entry (ewnium--get-list-of-buffers))
        (push entry options)))
    options))

(defun ewnium-open-url-or-bookmark-or-history (&optional arg)
  "Show prompt to either open a new query, bookmark or history.
Passing ARG as non-nil, means open in new eww buffer."
  (interactive "P")
  (let* ((options (ewnium--get-list-of-history-bookmarks-and-buffers t t t))
         ;; Filter out entries with nil titles
         (options (seq-filter (lambda (plist)
                               (or (plist-get plist :title)
                                   (plist-get plist :buffer)))
                             options))
         ;; Create alist of (title . plist) for better lookup
         (candidates (mapcar (lambda (plist)
                              (let ((title (or (plist-get plist :title)
                                              (when (plist-get plist :buffer)
                                                (buffer-name (plist-get plist :buffer))))))
                                (cons title plist)))
                            options))
         ;; Create annotation function that works with both default completion and consult
         (annotf (lambda (cand)
                  (let* ((plist (cdr (assoc cand candidates)))
                         (type (plist-get plist :type)))
                    (pcase type
                      ('buffer " [Buffer]")
                      ('bookmark " [Bookmark]")
                      ('history " [History]")
                      (_ "")))))
         ;; Set completion properties
         (completion-extra-properties `(:annotation-function ,annotf))
         ;; For consult compatibility
         (consult--read-config `((annotation-function . ,annotf)))
         ;; Get just the titles for completion
         (titles (mapcar #'car candidates))
         ;; Prompt for selection
         (chosen (completing-read "URL/Bookmark/History: " titles nil t))
         ;; Get the selected plist
         (target-plist (cdr (assoc chosen candidates))))

    (cond
     ;; Handle buffer selection
     ((and target-plist (eq (plist-get target-plist :type) 'buffer))
      (switch-to-buffer (plist-get target-plist :buffer)))

     ;; Handle bookmark or history selection
     ((and target-plist (plist-get target-plist :url))
      (eww (plist-get target-plist :url) (if arg 4 nil)))

     ;; Handle direct URL input
     ((and chosen (not (string-empty-p chosen)))
      (eww chosen (if arg 4 nil)))

     ;; Handle no selection
     (t (message "No valid selection made")))))

(defun ewnium-open-history (&optional arg)
  "Show prompt to open a page from history.
With prefix ARG, open in new eww buffer."
  (interactive "P")
  (let* ((options (ewnium--get-list-of-history-bookmarks-and-buffers t nil nil))
         (titles (mapcar (lambda (plist) (plist-get plist :title)) options))
         (completion-extra-properties
          `(:annotation-function ,(lambda (_) " [History]")))
         (chosen (completing-read "History: " titles nil t))
         (target-plist (car (seq-filter (lambda (plist)
                                        (equal (plist-get plist :title) chosen))
                                      options)))
         (url (plist-get target-plist :url)))
    (cond
     ((null url)
      (message "No history item chosen")
      nil)
     (t
      (eww url (when arg 4))
      url))))

(defun ewnium-open-bookmark (&optional arg)
  "Show prompt to open a bookmark.
With prefix ARG, open in new eww buffer.
Return nil if no bookmark was chosen, otherwise return the opened URL."
  (interactive "P")
  (let* ((options (ewnium--get-list-of-history-bookmarks-and-buffers nil t nil))
         (titles (mapcar (lambda (plist) (plist-get plist :title)) options))
         (completion-extra-properties
          `(:annotation-function ,(lambda (_) " [Bookmark]")))
         (chosen (completing-read "Bookmark: " titles nil t))
         (target-plist (car (seq-filter (lambda (plist)
                                        (equal (plist-get plist :title) chosen))
                                      options)))
         (url (plist-get target-plist :url)))
    (cond
     ((null url)
      (message "No bookmark chosen")
      nil)
     (t
      (eww url (when arg 4))
      url))))

(defun ewnium-open-buffers ()
  "Show a prompt of all EWW buffers to switch to."
  (interactive)
  (let* ((options (ewnium--get-list-of-history-bookmarks-and-buffers nil nil t))
         (titles (mapcar (lambda (plist)
                          (or (plist-get plist :title)
                              (buffer-name (plist-get plist :buffer))))
                        options))
         (completion-extra-properties
          `(:annotation-function ,(lambda (_) " [Buffer]")))
         (chosen (completing-read "EWW Buffer: " titles nil t))
         (target-plist (car (seq-filter (lambda (plist)
                                         (equal (or (plist-get plist :title)
                                                   (buffer-name (plist-get plist :buffer)))
                                                chosen))
                                       options)))
         (chosen-buffer (plist-get target-plist :buffer)))
    (if (not chosen-buffer)
        (message "No Buffer chosen.")
      (switch-to-buffer chosen-buffer))))

(defun ewnium-edit-current-url (&optional arg)
  "Edit the current URL or enter a new search.
With prefix ARG, open in a new EWW buffer."
  (interactive)
  (let* ((url (eww-copy-page-url))
         (uris (eww-suggested-uris)))
    (setq url (read-string "Edit URL or new search: " (car kill-ring) 'eww-promt-history uris))
    (setq url (eww--dwim-expand-url url))
    (eww url (if arg 4 nil))))

(defun ewnium-next-buffer (&optional arg)
  "Go to the next/previous EWW buffer in line.

If ARG is non-bil then go back one buffer, otherwise
go forward one buffer.

One thing to note, is the order in which they iterate.
Since I am ordering them by URL, the ordering can change.

Eg

Another.com
Boo.com
youtube.com

(Go from Another.com -> Zebra.com)
Order is now:

Boo.com
youtube.com
zebra.com
"
  ;; Not the prettiest function but who's watching :P
  (interactive)
  (cl-flet ((next-buffer (buffers index)
              (if (>= (1+ index) (length buffers))
                  (nth 0 buffers)
                (nth (1+ index) buffers)))
            (prev-buffer (buffers index)
              (if (= index 0)
                  (nth (1- (length buffers)) buffers)
                (nth (1- index) buffers))))

    (let* ((buffer-plist nil)
           (buffers (ewnium--get-list-of-buffers))
           ;; Sort them by URL
           (buffers (cl-sort buffers 'string< :key '(lambda (x) (plist-get x :url))))
           (index (cl-position (current-buffer) buffers :key '(lambda (x) (plist-get x :buffer)))))
      (if (= (length buffers) 1)
          (message "Only one EWW buffer present.")
        (setq buffer-plist (if arg (prev-buffer buffers index) (next-buffer buffers index)))
        (switch-to-buffer (plist-get buffer-plist :buffer))))))

(defun ewnium-previous-buffer ()
  "Go to the next/previous EWW buffer in line."
  (interactive)
  (ewnium-next-buffer t))

(defun ewnium-not-implemented ()
  (interactive)
  (message "Not Implemented :("))

(defun ewnium-advice-filter-args (args)
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

(defun gopar/avy--property-candidates (property &optional beg end pred group prop-pred)
  "Return all elements that have a property PROPERTY.
Each element of the list is ((BEG . END) . WND)
When PRED is non-nil, it's a filter for matching point positions."
  (setq group (or group 0))
  (let ((found-link nil)
        (case-fold-search (or avy-case-fold-search
                              (string= regex (downcase regex))))
        candidates)
    (avy-dowindows current-prefix-arg
      (dolist (pair (avy--find-visible-regions
                     (or beg (window-start))
                     (or end (window-end (selected-window) t))))
        (save-excursion
          (goto-char (car pair))
          (save-restriction
            (narrow-to-region (car pair) (cdr pair))
            (setq found-link (text-property-search-forward property nil prop-pred))
            (while found-link
              (when (avy--visible-p (1- (point)))
                (when (or (null pred)
                          (funcall pred))
                  (push (cons (cons (prop-match-beginning found-link)
                                    (prop-match-end found-link)) wnd) candidates)))
              (setq found-link (text-property-search-forward property nil prop-pred)))))))
    (nreverse candidates)))

(cl-defun gopar/avy-property-jump (property &key window-flip beg end action pred group prop-pred)
  "Provide avy jump completions but for text properties that match PROPERTY.
Usefule for when browsing EWW.

When ARG is non-nil, open in new EWW buffer."
  (interactive)
  (require 'avy)
  (setq avy-action (or action avy-action))
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy-process
     (gopar/avy--property-candidates property beg end pred group prop-pred))))

(defun ewnium--in-form-field-p ()
  "Return t if point is in a form field that should receive input."
  (and (eq major-mode 'eww-mode)
       (get-text-property (point) 'local-map)))

(defun ewnium-self-insert-or-command (original-command)
  "Execute ORIGINAL-COMMAND unless point is in a form field.
In form fields, insert the key that was pressed instead."
  (interactive)
  (if (ewnium--in-form-field-p)
      (call-interactively #'self-insert-command)
    (call-interactively original-command)))

(defun ewnium-g-prefix ()
  "Handle g prefix command.
If in a form field, insert 'g'. Otherwise, wait for the next key and execute
the corresponding command."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (let ((key (read-key "g-")))
      (cond
       ((eq key ?g) (beginning-of-buffer))
       ((eq key ?u) (ewnium--go-up-url-hierarchy))
       ((eq key ?U) (ewnium--go-to-root-url-hierarchy))
       ((eq key ?s) (eww-view-source))
       ((eq key ?e) (ewnium-edit-current-url))
       ((eq key ?i) (gopar/avy-property-jump
                     'eww-form
                     :prop-pred (lambda (val prop-val)
                                  (string= "text" (plist-get prop-val :type)))))
       (t (message "g-%c is undefined" key))))))

(defun ewnium-y-prefix ()
  "Handle y prefix command.
If in a form field, insert 'y'. Otherwise, wait for the next key and execute
the corresponding command."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (let ((key (read-key "y-")))
      (cond
       ((eq key ?y) (eww-copy-page-url))
       ((eq key ?f) (shr-maybe-probe-and-copy-url))
       (t (message "y-%c is undefined" key))))))

(defun ewnium-open-bracket-prefix ()
  "Handle [ prefix command.
If in a form field, insert '['. Otherwise, wait for the next key and execute
the corresponding command."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (let ((key (read-key "[-")))
      (cond
       ((eq key ?\[) (eww-back-url))
       (t (message "[-%c is undefined" key))))))

(defun ewnium-close-bracket-prefix ()
  "Handle ] prefix command.
If in a form field, insert ']'. Otherwise, wait for the next key and execute
the corresponding command."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (let ((key (read-key "]-")))
      (cond
       ((eq key ?\]) (eww-forward-url))
       (t (message "]-%c is undefined" key))))))

(defun ewnium-scroll-down ()
  "Scroll down one line unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (scroll-down 1)))

(defun ewnium-scroll-up ()
  "Scroll up one line unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (scroll-up 1)))

(defun ewnium-show-alt-text ()
  "Show alt text for image at point unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (shr-show-alt-text)))

(defun ewnium-zoom-image ()
  "Zoom image at point unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (shr-zoom-image)))

(defun ewnium-next-link ()
  "Move to next link unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (shr-next-link)))

(defun ewnium-previous-link ()
  "Move to previous link unless in a form field."
  (interactive)
  (if (ewnium--in-form-field-p)
      (self-insert-command 1)
    (shr-previous-link)))

(defvar ewnium-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "<tab>") #'shrface-outline-cycle)
    ;; (define-key map (kbd "S-<tab>") #'shrface-outline-cycle-buffer)
    (define-key map (kbd "C-i") #'shrface-links-consult)
    (define-key map (kbd "C-o") #'shrface-headline-consult)
    ;; (define-key map (kbd "a") #'ewnium-show-alt-text)
    ;; (define-key map (kbd "z") #'ewnium-zoom-image)
    (define-key map (kbd "i") #'ewnium-next-link)
    (define-key map (kbd "I") #'ewnium-previous-link)
    (define-key map (kbd "j") #'ewnium-scroll-up)
    (define-key map (kbd "J") #'ewnium-previous-buffer)
    (define-key map (kbd "k") #'ewnium-scroll-down)
    (define-key map (kbd "L") #'(lambda () (interactive) (ewnium-self-insert-or-command 'eww-forward-url)))
    (define-key map (kbd "l") #'(lambda () (interactive) (ewnium-self-insert-or-command 'recenter-top-bottom)))
    (define-key map (kbd "K") #'ewnium-next-buffer)
    (define-key map (kbd "n") #'(lambda () (interactive) (ewnium-self-insert-or-command 'shrface-next-headline)))
    (define-key map (kbd "N") #'(lambda () (interactive) (ewnium-self-insert-or-command 'shrface-previous-headline)))
    (define-key map (kbd "G") #'(lambda () (interactive) (ewnium-self-insert-or-command 'end-of-buffer)))
    (define-key map (kbd "d") #'(lambda () (interactive) (ewnium-self-insert-or-command 'View-scroll-half-page-forward)))
    (define-key map (kbd "u") #'(lambda () (interactive) (ewnium-self-insert-or-command 'View-scroll-half-page-backward)))
    (define-key map (kbd "r") #'(lambda () (interactive) (ewnium-self-insert-or-command 'eww-reload)))
    (define-key map (kbd "f") #'(lambda () (interactive) (ewnium-self-insert-or-command 'link-hint-open-link)))
    (define-key map (kbd "F") #'(lambda () (interactive)
                                  (gopar/avy-property-jump 'shr-url
                                                           :action '(lambda (pt) (avy-action-goto pt) (eww-open-in-new-buffer)))))
    ;; (define-key map (kbd "p") #'previous-line)
    ;; (define-key map (kbd "p") #'ewnium-open-eww-with-recent-kill-ring)
    ;; (define-key map (kbd "P") #'(lambda () (interactive) (ewnium-open-eww-with-recent-kill-ring t)))
    ;; (define-key map (kbd "o") #'ewnium-open-url-or-bookmark-or-history)
    ;; (define-key map (kbd "b") #'ewnium-open-bookmark)
    (define-key map (kbd "T") #'ewnium-open-buffers)
    (define-key map (kbd "H") #'(lambda () (interactive) (ewnium-self-insert-or-command 'eww-back-url)))
    (define-key map (kbd "g") #'ewnium-g-prefix)
    (define-key map (kbd "y") #'ewnium-y-prefix)
    (define-key map (kbd "[") #'ewnium-open-bracket-prefix)
    (define-key map (kbd "]") #'ewnium-close-bracket-prefix)
    map)
  "Keymap for `ewnium-mode'.")

;;;###autoload
(define-minor-mode ewnium-mode
  "Eww's version of the Vimium plugin"
  :keymap ewnium-mode-map)

(provide 'ewnium)
