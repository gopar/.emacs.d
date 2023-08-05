(require 'eww)
(require 'shr)
(require 'view)

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

(defun ewnium--go-up-url-heirarchy ()
  "Go up the URL heirarchy."
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

(defun ewnium--go-to-root-url-heirarchy ()
  "Go to root of current URL heirarchy"
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
  (let (options titles choesn chosen-type chosen-buffer target-plist)
    (setq options (ewnium--get-list-of-history-bookmarks-and-buffers t t t))
    (setq titles (mapcar (lambda (plist) (plist-get plist :title)) options))
    (setq chosen (completing-read "URL/Book mark/History: " titles))
    (setq target-plist (car (seq-filter (lambda (plist) (equal (plist-get plist :title) chosen)) options)))
    (setq chosen (or (plist-get target-plist :url) chosen)
          chosen-type (plist-get target-plist :type)
          chosen-buffer (plist-get target-plist :buffer))
    (cond
     ((eq chosen-type 'buffer)
      (switch-to-buffer chosen-buffer))
     ((eq chosen-type 'bookmark)
      (eww chosen (if arg 4 nil)))
     ((eq chosen-type 'history)
      (eww chosen (if arg 4 nil)))
     ;; When it's a string
     (t
      (eww chosen (if arg 4 nil))))))

(defun ewnium-open-bookmark (&optional arg)
  "Show prompt to open a bookmark.
Providing a prefix will open in new eww buffer."
  (interactive "P")
  (let ()
    (setq options (ewnium--get-list-of-history-bookmarks-and-buffers nil t nil))
    (setq titles (mapcar (lambda (plist) (plist-get plist :title)) options))
    (setq chosen (completing-read "Book-mark: " titles))
    (setq target-plist (car (seq-filter (lambda (plist) (equal (plist-get plist :title) chosen)) options)))
    (setq chosen (plist-get target-plist :url))
    (if (not chosen)
        (message "No Bookmark chosen.")
      (eww chosen (if arg 4 nil)))))

(defun ewnium-open-buffers ()
  "Show a prompt of all EWW buffers to switch to."
  (interactive)
  (let (chosen)
    (setq options (ewnium--get-list-of-history-bookmarks-and-buffers t nil nil))
    (setq titles (mapcar (lambda (plist) (plist-get plist :title)) options))
    (setq chosen (completing-read "EWW Buffer: " titles))
    (setq target-plist (car (seq-filter (lambda (plist) (equal (plist-get plist :title) chosen)) options)))
    (setq chosen (plist-get target-plist :buffer))
    (if (not chosen)
        (message "No Buffer chosen.")
      (switch-to-buffer chosen))))

(defun ewnium-edit-current-url (&optional arg)
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
    (print url)

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

(defun ewnium--element-has-local-map-p ()
  "Check if the current point is inside an element that has a local map in EWW."
  (and (eq major-mode 'eww-mode)
       (get-text-property (point) 'local-map)))

(defun ewnium--chose-keybinding-based-on-contenxt (original-binding)
  "Allow normal keybinding behavior when not inside elements such as text input."
  (if (keymapp original-binding)
      (ewnium--add-context-check-to-keymap original-binding)
    (lexical-let ((original-func original-binding))
      (lambda ()
        (interactive)
        (if (ewnium--element-has-local-map-p)
            (self-insert-command 1)
          (funcall-interactively original-func))))))

(defun ewnium--add-context-check-to-keymap (keymap)
  (let ((modified-keymap (make-sparse-keymap)))
    (map-keymap
     (lambda (key command)
       (let ((key-description (key-description (vector key))))
         (when (string-match-p "^[[:lower:]][[:alpha:]]*" key-description)
           (define-key modified-keymap (kbd key-description) (ewnium--chose-keybinding-based-on-contenxt command)))))
     keymap)
    modified-keymap))

(defvar ewnium-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'shrface-outline-cycle)
    (define-key map (kbd "S-<tab>") #'shrface-outline-cycle-buffer)
    ;; (define-key map (kbd "C-t") #'shrface-toggle-bullets)
    (define-key map (kbd "C-i") #'shrface-links-consult)
    (define-key map (kbd "C-o") #'shrface-headline-consult)
    (define-key map (kbd "a") #'shr-show-alt-text)
    (define-key map (kbd "z") #'shr-zoom-image)
    (define-key map (kbd "i") #'shr-next-link)
    (define-key map (kbd "I") #'shr-previous-link)
    (define-key map (kbd "j") #'(lambda () (interactive) (scroll-up 1)))
    (define-key map (kbd "k") #'(lambda () (interactive) (scroll-down 1)))
    (define-key map (kbd "n") #'shrface-next-headline)
    (define-key map (kbd "N") #'shrface-previous-headline)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "G") #'end-of-buffer)
    (define-key map (kbd "d") #'View-scroll-half-page-forward)
    (define-key map (kbd "u") #'View-scroll-half-page-backward)
    (define-key map (kbd "r") #'eww-reload)
    (define-key map (kbd "p") #'ewnium-open-eww-with-recent-kill-ring)
    (define-key map (kbd "P") #'(lambda () (interactive) (ewnium-open-eww-with-recent-kill-ring t)))
    (define-key map (kbd "f") #'(lambda () (interactive)
                                  (gopar/avy-property-jump 'shr-url
                                                           :action '(lambda (pt) (avy-action-goto pt) (shr-browse-url)))))
    (define-key map (kbd "F") #'(lambda () (interactive)
                                  (gopar/avy-property-jump 'shr-url
                                                           :action '(lambda (pt) (avy-action-goto pt) (eww-open-in-new-buffer)))))
    (define-key map (kbd "o") #'ewnium-open-url-or-bookmark-or-history)
    (define-key map (kbd "b") #'ewnium-open-bookmark)
    (define-key map (kbd "T") #'ewnium-open-buffers)
    (define-key map (kbd "H") #'eww-back-url)
    (define-key map (kbd "L") #'eww-forward-url)
    (define-key map (kbd "J") #'ewnium-previous-buffer)
    (define-key map (kbd "K") #'ewnium-next-buffer)
    (define-key map (kbd "g g") #'beginning-of-buffer)
    (define-key map (kbd "g u") #'ewnium--go-up-url-heirarchy)
    (define-key map (kbd "g U") #'ewnium--go-to-root-url-heirarchy)
    (define-key map (kbd "g s") #'eww-view-source)
    (define-key map (kbd "g e") #'ewnium-edit-current-url)
    (define-key map (kbd "g i") #'(lambda ()
                                    (interactive)
                                    (gopar/avy-property-jump
                                     'eww-form
                                     :prop-pred (lambda (val prop-val) (string= "text" (plist-get prop-val :type))))))
    (define-key map (kbd "y y") #'eww-copy-page-url)
    (define-key map (kbd "y f") #'shr-maybe-probe-and-copy-url)
    ;; (define-key map (kbd "y t") #'ewnium-noop)
    (define-key map (kbd "[ [") #'ewnium-open-bracket-map)
    (define-key map (kbd "] ]") #'ewnium-close-bracket-map)
    (ewnium--add-context-check-to-keymap map)))

;;;###autoload
(define-minor-mode ewnium-mode
  "Eww's version of the Vimium plugin"
  :keymap ewnium-mode-map
  (if ewnium-mode
      (add-hook 'eww-after-render-hook 'ewnium--rename-eww-buffer)
    (remove-hook 'eww-after-render-hook 'ewnium--rename-eww-buffer)))

(provide 'ewnium)
