;;; Set personal environment vars here

;; Set entire PATH at once
;; Check if they are in, otherwise add to PATH
(let ((paths '(":/usr/local/bin/"
               ":/usr/bin/"
               ":/usr/sbin/"
               ":/sbin/"
               ":/bin/"
               ":/opt/homebrew/opt/openjdk/bin/"
               ":/Applications/Emacs.app/Contents/MacOS/"
               ":/Applications/Firefox.app/Contents/MacOS/"
               ":/opt/homebrew/bin/"
               ":/Users/gopar/.nvm/versions/node/v16.14.2/bin/"
               ":/Library/TeX/texbin/"
               ":/opt/homebrew/opt/libpq/bin/"
               ))
      (PATH-ENV (getenv "PATH"))
      (non-existing-paths '()))
  (dolist (path paths)
    ;; When its not in the path and it exists
    (if (and (not (string-match-p path PATH-ENV)) (file-directory-p (substring path 1)))
        (setenv "PATH" (concat PATH-ENV path))
      (add-to-list 'non-existing-paths path)))
  (message "Unable to add the following paths: %s" non-existing-paths))

;; Random Stuff
(setenv "PIPENV_VERBOSITY" "-1")
(setenv "GIT_PAGER" "")
(setenv "BAT_PAGER" "")

(when (file-directory-p "/Users/gopar/Library/Android/sdk")
    (setenv "ANDROID_HOME" "/Users/gopar/Library/Android/sdk"))


(when (file-directory-p "/usr/local/lib")
  (setenv "LIBRARY_PATH" "${LIBRARY_PATH}:/usr/local/lib" t))

(setenv "EXA_ICON_SPACING" "2") ;; Default is 1
(setenv "PYTHONBREAKPOINT" "pudb.set_trace")

;; Ledger
(setenv "LEDGER_PAGER" "")
(setenv "LEDGER_FILE" "${HOME}/personal/finances/main.dat" t)
(setenv "LEDGER_PRICE_DB" "${HOME}/personal/finances/data/price_db.dat" t)

(setenv "MPLBACKEND" "TkAgg")

(setenv "EDITOR" "emacsclient")

;; Set color for most programs
;; Can't figure out how to set this dynamicall eg $(vivid generate theme)
;; With theme being an option from `vivid themes'
;; https://github.com/sharkdp/vivid
;; (setenv "LS_COLORS" "")

(setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs/"))
