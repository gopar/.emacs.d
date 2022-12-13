;;; Set personal environment vars here

;; Set entire PATH at once
;; Check if they are in, otherwise add to PATH
(let ((paths '(":/usr/local/bin/"
               ":/usr/bin/"
               ":/usr/sbin/"
               ":/sbin/"
               ":/bin/"
               ;; pdf tools
               "/Library/TeX/texbin/"
               "/Applications/Emacs.app/Contents/MacOS/"
               "/opt/homebrew/opt/openjdk/bin/"
               "/opt/homebrew/bin/"
               ))
      (PATH-ENV (getenv "PATH")))
  (dolist (path paths)
    (when (not (string-match-p path PATH-ENV))
      (setenv "PATH" (concat PATH-ENV path)))))

;; Random Stuff
(setenv "PIPENV_VERBOSITY" "-1")
(setenv "GIT_PAGER" "")
(setenv "BAT_PAGER" "")

(setenv "LIBRARY_PATH" "${LIBRARY_PATH}:/usr/local/lib" t)

(setenv "EXA_ICON_SPACING" "2") ;; Default is 1

;; Ledger
(setenv "LEDGER_PAGER" "")
(setenv "LEDGER_FILE" "${HOME}/personal/finances/main.dat" t)
(setenv "LEDGER_PRICE_DB" "${HOME}/personal/finances/data/price_db.dat" t)

(setenv "PEEKALINK_API_KEY" "36a7e527-27f0-4606-bb87-32327f1462f7")

(setenv "MPLBACKEND" "TkAgg")

;; Set color for most programs
;; Can't figure out how to set this dynamicall eg $(vivid generate theme)
;; With theme being an option from `vivid themes'
;; https://github.com/sharkdp/vivid
;; (setenv "LS_COLORS" "")
