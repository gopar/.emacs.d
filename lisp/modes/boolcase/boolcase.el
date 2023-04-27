;;; boolcase.el --- Automatically Capitalize True and False
;;
;; Filename: boolcase.el
;; Description: Automatically Capitalize True and False
;; Author: Daniel Gopar <gopardaniel@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Add (require 'boolcase-mode) to your init.el file
;; and add hooks to languages you want to automatically capitalize
;; True and False. Such as Python:
;;
;; (add-hook 'python-mode '(lambda () (boolcase-mode 1)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst boolcase-mode-words '("true" "false" "none")
  "Words to capitalize such as true and false.")

(defun boolcase-mode-check ()
  "Check if we capitalize or not"
  ;; If it's "e"
  (if (= last-command-event 101)
      (boolcase-mode-fix)))

(defun boolcase-mode-fix ()
  ;; (when (member (thing-at-point 'sexp) boolcase-mode-words)
  ;;   (capitalize-word -1))
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-sexp) (point)))
    (when (member (current-kill 0) boolcase-mode-words)
      (capitalize-word 1)))
  ;; Remove element we just saved to kill-ring
  (setq kill-ring (cdr kill-ring)))

;;;###autoload
(define-minor-mode boolcase-mode
  "Automatically capitalize booleans"
  :lighter " BC"
  (if boolcase-mode
      (add-hook 'post-self-insert-hook
                'boolcase-mode-check nil t)
    (remove-hook 'post-self-insert-hook
                 'boolcase-mode-check t)))

(provide 'boolcase)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boolcase.el ends here
