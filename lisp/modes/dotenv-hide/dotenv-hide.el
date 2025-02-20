;;; dotenv-hide.el --- Hide sensitive values in .env files -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dotenv-mode "1.0.0"))
;; Keywords: convenience
;; URL: https://github.com/yourusername/dotenv-hide

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to hide sensitive values in .env files.
;; It automatically hides values when opening a .env file and provides
;; a way to toggle their visibility.  Values are revealed when point is
;; over them and hidden again when point moves away.

;;; Code:

(require 'dotenv-mode)

(defgroup dotenv-hide nil
  "Hide sensitive values in .env files."
  :group 'dotenv-mode)

(defcustom dotenv-hide-elements t
  "Non-nil means hide values in .env files by default."
  :type 'boolean
  :group 'dotenv-hide)

(defvar dotenv-hide-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'dotenv-hide-toggle-value-at-point)
    map)
  "Keymap for dotenv-hide mode.")

;;;###autoload
(define-minor-mode dotenv-hide-mode
  "Minor mode for hiding values in .env files."
  :lighter " DotenvHide"
  :keymap dotenv-hide-map
  (if dotenv-hide-mode
      (progn
        (dotenv-hide-values (point-min) (point-max))
        (reveal-mode))
    (remove-overlays (point-min) (point-max) 'dotenv-hide t)))

(defun dotenv-hide--toggle-display (overlay &optional _)
  "Toggle the display of the value under OVERLAY.
The second argument _ is ignored and exists for compatibility with reveal-mode."
  (overlay-put overlay 'display
               (if (overlay-get overlay 'hidden)
                   nil
                 (propertize "****" 'face 'font-lock-doc-face)))
  (overlay-put overlay 'hidden
               (not (overlay-get overlay 'hidden))))

(defun dotenv-hide-toggle-value-at-point ()
  "Toggle the visibility of the value at point."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (dolist (overlay overlays)
      (when (overlay-get overlay 'dotenv-hide)
        (dotenv-hide--toggle-display overlay)))))

(defun dotenv-hide-values (start end)
  "Hide values in the region between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward "^\\([^#\n]*?\\)=" nil t)
        (let* ((key-end (match-end 0))
               (line-end (line-end-position))
               (value-text (buffer-substring-no-properties key-end line-end))
               (trimmed-value-text (string-trim value-text)))
          (unless (or (string-empty-p trimmed-value-text) (string= trimmed-value-text "\"\"") (string= trimmed-value-text "''"))
            (let ((overlay (make-overlay key-end line-end)))
              (overlay-put overlay 'dotenv-hide t)
              (overlay-put overlay 'display
                          (propertize "****" 'face 'font-lock-doc-face))
              (overlay-put overlay 'hidden t)
              (overlay-put overlay 'reveal-toggle-invisible
                          #'dotenv-hide--toggle-display))))))))

;;;###autoload
(add-hook 'dotenv-mode-hook #'dotenv-hide-mode)

(provide 'dotenv-hide)

;;; dotenv-hide.el ends here
