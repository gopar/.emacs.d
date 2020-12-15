;;; gcloud.el ---
;;
;; Filename: gcloud.el
;; Description:
;; Author: Daniel Gopar
;; Maintainer:
;; Created: Sun Jan 29 21:45:12 2017 (-0800)
;; Version:
;; Package-Requires: ()
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(defcustom gcloud-cmd "gcloud"
  "Path to `gcloud' executable"
  :type 'string
  :safe 'stringp)
(make-variable-buffer-local 'gcloud-cmd)

(defcustom gcloud-dev-appserver "dev_appserver.py"
  "Path to `dev_appserver.py' if it's not in Emacs path"
  :type 'string
  :safe 'stringp)
(make-variable-buffer-local 'gcloud-dev-appserver)

(defcustom gcloud-dev-appserver-host "localhost"
  "Host name to which app server should bind"
  :type 'string
  :safe 'stringp)
(make-variable-buffer-local 'gcloud-dev-appserver-host)

(defcustom gcloud-dev-appserver-port 8080
  "Port which the server should bind to"
  :type 'integer
  :safe 'integerp)
(make-variable-buffer-local 'gcloud-dev-appserver-port)

(defcustom gcloud-dev-appserver-admin-host "localhost"
  "Host name to which admin server should bind"
  :type 'string
  :safe 'stringp)
(make-variable-buffer-local 'gcloud-dev-appserver-admin-host)

(defcustom gcloud-dev-appserver-admin-port 8000
  "Port which the admin server should bind to"
  :type 'integer
  :safe 'integerp)
(make-variable-buffer-local 'gcloud-dev-appserver-admin-port)

(defcustom gcloud-app-yaml nil
  "Path to `app.yaml'"
  :type 'string
  :safe 'stringp)
(make-variable-buffer-local 'gcloud-app-yaml)

(defcustom gcloud-buffer-name "*gcloud dev_server*"
  "Name to use when running server"
  :type 'string
  :safe 'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gcloud-run-devserver ()
  (interactive)
  (let ((cmd gcloud-dev-appserver)
        (app-yaml (or gcloud-app-yaml (locate-dominating-file default-directory "app.yaml"))))
    (when (null app-yaml)
      (error "Cannot find path to `app.yaml'. Is it set?"))
    (when (get-buffer gcloud-buffer-name)
      (kill-buffer gcloud-buffer-name))
    (setq cmd (concat cmd
                      " --host " gcloud-dev-appserver-host
                      " --port " (number-to-string gcloud-dev-appserver-port)
                      " --admin_host " gcloud-dev-appserver-admin-host
                      " --admin_port " (number-to-string gcloud-dev-appserver-admin-port)
                      " " app-yaml))
    (compile cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer gcloud-buffer-name))))

(defun gcloud-deploy ()
  "Starts the deployment process of application. Creates a
commit buffer so that user can answer prompt questions."
  (interactive)
  (when (null gcloud-app-yaml)
    (error "Cannot find path to `app.yaml'. Is it set?"))
  (compile (concat "gcloud app deploy " gcloud-app-yaml  "app.yaml") t))

(defun gcloud-app-browse ()
  "Start a browser to see live app"
  (interactive)
  (start-process "gcloud" nil shell-file-name shell-command-switch
                 (concat gcloud-cmd " app browse")))

(defun gcloud-version-delete ()
  "Shows all the versions available to delete"
  (interactive)
  (shell-command-to-string (concat gcloud-cmd " app versions list") "*gcloud list*")
  (with-current-buffer "*gcloud list*"
    (goto-char (point-min))
    (while (< (point) (point-max))
      (insert "  ")
      (next-line)
      (beginning-of-line 1)))
  (switch-to-buffer "*gcloud list*")
  (read-only-mode 1))

;;;###autoload
(define-minor-mode gcloud-mode
  "Minor mode for controlling gcloud commands"
  :lighter " GC")

(provide 'gcloud)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gcloud.el ends here
