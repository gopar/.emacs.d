;;; em-compile.el --- Defer commands to compile buffer
;;
;; Filename: em-compile.el
;; Description:
;; Author: Daniel Gopar
;; Maintainer:
;; Created: Thu Dec 10 18:06:06 2020 (-0800)
;; Version: 0.1
;; Package-Requires: (alert) (s)
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  em-compile tries to determine what commands it should defer to a
;;  compilation buffer, and alerts you when it's done. It determines
;;  which commands it should defer by keeping track of how long each
;;  command took to finish. By default if it takes more than 5 seconds
;;  then it will start to defer that command whenever it's invoke via
;;  eshell. You can of course customize the amount of seconds or offer
;;  a list of always defer or not defer.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'esh-mode)
  (require 'eshell))

(require 'esh-util)

;;;###autoload
(progn
(defgroup eshell-compile nil
  "This module allows you to defer commands to a compilation buffer,
and be notified of when it's done."
  :tag "Smart deferment of commands"
  :group 'eshell-module))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizeable section
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom eshell-compile-threshold 5
  "The amount of seconds a command has to pass in order to be
deferred to a compile buffer on next invocation."
  :group 'eshell-compile
  :type 'integer)

(defcustom eshell-compile-deferred-list nil
  "A list of commands that will be deferred if they are encountered"
  :group 'eshell-compile
  :type 'list)

(defcustom eshell-compile-allow-list nil
  "A list of commands that will never be deferred even
if they go over `eshell-compile-threshold'.

Commands must be the *entire* command. eg.

- python script.py
- python (To not defer when starting the repl via eshell)
- gcc main.cpp
- curl url"
  :group 'eshell-compile
  :type 'list)

(defcustom eshell-compile-success-sound (when load-file-name
                                          (concat (file-name-directory load-file-name)
                                                  "resources/chime.wav"))
  "The path to a sound file that's to be played when the compilation buffer ends without error."
  :group 'eshell-compile
  :type 'file)

(defcustom eshell-compile-failure-sound (when load-file-name
                                          (concat (file-name-directory load-file-name)
                                                  "resources/boing.wav"))
  "The path to a sound file that's to be played when the compilation buffer ends with an error."
  :group 'eshell-compile
  :type 'file)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eshell-compile-load-hook nil
  "A hook that gets run when `eshell-compile' is loaded")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal vars
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eshell-compile-command-start-time nil
  "The time a command started")

(defvar eshell-compile-command-end-time nil
  "The time a command ended")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-compile--save-start-time ()
  (setq eshell-compile-command-start-time (current-time)))

(defun eshell-compile--calc-end-time ()
  (setq eshell-compile-command-end-time (current-time))
  (let (time-diff cmd args)
    (setq time-diff (time-convert
                     (time-since eshell-compile-command-start-time)
                     'integer))
    (when (>= time-diff eshell-compile-threshold)
      (setq args (eshell-stringify-list (flatten-tree eshell-last-arguments)))
      (setq cmd (format "%s %s" eshell-last-command-name (s-join " " args)))
      (add-to-list 'eshell-compile-deferred-list cmd))))

;; TODO: Need to output the compile buffer in eshell prompt
(defun eshell-compile--should-defer? (cmd args)
  "Determine whether to start a compilation buffer or not."
  (setq args (eshell-stringify-list (flatten-tree args)))
  (setq cmd (format "%s %s" cmd (s-join " " args)))

  (when (and (member cmd eshell-compile-deferred-list) (not (member cmd eshell-compile-allow-list)))
    (let* ((buffer-name (format "*%s*" cmd))
           (compilation-buffer-name-function (lambda (arg) buffer-name))
           (num-of-windows (count-windows)))
      (compile cmd)
      (with-current-buffer buffer-name
        (add-hook 'compilation-finish-functions 'eshell-compile--alert-when-finished nil t))
      (if (= 1 num-of-windows)
          (delete-windows-on buffer-name)
        (switch-to-prev-buffer (get-buffer-window buffer-name)))
      (eshell-printn (get-buffer buffer-name)))
    t))


(defun eshell-compile--alert-when-finished (buffer string)
  (when (not (get-buffer-window buffer t))
    (if (string=  (s-trim string) "finished")
        (progn
          (alert "Success!")
          (start-process-shell-command
           "em-compile-audio-player" nil (format "afplay %s" eshell-compile-success-sound)))
      (alert "Error Occurred")
      (start-process-shell-command
           "em-compile-audio-player" nil (format "afplay %s" eshell-compile-failure-sound)))))

(defun eshell-compile-initialize ()
  (add-hook 'eshell-named-command-hook 'eshell-compile--should-defer?)
  (add-hook 'eshell-pre-command-hook 'eshell-compile--save-start-time)
  (add-hook 'eshell-post-command-hook 'eshell-compile--calc-end-time))


(provide 'em-compile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; em-compile.el ends here
