;;; An extension of 'powerline' theme from `eshell-git-prompt`
;;; NOTE: Requires package `pyvenv' from melpa if you want virtualenv info

(defmacro with-read-only-face (str &rest properties)
  "Add face PROPERTIES to STR."
  (declare (indent 1))
  `(propertize ,str 'face (list ,@properties) 'read-only t 'rear-nonsticky '(read-only)))

(defun eshell-git-prompt-powerline-pyvenv ()
  "Display virualenv info if available"
  (let ((segment-separator "\xe0b0")
        (branch            "\xe0a0")
        (detached          "\x27a6")
        (cross             "\x2718")
        dir git git-face)
    (setq dir
          (propertize
           (concat
            " "
            (unless (eshell-git-prompt-exit-success-p)
              (concat cross " "))
            (eshell-git-prompt-powerline-dir)
            " ")
           'face 'eshell-git-prompt-powerline-dir-face 'read-only t))
    (setq git
          (when (eshell-git-prompt--git-root-dir)
            (setq git-face
                  (if (eshell-git-prompt--collect-status)
                      'eshell-git-prompt-powerline-not-clean-face
                    'eshell-git-prompt-powerline-clean-face))
            (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
            (propertize
                (concat " "
                        (-if-let (branch-name eshell-git-prompt-branch-name)
                            (concat branch " " branch-name)
                          (concat detached " "(eshell-git-prompt--commit-short-sha)))
                        " ")
              'face git-face 'read-only t)))
    (concat
     (when (and (boundp 'pyvenv-virtual-env-name) (getenv "VIRTUAL_ENV"))
       (concat
        (with-read-only-face pyvenv-virtual-env-name
          :background "#5B3758")
        (with-read-only-face segment-separator
          :foreground "#5B3758"
          :background (face-background 'eshell-git-prompt-powerline-dir-face))))
     (if git
         (concat dir
                 (with-read-only-face segment-separator
                   :foreground (face-background 'eshell-git-prompt-powerline-dir-face)
                   :background (face-background git-face))
                 git
                 (with-read-only-face segment-separator
                   :foreground (face-background git-face)))
       (concat dir
               (with-read-only-face segment-separator
                 :foreground (face-background 'eshell-git-prompt-powerline-dir-face))))
     (with-read-only-face (concat "\n" segment-separator)
       :foreground (face-background 'eshell-git-prompt-powerline-dir-face))
     (propertize "$" 'invisible t 'read-only t)
     (with-read-only-face " "))))

(defconst eshell-git-prompt-powerline-pyvenv-regexp "^[^$\n]*\\\$ ")

(provide 'powerline-with-pyvenv)
