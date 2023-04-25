(defvar gopar-pair-programming nil)

(defun gopar/pair-programming ()
  "Poor mans minor mode for setting up things that i like to make pair programmikng easier."
  (interactive)
  (if gopar-pair-programming
      (progn
        ;; dont display line numbers
        (global-display-line-numbers-mode -1)
        ;; disable all themes change to a friendlier theme
        (mapcar 'disable-theme custom-enabled-themes)
        (load-theme 'tao-yin)
        (setq gopar-pair-programming nil))

    (progn
      ;; display line numbers
      (global-display-line-numbers-mode)
      ;; disable all themes change to a friendlier theme
      (mapcar 'disable-theme custom-enabled-themes)
      (load-theme 'manoj-dark)
      (setq gopar-pair-programming t))))

(provide 'pair-progrmaming.el)
