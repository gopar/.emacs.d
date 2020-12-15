;;; Stolen from http://xenodium.com/eshell-pcomplete-company-completion/
;;; Provides company completion in eshell
(require 'cl-lib)
(require 'company)
(require 'dash)
(require 'pcomplete)
(require 's)

(defun company-pcomplete--overlap-tail (a b)
  "When A is \"SomeDev\" and B is \"Developer\\n\", return \"eloper\"."
  (let ((prefix a)
        (remaining nil))
    (while (and (not remaining) (> (length prefix) 0))
      (when (s-starts-with? prefix b)
        (setq remaining (substring b (length prefix))))
      (setq prefix (substring prefix 1)))
    remaining))

(defun company-pcomplete--candidates (prefix)
  "Get candidates for PREFIX company completion using `pcomplete'."
  ;; When prefix is: "~/Down" and completion is "Downloads\n", need
  ;; to find common string and join into "~/Downloads/".
  (let (ret-val)
    (setq ret-val (catch 'pcompleted
                    (-map (lambda (item)
                            (if (s-starts-with? prefix item)
                                item
                              (concat prefix (company-pcomplete--overlap-tail prefix item))))
                          (all-completions prefix (pcomplete-completions)))))
    (if (listp ret-val) ret-val nil)))

;;;###autoload
(defun company-pcomplete (command &optional arg &rest ignored)
  "Complete using pcomplete. See `company''s COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (company-grab-symbol))
    (candidates
     (company-pcomplete--candidates arg))))


(provide 'company-pcomplete)
