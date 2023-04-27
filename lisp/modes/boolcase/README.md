boolcase mode
==============

Minor mode to automatically capitalize true and false in languages such as
python.

```lisp
(require 'boolcase)
(add-hook 'python-mode-hook '(lambda () (boolcase-mode 1)))
```
