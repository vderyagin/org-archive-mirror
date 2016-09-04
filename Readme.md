# org-archive-subtree-preserve-structure #

This package provides an archiving function for org-mode, which makes sure
archived entry is under the same heading hierarchy as it was in its original
place. I wrote this because configurability provided by org-archive is not
enough to achieve this result.

## Installation ##

## Configuration ##

```lisp
(custom-set-variables
 '(org-archive-default-command #'org-archive-subtree-preserve-structure))

(with-eval-after-load 'org-agenda
  (fset 'org-agenda-archive #'org-agenda-archive-default))

(define-key org-mode-map (kbd "C-c $") #'org-archive-subtree-default)
```

By default an archive file is determined according to `org-archive-location`
variable, but if you need to employ more complex logic then it allows, you can
set `org-archive-subtree-preserve-structure-file-function` to a no-argument
function, which, when invoked at the original heading location, must return a
path to archive file.
