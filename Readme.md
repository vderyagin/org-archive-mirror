# org-archive-mirror #

This package provides an archiving functionality for org-mode, which makes sure archived entry is under the same heading hierarchy as it was in its original place.

## Installation ##

## Configuration ##

```lisp
(custom-set-variables
 '(org-archive-default-command #'org-archive-mirror-subtree))

(with-eval-after-load 'org-agenda
  (fset 'org-agenda-archive #'org-agenda-archive-default))

(define-key org-mode-map (kbd "C-c $") #'org-archive-subtree-default)
```

By default an archive file is determined according to `org-archive-location` variable, but if you need to employ more complex logic then it allows, you can set `org-archive-mirror-archive-file-function` to a no-argument function, which, when invoked at the original heading location, must return a path to archive file.

### Jumping between org file and its archive

Emacs got a "sibling file" concept, most commonly referred to a test or a header file associated with source file (or vice versa). This faciilty enables quick switching between org file ant its archive:

```lisp
;;; adjust code as needed
;;; `org-directory' must be bound appropriately before evaluating this

(add-to-list
 'find-sibling-rules
 ;; file.org → archive/file.org.gpg
 (list
  (rx-to-string (list 'and org-directory '(group "/" (+ not-newline) ".org") '(optional ".gpg") 'string-end) 'no-group)
  (rx-to-string (list 'and org-directory "/archive" '(backref 1) ".gpg" 'string-end) 'no-group)))

(add-to-list
 'find-sibling-rules
 ;; archive/file.org.gpg → file.org
 (list
  (rx-to-string (list 'and org-directory "/archive" '(group "/" (+ not-newline) ".org") ".gpg" 'string-end) 'no-group)
  (rx-to-string (list 'and org-directory '(backref 1) '(optional ".gpg") 'string-end) 'no-group))))
```
