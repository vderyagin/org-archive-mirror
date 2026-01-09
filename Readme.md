# org-archive-mirror #

This package provides an archiving functionality for org-mode, which makes sure archived entry is under the same heading hierarchy as it was in its original place.

## Installation & configuration example (use-package, straight.el) ##

```lisp
(use-package org-archive
  :defer t

  :custom
  (org-archive-default-command #'org-archive-mirror-subtree)

  :init
  (with-eval-after-load 'org-agenda
    (fset 'org-agenda-archive #'org-agenda-archive-default)))

(use-package org-archive-mirror
  :straight (:host github :repo "vderyagin/org-archive-mirror")
  :after org-archive

  :bind (:map org-mode-map
              ("C-c $" . org-archive-mirror-dwim)))
```

## Commands ##

### `org-archive-mirror-dwim` ###

The main entry point. Context-aware archiving command that does the right thing based on the current state:

- If region is active and contains headings: archives those headings (same as `org-archive-mirror-subtree`)
- If region is active with plain text only: archives the text as plain content (same as `org-archive-mirror-plain`)
- If no region and point is on a heading: archives the subtree at point

### `org-archive-mirror-subtree` ###

Archives the subtree at point (or selected headings in active region) to the archive file, preserving the original heading hierarchy. The archived entry appears under the same parent structure in the archive file as it had in the source file.

### `org-archive-mirror-plain` ###

Archives a selected region of plain text (non-heading content) to the archive file. The region must:
- Be active (text must be selected)
- Not contain any headings
- Begin and end at or adjacent to an empty line

The archived content is wrapped in an `:ARCHIVED:` drawer with a timestamp, and placed under the corresponding heading hierarchy in the archive file (if the region was under a heading in the source).

## Configuration ##

By default an archive file is determined according to `org-archive-location` variable, but if you need to employ more complex logic then it allows, you can set `org-archive-mirror-archive-file-function` to a no-argument function, which, when invoked at the original heading location, must return a path to archive file.

### Jumping between org file and its archive

Since version 29.1 Emacs got a "sibling file" concept, something like a test or a header file associated with source file (or vice versa), and a `find-sibling-file` command for switching to a sibling of current file. This facility enables quick switching between org file and its archive:

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

## Development ##

This project uses [Eldev](https://github.com/emacs-eldev/eldev) for development.

```sh
eldev compile --force-all
eldev test
```
