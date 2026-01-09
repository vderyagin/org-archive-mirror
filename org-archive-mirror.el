;;; org-archive-mirror.el --- A tool for archiving org subtrees mirroring original structure -*- lexical-binding: t -*-

;; Copyright (C) 2016-2026 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 30 Jun 2016
;; Version: 0.5.0

;; Package-Requires: ()

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-macs)
(require 'org-archive)
(require 'org-element)
(require 'ol)
(require 'subr-x)
(require 'cl-lib)

(defgroup org-archive-mirror nil
  "A tool for archiving org subtrees mirroring original structure"
  :group 'org-archive)

(defcustom org-archive-mirror-archive-file-function
  (lambda ()
    (car
     (org-archive--compute-location
      (or (org-entry-get nil "ARCHIVE" 'inherit)
          org-archive-location))))
  "A function used to determine the location of archive file. Is
invoked at the entry to be archived. Default implementation
uses `org-archive-location' to determine the file."
  :group 'org-archive-mirror
  :type 'function)

(defconst org-archive-mirror--progress-cookie-re
  (rx "["
      (or (any "/" "%")
          (and (+ digit) "%")
          (and (+ digit) "/" (+ digit)))
      "]")
  "Regexp matching Org progress cookies without surrounding whitespace.")

(defmacro org-archive-mirror--without-element-cache (&rest body)
  `(let ((org-element-use-cache nil))
     ,@body))

(defun org-archive-mirror--leaf-heading-p ()
  "True if heading at point does not have any child headings."
  (org-with-wide-buffer
   (when-let* ((result (org-archive-mirror--headline-and-path-at-point))
               (headline (car result)))
     (not (org-archive-mirror--headline-has-children-p headline)))))

(defun org-archive-mirror--maybe-insert-newline ()
  (unless (bolp)
    (insert "\n")))

(defun org-archive-mirror--strip-progress-cookie (title)
  (when title
    (replace-regexp-in-string
     (concat "[ \t]+" org-archive-mirror--progress-cookie-re "\\'")
     ""
     title)))

(defun org-archive-mirror--normalize-heading-title (title)
  (when title
    (org-archive-mirror--strip-progress-cookie
     (org-link-display-format (string-trim title)))))

(defun org-archive-mirror--headline-title (headline)
  (org-archive-mirror--normalize-heading-title
   (org-element-property :raw-value headline)))

(defun org-archive-mirror--normalize-outline (outline)
  (mapcar #'org-archive-mirror--normalize-heading-title outline))

(defun org-archive-mirror--update-outline-stack (stack level title)
  (let* ((target (max 0 (1- level)))
         (parent (cl-loop for i from 0 below target
                          for item in stack
                          collect item)))
    (append parent (list title))))

(defun org-archive-mirror--map-headlines-with-path (fn &optional ast)
  "Call FN with (HEADLINE PATH) for each headline in current buffer.
PATH is the normalized outline path to HEADLINE (list of titles).
If FN returns non-nil, stop traversal and return that value.
Traversal visits all headlines when FN always returns nil.
Optional AST is a pre-parsed org-element tree to avoid re-parsing."
  (let ((stack nil)
        (tree (or ast (org-archive-mirror--without-element-cache
                       (org-element-parse-buffer))))
        result)
    (org-element-map tree 'headline
      (lambda (headline)
        (let* ((level (org-element-property :level headline))
               (title (org-archive-mirror--headline-title headline)))
          (setq stack (org-archive-mirror--update-outline-stack stack level title))
          (setq result (funcall fn headline stack))
          (when result result)))
      nil 'first-match)
    result))

(defun org-archive-mirror--for-each-headline-with-path (fn &optional ast)
  "Call FN with (HEADLINE PATH) for each headline in current buffer.
PATH is the normalized outline path to HEADLINE (list of titles).
Always visits all headlines. Return value is undefined.
Optional AST is a pre-parsed org-element tree to avoid re-parsing."
  (let ((stack nil)
        (tree (or ast (org-archive-mirror--without-element-cache
                       (org-element-parse-buffer)))))
    (org-element-map tree 'headline
      (lambda (headline)
        (let* ((level (org-element-property :level headline))
               (title (org-archive-mirror--headline-title headline)))
          (setq stack (org-archive-mirror--update-outline-stack stack level title))
          (funcall fn headline stack)))
      nil nil)))

(defun org-archive-mirror--find-headline-by-outline (outline &optional ast)
  (let ((normalized (org-archive-mirror--normalize-outline outline)))
    (org-archive-mirror--map-headlines-with-path
     (lambda (headline path)
       (when (equal path normalized) headline))
     ast)))

(defun org-archive-mirror--get-full-outline-path ()
  (when-let* ((result (org-archive-mirror--headline-and-path-at-point)))
    (cadr result)))

(defun org-archive-mirror--headline-and-path-at-point ()
  (let ((pos (point))
        result)
    (org-archive-mirror--for-each-headline-with-path
     (lambda (headline path)
       (let ((begin (org-element-property :begin headline))
             (end (org-element-property :end headline)))
         (when (and (<= begin pos) (< pos end))
           (setq result (list headline path))))))
    result))

(defun org-archive-mirror--insert-outline (outline)
  "Make sure org outline OUTLINE exists in current buffer."
  (org-with-wide-buffer
   (cl-loop for level from 1 to (length outline)
            for item in outline
            for prefix = (seq-take outline level)
            for parent-outline = (butlast prefix)
            for full-heading = (format "%s %s" (make-string level ?*) item)
            unless (org-archive-mirror--find-headline-by-outline prefix)
            do
            (let* ((parent-headline (and parent-outline
                                         (org-archive-mirror--find-headline-by-outline parent-outline)))
                   (insert-position (if parent-headline
                                        (org-element-property :end parent-headline)
                                      (point-max)))
                   (at-eobp (= insert-position (point-max)))
                   (had-trailing-newline (and at-eobp
                                              (> (point-max) (point-min))
                                              (eq (char-before) ?\n))))
              (goto-char insert-position)
              (org-archive-mirror--maybe-insert-newline)
              (insert full-heading)
              (unless (looking-at-p "\n\|\'")
                (insert "\n"))
              (when (and at-eobp (not had-trailing-newline) (eq (char-before) ?\n))
                (delete-char -1))))))


(defun org-archive-mirror--heading-location (outline &optional ast)
  (when outline
    (org-with-wide-buffer
     (when-let* ((headline (org-archive-mirror--find-headline-by-outline outline ast)))
       (org-element-property :begin headline)))))

(defun org-archive-mirror--heading-duplicated-p (outline &optional ast)
  (when outline
    (let ((normalized (org-archive-mirror--normalize-outline outline))
          (occurrences 0))
      (org-with-wide-buffer
       (org-archive-mirror--map-headlines-with-path
        (lambda (_headline path)
          (when (equal path normalized)
            (setq occurrences (1+ occurrences))
            (when (> occurrences 1) t)))
        ast)
       (> occurrences 1)))))

(defun org-archive-mirror--outline-has-children-p (outline &optional ast)
  (let ((normalized (org-archive-mirror--normalize-outline outline)))
    (org-with-wide-buffer
     (org-archive-mirror--map-headlines-with-path
      (lambda (headline path)
        (when (equal path normalized)
          (org-archive-mirror--headline-has-children-p headline)))
      ast))))

(defun org-archive-mirror--narrow-to-parent (outline)
  "If heading corresponding to OUTLINE has parent, narrow to it's subtree.
Do nothing if outline is on top level or does not exist."
  (when-let* ((parent-outline (butlast outline))
              (parent-location (org-archive-mirror--heading-location parent-outline)))
    (goto-char parent-location)
    (org-narrow-to-subtree)))

(defun org-archive-mirror--remove-heading-extract-children (point-or-marker)
  (let (children)
    (org-with-point-at point-or-marker
      (when-let* ((result (org-archive-mirror--headline-and-path-at-point))
                  (headline (car result)))
        (let* ((headline-begin (org-element-property :begin headline))
               (headline-end (org-element-property :end headline))
               (first-child (org-archive-mirror--first-child-headline headline))
               (children-begin (when first-child
                                 (org-element-property :begin first-child))))

          (when children-begin
            (setq children
                  (string-trim
                   (buffer-substring-no-properties children-begin headline-end)))
            (delete-region children-begin headline-end))
          (delete-region headline-begin (or children-begin headline-end))
          (goto-char headline-begin)
          (while (looking-at-p "\n")
            (delete-char 1)))))
    children))

(defun org-archive-mirror--direct-child-outlines (parent-outline &optional ast)
  (let ((normalized (org-archive-mirror--normalize-outline parent-outline))
        (children nil))
    (org-archive-mirror--for-each-headline-with-path
     (lambda (_headline path)
       (when (and (= (length path) (1+ (length normalized)))
                  (equal (butlast path) normalized))
         (push path children)))
     ast)
    (nreverse children)))

(defun org-archive-mirror--deduplicate-children (parent-outline)
  (when parent-outline
    (org-with-wide-buffer
     (dolist (child-outline (org-archive-mirror--direct-child-outlines parent-outline))
       (org-archive-mirror--deduplicate-heading child-outline)))))

(defun org-archive-mirror--insert-content-at-heading (headline content)
  (when content
    (let* ((first-child (org-archive-mirror--first-child-headline headline))
           (insert-pos (if first-child
                           (org-element-property :begin first-child)
                         (or (org-element-property :contents-end headline)
                             (org-element-property :end headline)))))
      (goto-char insert-pos)
      (org-archive-mirror--maybe-insert-newline)
      (insert content "\n"))))

(defun org-archive-mirror--deduplicate-heading (outline)
  (save-excursion
    (save-restriction
      (when (and (org-archive-mirror--heading-duplicated-p outline)
                 (org-archive-mirror--outline-has-children-p outline))
        (cl-loop initially (org-archive-mirror--narrow-to-parent outline)
                 for first-headline = (org-with-wide-buffer
                                       (org-archive-mirror--find-headline-by-outline outline))
                 for first-instance = (org-element-property :begin first-headline)
                 for content = (org-archive-mirror--remove-heading-extract-children first-instance)
                 for second-headline = (org-with-wide-buffer
                                        (org-archive-mirror--find-headline-by-outline outline))
                 do (org-archive-mirror--insert-content-at-heading second-headline content)
                 while (org-archive-mirror--heading-duplicated-p outline)
                 finally (org-archive-mirror--deduplicate-children outline))))))

(defun org-archive-mirror--get-archive-file ()
  (when-let* ((file (funcall org-archive-mirror-archive-file-function)))
    (when (string= file (buffer-file-name))
      (user-error "org-archive-mirror: archive file can't match the original file"))
    file))

(defun org-archive-mirror--archive-subtree ()
  (let* ((outline-path (org-archive-mirror--get-full-outline-path))
         (parent-outline-path (butlast outline-path))
         (archive-file (org-archive-mirror--get-archive-file))
         (archive-buffer (or (find-buffer-visiting archive-file)
                             (find-file-noselect archive-file)))
         (org-archive-location (format
                                "%s::%s"
                                archive-file
                                (if parent-outline-path
                                    (format
                                     "%s %s"
                                     (make-string (length parent-outline-path) ?*)
                                     (org-last parent-outline-path))
                                  ""))))

    (with-current-buffer archive-buffer
      ;; make sure archive buffer contains relevant outline
      (org-archive-mirror--insert-outline parent-outline-path)
      ;; if entry to be archived has a parent, narrow archive buffer
      ;; correspondingly, so that archived entry does not end up in wrong
      ;; place
      (when-let* ((parent-heading (org-archive-mirror--heading-location parent-outline-path)))
        (goto-char parent-heading)
        (org-narrow-to-subtree)))

    ;; do the archiving
    (org-archive-subtree)

    (with-current-buffer archive-buffer
      ;; get rid of any previous narrowing
      (widen)
      ;; clean up duplications, if any were introduced
      (org-archive-mirror--deduplicate-heading outline-path))))

;;;###autoload
(defun org-archive-mirror-subtree ()
  (interactive)
  ;; Code for handling headings in region adapted from `org-archive-subtree' function
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((scope (if (eq org-loop-over-headlines-in-active-region 'start-level)
                       'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (save-mark-and-excursion
          (let* ((bounds (org-archive-mirror--normalized-region-bounds))
                 (beg (car bounds))
                 (end (cdr bounds)))
            (goto-char beg)
            (set-mark end)
            (activate-mark)
            (org-map-entries
             '(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                     (org-archive-mirror--archive-subtree))
             org-loop-over-headlines-in-active-region
             scope
             (if (org-invisible-p) (org-end-of-subtree nil t))))))
    (org-archive-mirror--archive-subtree)))

(defun org-archive-mirror--first-headline ()
  (org-archive-mirror--without-element-cache
   (org-element-map (org-element-parse-buffer) 'headline
     #'identity nil 'first-match)))

(defun org-archive-mirror--first-child-headline (headline)
  (org-element-map (org-element-contents headline) 'headline
    #'identity nil 'first-match))

(defun org-archive-mirror--headline-has-children-p (headline)
  (and (org-archive-mirror--first-child-headline headline) t))

(defun org-archive-mirror--around-empty-line-p (point)
  "Return t if POINT is on or adjacent to an empty line."
  (org-with-wide-buffer
   (goto-char point)
   (or (and (bolp) (eolp))
       (looking-at-p "\n\n")
       (and (>= (point) 2)
            (equal (buffer-substring-no-properties (- (point) 2) (point)) "\n\n")))))

(defun org-archive-mirror--includes-headings-p (beg end)
  (org-with-wide-buffer
   (save-restriction
     (narrow-to-region beg end)
     (org-archive-mirror--without-element-cache
      (org-element-map (org-element-parse-buffer) 'headline
        #'identity nil 'first-match)))))

(defun org-archive-mirror--headline-boundary (position boundary)
  (org-with-point-at position
    (if-let* ((result (org-archive-mirror--headline-and-path-at-point)))
        (let ((headline (car result)))
          (if (eq boundary 'begin)
              (org-element-property :begin headline)
            (org-element-property :end headline)))
      position)))

(defun org-archive-mirror--normalized-region-bounds ()
  (let* ((beg (region-beginning))
         (end (region-end))
         (normalized-beg (org-archive-mirror--headline-boundary beg 'begin))
         (normalized-end (org-archive-mirror--headline-boundary end 'end)))
    (cons normalized-beg (max normalized-beg normalized-end))))

(defun org-archive-mirror--format-plain-archive (content)
  "Format CONTENT as an archived plain text block with timestamp.
Returns a string with the content wrapped in an :ARCHIVED: drawer
containing an inactive timestamp."
  (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (format ":ARCHIVED:\n%s\n\n%s\n:END:" timestamp content)))

(defun org-archive-mirror-plain ()
  (interactive)

  (unless (region-active-p)
    (user-error "Region must be selected"))

  (when (org-archive-mirror--includes-headings-p (region-beginning) (region-end))
    (user-error "Can not archive headings this way"))

  (unless (and (org-archive-mirror--around-empty-line-p (region-beginning))
               (org-archive-mirror--around-empty-line-p (region-end)))
    (user-error "Region has to begin and end with an empty line"))

  (let* ((outline-path (unless (zerop (org-outline-level))
                         (org-archive-mirror--get-full-outline-path)))
         (archived-content (delete-and-extract-region (region-beginning) (region-end)))
         (formatted-content (org-archive-mirror--format-plain-archive (string-trim archived-content)))
         (archive-file (org-archive-mirror--get-archive-file))
         (archive-buffer (or (find-buffer-visiting archive-file)
                             (find-file-noselect archive-file))))
    (with-current-buffer archive-buffer
      (org-with-wide-buffer
       (let ((insert-position
              (if outline-path
                  (progn
                    (org-archive-mirror--insert-outline outline-path)
                    (when-let* ((headline (org-archive-mirror--find-headline-by-outline outline-path)))
                      (if-let* ((child (org-archive-mirror--first-child-headline headline)))
                          (org-element-property :begin child)
                        (or (org-element-property :contents-end headline)
                            (org-element-property :end headline)))))
                (if-let* ((first-headline (org-archive-mirror--first-headline)))
                    (org-element-property :begin first-headline)
                  (point-max)))))
         (goto-char insert-position)
         (org-archive-mirror--maybe-insert-newline)
         (insert formatted-content "\n"))))))

;;;###autoload
(defun org-archive-mirror-dwim ()
  "Archive the region or subtree at point, depending on context.
If region is active and contains headings, archive those headings.
If region is active with plain text only, archive as plain text.
If no region, archive the subtree at point."
  (interactive)
  (cond
   ((region-active-p)
    (if (org-archive-mirror--includes-headings-p (region-beginning) (region-end))
        (org-archive-mirror-subtree)
      (org-archive-mirror-plain)))
   ((org-at-heading-p)
    (org-archive-mirror-subtree))
   (t
    (user-error "org-archive-mirror: not on a heading"))))

(provide 'org-archive-mirror)

;;; org-archive-mirror.el ends here
