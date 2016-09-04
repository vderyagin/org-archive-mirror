;;; org-archive-subtree-preserve-structure.el --- A tool for archiving org subtrees mirroring original structure -*- lexical-binding: t -*-

;; Copyright (C) 2016 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 30 Jun 2016
;; Version: 0.2.0

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

(eval-when-compile
  (require 'org-macs))

(require 'org)
(require 'org-archive)
(require 'subr-x)

(defgroup org-archive-subtree-preserve-structure nil
  "A tool for archiving org subtrees mirroring original structure"
  :group 'org-archive)

(defcustom org-archive-subtree-preserve-structure-file-function
  (lambda () (org-extract-archive-file (org-get-local-archive-location)))
  "A function used to determine the location of archive file. Is
invoked at the entry to be archived. Default implementation
uses `org-archive-location' to determine the file."
  :group 'org-archive-subtree-preserve-structure
  :type 'function)

(defun oasps/leaf-heading-p ()
  "True if heading at point does not have any child headings"
  (unless (zerop (org-outline-level))
    (org-with-wide-buffer
     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
       (outline-next-heading)
       (>= (point) subtree-end)))))

(defun oasps/maybe-insert-newline ()
  (unless (looking-back "\n\\|\\`" 1)
    (insert "\n")))

(defun oasps/get-full-outline-path ()
  (append (org-get-outline-path)
          (list (nth 4 (org-heading-components)))))

(defun oasps/goto-heading (text level)
  "If heading TEXT on level LEVEL exists, move point just past it
and return a truthy value, move to (point-max) and return nil otherwise"
  (cl-loop with re = (format org-complex-heading-regexp-format text)
           while (re-search-forward re nil 'noerror)
           thereis (= (length (match-string 1)) level)))

(defun oasps/insert-outline (outline)
  "Make sure org outline OUTLINE exists in current buffer"
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            for level from 1 to (length outline)
            for item in outline
            for full-heading = (format "%s %s" (make-string level ?*) item)
            unless (oasps/goto-heading item level)
            do
            (goto-char (point-max))
            (oasps/maybe-insert-newline)
            (insert full-heading)
            end
            do
            (org-back-to-heading 'invisible-ok)
            (org-narrow-to-subtree))))

(defun oasps/heading-location (outline)
  (org-with-wide-buffer
   (when outline
     (cl-loop initially (goto-char (point-min))
              for level from 1 to (length outline)
              for item in outline
              always (oasps/goto-heading item level)
              do
              (org-back-to-heading 'invisible-ok)
              (org-narrow-to-subtree)
              finally return (point)))))

(defun oasps/heading-duplicated-p (outline)
  (org-with-wide-buffer
   (cl-loop initially (if-let (parent (oasps/heading-location (butlast outline)))
                          (progn
                            (goto-char parent)
                            (org-narrow-to-subtree))
                        (goto-char (point-min)))
            with level = (length outline)
            with heading = (car (last outline))
            while (< (point) (point-max))
            count (oasps/goto-heading heading level)
            into heading-occurrences
            if (>= heading-occurrences 2) return t)))

(defun oasps/narrow-to-parent (outline)
  "If heading corresponding to OUTLINE has parent, narrow to it's subtree.
Do nothing if outline is on top level or does not exist."
  (when-let ((parent-outline (butlast outline))
             (parent-location (oasps/heading-location parent-outline)))
    (goto-char parent-location)
    (org-narrow-to-subtree)))

(defun oasps/remove-heading-extract-children (point-or-marker)
  (prog1 (org-with-point-at point-or-marker
           (let ((subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
             (outline-next-heading)
             (when (< (point) subtree-end)
               (string-trim (delete-and-extract-region (point) subtree-end)))))

    (org-with-point-at point-or-marker
      (delete-region (point) (save-excursion (org-end-of-subtree 'invisible-ok)))
      (while (looking-at "\n")
        (delete-char 1)))))

(defun oasps/deduplicate-children (parent-outline)
  (let ((parent (oasps/heading-location parent-outline)))
    (org-with-point-at parent
      (when (org-goto-first-child)
        (cl-loop for subtree-end = (org-with-point-at parent
                                     (org-end-of-subtree 'invisible-ok))
                 while (< (point) subtree-end)
                 do (oasps/deduplicate-heading (oasps/get-full-outline-path))
                 always (outline-get-next-sibling))))))

(defun oasps/insert-content (point-or-marker content)
  (when content
    (org-with-point-at point-or-marker
      (save-restriction
        (org-narrow-to-subtree)
        (outline-next-heading)
        (oasps/maybe-insert-newline)
        (insert content "\n")))))

(defun oasps/deduplicate-heading (outline)
  (save-excursion
    (save-restriction
      (when (and (org-with-point-at (oasps/heading-location outline)
                   (not (oasps/leaf-heading-p)))
                 (oasps/heading-duplicated-p outline))
        (cl-loop initially (oasps/narrow-to-parent outline)
                 for first-instance = (oasps/heading-location outline)
                 for content = (oasps/remove-heading-extract-children first-instance)
                 for second-instance = (oasps/heading-location outline)
                 do (oasps/insert-content second-instance content)
                 while (oasps/heading-duplicated-p outline)
                 finally (oasps/deduplicate-children outline))))))

(defun org-archive-subtree-preserve-structure-1 ()
  (let* ((outline-path (oasps/get-full-outline-path))
         (parent-outline-path (butlast outline-path))
         (archive-file (funcall org-archive-subtree-preserve-structure-file-function))
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
      (oasps/insert-outline parent-outline-path)
      ;; if entry to be archived has a parent, narrow archive buffer
      ;; correspondingly, so that archived entry does not end up in wrong
      ;; place
      (when-let (parent-heading (oasps/heading-location parent-outline-path))
        (goto-char parent-heading)
        (org-narrow-to-subtree)))

    ;; do the archiving
    (org-archive-subtree)

    (with-current-buffer archive-buffer
      ;; get rid of any previous narrowing
      (widen)
      ;; clean up duplications, if any were introduced
      (oasps/deduplicate-heading outline-path))))

;;;###autoload
(defun org-archive-subtree-preserve-structure ()
  (interactive)
  ;; Code for handling headings in region adapted from `org-archive-subtree' function
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((scope (if (eq org-loop-over-headlines-in-active-region 'start-level)
                       'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         '(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                 (org-archive-subtree-preserve-structure-1))
         org-loop-over-headlines-in-active-region
         scope
         (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (org-archive-subtree-preserve-structure-1)))

(provide 'org-archive-subtree-preserve-structure)

;;; org-archive-subtree-preserve-structure.el ends here
