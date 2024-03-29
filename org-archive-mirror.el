;;; org-archive-mirror.el --- A tool for archiving org subtrees mirroring original structure -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 30 Jun 2016
;; Version: 0.3.0

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
(require 'subr-x)

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

(defun org-archive-mirror--leaf-heading-p ()
  "True if heading at point does not have any child headings"
  (unless (zerop (org-outline-level))
    (org-with-wide-buffer
     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
       (outline-next-heading)
       (>= (point) subtree-end)))))

(defun org-archive-mirror--maybe-insert-newline ()
  (unless (save-excursion
            (re-search-backward "\n\\|\\`" (- (point) 1) 'no-error 1))
    (insert "\n")))

(defun org-archive-mirror--get-full-outline-path ()
  (let ((progress-indicator-regexp
         (rx (+ " ")
             "["
             (or (any "/" "%")                              ; "[/]" and "[%]" cases
                 (and (+ (any digit)) "%")                  ; "[x%]" case
                 (and (+ (any digit)) "/" (+ (any digit)))) ; "[x/y]" case
             "]" string-end)))
    (append (org-get-outline-path)
            (list (replace-regexp-in-string
                   progress-indicator-regexp ""
                   (nth 4 (org-heading-components)))))))

(defun org-archive-mirror--goto-heading (text level)
  "If heading TEXT on level LEVEL exists, move point just past it
and return a truthy value, move to (point-max) and return nil otherwise"
  (cl-loop with re = (format org-complex-heading-regexp-format text)
           while (re-search-forward re nil 'noerror)
           thereis (= (length (match-string 1)) level)))

(defun org-archive-mirror--insert-outline (outline)
  "Make sure org outline OUTLINE exists in current buffer"
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            for level from 1 to (length outline)
            for item in outline
            for full-heading = (format "%s %s" (make-string level ?*) item)
            unless (org-archive-mirror--goto-heading item level)
            do
            (goto-char (point-max))
            (org-archive-mirror--maybe-insert-newline)
            (insert full-heading)
            end
            do
            (org-back-to-heading 'invisible-ok)
            (org-narrow-to-subtree))))

(defun org-archive-mirror--heading-location (outline)
  (org-with-wide-buffer
   (when outline
     (cl-loop initially (goto-char (point-min))
              for level from 1 to (length outline)
              for item in outline
              always (org-archive-mirror--goto-heading item level)
              do
              (org-back-to-heading 'invisible-ok)
              (org-narrow-to-subtree)
              finally return (point)))))

(defun org-archive-mirror--heading-duplicated-p (outline)
  (org-with-wide-buffer
   (cl-loop initially (if-let (parent (org-archive-mirror--heading-location (butlast outline)))
                          (progn
                            (goto-char parent)
                            (org-narrow-to-subtree))
                        (goto-char (point-min)))
            with level = (length outline)
            with heading = (car (last outline))
            until (eobp)
            count (org-archive-mirror--goto-heading heading level)
            into heading-occurrences
            if (>= heading-occurrences 2) return t)))

(defun org-archive-mirror--narrow-to-parent (outline)
  "If heading corresponding to OUTLINE has parent, narrow to it's subtree.
Do nothing if outline is on top level or does not exist."
  (when-let ((parent-outline (butlast outline))
             (parent-location (org-archive-mirror--heading-location parent-outline)))
    (goto-char parent-location)
    (org-narrow-to-subtree)))

(defun org-archive-mirror--remove-heading-extract-children (point-or-marker)
  (prog1 (org-with-point-at point-or-marker
           (let ((subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
             (outline-next-heading)
             (when (< (point) subtree-end)
               (string-trim (delete-and-extract-region (point) subtree-end)))))

    (org-with-point-at point-or-marker
      (delete-region (point) (save-excursion (org-end-of-subtree 'invisible-ok)))
      (while (looking-at-p "\n")
        (delete-char 1)))))

(defun org-archive-mirror--deduplicate-children (parent-outline)
  (let ((parent (org-archive-mirror--heading-location parent-outline)))
    (org-with-point-at parent
      (when (org-goto-first-child)
        (cl-loop for subtree-end = (org-with-point-at parent
                                     (org-end-of-subtree 'invisible-ok))
                 while (< (point) subtree-end)
                 do (org-archive-mirror--deduplicate-heading (org-archive-mirror--get-full-outline-path))
                 always (org-get-next-sibling))))))

(defun org-archive-mirror--insert-content-at-heading (point-or-marker content)
  (when content
    (org-with-point-at point-or-marker
      (save-restriction
        (org-narrow-to-subtree)
        (outline-next-heading)
        (org-archive-mirror--maybe-insert-newline)
        (insert content "\n")))))

(defun org-archive-mirror--deduplicate-heading (outline)
  (save-excursion
    (save-restriction
      (when (and (org-with-point-at (org-archive-mirror--heading-location outline)
                   (not (org-archive-mirror--leaf-heading-p)))
                 (org-archive-mirror--heading-duplicated-p outline))
        (cl-loop initially (org-archive-mirror--narrow-to-parent outline)
                 for first-instance = (org-archive-mirror--heading-location outline)
                 for content = (org-archive-mirror--remove-heading-extract-children first-instance)
                 for second-instance = (org-archive-mirror--heading-location outline)
                 do (org-archive-mirror--insert-content-at-heading second-instance content)
                 while (org-archive-mirror--heading-duplicated-p outline)
                 finally (org-archive-mirror--deduplicate-children outline))))))

(defun org-archive-mirror--get-archive-file ()
  (when-let (file (funcall org-archive-mirror-archive-file-function))
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
      (when-let (parent-heading (org-archive-mirror--heading-location parent-outline-path))
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
        (org-map-entries
         '(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                 (org-archive-mirror--archive-subtree))
         org-loop-over-headlines-in-active-region
         scope
         (if (org-invisible-p) (org-end-of-subtree nil t))))
    (org-archive-mirror--archive-subtree)))

;;;###autoload
(defun org-archive-mirror-toggle ()
  (interactive)

  (unless buffer-file-name
    (user-error "Current buffer is not associated with file"))
  (unless (eq major-mode 'org-mode)
    (user-error "Can only be invoked from org-mode"))

  (if-let* ((other-file (if (org-archive-mirror--in-archive-p)
                            (org-archive-mirror--find-archive-source)
                          (org-archive-mirror--get-archive-file)))
            (other-file-full-path (expand-file-name other-file))
            ((file-exists-p other-file-full-path)))
      (find-file (expand-file-name other-file))
    (user-error "Failed to find corresponding file")))

(defun org-archive-mirror--in-archive-p ()
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            until (eobp)
            thereis (and (org-at-heading-p)
                         (seq-some
                          (apply-partially #'string-prefix-p "ARCHIVE_")
                          (seq-map #'car (org-entry-properties))))
            do (outline-next-heading))))

(defun org-archive-mirror--find-archive-source ()
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            until (eobp)
            thereis (when-let* (((org-at-heading-p))
                                (file (org-entry-get (point) "ARCHIVE_FILE"))
                                ((file-exists-p file)))
                      file)
            do (outline-next-heading))))

(defun org-archive-mirror--around-empty-line-p (point)
  "Return `t' if POINT is either on, or immediately
preceding/following an empty line, `nil' otherwise."
  (org-with-wide-buffer
   (goto-char point)
   (or (and (looking-at-p "\n")
            (re-search-backward "\n" (- (point) 1) 'no-error 1))
       (looking-at-p "\n\n")
       (re-search-backward "\n\n" (- (point) 2) 'no-error 1))))

(defun org-archive-mirror--includes-headings-p (beg end)
  (org-with-wide-buffer
   (narrow-to-region beg end)
   (goto-char (point-min))
   (or (org-at-heading-p)
       (outline-next-heading))))

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
         (archive-file (org-archive-mirror--get-archive-file))
         (archive-buffer (or (find-buffer-visiting archive-file)
                             (find-file-noselect archive-file))))
    (with-current-buffer archive-buffer
      (org-with-wide-buffer
       (if outline-path
           (progn
             (org-archive-mirror--insert-outline outline-path)
             (goto-char (org-archive-mirror--heading-location outline-path))
             (org-narrow-to-subtree)
             (outline-next-heading))
         (goto-char (point-min))
         (or (org-at-heading-p)
             (outline-next-heading)))
       (org-archive-mirror--maybe-insert-newline)
       (insert (string-trim archived-content) "\n")))))

(provide 'org-archive-mirror)

;;; org-archive-mirror.el ends here
