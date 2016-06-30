;;; org-archive-subtree-preserve-structure.el --- A tool for archiving org subtrees mirroring original structure -*- lexical-binding: t -*-

;; Copyright (C) 2016 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 30 Jun 2016
;; Version: 0.0.1

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

(require 'subr-x)
(require 'org)

(defgroup org-archive-subtree-preserve-structure nil
  "A tool for archiving org subtrees mirroring original structure"
  :group 'org)

(defun org--leaf-heading-p ()
  "True if heading does not have any headings under it"
  (org-with-wide-buffer
   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
     (save-excursion
       (or (outline-next-heading)
           (point-max))
       (> (point) subtree-end)))))

(defun org--insert-outline (outline)
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            for level from 1 to (length outline)
            for item in outline
            for full-heading = (format "%s %s" (make-string level ?*) item)
            for re = (format org-complex-heading-regexp-format item)
            unless (and (re-search-forward re nil 'noerror)
                        (= level (length (match-string 1))))
            do
            (goto-char (point-max))
            (unless (looking-back "\n" 1)
              (insert "\n"))
            (insert full-heading)
            end
            do
            (org-back-to-heading 'invisible-ok)
            (org-narrow-to-subtree))))

(defun org--heading-location (outline)
  (org-with-wide-buffer
   (and outline
        (cl-loop initially (goto-char (point-min))
                 for level from 1 to (length outline)
                 for item in outline
                 for re = (format org-complex-heading-regexp-format item)
                 always (and (re-search-forward re nil 'noerror)
                             (= level (length (match-string 1))))
                 do
                 (org-back-to-heading 'invisible-ok)
                 (org-narrow-to-subtree))
        (point-marker))))

(defun org--deduplicate-heading (outline)
  (unless (org--leaf-heading-p)
    (let ((heading-re (format org-complex-heading-regexp-format (car (last outline)))))
      (save-excursion
        (save-restriction
          (when (butlast outline)
            (goto-char (marker-position (org--heading-location (butlast outline))))
            (org-narrow-to-subtree))

          (when (cl-loop initially (goto-char (point-min))
                         with level = (length outline)
                         while (< (point) (point-max))
                         count (and (re-search-forward heading-re nil 'noerror)
                                    (= level (length (match-string 1))))
                         into heading-instances
                         finally return (>= heading-instances 2))

            (let ((first-instance (org--heading-location outline))
                  content
                  second-instance)

              (org-with-point-at first-instance
                (let ((subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
                  (outline-next-heading)
                  (when (< (point) subtree-end)
                    (setq content (string-trim (delete-and-extract-region (point) subtree-end))))))

              (org-with-point-at first-instance
                (delete-region (point) (save-excursion (org-end-of-subtree 'invisible-ok))))

              (setq second-instance (org--heading-location outline))

              (unless second-instance
                (error "Heading passed for deduplication does not seem to be duplicated"))

              (org-with-point-at second-instance
                (save-restriction
                  (org-narrow-to-subtree)
                  (outline-next-heading)
                  (unless (looking-back "\n" 1)
                    (insert "\n"))
                  (insert content "\n")))

              ;; make sure there's no duplication in children, recursively
              (org-with-point-at second-instance
                (cl-loop for subtree-end = (org-with-point-at second-instance
                                             (org-end-of-subtree 'invisible-ok))
                         while (< (point) subtree-end)
                         do (outline-next-heading)
                         if (outline-on-heading-p 'invisible-ok)
                         do (org--deduplicate-heading (org-get-outline-path 'with-self)))))))))))

;;;###autoload
(defun org-archive-subtree-preserve-structure ()
  (interactive)
  (let* ((full-outline-path (org-get-outline-path 'with-self))
         (outline-path (butlast full-outline-path))
         (org-file (file-name-nondirectory (buffer-file-name (buffer-base-buffer))))
         (archive-file (expand-file-name (format "archive/archive_%s" org-file)
                                         org-directory))
         (parent-heading-line (format "%s %s"
                                      (make-string (seq-length outline-path) ?*)
                                      (car (last outline-path))))
         (org-archive-location (format (expand-file-name "archive/archive_%%s::%s"
                                                         org-directory)
                                       parent-heading-line))
         (archive-buffer (or (find-buffer-visiting archive-file)
                             (find-file-noselect archive-file))))

    ;; make sure archive buffer contains relevant outline
    (with-current-buffer archive-buffer
      (org--insert-outline outline-path))

    ;; do the archiving
    (org-archive-subtree)

    ;; clean up duplication, if any
    (with-current-buffer archive-buffer
      (org--deduplicate-heading full-outline-path))))

(provide 'org-archive-subtree-preserve-structure)

;;; org-archive-subtree-preserve-structure.el ends here
