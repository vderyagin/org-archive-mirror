;;; -*- lexical-binding: t -*-

(require 'org-archive-mirror)
(require 'subr-x)

(setq org-element-use-cache nil)

(defvar org-archive-mirror-test--allow-point-move nil
  "Non-nil allows tests to move point inside `with-org'.")

(defun org-archive-mirror-test--trim-blank-edges (lines)
  (while (and lines (string-empty-p (car lines)))
    (setq lines (cdr lines)))
  (while (and lines (string-empty-p (car (last lines))))
    (setq lines (butlast lines)))
  lines)

(defun org-archive-mirror-test--common-indent (lines)
  (let ((indent most-positive-fixnum))
    (dolist (line lines)
      (unless (string-empty-p line)
        (string-match "\\`[ \t]*" line)
        (setq indent (min indent (- (match-end 0) (match-beginning 0))))))
    (if (eq indent most-positive-fixnum) 0 indent)))

(defun org-archive-mirror-test--fixture (text)
  "Return TEXT trimmed and dedented for inline fixtures.
Leading/trailing blank lines are removed and common indentation
is stripped so multi-line strings can be indented naturally."
  (let* ((lines (org-archive-mirror-test--trim-blank-edges (split-string text "\n")))
         (indent (org-archive-mirror-test--common-indent lines)))
    (mapconcat
     (lambda (line)
       (if (string-empty-p line)
           ""
         (substring line indent)))
     lines
     "\n")))

(defun org-archive-mirror-test--remove-marker (marker)
  (goto-char (point-min))
  (when (search-forward marker nil t)
    (let ((pos (match-beginning 0)))
      (replace-match "")
      pos)))

(defmacro with-org (text &rest body)
  (declare (indent 1))
  (let ((position (gensym)))
    `(with-temp-buffer
       (org-mode)
       (setq-local org-element-use-cache nil)
       (insert (org-archive-mirror-test--fixture ,text))
       (let ((point-pos (org-archive-mirror-test--remove-marker "<POINT>")))
         (goto-char (or point-pos (point-min)))
         (let ((,position (point)))
           ,@body
           (unless (or org-archive-mirror-test--allow-point-move
                       (= ,position (point)))
             (error "Point moved from %d to %d when it should not have"
                    ,position
                    (point))))))))

(defmacro with-org-allow-point-move (text &rest body)
  (declare (indent 1))
  `(let ((org-archive-mirror-test--allow-point-move t))
     (with-org ,text ,@body)))

(defun org-archive-mirror-test--setup-buffer (buffer text)
  (with-current-buffer buffer
    (erase-buffer)
    (org-mode)
    (setq-local org-element-use-cache nil)
    (insert (org-archive-mirror-test--fixture text))
    (let ((point-pos (org-archive-mirror-test--remove-marker "<POINT>"))
          (region-begin (org-archive-mirror-test--remove-marker "<REGION_BEGIN>"))
          (region-end (org-archive-mirror-test--remove-marker "<REGION_END>")))
      (goto-char (or point-pos (point-min)))
      (when (and region-begin region-end)
        (goto-char region-end)
        (set-mark region-begin)
       (activate-mark))
      (current-buffer))))

(defmacro expect-org-content (expected &optional buffer)
  "Expect BUFFER (defaults to current) to match EXPECTED org text.
Text is normalized via `org-archive-mirror-test--fixture' and
trailing newlines in the buffer are ignored."
  `(with-current-buffer ,(or buffer '(current-buffer))
     (expect (string-trim-right (buffer-string) "\n+")
             :to-equal (org-archive-mirror-test--fixture ,expected))))

(defmacro expect-org-string (actual expected)
  "Expect ACTUAL string to match EXPECTED org text.
Text is normalized via `org-archive-mirror-test--fixture'."
  `(expect ,actual :to-equal (org-archive-mirror-test--fixture ,expected)))

(defun org-archive-mirror-test--cleanup-buffers (buffers)
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(defun org-archive-mirror-test--cleanup-files (files)
  (dolist (file files)
    (when (and file (file-exists-p file))
      (delete-file file))))

(defmacro with-org-archive-buffers (source-text archive-text &rest body)
  (declare (indent 2))
  `(let* ((source-file (make-temp-file "org-archive-mirror-source-" nil ".org"))
          (archive-file (make-temp-file "org-archive-mirror-archive-" nil ".org"))
          (source-buffer (find-file-noselect source-file))
          (archive-buffer (find-file-noselect archive-file)))
     (unwind-protect
         (progn
           (org-archive-mirror-test--setup-buffer source-buffer ,source-text)
           (org-archive-mirror-test--setup-buffer archive-buffer ,archive-text)
           (let ((org-archive-mirror-archive-file-function (lambda () archive-file)))
             ,@body))
       (org-archive-mirror-test--cleanup-buffers (list source-buffer archive-buffer))
       (org-archive-mirror-test--cleanup-files (list source-file archive-file)))))

(provide 'test-helper)
