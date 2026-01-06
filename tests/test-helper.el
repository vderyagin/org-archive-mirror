;;; -*- lexical-binding: t -*-

(require 'org-archive-mirror)
(require 'cl-macs)

(defvar org-archive-mirror-test--allow-point-move nil
  "Non-nil allows tests to move point inside `with-org'.")

(defmacro with-org (text &rest body)
  (declare (indent 1))
  (let ((position (gensym)))
    `(with-temp-buffer
       (org-mode)
       (insert ,text)
       (goto-char (point-min))
       (search-forward "<POINT>" nil t)
       (let ((,position (point)))
         ,@body
         (unless (or org-archive-mirror-test--allow-point-move
                     (= ,position (point)))
           (error "Point moved from %d to %d when it should not have"
                  ,position
                  (point)))))))

(defmacro with-org-allow-point-move (text &rest body)
  (declare (indent 1))
  `(let ((org-archive-mirror-test--allow-point-move t))
     (with-org ,text ,@body)))
