(require 'org-archive-mirror)
(require 'cl-macs)

(defmacro with-org (text &rest body)
  (declare (indent 1))
  (let ((position (cl-gensym)))
    `(with-temp-buffer
       (org-mode)
       (insert ,text)
       (goto-char (point-min))
       (search-forward "<POINT>" nil t)
       (let ((,position (point)))
         ,@body
         (unless (= ,position (point))
           (error "Point moved from %d to %d when it should not have"
                  ,position
                  (point)))))))
