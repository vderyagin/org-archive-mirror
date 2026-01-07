;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--format-plain-archive"
  (it "wraps content in ARCHIVED drawer with timestamp"
    (let* ((content "Some plain text content\nwith multiple lines")
           (result (org-archive-mirror--format-plain-archive content)))
      (expect result :to-match "^:ARCHIVED:\n\\[.*\\]\n\n")
      (expect result :to-match "Some plain text content\nwith multiple lines")
      (expect result :to-match ":END:$")))

  (it "includes inactive timestamp in correct format"
    (let* ((content "test")
           (result (org-archive-mirror--format-plain-archive content)))
      (expect result :to-match "\\[20[0-9][0-9]-[0-9][0-9]-[0-9][0-9] [A-Z][a-z][a-z] [0-9][0-9]:[0-9][0-9]\\]")))

  (it "handles empty content"
    (let ((result (org-archive-mirror--format-plain-archive "")))
      (expect result :to-match "^:ARCHIVED:\n\\[.*\\]\n\n\n:END:$")))

  (it "handles content with leading whitespace"
    (let* ((content "  indented text")
           (result (org-archive-mirror--format-plain-archive content)))
      (expect result :to-match "  indented text")))

  (it "handles content with trailing whitespace"
    (let* ((content "text with trailing  ")
           (result (org-archive-mirror--format-plain-archive content)))
      (expect result :to-match "text with trailing  ")))

  (it "produces parseable org format"
    (let* ((content "Test content")
           (result (org-archive-mirror--format-plain-archive content)))
      (with-temp-buffer
        (org-mode)
        (insert result)
        (goto-char (point-min))
        (expect (looking-at ":ARCHIVED:") :to-be-truthy)))))
