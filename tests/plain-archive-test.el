;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror-plain"
  (it "archives region into the current outline"
    (with-org-archive-buffers
        "
         * foo <POINT>

         <REGION_BEGIN>Alpha
         <REGION_END>

         * bar"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-plain))
      (with-current-buffer archive-buffer
        (goto-char (point-min))
        (expect (buffer-string) :to-match "\\* foo\n:ARCHIVED:\n\\[.*\\]\n\nAlpha\n:END:"))))

  (it "archives region before first heading when no outline"
    (with-org-archive-buffers
        "
         <POINT>

         <REGION_BEGIN>Alpha
         <REGION_END>

         * source"
        "* archived"
      (with-current-buffer source-buffer
        (org-archive-mirror-plain))
      (with-current-buffer archive-buffer
        (expect (buffer-string) :to-match ":ARCHIVED:\n\\[.*\\]\n\nAlpha\n:END:\n\\* archived"))))

  (it "fails when region includes a heading"
    (with-org-archive-buffers
        "
         <REGION_BEGIN>* foo
         bar
         <REGION_END>"
        ""
      (with-current-buffer source-buffer
        (expect (org-archive-mirror-plain) :to-throw))))

  (it "fails when region does not begin and end at empty lines"
    (with-org-archive-buffers
        "
         * foo <POINT>
         <REGION_BEGIN>Alpha
         <REGION_END>"
        ""
      (with-current-buffer source-buffer
        (expect (org-archive-mirror-plain) :to-throw))))

  (it "creates separate drawer blocks for multiple archives"
    (let* ((source-file (make-temp-file "org-archive-mirror-source-" nil ".org"))
           (archive-file (make-temp-file "org-archive-mirror-archive-" nil ".org"))
           (source-buffer (find-file-noselect source-file))
           (archive-buffer (find-file-noselect archive-file)))
      (unwind-protect
          (progn
            (with-current-buffer source-buffer
              (erase-buffer)
              (org-mode)
              (insert "* foo\n\nFirst content\n\nSecond content\n")
              (goto-char (point-min))
              (re-search-forward "First content")
              (let ((org-archive-mirror-archive-file-function (lambda () archive-file)))
                (goto-char (line-beginning-position))
                (forward-line -1)
                (set-mark (point))
                (forward-line 2)
                (activate-mark)
                (org-archive-mirror-plain)
                (goto-char (point-min))
                (re-search-forward "Second content")
                (goto-char (line-beginning-position))
                (forward-line -1)
                (set-mark (point))
                (forward-line 2)
                (activate-mark)
                (org-archive-mirror-plain)))
            (with-current-buffer archive-buffer
              (let ((content (buffer-string)))
                (expect content :to-match ":ARCHIVED:\n\\[.*\\]\n\nFirst content\n:END:")
                (expect content :to-match ":ARCHIVED:\n\\[.*\\]\n\nSecond content\n:END:")
                (expect (how-many ":ARCHIVED:" (point-min) (point-max)) :to-equal 2))))
        (org-archive-mirror-test--cleanup-buffers (list source-buffer archive-buffer))
        (org-archive-mirror-test--cleanup-files (list source-file archive-file))))))
