;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror-plain"
  (it "archives region into the current outline"
    (with-org-archive-buffers
        "* foo <POINT>\n\n<REGION_BEGIN>Alpha\n<REGION_END>\n\n* bar\n"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-plain))
      (with-current-buffer archive-buffer
        (goto-char (point-min))
        (expect (buffer-string) :to-equal "* foo\nAlpha\n"))))

  (it "archives region before first heading when no outline"
    (with-org-archive-buffers
        "<POINT>\n\n<REGION_BEGIN>Alpha\n<REGION_END>\n\n* source\n"
        "* archived\n"
      (with-current-buffer source-buffer
        (org-archive-mirror-plain))
      (with-current-buffer archive-buffer
        (expect (buffer-string) :to-equal "Alpha\n* archived\n"))))

  (it "fails when region includes a heading"
    (with-org-archive-buffers
        "<REGION_BEGIN>* foo\nbar\n<REGION_END>"
        ""
      (with-current-buffer source-buffer
        (expect (org-archive-mirror-plain) :to-throw))))

  (it "fails when region does not begin and end at empty lines"
    (with-org-archive-buffers
        "* foo <POINT>\n<REGION_BEGIN>Alpha\n<REGION_END>\n"
        ""
      (with-current-buffer source-buffer
        (expect (org-archive-mirror-plain) :to-throw)))))
