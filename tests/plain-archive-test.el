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
        (expect-org-content "
                          * foo
                          Alpha"))))

  (it "archives region before first heading when no outline"
    (with-org-archive-buffers
        "
          <POINT>

          <REGION_BEGIN>Alpha
          <REGION_END>

          * source"
        "
                  * archived
                  "
      (with-current-buffer source-buffer
        (org-archive-mirror-plain))
      (with-current-buffer archive-buffer
        (expect-org-content "
                          Alpha
                          * archived"))))

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
        (expect (org-archive-mirror-plain) :to-throw)))))
