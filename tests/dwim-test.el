;;; -*- lexical-binding: t -*-

(require 'tests/test-helper)

(describe "org-archive-mirror-dwim"
  (describe "with no region"
    (it "archives subtree when point is on a heading"
      (with-org-archive-buffers
          "
           * <POINT>foo
           ** bar"
          ""
        (with-current-buffer source-buffer
          (org-archive-mirror-dwim))
        (expect (org-archive-mirror-test--find-headline archive-buffer '("foo"))
                :to-be-truthy)
        (with-current-buffer source-buffer
          (expect (string-match-p "\\* foo" (buffer-string)) :to-be nil))))

    (it "errors when point is not on a heading"
      (with-org-archive-buffers
          "
           <POINT>some text

           * foo"
          ""
        (with-current-buffer source-buffer
          (expect (org-archive-mirror-dwim) :to-throw 'user-error)))))

  (describe "with region containing headings"
    (it "archives all headings in the region"
      (with-org-archive-buffers
          "
           <REGION_BEGIN><POINT>* foo
           ** a
           * bar
           ** b
           <REGION_END>"
          ""
        (with-current-buffer source-buffer
          (org-archive-mirror-dwim))
        (expect (org-archive-mirror-test--find-headline archive-buffer '("foo"))
                :to-be-truthy)
        (expect (org-archive-mirror-test--find-headline archive-buffer '("bar"))
                :to-be-truthy))))

  (describe "with region containing only plain text"
    (it "archives plain text when region has proper boundaries"
      (with-org-archive-buffers
          "
           * foo

           <REGION_BEGIN>Some plain text
           <REGION_END>

           * bar"
          ""
        (with-current-buffer source-buffer
          (org-archive-mirror-dwim))
        (with-current-buffer archive-buffer
          (expect (buffer-string) :to-match ":ARCHIVED:")
          (expect (buffer-string) :to-match "Some plain text"))))

    (it "archives plain text under correct heading"
      (with-org-archive-buffers
          "
           * parent
           ** child

           <REGION_BEGIN>Notes under child
           <REGION_END>

           ** other"
          ""
        (with-current-buffer source-buffer
          (org-archive-mirror-dwim))
        (expect (org-archive-mirror-test--find-headline archive-buffer '("parent" "child"))
                :to-be-truthy)
        (with-current-buffer archive-buffer
          (expect (buffer-string) :to-match "Notes under child"))))

    (it "errors when region boundaries are invalid"
      (with-org-archive-buffers
          "
           * foo
           <REGION_BEGIN>Some text<REGION_END> without proper boundaries
           * bar"
          ""
        (with-current-buffer source-buffer
          (expect (org-archive-mirror-dwim) :to-throw 'user-error))))))
