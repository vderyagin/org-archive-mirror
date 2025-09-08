;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--deduplicate-heading"
  (it "does nothing if heading is not duplicated"
    (let ((content "* foo\n** bar"))
      (with-org content
        (org-archive-mirror--deduplicate-heading '("foo" "bar"))
        (expect (buffer-string) :to-equal content))))

  (it "does nothing (and does not explode) if heading is not there"
    (with-org ""
      (org-archive-mirror--deduplicate-heading '("foo" "bar"))
      (expect (buffer-string) :to-equal "")))

  (it "does not touch duplicated leaf headings"
    (let ((content "* foo\n** bar\n** quux\n** bar\n** bar"))
      (with-org content
        (org-archive-mirror--deduplicate-heading '("foo" "bar"))
        (expect (buffer-string) :to-equal content))))

  (it "combines children of duplicated headings"
    (with-org "* foo\n** one\n* foo\n** two\n* foo\n** three"
      (org-archive-mirror--deduplicate-heading '("foo"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** one\n** two\n** three")))

  (it "leaves the last instance, if headings have progress indicators in square brackets"
    (with-org "* foo [0/3]\n** one\n* foo [1/3]\n** two\n* foo [3/3]\n** three"
      (org-archive-mirror--deduplicate-heading '("foo"))
      (expect (buffer-string)
              :to-equal
              "* foo [3/3]\n** one\n** two\n** three")))

  (it "deduplicates children recursively"
    (with-org "* foo
** bar
*** quux
**** one
** baz
*** corge
**** thud
***** plugh
** bar
*** quux
**** two
**** three"
      (org-archive-mirror--deduplicate-heading '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo
** baz
*** corge
**** thud
***** plugh
** bar
*** quux
**** one
**** two
**** three"))))
