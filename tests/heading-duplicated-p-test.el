;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--heading-duplicated-p"
  (it "returns nil when called with empty outline"
    (with-org-allow-point-move ""
      (expect (org-archive-mirror--heading-duplicated-p nil) :to-be nil)))

  (it "does not consider non-duplicated heading duplicated"
    (with-org-allow-point-move "* foo\n* bar"
      (expect (org-archive-mirror--heading-duplicated-p '("foo")) :to-be nil)))

  (it "detects a top-level duplicated heading"
    (with-org-allow-point-move "* foo\n* foo"
      (expect (org-archive-mirror--heading-duplicated-p '("foo")) :to-be-truthy)))

  (it "treats progress indicators as part of the same heading"
    (with-org-allow-point-move "* foo [0/2]\n* foo [2/2]"
      (expect (org-archive-mirror--heading-duplicated-p '("foo")) :to-be-truthy)))

  (it "returns nil when asked for nonexistent heading"
    (with-org-allow-point-move ""
      (expect (org-archive-mirror--heading-duplicated-p '("bar" "baz")) :to-be nil)))

  (it "detects deeply nested duplicated heading"
    (with-org-allow-point-move "* foo\n** bar\n*** baz\n*** quux\n*** baz\n*** corge"
      (expect (org-archive-mirror--heading-duplicated-p '("foo" "bar" "baz")) :to-be-truthy)))

  (it "is not confused by heading with same name in different subtrees"
    (with-org-allow-point-move "* foo\n** bar\n* baz\n** bar"
      (expect (org-archive-mirror--heading-duplicated-p '("foo" "bar")) :to-be nil)
      (expect (org-archive-mirror--heading-duplicated-p '("baz" "bar")) :to-be nil)))

  (it "is not confused by heading with same name on different levels"
    (with-org-allow-point-move "* foo\n** foo\n*** foo\n**** foo"
      (expect (org-archive-mirror--heading-duplicated-p '("foo")) :to-be nil)
      (expect (org-archive-mirror--heading-duplicated-p '("foo" "foo")) :to-be nil))))
