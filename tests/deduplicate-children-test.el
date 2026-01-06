;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--deduplicate-children"
  (it "deduplicates direct children with subtrees"
    (with-org-allow-point-move "* foo\n** bar\n*** one\n** bar\n*** two\n** baz"
      (org-archive-mirror--deduplicate-children '("foo"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar\n*** one\n*** two\n** baz")))

  (it "leaves leaf-only duplicates untouched"
    (with-org-allow-point-move "* foo\n** bar\n** bar\n** baz"
      (org-archive-mirror--deduplicate-children '("foo"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar\n** bar\n** baz"))))
