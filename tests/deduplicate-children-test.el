;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--deduplicate-children"
  (it "deduplicates direct children with subtrees"
    (with-org-allow-point-move "
                                 * foo
                                 ** bar
                                 *** one
                                 ** bar
                                 *** two
                                 ** baz"
      (org-archive-mirror--deduplicate-children '("foo"))
      (expect-org-content "
                * foo
                ** bar
                *** one
                *** two
                ** baz")))

  (it "leaves leaf-only duplicates untouched"
    (with-org-allow-point-move "
                                 * foo
                                 ** bar
                                 ** bar
                                 ** baz"
      (org-archive-mirror--deduplicate-children '("foo"))
      (expect-org-content "
                * foo
                ** bar
                ** bar
                ** baz"))))
