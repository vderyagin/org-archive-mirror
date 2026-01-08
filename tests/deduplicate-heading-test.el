;;; -*- lexical-binding: t -*-

(require 'tests/test-helper)

(describe "org-archive-mirror--deduplicate-heading"
  (it "does nothing if heading is not duplicated"
    (with-org-allow-point-move "
                                * foo
                                ** bar"
      (org-archive-mirror--deduplicate-heading '("foo" "bar"))
      (expect-org-content "
                           * foo
                           ** bar")))

  (it "does nothing (and does not explode) if heading is not there"
    (with-org-allow-point-move ""
      (org-archive-mirror--deduplicate-heading '("foo" "bar"))
      (expect-org-content "")))

  (it "does not touch duplicated leaf headings"
    (with-org-allow-point-move "
                                * foo
                                ** bar
                                ** quux
                                ** bar
                                ** bar"
      (org-archive-mirror--deduplicate-heading '("foo" "bar"))
      (expect-org-content "
                           * foo
                           ** bar
                           ** quux
                           ** bar
                           ** bar")))

  (it "combines children of duplicated headings"
    (with-org-allow-point-move "
                                * foo
                                ** one
                                * foo
                                ** two
                                * foo
                                ** three"
      (org-archive-mirror--deduplicate-heading '("foo"))
      (expect-org-content "
                           * foo
                           ** one
                           ** two
                           ** three")))

  (it "leaves the last instance, if headings have progress indicators in square brackets"
    (with-org-allow-point-move "
                                * foo [0/3]
                                ** one
                                * foo [1/3]
                                ** two
                                * foo [3/3]
                                ** three"
      (org-archive-mirror--deduplicate-heading '("foo"))
      (expect-org-content "
                           * foo [3/3]
                           ** one
                           ** two
                           ** three")))

  (it "deduplicates children recursively"
    (with-org-allow-point-move "
                                * foo
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
      (expect-org-content "
                           * foo
                           ** baz
                           *** corge
                           **** thud
                           ***** plugh
                           ** bar
                           *** quux
                           **** one
                           **** two
                           **** three"))))
