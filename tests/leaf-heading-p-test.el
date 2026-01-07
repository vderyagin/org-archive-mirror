;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--leaf-heading-p"
  (it "accepts an empty heading"
    (with-org-allow-point-move "* foo"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "accepts a heading with some plain text content"
    (with-org-allow-point-move "
                                * TODO foo
                                foo bar baz"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "accepts deeply nested heading with siblings"
    (with-org-allow-point-move "
                                * foo
                                ** bar
                                *** baz
                                *** <POINT>quux
                                *** corge"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "rejects a heading with children"
    (with-org-allow-point-move "
                                * foo
                                ** bar"
      (expect (org-archive-mirror--leaf-heading-p) :to-be nil)))

  (it "does not explode when called outside of any subtree"
    (with-org-allow-point-move "" (org-archive-mirror--leaf-heading-p))))
