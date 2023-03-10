(describe "org-archive-mirror--leaf-heading-p"
  (it "accepts an empty heading"
    (with-org "* foo"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "accepts a heading with some plain text content"
    (with-org "* TODO foo\nfoo bar baz"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "accepts deeply nested heading with siblings"
    (with-org "* foo
** bar
*** baz
*** <POINT>quux
*** corge"
      (expect (org-archive-mirror--leaf-heading-p) :to-be-truthy)))

  (it "rejects a heading with children"
    (with-org "* foo\n** bar"
      (expect (org-archive-mirror--leaf-heading-p) :to-be nil)))

  (it "does not explode when called outside of any subtree"
    (with-org "" (org-archive-mirror--leaf-heading-p))))
