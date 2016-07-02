(describe "oasps/leaf-heading-p"
  (it "accepts an empty heading"
    (with-org "* foo"
      (expect (oasps/leaf-heading-p) :to-be-truthy)))

  (it "accepts a heading with some plain text content"
    (with-org "* TODO foo\nfoo bar baz"
      (expect (oasps/leaf-heading-p) :to-be-truthy)))

  (it "accepts deeply nested heading with siblings"
    (with-org "* foo
** bar
*** baz
*** <POINT>quux
*** corge"
      (expect (oasps/leaf-heading-p) :to-be-truthy)))

  (it "rejects a heading with children"
    (with-org "* foo\n** bar"
      (expect (oasps/leaf-heading-p) :not :to-be-truthy))))
