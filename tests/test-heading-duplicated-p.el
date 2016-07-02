(describe "oasps/heading-duplicated-p"
  (it "returns nil when called with empty outline"
    (with-org ""
      (expect (oasps/heading-duplicated-p nil) :to-be nil)))

  (it "does not consider non-duplicated heading duplicated"
    (with-org "* foo\n* bar"
      (expect (oasps/heading-duplicated-p '("foo")) :to-be nil)))

  (it "detects a top-level duplicated heading"
    (with-org "* foo\n* foo"
      (expect (oasps/heading-duplicated-p '("foo")) :to-be-truthy)))

  (it "returns nil when asked for nonexistent heading"
    (with-org ""
      (expect (oasps/heading-duplicated-p '("bar" "baz")) :to-be nil)))

  (it "detects deeply nested duplicated heading"
    (with-org "* foo\n** bar\n*** baz\n*** quux\n*** baz\n*** corge"
      (expect (oasps/heading-duplicated-p '("foo" "bar" "baz")) :to-be-truthy)))

  (it "is not confused by heading with same name in different subtrees"
    (with-org "* foo\n** bar\n* baz\n** bar"
      (expect (oasps/heading-duplicated-p '("foo" "bar")) :to-be nil)
      (expect (oasps/heading-duplicated-p '("baz" "bar")) :to-be nil)))

  (it "is not confused by heading with same name on different levels"
    (with-org "* foo\n** foo\n*** foo\n**** foo"
      (expect (oasps/heading-duplicated-p '("foo")) :to-be nil)
      (expect (oasps/heading-duplicated-p '("foo" "foo")) :to-be nil))))
