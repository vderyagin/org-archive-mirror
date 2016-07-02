(describe "oasps/heading-location"
  (it "returns nil when called with empty outline"
    (with-org ""
      (expect (oasps/heading-location nil) :to-be nil)))

  (it "returns nil when sought heading is not there"
    (with-org ""
      (expect (oasps/heading-location '("foo" "bar")) :to-be nil)))

  (it "returns nil when sought heading is not there, but it's parent is"
    (with-org "* foo"
      (expect (oasps/heading-location '("foo" "bar")) :to-be nil)))

  (it "finds a top-level heading"
    (with-org "* corge"
      (expect (oasps/heading-location '("corge")) :to-be 1)))

  (it "finds a deeply nested heading"
    (with-org "* foo\n** bar\n*** baz\n**** quux"
      (expect (oasps/heading-location '("foo" "bar" "baz" "quux"))
              :to-be
              (save-excursion
                (search-forward "quux")
                (outline-back-to-heading)
                (point)))))

  (it "handles tags and todo keywords well"
    (with-org "* TODO foo :tag:\n** DONE bar :tag:"
      (expect (oasps/heading-location '("foo" "bar"))
              :to-be
              (save-excursion
                (search-forward "bar")
                (outline-back-to-heading)
                (point)))))

  (it "is not confused by heading of same name in different branches"
    (with-org "* foo\** baz :wrong:\n* bar\n** baz :correct:"
      (expect (oasps/heading-location '("bar" "baz"))
              :to-be
              (save-excursion
                (search-forward ":correct:")
                (outline-back-to-heading)
                (point))))))
