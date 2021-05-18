(describe "org-archive-mirror--remove-heading-extract-children"
  (it "removes empty heading"
    (with-org "* foo"
      (expect (org-archive-mirror--remove-heading-extract-children (point-min)) :to-be nil)
      (expect (buffer-string) :to-equal "")))

  (it "removes heading with children, returns children"
    (with-org "* foo\n** bar\n*** baz\n** quux"
      (expect (org-archive-mirror--remove-heading-extract-children (point-min))
              :to-equal
              "** bar\n*** baz\n** quux")
      (expect (buffer-string) :to-equal "")))

  (it "does not touch any surrounding stuff"
    (with-org "* foo\n* bar\n** quux\n* baz"
      (expect (org-archive-mirror--remove-heading-extract-children
               (save-excursion
                 (search-forward "bar")
                 (org-back-to-heading)
                 (point)))
              :to-equal
              "** quux")
      (expect (buffer-string) :to-equal "* foo\n* baz"))))
