(describe "oasps/deduplicate-heading"
  (it "does nothing if heading is not duplicated"
    (let ((content "* foo\n** bar"))
      (with-org content
        (oasps/deduplicate-heading '("foo" "bar"))
        (expect (buffer-string) :to-equal content))))

  (it "does nothing (and does not explode) if heading is not there"
    (with-org ""
      (oasps/deduplicate-heading '("foo" "bar"))
      (expect (buffer-string) :to-equal "")))

  (it "does not touch duplicated leaf headings"
    (let ((content "* foo\n** bar\n** quux\n** bar\n** bar"))
      (with-org content
        (oasps/deduplicate-heading '("foo" "bar"))
        (expect (buffer-string) :to-equal content))))

  (it "combines children of duplicated headings"
    (with-org "* foo\n** one\n* foo\n** two\n* foo\n** three"
      (oasps/deduplicate-heading '("foo"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** one\n** two\n** three")))

  (it "deduplicates children recursively"
    (with-org "* foo
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
      (oasps/deduplicate-heading '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo
** baz
*** corge
**** thud
***** plugh
** bar
*** quux
**** one
**** two
**** three"))))
