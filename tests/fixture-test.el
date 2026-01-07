;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror-test--fixture"
  (it "returns text unchanged when there is no leading whitespace to trim"
    (expect (org-archive-mirror-test--fixture "* foo\n** bar")
            :to-equal "* foo\n** bar"))

  (it "handles multiline input without indentation"
    (expect (org-archive-mirror-test--fixture "* foo\n\n** bar")
            :to-equal "* foo\n\n** bar"))

  (it "trims leading and trailing blank lines"
    (expect (org-archive-mirror-test--fixture "

                                              * foo

                                              ")
            :to-equal "* foo"))

  (it "dedents by the smallest common indentation"
    (expect (org-archive-mirror-test--fixture "
                                              * foo
                                              ** bar
                                                *** baz")
            :to-equal "* foo\n** bar\n  *** baz"))

  (it "preserves internal blank lines after dedent"
    (expect (org-archive-mirror-test--fixture "
                                              * foo

                                              ** bar")
            :to-equal "* foo\n\n** bar"))

  (it "handles all-whitespace input as empty string"
    (expect (org-archive-mirror-test--fixture "   \n \n")
            :to-equal "")))
