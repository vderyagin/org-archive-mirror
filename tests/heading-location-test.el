;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--heading-location"
  (it "returns nil when called with empty outline"
    (with-org ""
      (expect (org-archive-mirror--heading-location nil) :to-be nil)))

  (it "returns nil when sought heading is not there"
    (with-org ""
      (expect (org-archive-mirror--heading-location '("foo" "bar")) :to-be nil)))

  (it "returns nil when sought heading is not there, but it's parent is"
    (with-org "* foo"
      (expect (org-archive-mirror--heading-location '("foo" "bar")) :to-be nil)))

  (it "finds a top-level heading"
    (with-org "* corge"
      (expect (org-archive-mirror--heading-location '("corge")) :to-be 1)))

  (it "finds a deeply nested heading"
    (with-org "* foo\n** bar\n*** baz\n**** quux"
      (expect (org-archive-mirror--heading-location '("foo" "bar" "baz" "quux"))
              :to-be
              (save-excursion
                (search-forward "quux")
                (org-back-to-heading)
                (point)))))

  (it "handles tags and todo keywords well"
    (with-org "* TODO foo :tag:\n** DONE bar :tag:"
      (expect (org-archive-mirror--heading-location '("foo" "bar"))
              :to-be
              (save-excursion
                (search-forward "bar")
                (org-back-to-heading)
                (point)))))

  (it "is not confused by heading of same name in different branches"
    (with-org "* foo\** baz :wrong:\n* bar\n** baz :correct:"
      (expect (org-archive-mirror--heading-location '("bar" "baz"))
              :to-be
              (save-excursion
                (search-forward ":correct:")
                (org-back-to-heading)
                (point))))))
