;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--heading-location"
  (it "returns nil when called with empty outline"
    (with-org-allow-point-move ""
      (expect (org-archive-mirror--heading-location nil) :to-be nil)))

  (it "returns nil when sought heading is not there"
    (with-org-allow-point-move ""
      (expect (org-archive-mirror--heading-location '("foo" "bar")) :to-be nil)))

  (it "returns nil when sought heading is not there, but it's parent is"
    (with-org-allow-point-move "* foo"
      (expect (org-archive-mirror--heading-location '("foo" "bar")) :to-be nil)))

  (it "finds a top-level heading"
    (with-org-allow-point-move "* corge"
      (expect (org-archive-mirror--heading-location '("corge")) :to-be 1)))

  (it "finds a deeply nested heading"
    (with-org-allow-point-move "
                                 * foo
                                 ** bar
                                 *** baz
                                 **** quux"
      (expect (org-archive-mirror--heading-location '("foo" "bar" "baz" "quux"))
              :to-be
              (save-excursion
                (search-forward "quux")
                (org-back-to-heading)
                (point)))))

  (it "handles tags and todo keywords well"
    (with-org-allow-point-move "
                                 * TODO foo :tag:
                                 ** DONE bar :tag:"
      (expect (org-archive-mirror--heading-location '("foo" "bar"))
              :to-be
              (save-excursion
                (search-forward "bar")
                (org-back-to-heading)
                (point)))))

  (it "ignores progress indicators in headings"
    (with-org-allow-point-move "
                                 * foo [1/3]
                                 ** bar [0/2]"
      (expect (org-archive-mirror--heading-location '("foo" "bar"))
              :to-be
              (save-excursion
                (search-forward "bar")
                (org-back-to-heading)
                (point)))))

  (it "is not confused by heading of same name in different branches"
    (with-org-allow-point-move "
                                 * foo
                                 ** baz :wrong:
                                 * bar
                                 ** baz :correct:"
      (expect (org-archive-mirror--heading-location '("bar" "baz"))
              :to-be
              (save-excursion
                (search-forward ":correct:")
                (org-back-to-heading)
                (point))))))
