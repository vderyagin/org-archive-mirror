;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--remove-heading-extract-children"
  (it "removes empty heading"
    (with-org-allow-point-move "* foo"
      (expect (org-archive-mirror--remove-heading-extract-children (point-min)) :to-be nil)
      (expect (buffer-string) :to-equal "")))

  (it "removes heading with children, returns children"
    (with-org-allow-point-move "
                                * foo
                                ** bar
                                *** baz
                                ** quux"
      (expect-org-string
       (org-archive-mirror--remove-heading-extract-children (point-min))
       "
        ** bar
        *** baz
        ** quux")
      (expect-org-content "")))

  (it "does not touch any surrounding stuff"
    (with-org-allow-point-move "
                                * foo
                                * bar
                                ** quux
                                * baz"
      (expect-org-string
       (org-archive-mirror--remove-heading-extract-children
        (save-excursion
          (search-forward "bar")
          (org-back-to-heading)
          (point)))
       "** quux")
      (expect-org-content "
                           * foo
                           * baz"))))
