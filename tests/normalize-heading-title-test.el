;;; -*- lexical-binding: t -*-

(require 'tests/test-helper)

(describe "org-archive-mirror--normalize-heading-title"
  (it "returns nil for nil input"
    (expect (org-archive-mirror--normalize-heading-title nil) :to-be nil))

  (it "trims whitespace"
    (expect (org-archive-mirror--normalize-heading-title "  foo  ")
            :to-equal "foo"))

  (it "strips progress cookies"
    (expect (org-archive-mirror--normalize-heading-title "foo [1/2]")
            :to-equal "foo")
    (expect (org-archive-mirror--normalize-heading-title "foo [50%]")
            :to-equal "foo"))

  (it "replaces link with description"
    (expect (org-archive-mirror--normalize-heading-title "[[https://example.com][Example]]")
            :to-equal "Example"))

  (it "replaces link without description with target"
    (expect (org-archive-mirror--normalize-heading-title "[[https://example.com]]")
            :to-equal "https://example.com"))

  (it "replaces link within text"
    (expect (org-archive-mirror--normalize-heading-title "Check [[https://example.com][this link]] for details")
            :to-equal "Check this link for details"))

  (it "handles multiple links"
    (expect (org-archive-mirror--normalize-heading-title "[[a][A]] and [[b][B]]")
            :to-equal "A and B")))
