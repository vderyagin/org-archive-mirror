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

  (it "preserves links with description"
    (expect (org-archive-mirror--normalize-heading-title "[[https://example.com][Example]]")
            :to-equal "[[https://example.com][Example]]"))

  (it "preserves links without description"
    (expect (org-archive-mirror--normalize-heading-title "[[https://example.com]]")
            :to-equal "[[https://example.com]]"))

  (it "preserves links within text"
    (expect (org-archive-mirror--normalize-heading-title "Check [[https://example.com][this link]] for details")
            :to-equal "Check [[https://example.com][this link]] for details"))

  (it "preserves multiple links"
    (expect (org-archive-mirror--normalize-heading-title "[[a][A]] and [[b][B]]")
            :to-equal "[[a][A]] and [[b][B]]")))

(describe "org-archive-mirror--normalize-outline-for-comparison"
  (it "returns nil elements for nil input"
    (expect (org-archive-mirror--normalize-outline-for-comparison '(nil))
            :to-equal '(nil)))

  (it "normalizes outline and strips links for comparison"
    (expect (org-archive-mirror--normalize-outline-for-comparison
             '("[[https://example.com][Example]]" "foo"))
            :to-equal '("Example" "foo")))

  (it "replaces link without description with target"
    (expect (org-archive-mirror--normalize-outline-for-comparison
             '("[[https://example.com]]"))
            :to-equal '("https://example.com")))

  (it "handles mixed content with links"
    (expect (org-archive-mirror--normalize-outline-for-comparison
             '("Check [[https://example.com][this link]] for details"))
            :to-equal '("Check this link for details")))

  (it "handles multiple links in one title"
    (expect (org-archive-mirror--normalize-outline-for-comparison
             '("[[a][A]] and [[b][B]]"))
            :to-equal '("A and B")))

  (it "strips progress cookies before link normalization"
    (expect (org-archive-mirror--normalize-outline-for-comparison
             '("[[https://example.com][Example]] [1/2]"))
            :to-equal '("Example"))))
