;;; -*- lexical-binding: t -*-

(require 'tests/test-helper)

(require 'org-element)

(defun org-archive-mirror-test--find-headline (buffer outline)
  (with-current-buffer buffer
    (org-archive-mirror--find-headline-by-outline outline)))

(defun org-archive-mirror-test--count-headlines (buffer outline)
  (let ((count 0))
    (with-current-buffer buffer
      (org-archive-mirror--for-each-headline-with-path
       (lambda (_headline path)
         (when (org-archive-mirror--outlines-equal-p path outline)
           (setq count (1+ count))))))
    count))

(defun org-archive-mirror-test--headline-children-titles (headline)
  (mapcar #'org-archive-mirror--headline-title
          (org-element-map (org-element-contents headline) 'headline #'identity)))

(describe "org-archive-mirror integration"
  (it "archives a subtree into an empty archive"
    (with-org-archive-buffers
        "
         * foo
         ** <POINT>bar
         *** baz"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo" "bar"))))
        (expect headline :to-be-truthy)
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("baz")))
      (with-current-buffer source-buffer
        (expect (string-match-p "\\*\\* bar" (buffer-string)) :to-be nil))))

  (it "archives into an existing outline without duplication"
    (with-org-archive-buffers
        "
         * foo
         * foo
         ** <POINT>bar"
        "* foo"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "bar")) :to-be 1)))

  (it "treats TODO and progress cookie changes as same heading"
    (with-org-archive-buffers
        "
         * TODO foo [2/2]
         ** <POINT>bar [1/1]
         *** baz"
        "
         * foo [1/2]
         ** DONE bar [0/1]"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "bar")) :to-be 1)
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo" "bar"))))
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("baz")))))

  (it "deduplicates existing archive headings"
    (with-org-archive-buffers
        "
         * <POINT>foo
         ** three"
        "
         * foo
         ** one
         * foo
         ** two"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo"))))
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("one" "two" "three")))))

  (it "archives all headings in a region spanning multiple headings"
    (with-org-archive-buffers
        "
         <REGION_BEGIN><POINT>* foo
         ** a
         * bar
         ** b
         <REGION_END>"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("bar")) :to-be 1)))

  (it "archives all headings in a region spanning multiple headings below top-level"
    (with-org-archive-buffers
        "
         * foo
         ** <REGION_BEGIN><POINT>a
         ** b<REGION_END>cd
         * bar"
        ""
      (with-current-buffer source-buffer
        (condition-case nil
            (org-archive-mirror-subtree)
          (end-of-buffer nil)))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "a")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "bcd")) :to-be 1)))

  (it "archives headings when region begins mid-heading"
    (with-org-archive-buffers
        "
         * <REGION_BEGIN><POINT>foo
         ** a
         * bar
         ** b
         <REGION_END>"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("bar")) :to-be 1)))

  (it "preserves links in archived headings"
    (with-org-archive-buffers
        "
         * [[https://example.com][Example]]
         ** <POINT>child"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("Example")) :to-be 1)
      (with-current-buffer archive-buffer
        (expect (string-match-p "\\[\\[https://example.com\\]\\[Example\\]\\]" (buffer-string)) :to-be-truthy))))

  (it "matches link heading with existing non-link heading in archive"
    (with-org-archive-buffers
        "
         * [[https://example.com][Example]]
         ** <POINT>new-child"
        "* Example
** old-child"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("Example")) :to-be 1)
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("Example"))))
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("old-child" "new-child"))))))
