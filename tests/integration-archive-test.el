;;; -*- lexical-binding: t -*-

(require 'org-element)
(require 'cl-lib)

(defun org-archive-mirror-test--update-outline-stack (stack level title)
  (let* ((target (max 0 (1- level)))
         (parent (cl-loop for i from 0 below target
                          for item in stack
                          collect item)))
    (append parent (list title))))

(defun org-archive-mirror-test--find-headline (buffer outline)
  (with-current-buffer buffer
    (let ((org-element-use-cache nil)
          (stack nil)
          result)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (let* ((level (org-element-property :level headline))
                 (title (org-archive-mirror--headline-title headline)))
            (setq stack (org-archive-mirror-test--update-outline-stack stack level title))
            (when (equal stack outline)
              (setq result headline)
              headline)))
        nil 'first-match)
      result)))

(defun org-archive-mirror-test--count-headlines (buffer outline)
  (let ((count 0))
    (with-current-buffer buffer
      (let ((org-element-use-cache nil)
            (stack nil))
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (let* ((level (org-element-property :level headline))
                   (title (org-archive-mirror--headline-title headline)))
              (setq stack (org-archive-mirror-test--update-outline-stack stack level title))
              (when (equal stack outline)
                (setq count (1+ count)))))
          nil nil)))
    count))

(defun org-archive-mirror-test--headline-children-titles (headline)
  (mapcar #'org-archive-mirror--headline-title
          (org-element-map (org-element-contents headline) 'headline #'identity)))

(describe "org-archive-mirror integration"
  (it "archives a subtree into an empty archive"
    (with-org-archive-buffers "* foo\n** <POINT>bar\n*** baz\n" ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo" "bar"))))
        (expect headline :to-be-truthy)
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("baz")))
      (with-current-buffer source-buffer
        (expect (string-match-p "\\*\\* bar" (buffer-string)) :to-be nil))))

  (it "archives into an existing outline without duplication"
    (with-org-archive-buffers "* foo\n** <POINT>bar\n" "* foo\n"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "bar")) :to-be 1)))

  (it "treats TODO and progress cookie changes as same heading"
    (with-org-archive-buffers
        "* TODO foo [2/2]\n** <POINT>bar [1/1]\n*** baz\n"
        "* foo [1/2]\n** DONE bar [0/1]\n"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo" "bar")) :to-be 1)
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo" "bar"))))
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("baz")))))

  (it "deduplicates existing archive headings"
    (with-org-archive-buffers
        "* <POINT>foo\n** three\n"
        "* foo\n** one\n* foo\n** two\n"
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (let ((headline (org-archive-mirror-test--find-headline archive-buffer '("foo"))))
        (expect (org-archive-mirror-test--headline-children-titles headline)
                :to-equal '("one" "two" "three")))))

  (it "archives all headings in a region spanning multiple headings"
    (with-org-archive-buffers
        "<REGION_BEGIN><POINT>* foo\n** a\n* bar\n** b\n<REGION_END>"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("bar")) :to-be 1)))

  (it "archives headings when region begins mid-heading"
    (with-org-archive-buffers
        "* <REGION_BEGIN><POINT>foo\n** a\n* bar\n** b\n<REGION_END>"
        ""
      (with-current-buffer source-buffer
        (org-archive-mirror-subtree))
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("foo")) :to-be 1)
      (expect (org-archive-mirror-test--count-headlines archive-buffer '("bar")) :to-be 1))))
