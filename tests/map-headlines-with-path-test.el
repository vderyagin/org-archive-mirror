;;; -*- lexical-binding: t -*-

(require 'tests/test-helper)

(describe "org-archive-mirror--map-headlines-with-path"
  (it "returns nil for empty buffer"
    (with-org ""
      (expect (org-archive-mirror--map-headlines-with-path #'ignore) :to-be nil)))

  (it "calls function with headline and path for each headline"
    (with-org "
               * foo
               ** bar
               ** baz
               * quux"
      (let (collected)
        (org-archive-mirror--map-headlines-with-path
         (lambda (headline path)
           (push (list (org-archive-mirror--headline-title headline) path) collected)
           nil))
        (expect (nreverse collected)
                :to-equal
                '(("foo" ("foo"))
                  ("bar" ("foo" "bar"))
                  ("baz" ("foo" "baz"))
                  ("quux" ("quux")))))))

  (it "stops when function returns non-nil"
    (with-org "
               * one
               * two
               * three"
      (let ((count 0))
        (org-archive-mirror--map-headlines-with-path
         (lambda (_headline path)
           (setq count (1+ count))
           (when (equal path '("two")) 'found)))
        (expect count :to-equal 2))))

  (it "returns the non-nil value from function"
    (with-org "
               * foo
               ** bar"
      (expect (org-archive-mirror--map-headlines-with-path
               (lambda (headline path)
                 (when (equal path '("foo" "bar"))
                   (org-element-property :begin headline))))
              :to-equal 7)))

  (it "normalizes titles (strips TODO, progress cookies)"
    (with-org "
               * TODO foo [1/2]
               ** DONE bar [100%]"
      (let (paths)
        (org-archive-mirror--map-headlines-with-path
         (lambda (_headline path)
           (push path paths)
           nil))
        (expect (nreverse paths)
                :to-equal
                '(("foo") ("foo" "bar")))))))

(describe "org-archive-mirror--for-each-headline-with-path"
  (it "visits all headlines even when buffer has many"
    (with-org "
               * a
               ** b
               ** c
               * d
               ** e"
      (let ((count 0))
        (org-archive-mirror--for-each-headline-with-path
         (lambda (_headline _path)
           (setq count (1+ count))))
        (expect count :to-equal 5))))

  (it "handles deeply nested outlines correctly"
    (with-org "
               * a
               ** b
               *** c
               **** d
               ** e"
      (let (paths)
        (org-archive-mirror--for-each-headline-with-path
         (lambda (_headline path)
           (push path paths)))
        (expect (nreverse paths)
                :to-equal
                '(("a")
                  ("a" "b")
                  ("a" "b" "c")
                  ("a" "b" "c" "d")
                  ("a" "e")))))))
