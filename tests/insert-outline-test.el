;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--insert-outline"
  (it "does nothing if no outline provided"
    (with-org-allow-point-move ""
      (org-archive-mirror--insert-outline nil)
      (expect (buffer-string) :to-equal "")))

  (it "inserts outlines in empty buffer"
    (with-org-allow-point-move ""
      (org-archive-mirror--insert-outline '("foo" "bar"))
      (expect-org-content "
                * foo
                ** bar")))

  (it "does nothing if said outline is already there"
    (let ((text "
                  * foo
                  ** bar
                  *** baz"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect-org-content "
                  * foo
                  ** bar
                  *** baz"))))

  (it "is not confused by todo keywords"
    (let ((text "
                  * TODO foo
                  ** TODO bar
                  *** DONE baz"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect-org-content "
                  * TODO foo
                  ** TODO bar
                  *** DONE baz"))))

  (it "ignores progress indicators when reusing headings"
    (let ((text "
                  * foo [1/3]
                  ** bar [0/2]
                  *** baz [2/2]"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect-org-content "
                  * foo [1/3]
                  ** bar [0/2]
                  *** baz [2/2]"))))

  (it "reuses existing partial outline"
    (with-org-allow-point-move "
                                 * foo
                                 ** bar"
      (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
      (expect-org-content "
                * foo
                ** bar
                *** baz")))

  (it "leaves other siblings intact"
    (let ((text "
                  * foo
                  ** baz
                  ** quux
                  *** grault
                  *** plugh
                  *** garply
                  ** foobar
                  * bar
                  ** corge"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "quux" "plugh"))
        (expect-org-content "
                  * foo
                  ** baz
                  ** quux
                  *** grault
                  *** plugh
                  *** garply
                  ** foobar
                  * bar
                  ** corge"))))

  (it "inserts under the correct branch with duplicate names"
    (let ((text "
                  * foo
                  ** bar
                  * baz
                  ** bar"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("baz" "bar" "quux"))
        (expect-org-content "
                  * foo
                  ** bar
                  * baz
                  ** bar
                  *** quux"))))

  (it "does not produce any extra newlines when inserting heading"
    (with-org-allow-point-move "
                                 * foo
                                 * quux"
      (org-archive-mirror--insert-outline '("foo" "bar"))
      (expect-org-content "
                * foo
                ** bar
                * quux"))))
