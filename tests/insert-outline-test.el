;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--insert-outline"
  (it "does nothing if no outline provided"
    (with-org-allow-point-move ""
      (org-archive-mirror--insert-outline nil)
      (expect (buffer-string) :to-equal "")))

  (it "inserts outlines in empty buffer"
    (with-org-allow-point-move ""
      (org-archive-mirror--insert-outline '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar")))

  (it "does nothing if said outline is already there"
    (let ((text "* foo\n** bar\n*** baz"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect (buffer-string) :to-equal text))))

  (it "is not confused by todo keywords"
    (let ((text "* TODO foo\n** TODO bar\n*** DONE baz"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect (buffer-string) :to-equal text))))

  (it "ignores progress indicators when reusing headings"
    (let ((text "* foo [1/3]\n** bar [0/2]\n*** baz [2/2]"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
        (expect (buffer-string) :to-equal text))))

  (it "reuses existing partial outline"
    (with-org-allow-point-move "* foo\n** bar"
      (org-archive-mirror--insert-outline '("foo" "bar" "baz"))
      (expect (buffer-string) :to-equal "* foo\n** bar\n*** baz")))

  (it "leaves other siblings intact"
    (let ((text "* foo\n** baz\n** quux\n*** grault\n*** plugh\n*** garply\n** foobar
* bar\n** corge"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("foo" "quux" "plugh"))
        (expect (buffer-string) :to-equal text))))

  (it "inserts under the correct branch with duplicate names"
    (let ((text "* foo\n** bar\n* baz\n** bar"))
      (with-org-allow-point-move text
        (org-archive-mirror--insert-outline '("baz" "bar" "quux"))
        (expect (buffer-string)
                :to-equal
                "* foo\n** bar\n* baz\n** bar\n*** quux"))))

  (it "does not produce any extra newlines when inserting heading"
    (with-org-allow-point-move "* foo\n* quux"
      (org-archive-mirror--insert-outline '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar\n* quux"))))
