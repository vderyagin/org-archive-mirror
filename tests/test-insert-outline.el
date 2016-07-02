(describe "oasps/insert-outline"
  (it "does nothing if no outline provided"
    (with-org ""
      (oasps/insert-outline nil)
      (expect (buffer-string) :to-equal "")))

  (it "inserts outlines in empty buffer"
    (with-org ""
      (oasps/insert-outline '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar")))

  (it "does nothing if said outline is already there"
    (let ((text "* foo\n** bar\n*** baz"))
      (with-org text
        (oasps/insert-outline '("foo" "bar" "baz"))
        (expect (buffer-string) :to-equal text))))

  (it "is not confused by todo keywords"
    (let ((text "* TODO foo\n** TODO bar\n*** DONE baz"))
      (with-org text
        (oasps/insert-outline '("foo" "bar" "baz"))
        (expect (buffer-string) :to-equal text))))

  (it "reuses existing partial outline"
    (with-org "* foo\n** bar"
      (oasps/insert-outline '("foo" "bar" "baz"))
      (expect (buffer-string) :to-equal "* foo\n** bar\n*** baz")))

  (it "leaves other siblings intact"
    (let ((text "* foo\n** baz\n** quux\n*** grault\n*** plugh\n*** garply\n** foobar
* bar\n** corge"))
      (with-org text
        (oasps/insert-outline '("foo" "quux" "plugh"))
        (expect (buffer-string) :to-equal text))))

  (it "does not produce any extra newlines when inserting heading"
    (with-org "* foo\n* quux"
      (oasps/insert-outline '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar\n* quux"))))
