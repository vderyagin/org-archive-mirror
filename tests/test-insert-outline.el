(describe "oasps/insert-outline"
  (it "inserts outlines in empty buffer"
    (with-org ""
      (oasps/insert-outline '("foo" "bar"))
      (expect (buffer-string)
              :to-equal
              "* foo\n** bar"))))
