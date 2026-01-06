;;; -*- lexical-binding: t -*-

(defun org-archive-mirror-test--with-temp-file (fn)
  (let ((file (make-temp-file "org-archive-mirror-test-" nil ".org")))
    (unwind-protect
        (funcall fn file)
      (when (file-exists-p file)
        (delete-file file)))))

(describe "org-archive-mirror--in-archive-p"
  (it "returns nil when no archive properties exist"
    (with-org-allow-point-move "* foo\n:PROPERTIES:\n:FOO: bar\n:END:\n"
      (expect (org-archive-mirror--in-archive-p) :to-be nil)))

  (it "returns t when any heading has archive property"
    (with-org-allow-point-move "* foo\n:PROPERTIES:\n:ARCHIVE_FILE: /tmp/arch.org\n:END:\n"
      (expect (org-archive-mirror--in-archive-p) :to-be-truthy)))

  (it "detects archive properties in later headings"
    (with-org-allow-point-move "* foo\n:PROPERTIES:\n:FOO: bar\n:END:\n* bar\n:PROPERTIES:\n:ARCHIVE_TIME: 2024-01-01\n:END:\n"
      (expect (org-archive-mirror--in-archive-p) :to-be-truthy))))

(describe "org-archive-mirror--find-archive-source"
  (it "returns nil when no archive file exists"
    (with-org-allow-point-move "* foo\n:PROPERTIES:\n:ARCHIVE_FILE: /tmp/does-not-exist.org\n:END:\n"
      (expect (org-archive-mirror--find-archive-source) :to-be nil)))

  (it "returns archive file when it exists"
    (org-archive-mirror-test--with-temp-file
     (lambda (file)
       (with-org-allow-point-move (format "* foo\n:PROPERTIES:\n:ARCHIVE_FILE: %s\n:END:\n" file)
         (expect (org-archive-mirror--find-archive-source) :to-equal file))))))
