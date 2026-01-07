;;; -*- lexical-binding: t -*-

(describe "org-archive-mirror--around-empty-line-p"
  (it "returns t when point is on an empty line (at beginning)"
    (with-org "
               abc
               <POINT>
               def"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be-truthy)))

  (it "returns t when point is at beginning of line after empty line"
    (with-org "
               abc

               <POINT>def"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be-truthy)))

  (it "returns t when looking at two consecutive newlines (empty line ahead)"
    (with-org "
               abc<POINT>

               def"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be-truthy)))

  (it "returns nil when point is in middle of text with no adjacent empty lines"
    (with-org "
               abc
               d<POINT>ef
               ghi"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be nil)))

  (it "returns nil when point is at end of line with no adjacent empty lines"
    (with-org "
               abc
               def<POINT>
               ghi"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be nil)))

  (it "returns nil when point is at beginning of line with no adjacent empty lines"
    (with-org "
               abc
               <POINT>def
               ghi"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be nil)))

  (it "returns t when between two empty lines"
    (with-org "
               abc

               <POINT>
               def"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be-truthy)))

  (it "returns t at start of empty line following another empty line"
    (with-org "
               abc

               <POINT>

               def"
      (expect (org-archive-mirror--around-empty-line-p (point)) :to-be-truthy))))
