;;; discourse-topic-tests.el --- Topic tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand
;; URL: http://github.com/manojm321/discourse-el
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (dash "2.19"))
;; Version: 0.0.1

;;; Commentary:
;; Tests for discourse-topic.el

;;; Code:
;;;; Dependencies
(require 'ert)
(require 'discourse-topic)

(ert-deftest discourse-topic-tests-populate-topic ()
  "Test topic rendering."
  (let* ((file (expand-file-name "data/238821.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topic-populate-topic (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topic-buffer-name
    (should (equal major-mode 'discourse-topic-mode))
    (let* ((expected-line "238821: Try out the new sidebar and notification menus!")
           (actual-line (buffer-substring-no-properties (point-min) (line-end-position))))
      (should (equal actual-line expected-line)))
    (forward-line 2)
    (let* ((expected-line "posts:222 views:10785 tags:[sidebar new-feature notifications]")
           (actual-line (buffer-substring-no-properties (point) (line-end-position))))
      (should (equal actual-line expected-line)))))

(provide 'discourse-topic-tests)

;;; discourse-topic-tests.el ends here
