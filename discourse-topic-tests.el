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
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (expected-line1 "238821: Try out the new sidebar and notification menus!")
           (expected-line2 "posts:222 views:10785 tags:[sidebar new-feature notifications]")
           ;; first node will be '(a ((href
           ;; . "https://nil/t/238821/")) "238821: Try out the
           ;; new sidebar and notification menus!")
           (actual-line1 (dom-text (car (dom-by-tag dom 'a))))
           ;; "(div nil posts:222 views:10785 tags:[sidebar new-feature notifications])"
           (actual-line2 (dom-text (car (dom-by-tag dom 'div)))))
      (should (equal actual-line1 expected-line1))
      (should (equal actual-line2 expected-line2)))))

(ert-deftest discourse-topic-tests-populate-topic2 ()
  "Disable avatar rendering"
  (let* ((file (expand-file-name "data/238821.json")))
    (setq discourse-topic-load-avatar nil)
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topic-populate-topic (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topic-buffer-name
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (img-node (dom-child-by-tag (nth 1 (dom-by-tag dom 'h2)) 'img)))
      (should-not img-node))))

(ert-deftest discourse-topic-tests-populate-topic3 ()
  "Enable avatar rendering"
  (let* ((file (expand-file-name "data/238821.json")))
    (setq discourse-topic-load-avatar t)
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topic-populate-topic (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topic-buffer-name
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (img-node (dom-child-by-tag (nth 1 (dom-by-tag dom 'h2)) 'img)))
      (should img-node))))

(provide 'discourse-topic-tests)

;;; discourse-topic-tests.el ends here
