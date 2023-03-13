;;; discourse-topics-tests.el --- Topic tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand
;; URL: http://github.com/manojm321/discourse-el
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (dash "2.19"))
;; Version: 0.0.1

;;; Commentary:
;; Tests for discourse-topics.el

(require 'ert)
(require 'discourse-topics)

(ert-deftest discourse-topics-tests-populate-topics ()
  "Topics buffer must contain title and a set of topics."
  (let* ((file (expand-file-name "data/top.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topics-populate-topics (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topics-buffer-name
    (let* ((title "ID       Posts New Reply Title")
           (actual-title (buffer-substring-no-properties (point-min) (+ (point-min) (length title)))))
      (should (equal actual-title title))
      (should (equal 51  (count-lines (point-min) (point-max)))))))

(ert-deftest discourse-topics-tests-populate-topics-2 ()
  "Closed topics must be greyed out."
  (let* ((file (expand-file-name "data/top.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topics-populate-topics (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topics-buffer-name
    (let* ((topic-id 238821)
           (text "238821   222   0   176   Try out the new sidebar and notification menus!             ")
           (expected-line (propertize text 'discourse-nav topic-id 'face 'shadow)))
      (goto-char (point-min))
      (search-forward (format "%s" topic-id))
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq actual-line (buffer-substring start (point)))
      (should (ert-equal-including-properties actual-line expected-line)))))

(ert-deftest discourse-topics-tests-populate-topics-3 ()
  "Unread topics must be bold."
  (let* ((file (expand-file-name "data/top.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topics-populate-topics (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topics-buffer-name
    (let* ((topic-id 253588)
           (text "253588   19    0   3     Sam Saffron and Sarah Hawk named Discourse Co-CEOs          ")
           (expected-line (propertize text 'discourse-nav topic-id 'face 'discourse-new-face)))
      (goto-char (point-min))
      (search-forward (format "%s" topic-id))
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq actual-line (buffer-substring start (point)))
      (should (ert-equal-including-properties actual-line expected-line)))))

(provide 'discourse-topics-tests)

;;; discourse-topics-tests.el ends here
