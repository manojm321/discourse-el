;;; Code:

(require 'ert)
(require 'discourse-topics)

(ert-deftest test-discourse-topics-populate-topics ()
  "Tests that the topics buffer contains title and topics"
  (let* ((file (expand-file-name "data/top.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topics-populate-topics (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topics-buffer-name
    (let* ((title "ID       Posts New Reply Title")
           (actual-title (buffer-substring-no-properties (point-min) (+ (point-min) (length title)))))
      (should (equal actual-title title))
      (should (equal 51  (count-lines (point-min) (point-max)))))))

(ert-deftest test-discourse-topics-populate-topics-2 ()
  "Test that closed topics are greyed out"
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

(ert-deftest test-discourse-topics-populate-topics-3 ()
  "Test that unread topics are bold"
  (let* ((file (expand-file-name "data/top.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topics-populate-topics (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topics-buffer-name
    (let* ((topic-id 253588)
           (text "253588   19    0   3     Sam Saffron and Sarah Hawk named Discourse Co-CEOs          ")
           (expected-line (propertize text 'discourse-nav topic-id 'face 'bold)))
      (goto-char (point-min))
      (search-forward (format "%s" topic-id))
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq actual-line (buffer-substring start (point)))
      (should (ert-equal-including-properties actual-line expected-line)))))

(provide 'discourse-topics-tests)

;;; discourse-topics-tests.el ends here
