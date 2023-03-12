(ert-deftest test-discourse-topic-populate-topic ()
  "Load topic"
  (let* ((file (expand-file-name "data/238821.json")))
    (with-temp-buffer
      (insert-file-contents file)
      (discourse-topic-populate-topic (json-read-from-string (buffer-string)))))

  (with-current-buffer discourse-topic-buffer-name
    (should (equal major-mode 'discourse-topic-mode))
    (let* ((expected-line "238821: Try out the new sidebar and notification menus!")
           (actual-line (buffer-substring-no-properties (point-min) (line-end-position))))
      (should (equal actual-line expected-line)))
    (next-line 2)
    (let* ((expected-line "posts:222 views:10785 tags:[sidebar new-feature notifications]")
           (actual-line (buffer-substring-no-properties (point) (line-end-position))))
      (should (equal actual-line expected-line)))))
