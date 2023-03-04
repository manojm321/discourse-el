;;; -*- lexical-binding: t; -*-
;;; Code:
;;;; Dependencies

(require 'shr)
(require 'discourse-api)

;;;; Customizations

;;;; Variables
;;;;; Keymap

;;;###autoload
(defvar discourse-topic-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "q") #'quit-window)

    keymap)
  "Keymap for `discourse-topic-mode'." )

;;;;; discourse-topic-mode functions

(defun discourse-topic--format-post (post)
  "Format POST to be displayed in a buffer."
  (let* ((topic-id (cdr-safe (assoc 'topic_id post)))
         (post-number (cdr-safe (assoc 'post_number post)))
         (post-url (format "%s/t/%s/%s" discourse-server topic-id post-number))
         (post-number-propertized (propertize (string post-number) 'discourse-post-nav post-url))
         (name (cdr-safe (assoc 'name post)))
         (username (cdr-safe (assoc 'username post)))
         (unseen (cdr-safe (assoc 'unseen post)))
         (avatar-template (cdr-safe (assoc 'avatar_template post)))
         (cooked (cdr-safe (assoc 'cooked post)))
         (action-code (cdr-safe (assoc 'action_code post)))
         (action-code-who (cdr-safe (assoc 'action_code_who post)))
         (line (format "<h2>%s.<img src=\"%s%s\">%s(%s)</h2> %s<hr>"
                       post-number
                       discourse-server
                       (string-replace "{size}" "40" avatar-template)
                       name
                       username
                       cooked)))
    line))

(defun discourse-topic-populate-topic (topic)
  "Populate TOPIC into a buffer and switch to it."
  (let* ((topic-buf (get-buffer-create "*discourse topic info*")))
    (with-current-buffer topic-buf
      (let* ((posts (cdr-safe (assoc 'posts (assoc 'post_stream topic))))
             (fmt-posts (mapcar #'discourse-topic--format-post posts))
             (id (cdr-safe (assoc 'id topic)))
             (title (cdr-safe (assoc 'title topic)))
             (posts-count (cdr-safe (assoc 'posts_count topic)))
             (views (cdr-safe (assoc 'views topic)))
             (tags (cdr-safe (assoc 'tags topic)))
             (inhibit-read-only t))
        (erase-buffer)
        (insert "<!DOCTYPE html>\n<html>\n<body>\n")
        (insert (format "<h2>%s: %s </h2><br>posts:%s views:%s tags:%s<hr>"
                        id title posts-count views tags))
        (dolist (post fmt-posts)
          (insert post))
        (insert "\n</body>\n</html>")
        (shr-render-region (point-min) (point-max) topic-buf)
        (view-mode)
        (switch-to-buffer topic-buf)
        (goto-char (point-min))))))

(defun discourse-topic-show-topic ()
  "Show topic under point."
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (discourse-api-get-topic 'discourse-topic-populate-topic topicid )
      (message "No topic under point"))))

(provide 'discourse-topic)

;;; discourse-topic.el ends here
