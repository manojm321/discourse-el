;;; -*- lexical-binding: t; -*-
;;; Code:
;;;; Dependencies

(require 'dash)
(require 'shr)
(require 'discourse-api)

;;;; Customizations

;;;; Variables
(defvar discourse-topic-buffer-name "*discourse-topic*"
  "Name of topic buffer")

;;;;; Keymap

;;;###autoload
(defvar discourse-topic-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "n") #'discourse-topic-next-post)
    (define-key keymap (kbd "p") #'discourse-topic-previous-post)
    (define-key keymap [?\t] #'shr-next-link)
    (define-key keymap [?\M-\t] #'shr-previous-link)

    keymap)
  "Keymap for `discourse-topic-mode'." )

;;;;; discourse-topic-mode functions

(defun discourse-topic--format-post (post)
  "Format POST to be displayed in a buffer."
  (let* ((topic-id (cdr-safe (assoc 'topic_id post)))
         (post-number (cdr-safe (assoc 'post_number post)))
         (post-num-url (format "<a href=\"%s/t/%s/%s\">%s</a>"
                           discourse-api-server topic-id post-number post-number))
         (name (cdr-safe (assoc 'name post)))
         (username (cdr-safe (assoc 'username post)))
         (unseen (cdr-safe (assoc 'unseen post)))
         (avatar-template (cdr-safe (assoc 'avatar_template post)))
         (cooked (cdr-safe (assoc 'cooked post)))
         (action-code (cdr-safe (assoc 'action_code post)))
         (action-code-who (cdr-safe (assoc 'action_code_who post)))
         (line (format "<h2>%s.<img src=\"%s%s\">%s(%s)</h2> %s<hr>"
                       post-num-url
                       discourse-api-server
                       (string-replace "{size}" "40" avatar-template)
                       name
                       username
                       cooked)))
    line))

(defun discourse-topic-populate-topic (topic)
  "Populate TOPIC into a buffer and switch to it."
  (let* ((topic-buf (get-buffer-create discourse-topic-buffer-name)))
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
        (insert (format "<h2>%s</h2>posts:%s views:%s tags:%s<hr>"
                        (format "<a href=\"%s/t/%s/\">%s: %s</a>"
                           discourse-api-server id id title)
                        posts-count views tags))
        (dolist (post fmt-posts)
          (insert post))
        (insert "\n</body>\n</html>")
        (shr-render-region (point-min) (point-max) topic-buf)
        (discourse-topic-mode)
        (switch-to-buffer topic-buf)
        (goto-char (point-min))))))

(defun discourse-topic-show-topic ()
  "Show topic under point."
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (progn
          (discourse-topic-mark-as-read)
          (discourse-api-get-topic 'discourse-topic-populate-topic topicid))
      (message "No topic under point"))))

;; TODO: integration test
(defun discourse-topic-mark-as-read ()
  "Mark as read"
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (progn
          (discourse-api-mark-as-read topicid)
          (remove-text-properties (line-beginning-position) (line-end-position) '(face bold))
          (next-line))
      (message "No topic under point"))))

(defun discourse-topic-next-post ()
  "Jump to next post in a topic"
  (interactive)
  (end-of-line)
  (-when-let* ((sep "-------------------------")
               (pt (search-forward sep nil t)))
    (beginning-of-line)))

(defun discourse-topic-previous-post ()
  "Jump to next post in a topic"
  (interactive)
  (-when-let* ((sep "-------------------------")
               (pt (search-backward sep nil t)))
    (beginning-of-line)))

;;;###autoload
(define-derived-mode discourse-topic-mode special-mode "discourse-topic"
  "Topic mode.

\\{discourse-topic-mode-map}"
  :group 'discourse
  (buffer-disable-undo))

(provide 'discourse-topic)

;;; discourse-topic.el ends here
