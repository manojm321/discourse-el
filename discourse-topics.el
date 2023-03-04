;;; -*- lexical-binding: t; -*-
;;; Code:
;;;; Dependencies

(require 'shr)
(require 'discourse-api)
(require 'discourse-topic)

;;;; Customizations

;;;; Variables
;;;;; Keymap

;;;###autoload
(defvar discourse-topics-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "q")   #'quit-window)
    (define-key keymap (kbd "RET") #'discourse-topic-show-topic)
    (define-key keymap (kbd "w")   #'discourse-topics-copy-topic-url)
    (define-key keymap (kbd "v")   #'discourse-topics-visit-topic)

    keymap)
  "Keymap for `discourse-topics-mode'." )

;;;;; discourse-topics-mode functions

(defun discourse-topics--format-topic-line (topic)
  "Format a given TOPIC to be diplayed in ‘discourse’."
  (let* ((id (cdr-safe (assoc 'id topic)))
        (title (cdr-safe (assoc 'title topic)))
        (posts-count (cdr-safe (assoc 'posts_count topic)))
        (unseen (cdr-safe (assoc 'unseen topic)))
        (closed (cdr-safe (assoc 'closed topic)))
        (reply-count (cdr-safe (assoc 'reply_count topic)))
        (unread-posts (cdr-safe (assoc 'unread_posts topic)))
        (line (format "%-8s %-5s %-3s %-5s %-60s"
                id
                posts-count
                (or unread-posts 0)
                reply-count
                title))
        (line (propertize line 'discourse-nav id)))
    (if (eq unseen 't)
        (setq line (propertize line 'face 'bold)))
    (if (eq closed 't)
        (setq line (propertize line 'face 'shadow)))
      line))

(defun discourse-topics-populate-topics (response)
  "Populate discourse topics RESPONSE."
  (let* ((buf (get-buffer-create "*discourse-topics*")))
    (with-current-buffer buf
        (let* ((topics (cdr-safe (assoc 'topics (assoc 'topic_list response))))
               (topic-lines (mapcar #'discourse-topics--format-topic-line topics))
               (inhibit-read-only t))
          (save-excursion
            (erase-buffer)
            (insert (propertize (format "%-8s %-5s %-3s %-5s %-60s\n"
                                        "ID" "Posts" "New" "Reply" "Title")
                                'face 'bold))
            (dolist (topic-line topic-lines)
              (insert topic-line)
              (insert "\n"))
            (discourse-topics-mode)
            (switch-to-buffer buf))))))

;;;;; Commands

;;;###autoload
(defun discourse-topics-latest ()
  "Display latest topics in a buffer."
  (interactive)
  (discourse-api-latest-topics 'discourse-topics-populate-topics))


;;;###autoload
(defun discourse-topics-top ()
  "Display top topics in a buffer."
  (interactive)
  (discourse-api-top-topics 'discourse-topics-populate-topics))

;;;###autoload
(defun discourse-topics-unread ()
  "Display unread topics in a buffer."
  (interactive)
  (discourse-api-unread-topics 'discourse-topics-populate-topics))

(defun discourse-topics-visit-topic()
  "Visit topic in a browser"
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (browse-url (format "%s/t/%s" discourse-server topicid))))

(defun discourse-topics-copy-topic-url()
  "Visit topic in a browser"
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav))
         (url (format "%s/t/%s" discourse-server topicid)))
    (kill-new url)
    (message (format "copied: %s" url))))

;;;###autoload
(define-derived-mode discourse-topics-mode special-mode "discourse-topics"
  "Base mode for Discourse modes.

\\{discourse-topics-mode-map}"
  :group 'discourse
  (buffer-disable-undo))

(provide 'discourse-topics)

;;; discourse-topics.el ends here

