;;; discourse-topic.el --- Topic interaction -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand
;; URL: http://github.com/manojm321/discourse-el
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (dash "2.19"))
;; Version: 0.0.1

;;; Commentary:
;; Functions dealing with a single discourse topic

;;; Code:
;;;; Dependencies

(require 'dash)
(require 'shr)
(require 'discourse-api)

;;;; Customizations

;;;; Variables
(defvar discourse-topic-buffer-name "*discourse-topic*"
  "Name of topic buffer.")

(defvar discourse-topic-load-avatar t
  "Set to nil for faster topic load.")

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
         (avatar-template (cdr-safe (assoc 'avatar_template post)))
         (avatar-url (format "<img src=\"%s%s\">"
                             discourse-api-server
                             (string-replace "{size}" "40" avatar-template)))
         (cooked (cdr-safe (assoc 'cooked post)))
         (line (format "<h2>%s.%s%s(%s)</h2> %s<hr>"
                       post-num-url
                       (if discourse-topic-load-avatar avatar-url "")
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
        (insert (format "<h2>%s</h2><div>posts:%s views:%s tags:%s</div><hr>"
                        (format "<a href=\"%s/t/%s/\">%s: %s</a>"
                           discourse-api-server id id title)
                        posts-count views tags))
        (dolist (post fmt-posts)
          (insert post))
        (insert "\n</body>\n</html>")))))

(defun discourse-topic-show-topic ()
  "Show topic under point."
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (progn
          (discourse-topic-mark-as-read)
          (discourse-api-get-topic 'discourse-topic-populate-topic topicid t)
          ;; this is a bit ugly. Ideally next 4 lines should be part of
          ;; -populate-topic but separating out the redering this way simplifies
          ;; the testing. shr-render-buffer doesn't work well in --batch mode
          ;; used for tests.
          (with-current-buffer discourse-topic-buffer-name
            (discourse-topic-mode)
            (switch-to-buffer discourse-topic-buffer-name)
            (goto-char (point-min))))
      (message "No topic under point"))))

(defun discourse-topic-get-topic-at-point ()
  "Returns topic under point as json and marks it as read."
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (progn
          (discourse-topic-mark-as-read)
          (discourse-api-get-topic (lambda (json) json) topicid t))
      (message "No topic under point"))))

;; TODO: integration test
(defun discourse-topic-mark-as-read ()
  "Mark as read."
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (progn
          (discourse-api-mark-as-read topicid)
          (remove-text-properties (line-beginning-position) (line-end-position) '(face bold))
          (forward-line))
      (message "No topic under point"))))

(defun discourse-topic-next-post ()
  "Jump to next post in a topic."
  (interactive)
  (end-of-line)
  (-when-let* ((sep "-------------------------")
               (pt (search-forward sep nil t)))
    (beginning-of-line)))

(defun discourse-topic-previous-post ()
  "Jump to next post in a topic."
  (interactive)
  (-when-let* ((sep "-------------------------")
               (pt (search-backward sep nil t)))
    (beginning-of-line)))

;;;###autoload
(define-derived-mode discourse-topic-mode special-mode "discourse-topic"
  "Topic mode.

\\{discourse-topic-mode-map}"
  :group 'discourse
  (shr-render-region (point-min) (point-max))
  (buffer-disable-undo))

(provide 'discourse-topic)

;;; discourse-topic.el ends here
