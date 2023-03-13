;;; discourse-topics.el --- Interaction with a set of topics  -*- lexical-binding: t; -*-

;; Author: Manoj Kumar Manikchand
;; URL: http://github.com/manojm321/discourse-el
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (dash "2.19"))
;; Version: 0.0.1

;;; Commentary:
;; Functions dealing with a set of topics

;;; Code:
;;;; Dependencies

(require 'shr)
(require 'discourse-api)
(require 'discourse-topic)

;;;; Customizations

;;;;; Variables
(defvar discourse-topics-buffer-name "*discourse-topics*"
  "Name of topics buffer")

;;;;; Faces
(defgroup discourse-faces nil
  "Type faces (fonts) used in Discourse."
  :group 'discourse
  :group 'faces)

(defface discourse-new-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for a new topic title."
  :group 'discourse-faces)

(defface discourse-topics-highlight-face
  `((t :inherit hl-line :weight bold :underline t
       ,@(and (>= emacs-major-version 27) '(:extend t))))
  "Face for the header at point."
  :group 'discourse-faces)

;;;;; Keymap

;;;###autoload
(defvar discourse-topics-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Section controls
    (define-key keymap (kbd "q")   #'quit-window)
    (define-key keymap (kbd "RET") #'discourse-topic-show-topic)
    (define-key keymap (kbd "w")   #'discourse-topics-copy-topic-url)
    (define-key keymap (kbd "v")   #'discourse-topics-visit-topic)
    (define-key keymap (kbd "g")   #'discourse-topics-refresh)
    (define-key keymap (kbd "n")   #'next-line)
    (define-key keymap (kbd "p")   #'previous-line)
    (define-key keymap (kbd "r")   #'discourse-topic-mark-as-read)

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
        (setq line (propertize line 'face 'discourse-new-face)))
    (if (eq closed 't)
        (setq line (propertize line 'face 'shadow)))
      line))

(defun discourse-topics-populate-topics (response)
  "Populate discourse topics RESPONSE."
  (let* ((buf (get-buffer-create discourse-topics-buffer-name)))
    (with-current-buffer buf
        (let* ((topics (cdr-safe (assoc 'topics (assoc 'topic_list response))))
               (topic-lines (mapcar #'discourse-topics--format-topic-line topics))
               (inhibit-read-only t))
          (save-excursion
            (erase-buffer)
            (insert (format "%-8s %-5s %-3s %-5s %-60s\n"
                            "ID" "Posts" "New" "Reply" "Title"))
            (dolist (topic-line topic-lines)
              (insert topic-line)
              (insert "\n"))
            (discourse-topics-mode)
            (hl-line-mode 1)
            (switch-to-buffer buf))
          (next-line)))))

(defun discourse-topics-count-topics (response)
  "Return count of topics"
  (length (cdr-safe (assoc 'topics (assoc 'topic_list response)))))

;;;;; Commands

;;;###autoload
(defun discourse-topics-new ()
  "Display new topics in a buffer."
  (interactive)
  (discourse-api-get-topics 'new 'discourse-topics-populate-topics))

;;;###autoload
(defun discourse-topics-top ()
  "Display top topics in a buffer."
  (interactive)
  (discourse-api-get-topics 'top 'discourse-topics-populate-topics))

;;;###autoload
(defun discourse-topics-unread ()
  "Display unread topics in a buffer."
  (interactive)
  (discourse-api-get-topics 'unread 'discourse-topics-populate-topics))

(defun discourse-topics-visit-topic()
  "Visit topic in a browser"
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav)))
    (if topicid
        (browse-url (format "%s/t/%s" discourse-api-server topicid)))
    (message "No URL found under point")))

(defun discourse-topics-copy-topic-url()
  "Visit topic in a browser"
  (interactive)
  (let* ((topicid (get-text-property (point) 'discourse-nav))
         (url (format "%s/t/%s" discourse-api-server topicid)))
    (kill-new url)
    (message (format "copied: %s" url))))

(defun discourse-topics-refresh()
  "Refresh current topics view"
  (interactive)
  (discourse-api-get-topics discourse-api--last-call 'discourse-topics-populate-topics))

;;;###autoload
(define-derived-mode discourse-topics-mode special-mode "discourse-topics"
  "Base mode for Discourse modes.

\\{discourse-topics-mode-map}"
  :group 'discourse
  (buffer-disable-undo)
  (setq truncate-lines t
        overwrite-mode nil)
  (set (make-local-variable 'hl-line-face) 'discourse-topics-highlight-face))

(provide 'discourse-topics)

;;; discourse-topics.el ends here

