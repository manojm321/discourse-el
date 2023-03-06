;;; discourse-main.el --- A mu4e like interface for Discourse   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand
;; Keywords: extensions, tools

;;; Commentary:
;; A mu4e like interface for Discourse

;;; Code:
;;;; Dependencies

(require 'discourse-topics)

;;; Options
;;;; Customizations

;;;; Variables
;;;;; Keymap

;;;###autoload
(defvar discourse-main-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "u") #'discourse-topics-unread)
    (define-key keymap (kbd "n") #'discourse-topics-new)
    (define-key keymap (kbd "t") #'discourse-topics-top)
    (define-key keymap (kbd "g") #'discourse)

    keymap)
  "Keymap for `discourse-main-mode'." )

(defun discourse-main--action-str (str func)
  "Highlight the key str"
  (let* ((newstr (replace-regexp-in-string
                  "\\[\\(..?\\)\\]"
                  (lambda(m)
                    (format "[%s]"
                            (propertize (match-string 1 m) 'face 'highlight)))
                  str))
         (map (make-sparse-keymap)))
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    newstr))

(defun discourse-main--buffer ()
  "Draw main buffer."
  (let* ((buf (get-buffer-create "*discourse-main*"))
         (num-unread (discourse-api-get-topics 'unread #'discourse-topics-count-topics t))
         (num-new (discourse-api-get-topics 'new #'discourse-topics-count-topics t))
         (num-top (discourse-api-get-topics 'top #'discourse-topics-count-topics t)))
    (with-current-buffer buf
      (save-excursion
        (setq inhibit-read-only t)
        (erase-buffer)
        (insert
         (propertize "* Discourse" 'face 'bold)
         "\n\n"
         ;; (propertize "  Basics" 'face 'success)
         ;; "\n\n"
         ;; (discourse-main--action-str "\t* [j]ump to some view\n" nil)
         ;; (discourse-main--action-str "\t* enter a [s]earch query\n" nil)
         ;; (discourse-main--action-str "\t* [C]ompose a new topic\n" nil)
         ;; "\n"
         (propertize "  Bookmarks\n\n" 'face 'success)
         (discourse-main--action-str (format "\t* [u]nread topics  %s\n" num-unread)
                                     #'discourse-topics-unread)
         (discourse-main--action-str (format "\t* [n]ew topics     %s\n" num-new)
                                     #'discourse-topics-new)
         (discourse-main--action-str (format "\t* [t]op topics(1w) %s\n" num-top)
                                     #'discourse-topics-top))
        (discourse-main-mode)
        (pop-to-buffer buf)))))

;;;;; discourse-main-mode functions

;;;###autoload
(defun discourse ()
  "main function."
  (interactive)
  (discourse-main--buffer))

;;;;; Commands

;;;###autoload
(define-derived-mode discourse-main-mode special-mode "discourse-main"
  "Major mode for Discourse main buffer.

\\{discourse-main-mode-map}"
  :group 'discourse
  (buffer-disable-undo))

(provide 'discourse-main)

;;; discourse-main.el ends here
