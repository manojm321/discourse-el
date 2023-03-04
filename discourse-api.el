;;; -*- lexical-binding: t; -*-
;;; Code:
;;;; Dependencies

(require 'json)
(require 'subr-x)

;;; Options
;;;; Customizations

(defvar discourse-server "http://localhost"
  "Discourse server url.")

(defconst discourse-api-key "some-key" )
(defconst discourse-username "civilized" )

;;;; Variables
;;;;; Keymap

;;;; Functions
;;;;; Discourse API

(defun discourse-api-process-kill-quietly (proc)
  "Kill discourse sentinel process PROC quitely."
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defun discourse-api-curl-ep (ep method on-success &optional on-error)
  "Curl the endpoint EP with METHOD and call ON-SUCCESS if the exit code is 0.

call ON-ERROR on any other exit code. Both callbacks will receive
the result of the call as an argument."
  (let* ((buf (generate-new-buffer " discourse"))
         (err-buf (generate-new-buffer " discourse-err"))
         (command (list "curl"
                        "-H" "Content-Type: application/json"
                        "-H" (format "%s: %s" "Api-Key" discourse-api-key)
                        "-H" (format "%s: %s" "Api-Username" discourse-username)
                        "-X" method
                        (concat discourse-server ep))))
    (make-process
     :name "discourse"
     :buffer buf
     :stderr err-buf
     :command command
     :noquery t
     :connection-type 'pipe'
     :sentinel
     (lambda (proc _)
       (unwind-protect
           (let* ((exit-code (process-exit-status proc)))
             (cond
              ((zerop exit-code)
               (funcall on-success buf))
              (t
               (if on-error
                   (funcall on-error err-buf)
                 (message (format "%s Failed with exit code %d" command exit-code))))))
         (discourse-api-process-kill-quietly proc))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (ignore-errors (kill-buffer err-buf))))))

(defun discourse-api-latest-topics (cb)
  "Fetch latest topics and call CB with resulting json string."
  (discourse-api-curl-ep "/latest.json"
                         "GET"
                         (lambda (buf)
                           (let ((json (with-current-buffer buf
                                         (json-read-from-string (buffer-string)))))
                             (funcall cb json)))))

(defun discourse-api-top-topics (cb)
  "Fetch top topics and call CB with resulting json string."
  (discourse-api-curl-ep "/top.json"
                         "GET"
                         (lambda (buf)
                           (let ((json (with-current-buffer buf
                                         (json-read-from-string (buffer-string)))))
                             (funcall cb json)))))

(defun discourse-api-unread-topics (cb)
  "Fetch unread topics and call CB with resulting json string."
  (discourse-api-curl-ep "/unread.json"
                     "GET"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(defun discourse-api-get-topic(cb topicid)
  "Fetch topic info for TOPICID and call CB with resulting json."
  (discourse-api-curl-ep (format "/t/%s.json" topicid)
                     "GET"
                     (lambda (buf)
                       (let ((json (with-current-buffer buf
                                     (json-read-from-string (buffer-string)))))
                         (funcall cb json)))))

(provide 'discourse-api)

;;; discourse-api.el ends here
