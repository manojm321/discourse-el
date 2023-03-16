;;; discourse-api.el --- API interaction -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manoj Kumar Manikchand

;; Author: Manoj Kumar Manikchand
;; URL: http://github.com/manojm321/discourse-el
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (dash "2.19"))
;; Version: 0.0.1

;;; Commentary:
;; Functions dealing with discourse API

;;; Code:
;;;; Dependencies

(require 'json)
(require 'subr-x)

;;; Options
;;;; Customizations

(defvar discourse-api-server "http://localhost"
  "Discourse server url.")

(defvar discourse-api-key "some-key"
  "API key.")
(defvar discourse-api-username "civilized"
  "Username.")

;;;; Variables
(defconst discourse-api-topics-endpoints '((latest . "/latest.json")
                                       (new . "/new.json")
                                       (top . "/top.json")
                                       (unread . "/unread.json"))
"Topics endpoint mapping.")

(defvar discourse-api--last-call nil
  "Last api endpoint type that was called.")

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

(defun discourse-api-curl-ep (ep method on-success &optional on-error sync data)
  "Curl the endpoint EP with METHOD and call ON-SUCCESS if the exit code is 0.

call ON-ERROR on any other exit code.  Both callbacks will receive
the result of the call as an argument.  Set SYNC to wait for the
call to complete.  DATA will be encoded into json before pass to
-d option"
  (let* ((buf (generate-new-buffer " discourse"))
         (proc-finished nil)
         (err-buf (generate-new-buffer " discourse-err"))
         (command (list "curl"
                        "-H" "Content-Type: application/json"
                        "-H" (format "%s: %s" "Api-Key" discourse-api-key)
                        "-H" (format "%s: %s" "Api-Username" discourse-api-username)
                        "-X" method
                        (if data (concat "-d " (json-encode data)) "")
                        (concat discourse-api-server ep))))
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
         (discourse-api-process-kill-quietly proc)
         (setq proc-finished t))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (ignore-errors (kill-buffer err-buf))))
    (while (and sync (not proc-finished))
      (sleep-for 0.1))))

(defun discourse-api-get-topics (type cb &rest sync)
  "Fetch topics of TYPE and call CB with resulting json string.

return value returned by CB, valid when SYNC is set to t."
  (let* ((return nil))
    (setq discourse-api--last-call type)
    (discourse-api-curl-ep (cdr (assoc type discourse-api-topics-endpoints))
                         "GET"
                         (lambda (buf)
                           (let* ((json (with-current-buffer buf
                                         (json-read-from-string (buffer-string)))))
                                 (setq return (funcall cb json))))
                         nil
                         sync)
    return))

(defun discourse-api-mark-as-read (topicid)
  "Mark TOPICID as read."
  (discourse-api-curl-ep "/topics/timings"
                     "POST"
                     (lambda (_) ())
                     nil
                     t
                     `((topic_id . ,topicid)
                       ;; dummy timing values, in ms?
                       (topic_time . 2000)
                       (timings\[1\] . 10))))

(defun discourse-api-get-topic (cb topicid &rest sync)
  "Fetch topic info for TOPICID and call CB with resulting json."
  (let* ((return nil))
    (discourse-api-curl-ep (format "/t/%s.json" topicid)
                           "GET"
                           (lambda (buf)
                             (let ((json (with-current-buffer buf
                                           (json-read-from-string (buffer-string)))))
                                 (setq return (funcall cb json))))
                         nil
                         sync)
    (discourse-api-mark-as-read topicid)
    return))

(provide 'discourse-api)

;;; discourse-api.el ends here
