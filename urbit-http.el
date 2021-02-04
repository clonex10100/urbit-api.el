;; urbit-http.el --- Library to interact with an urbit ship over HTTP -*- lexical-binding: t -*-

;; Author: Noah Evans <noah@nevans.me>
;; Maintainer: Noah Evans <noah@nevans.me>
;; Version: 
;; Package-Requires: (dependencies)
;; Homepage: 
;; Keywords: urbit, http, api


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'url)
(require 'sse)
(require 'request)
(require 'aio)


;;
;; Variables
;;
(defvar urbit-url
  "Urbit ship url.")

(defvar urbit-code
  "Urbit ship code.")

(defvar urbit-ship
  "Name of connected urbit ship.")

(defvar urbit-log t
  "Logging toggle")

(defvar urbit--uid
  "UID for this urbit connection.
Should be set to the current unix time plus a 6 digit random hex string.")

(defvar urbit--last-event-id
  "Id of last sent event.")

(defvar urbit--channel-url
  "The channel url for connected ship.")

(defvar urbit--cookie
  "Auth cookie for current connection.")

(defvar urbit--sse-buff nil
  "Buffer that urbit is listening to SSE events on.")

(defvar urbit--poke-handlers '()
  "Alist of poke id's to handler functions")

(defvar urbit--subscription-handlers '()
  "Alist of subscription id's to handler functions")

(defmacro urbit--let-if-nil (spec &rest body)
  "Bind variables according to SPEC only if they are nil, then evaluate BODY.
 Useful for assigning defaults to optional args."
  (declare (indent 1))
  `(let ,(seq-map (lambda (s)
                    (let ((sym (car s))
                          (else (cadr s)))
                      `(,sym (or ,sym ,else))))
                  spec)
     ,@body))

(defun urbit--log (&rest msg-args)
  "If urbit-verbose is t, log to *urbit-log*"
  (when urbit-log
    (with-current-buffer "*urbit-log*"
      (goto-char (point-max))
      (insert (apply #'format msg-args))
      (insert "\n\n"))))

(defun urbit--random-hex-string (n)
  "Generate a random N digit hexadecimal string."
  (format "%x" (random (expt 16 n))))

(defun urbit--event-id ()
  "Get id for next event."
  (setq urbit--last-event-id (+ urbit--last-event-id 1)))

(defun urbit-init (url code)
  "Initialize urbit-http with URL and CODE."
  (setq urbit-url url)
  (setq urbit-code code)
  (setq urbit--uid (concat (format-time-string "%s")
                           "-"
                           (urbit--random-hex-string 6)))
  (setq urbit--request-cookie-jar nil)
  (setq urbit--last-event-id 0)
  (setq urbit--poke-handlers nil)
  (setq urbit--subscription-handlers nil)
  (setq urbit--channel-url (concat urbit-url "/~/channel/" urbit--uid)))

(defun urbit-connect ()
  "Connect to ship described by `urbit-url' and `urbit-code'."
  (let ((url-request-method "POST")
        (url-request-data (concat "password=" urbit-code))
        (login-url (concat urbit-url "/~/login")))
    (with-current-buffer (url-retrieve-synchronously login-url)
      (urbit--temp-request-login login-url urbit-code)
      (urbit--log "Login buff:\n%s" (buffer-string))
      (goto-char (point-min))
      (search-forward "set-cookie: ")
      (setq urbit--cookie (buffer-substring (point) (line-end-position)))
      (string-match "-~\\([[:alpha:]-]*\\)=" urbit--cookie)
      (setq urbit-ship (match-string 1 urbit--cookie)))))

(defun urbit-start-sse ()
  "Start recieving SSEs for current urbit connection."
  ;; Make sure we don't have more than one sse buff
  (when (buffer-live-p urbit--sse-buff) (kill-buffer urbit--sse-buff))
  (setq urbit--sse-buff
        (sse-listener urbit--channel-url #'urbit--sse-callback)))

(defun urbit--json-request-wrapper (method url &optional object)
  "Make a json request with METHOD to URL with json encodable OBJECT as data.
Return a promise that resolves to response object.
Uses `urbit--cookie' for authentication."
  ;; The only working way I've found to pass a cookie to `request' is
  ;; to set a cookie header on the first request, let request put it
  ;; in it's cookie jar, then stop passing the cookie header.
  (let* ((p (aio-make-callback :once t))
         (callback (car p))
         (promise (cdr p))
         (first-run (not urbit--request-cookie-jar))
         (request--curl-cookie-jar
          (progn
            (when first-run
              (setq urbit--request-cookie-jar (make-temp-file "urbit-request-cookie-jar-")))
            urbit--request-cookie-jar)))
    (request url
      :type method
      :headers `(("Content-Type" . "application/json")
                 ,@(when first-run `(("Cookie" . ,urbit--cookie))))
      :data (when object (json-encode object))
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-oter-keys)
                (funcall callback error-thrown)))
      :complete (lambda (&rest _)
                  (funcall callback nil)))
    promise))

(aio-defun urbit--send-message (action &optional data)
  "Send a message to urbit with ACTION and DATA. Return id of sent message."
  (let ((id (urbit--event-id)))
    (aio-await (urbit--json-request-wrapper "PUT"
                                            urbit--channel-url
                                            `[((id . ,id)
                                               (action . ,action)
                                               ,@data)]))
    id))

(aio-defun urbit-ack (event-id)
  "Acknowledge EVENT-ID."
  (aio-await (urbit--send-message "ack" `((event-id . ,event-id))))
  event-id)

(aio-defun urbit-poke (app mark data &optional ok-callback err-callback)
  "Pokes APP with MARK DATA. Return id of poke.
If OK-CALLBACK and ERR-CALLBACK are passed, the correct one will
be called when a poke response is recieved."
  (let ((id (aio-await
             (urbit--send-message "poke"
                                  `((ship . ,urbit-ship)
                                    (app . ,app)
                                    (mark . , mark)
                                    (json . ,data))))))
    (urbit--let-if-nil ((ok-callback
                         (lambda ()
                           (urbit--log "Poke %s is ok" id)))
                        (err-callback
                         (lambda (err)
                           (urbit--log "Poke %s error: %s" id err))))
      (add-to-list 'urbit--poke-handlers
                   `(,id (ok . ,ok-callback)
                         (err . ,err-callback)))
      id)))

(aio-defun urbit-subscribe (app
                            path
                            &optional
                            event-callback
                            err-callback
                            quit-callback)
  "Subscribe to an APP on PATH.
EVENT-CALLBACK for each event recieved with the event as argument.
ERR-CALLBACK is called on errors with the error as argument.
QUIT-CALLBACK is called on quit."
  (let ((id (aio-await
             (urbit--send-message "subscribe"
                                  `((ship . ,urbit-ship)
                                    (app . ,app)
                                    (path . ,path))))))
    (urbit--let-if-nil ((event-callback
                         (lambda (data)
                           (urbit--log "Subscription %s event: %s" id data)))
                        (err-callback
                         (lambda (err)
                           (urbit--log "Subscription %s error: %s" id err)))
                        (quit-callback
                         (lambda (data)
                           (urbit--log "Subscription %s quit: %s" id data))))
      (add-to-list 'urbit--subscription-handlers
                   `(,id (event . ,event-callback)
                         (err . ,err-callback)
                         (quit . ,quit-callback)))
      id)))

(aio-defun urbit-unsubscribe (subscription)
  "Unsubscribe from SUBSCRIPTION."
  (aio-await
   (urbit--send-message "unsubscribe"
                        `((subscription . ,subscription)))))

(aio-defun urbit-delete ()
  "Delete current channel connection."
  (aio-await
   (urbit--send-message "delete")))

(aio-defun urbit-scry (app path)
  (aio-await
   (urbit--request-wrapper "GET"
                           (concat urbit-url "/~/scry/" app path ".json"))))

(aio-defun urbit-spider (input-mark output-mark thread-name data)
  (aio-await
   (urbit--request-wrapper "POST"
                           (format "%s/spider/%s/%s/%s.json"
                                   urbit-url
                                   input-mark
                                   thread-name
                                   output-mark))))

(defun urbit--handle-poke-response (data)
  (let-alist data
    (let ((handlers (alist-get .id urbit--poke-handlers)))
      (cond
       (.ok (funcall (alist-get 'ok handlers)))
       (.err (funcall (alist-get 'err handlers) .err))
       (t (urbit--log "Invalid poke response.")))
      (setq urbit--poke-handlers
            (assq-delete-all .id urbit--poke-handlers)))))

(defun urbit--handle-subscription-response (data)
  (let-alist data
    (let ((handlers (alist-get .id urbit--subscription-handlers)))
      (pcase .response
        ((or "subscribe" "poke")
         (when .err
           (funcall (alist-get 'err handlers) .err)
           (setq urbit--subscription-handlers
                 (assq-delete-all .id urbit--subscription-handlers))))
        ("diff"
         (funcall (alist-get 'event handlers) .json))
        ("quit"
         (funcall (alist-get 'quit handlers) .json)
         (setq urbit--subscription-handlers
               (assq-delete-all .id urbit--subscription-handlers)))
        (--- (urbit--log "Invalid subscription response."))))))

(defun urbit--sse-callback (event)
  "Handle server sent EVENTs."
  (urbit--log "SSE recieved: %S" event)
  (aio-wait-for (urbit-ack (string-to-number (alist-get 'id event))))
  (let* ((data (json-parse-string (alist-get 'data event)
                                  :object-type 'alist)))
    (let-alist data
      (cond ((and (string= .response "poke")
                  (assq .id urbit--poke-handlers))
             (urbit--handle-poke-response data))
            ((assq .id urbit--subscription-handlers)
             (urbit--handle-subscription-response data))
            (t (urbit--log "Got response for untracked id: %s" .id))))))

;; TODO: disconnect function/other cleanup

(provide 'urbit-http)

;;; urbit-http.el ends here
