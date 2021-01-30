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

;; TODO: store SSE process buffer

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

(defun urbit-init (url code)
  "Initialize urbit-http with URL and CODE."
  (setq urbit-url url)
  (setq urbit-code code)
  (setq urbit--uid (concat (format-time-string "%s")
                           "-"
                           (urbit--random-hex-string 6)))
  (setq urbit--last-event-id 0)
  (setq urbit--channel-url (concat urbit-url "/~/channel/" urbit--uid)))

(defun urbit-connect ()
  "Connect to ship described by `urbit-url' and `urbit-code'."
  (let ((url-request-method "POST")
        (url-request-data (concat "password=" urbit-code))
        (login-url (concat urbit-url "/~/login")))
    (message login-url)
    (message url-request-data)
    (with-current-buffer (url-retrieve-synchronously login-url)
      (goto-char (point-min))
      (search-forward "set-cookie: ")
      (setq urbit--cookie (buffer-substring (point) (line-end-position)))
      (string-match "-~\\([[:alpha:]-]*\\)=" urbit--cookie)
      (setq urbit-ship (match-string 1 urbit--cookie)))))

(defun urbit-start-sse ()
  ;; Make sure we don't have more than one sse buff
  (when urbit--sse-buff
    (when (get-buffer-process urbit--sse-buff)
      (delete-process urbit--sse-buff))
    (kill-buffer urbit--sse-buff))
  (setq urbit--sse-buff (sse-listener urbit--channel-url #'urbit--sse-callback)))

(defun urbit--event-id ()
  "Get id for next event."
  (setq urbit--last-event-id (+ urbit--last-event-id 1)))

;; TODO: handle errors in put
(defun urbit--put-object (url object &optional callback)
  "Asynchronously send a put request to URL with OBJECT as the request data.
Optionally calls CALLBACK on completion."
  (let ((url-request-method "PUT")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Cookie" . ,urbit--cookie)))
        (url-request-data (json-serialize object)))
    (url-retrieve url (if callback callback
                        (lambda (x)
                          (message "Object: %s" object)
                          (message "HTTP status: %s" x)
                          (message "Buffer: ")
                          (message (buffer-string)))))))

(defun urbit--send-message (action data)
  "Send a message to urbit with ACTION and DATA. Return id of sent message."
  (let ((id (urbit--event-id)))
    (urbit--put-object urbit--channel-url
                     (vector 
                      `((id . ,id)
                        (action . ,action)
                        ,@data)))
    id))

;; TODO: needs handles
(defun urbit-poke (app mark data &optional ok-callback err-callback)
  "Pokes APP with MARK DATA.
If OK-CALLBACK and ERR-CALLBACK are passed, the correct one will
be called when a poke response is recieved."
  ;; TODO: probably should set the callbacks before doing
  (let ((id (urbit--send-message "poke" `((ship . ,urbit-ship)
                                          (app . ,app)
                                          (mark . , mark)
                                          (json . ,data)))))
    (urbit--let-if-nil ((ok-callback
                         (lambda () (urbit--log "Poke %s is ok." id)))
                        (err-callback
                         (lambda (err)
                           (urbit--log "Error: %s For poke: %s." err id))))
      (add-to-list 'urbit--poke-handlers `(,id (ok . ,ok-callback) (err . ,err-callback))))))

;; TODO: needs handles
(defun urbit-subscribe (app path &optional
                            event-callback err-callback quit-callback)
  "Subscribe to an APP on PATH.
EVENT-CALLBACK for each event recieved with the event as argument.
ERR-CALLBACK is called on errors with the error as argument.
QUIT-CALLBACK is called on quit."
  (let ((id (urbit--send-message "subscribe" `((ship . ,urbit-ship)
                                               (app . ,app)
                                               (path . ,path)))))
    (urbit--let-if-nil ((event-callback (lambda (event)))
                        (err-callback (lambda (err)))
                        (quit-callback (lambda ())))
      (add-to-list 'urbit--subscription-handlers
                   `(,id (event ,event-callback)
                         (err ,err-callback)
                         (quit ,quit-callback))))))

(defun urbit-unsubscribe (subscription)
  "Unsubscribe from SUBSCRIPTION."
  (urbit--send-message "unsubscribe" `((subscription . ,subscription))))

(defun urbit-ack (event-id)
  "Acknowledge EVENT-ID."
  (urbit--log "Acking %s" event-id)
  (urbit--send-message "ack" `((event-id . ,event-id))))

(defun urbit--handle-poke-response (data)
  (let-alist data
    (let ((handlers (alist-get .id urbit--poke-handlers)))
      (pcase data
        ((pred (assq 'ok))
         (funcall (alist-get 'ok handlers)))
        ((pred (assq 'err))
         (funcall (alist-get 'err handlers) .err))
        (-- (urbit--log "Invalid poke response."))))))

(defun urbit--sse-callback (event)
  "Handle server sent EVENTs."
  (urbit--log "SSE recieved: %s" event)
  (let-alist event
    (urbit-ack (string-to-number .id))
    (let ((data (json-parse-string .data :object-type 'alist)))
      (pcase (alist-get 'response data)
        ("poke" (urbit--handle-poke-response data))
        (-- (urbit--log "Unknown data"))))))

;; TODO: close connection

;;
;; Application layer
;;

(defun urbit-helm-hi (msg)
  (urbit-poke "hood" "helm-hi" msg))

;;
;; Debugging tools
;;

(defun hello-world ()
  (urbit-helm-hi "Hello, World!"))

(defun gozod ()
  (urbit-init "http://localhost:8080" "lidlut-tabwed-pillex-ridrup")
  (urbit-connect)
  (hello-world)
  (urbit-start-sse))

;; TODO: disconnect function

(provide 'urbit-http)

;;; urbit-http.el ends here
