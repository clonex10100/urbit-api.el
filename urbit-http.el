;;; urbit-http.el --- Library to interact with an urbit ship over HTTP -*- lexical-binding: t -*-

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
(require 'sse-listener)


;;
;; Variables
;;
(setq urbit-url nil)
(setq urbit-code nil)

(setq urbit-uid (concat (format-time-string "%s") "-" "ntsand"))
(setq urbit-channel-url (concat urbit-url "/~/channel/" urbit-uid))

(setq urbit-ship nil)
(setq urbit-event-id 0)
(setq urbit-cookie nil)


(defun random-hex-string (digits)
  (format "%x" (random (expt 16 digits))))

(defun urbit-init (url code)
  "Run before connecting"
  (setq urbit-url url)
  (setq urbit-code code)
  (setq urbit-uid (concat (format-time-string "%s") "-" (random-hex-string 6)))
  (setq urbit-channel-url (concat urbit-url "/~/channel/" urbit-uid)))

(defun urbit-connect ()
  "Create a connection to an urbit ship. Sets URBIT-COOKIE and URBIT-SHIP"
  (let ((url-request-method "POST")
        (url-request-data (concat "password=" urbit-code))
        (login-url (concat urbit-url "/~/login")))
    (message login-url)
    (message url-request-data)
    (with-current-buffer (url-retrieve-synchronously login-url)
      ;; (message (buffer-string))
      (goto-char (point-min))
      (search-forward "set-cookie: ")
      (setq urbit-event-id 0)
      (setq urbit-cookie (buffer-substring (point) (line-end-position)))
      (string-match "-~\\([[:alpha:]-]*\\)=" urbit-cookie)
      (setq urbit-ship (match-string 1 urbit-cookie)))))

(defun urbit-get-event-id ()
  (setq urbit-event-id (+ urbit-event-id 1)))

(defun http-put-object (url object)
  ;; TODO: async this
  (let ((url-request-method "PUT")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ;; ("Cookie" . ,urbit-cookie)
                                     ))
        (url-request-data (json-serialize object)))
    ;; (message url-request-data)
    ;; (message urbit-cookie)
    (with-current-buffer (url-retrieve-synchronously url)
      (buffer-string))))

(defun urbit-send-message (action data)
  (http-put-object urbit-channel-url
                   (vector 
                    (append
                     `((id . ,(urbit-get-event-id))
                       (action . ,action))
                     data))))

(defun urbit-poke (app mark data)
  (urbit-send-message "poke" `((ship . ,urbit-ship)
                               (app . ,app)
                               (mark . , mark)
                               (json . ,data))))

(defun urbit-subscribe (app path)
  (urbit-send-message "subscribe" `((ship . ,urbit-ship)
                                    (app . ,app)
                                    (path . ,path))))

(defun urbit-ack (event-id)
  (urbit-send-message "ack" `((event-id . ,event-id))))

;;
;; Debugging tools
;;

(defun hello-world ()
  (urbit-poke "hood" "helm-hi" "Hello, World!"))

(defun log-sse (sse)
  (message "SSE Event: ")
  (message "%s" sse)
  (message "-----------"))

(defun urbit-log-channel ()
  (sse-listener urbit-channel-url #'log-sse))

(defun gozod ()
  (urbit-init "http://localhost:8080" "lidlut-tabwed-pillex-ridrup")
  (urbit-connect)
  (hello-world))


(provide 'urbit-http)

;;; urbit-http.el ends here
