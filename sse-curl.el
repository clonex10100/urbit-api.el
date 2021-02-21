;;; sse-curl.el --- Code for getting SSEs with curl -*- lexical-binding: t -*-

;; Author: Noah Evans <noah@nevans.me>

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

;; Somewhat based off of elfeed-curl.el

;;; Code:

(defvar sse-curl-program-name "curl"
  "Name of the executable sse-curl uses.")

(defvar-local sse-curl--callback nil
  "Callback function for when curl dissconnects")

(defun sse-curl--args (url &optional cookie extra-args)
  "Return list of args for curl."
  (let ((args ()))
    (push "--disable" args)
    (push "--silent" args)
    (push "--location" args)
    (push "-HAccept: text/event-stream" args)
    (push "-HCache-Control: no-cache" args)
    (when cookie
      (push "--cookie" args)
      (push cookie args))
    (when extra-args
      (setf args (nconc (reverse extra-args) args)))
    (nreverse (cons url args))))

(defun sse-curl--sentinel (process status)
  "Manage the end of curl process' life.
We should reconnect unless given status 204."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (funcall sse-curl--callback
               (if (string-match "exited abnormally with code \\([0-9]+\\)" status)
                   (string-to-number (match-string 1 status))
                 (format "Unknown status: %s" status))))))

(defun sse-curl-retrieve (url buffer &optional callback cookie extra-args)
  (with-current-buffer buffer
    (let* ((args (sse-curl--args url cookie extra-args))
           (process (apply #'start-process "sse-curl" (current-buffer)
                           sse-curl-program-name args)))
      (setq sse-curl--callback callback)
      (set-process-query-on-exit-flag process nil)
      (setf (process-sentinel process) #'sse-curl--sentinel))))

(provide 'sse-curl)

;;; sse-curl.el ends here
