;;; sse.el --- SSE client library -*- lexical-binding: t -*-

;; Author: Noah Evans <noah@nevans.me>
;; Maintainer: Noah Evans <noah@nevans.me>
;; Version: 1.0
;; Package-Requires: (sse, url)
;; Keywords: SSE

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

;;; Code:

(require 'url)
(require 'url-http)
(require 'aio)
(require 'seq)
(provide 'subr-x)

(defconst sse-delim-regex ".\\(\\(\r\r\\)\\|\\(\n\n\\)\\|\\(\r\n\r\n\\)\\)"
  "Regex to delimit sse events.")


(defun sse--strip-outer-newlines (string)
  "Strip outer newlines from STRING."
  (save-match-data
    (let ((string (replace-regexp-in-string "\\(\n\\|\r\\)*\\'" "" string)))
      (replace-regexp-in-string "\\`\\(\n\\|\r\\)*" "" string))))

(defun sse--parse-line (sse-line)
  "Parse SSE-LINE into a pair of name and data.
Return nil if it's a comment or can't be parsed."
  ;; If it starts with a colon it's a comment, ignore it
  (if (= (elt sse-line 0) ?:) nil
    (save-match-data
      (when-let ((match (string-match ".*?: " sse-line)))
        (let ((name (substring sse-line 0 (- (match-end 0) 2)))
              (data (substring sse-line (match-end 0))))
          (cons (intern name) data))))))

(defun sse--parse (sse-string)
  "Parse SSE-STRING into an alist.
Return nil if it can't be parsed."
  ;; Strip leading and trailing newlines
  (let ((sse-string (sse--strip-outer-newlines sse-string)))
    (if (= (length sse-string) 0) nil
      (delq nil
            (seq-map #'sse--parse-line
                     (split-string sse-string "\n\\|\r\\'"))))))

(defun sse--on-change (&rest _)
  "Try to parse SSE's in buffer."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward sse-delim-regex nil t)
        (if (not sse--passed-header) (setq sse--passed-header t)
          (when-let* ((sse-string (buffer-substring (point-min)
                                                    (point)))
                      (sse-event (sse--parse sse-string)))
            (when-let (retry (alist-get 'retry sse-event))
              (setq sse--retry retry))
            (when-let (id (alist-get 'id sse-event))
              (setq sse--last-id id))
            (funcall sse--callback sse-event)))
        (delete-region (point-min) (point))))))

(defun sse--make-sse-buff (url callback &optional last-id)
  "Return a new buffer for parsing SSE's, calling CALLBACK for each one."
  (let ((sse-buff (generate-new-buffer "*sse*")))
    (with-current-buffer sse-buff
      (setq-local after-change-functions nil)
      (setq-local inhibit-modification-hooks nil)
      (setq-local sse--passed-header nil)
      (setq-local sse--callback callback)
      (setq-local sse--retry 3000)
      (setq-local sse--url url)
      (setq-local sse--last-id last-id)
      (add-to-list 'after-change-functions #'sse--on-change))
    sse-buff))

(defun sse--make-closed-callback (sse-buff)
  "Return a function to attempt to recconect to sse when url-retrive closes."
  (lambda (&rest _)
    (urbit--log "SSE stream closed, atempting recconect.")
    (with-current-buffer sse-buff
      (erase-buffer)
      ;; TODO: modify header with last-id
      (run-at-time (/ sse-retry 1000.0) nil #'sse--start-retrieve sse-buff))))

;; TODO: Handle cookies
(defun sse--start-retrieve (sse-buff)
  (with-current-buffer sse-buff
    (let* ((url-request-method "GET")
           (url-request-extra-headers (append
                                       `("Cache-Control" . "no-cache") 
                                       (when sse--last-event-id
                                         `("Last-Event-ID" . sse--last-event-id))))
           (callback (sse--make-closed-callback sse-buff))
           (retrieve-buff (url-retrieve sse--url callback)))
      (with-current-buffer retrieve-buff
        (setq-local sse--buff sse-buff)
        (setq-local sse--start (point-min))))))

(defun sse-listener (url callback)
  "Listen to URL for SSEs, calling CALLBACK on each one."
  (let ((sse-buff (sse--make-sse-buff url callback)))
    (sse--start-retrieve sse-buff)
    sse-buff))


(defun sse--url-filter-advice (proc _)
  "Run buffer local variable `sse-handler-function' if it is bound."
  (when (process-buffer proc)
    (with-current-buffer (process-buffer proc)
      (when (boundp 'sse--buff)
        (let ((data (prog1 (buffer-substring sse--start (point-max))
                      (setq sse--start (point-max-marker)))))
          (with-current-buffer sse--buff
            (save-excursion
              (goto-char (point-max))
              (insert data))))))))

(advice-add #'url-http-generic-filter :after #'sse--url-filter-advice)

;; TODO: Ability to close connection
;; TODO: Pass in cookies, headers, etc
;; TODO: Look at curl backend

;; TODO: Last-Event-ID header for reconnects
;; (advice-remove #'url-http-generic-filter #'sse--url-filter-advice)

(provide 'sse)

;;; sse.el ends here
