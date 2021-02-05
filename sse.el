;;; sse.el --- SSE client library -*- lexical-binding: t; -*-

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

;; Library to listen for SSEs.  Uses `url-retrieve', so if your stream
;; needs cookies they must be in its list.

;; Example usage:
;; (setq sse-buff
;;       (sse-listener "https://example.com/stream"
;;                     (lambda (sse)
;;                       (message "SSE recieved: %s" sse))))
;; When finished
;; (kill-buffer sse-buff)

;;; Code:

(require 'url)
(require 'url-http)
(provide 'subr-x)

(defvar-local sse--passed-header nil
  "Flag to keep track of if the http header has been passed.")
(defvar-local sse--callback nil
  "Callback for sse buffer")
(defvar-local sse--retry 3000
  "Time in milliseconds to wait before trying to reopen closed SSE connection.")
(defvar-local sse--url nil
  "Url for SSE buffer's stream.")
(defvar-local sse--last-id nil
  "Id of last SSE recieved.")
(defvar-local sse--retrieval-buff nil
  "`url-retrieve' buffer for this SSE buffer.")

(defvar-local sse--buff nil
  "SSE buffer for associated with current `url-retrieve' buffer.")
(defvar-local sse--read-point nil
  "Point marking the end of copied region in `url-retrieve' buffer.")

(defconst sse-delim-regex ".\\(\\(\r\r\\)\\|\\(\n\n\\)\\|\\(\r\n\r\n\\)\\)"
  "Regex to delimit SSEs.")


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
  (let ((sse-string (sse--strip-outer-newlines sse-string)))
    (if (= (length sse-string) 0) nil
      (delq nil
            (mapcar #'sse--parse-line
                    (split-string sse-string "\n\\|\r\\'"))))))

(defun sse--on-change (&rest _)
  "Try to parse new SSEs in buffer."
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

(defun sse--make-sse-buff (url callback)
  "Return a new buffer for parsing SSEs from URL, calling CALLBACK for each one."
  (let ((sse-buff (generate-new-buffer "*sse*")))
    (with-current-buffer sse-buff
      (setq-local after-change-functions nil)
      (setq-local inhibit-modification-hooks nil)
      (setq sse--passed-header nil)
      (setq sse--callback callback)
      (setq sse--retry 3000)
      (setq sse--url url)
      (setq sse--last-id nil)
      (setq sse--retrieval-buff nil)
      (make-local-variable 'kill-buffer-hook)
      ;; Kill the `url-retrive' buffer when this sse-buffer is kill.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (buffer-live-p sse--retrieval-buff)
                    (when (get-buffer-process sse--retrieval-buff)
                      (delete-process sse--retrieval-buff)
                      (kill-buffer sse--retrieval-buff)))))
      (add-to-list 'after-change-functions #'sse--on-change))
    sse-buff))

(defun sse--make-closed-callback (sse-buff)
  "Return a callback for SSE-BUFF's `url-retrieve' that will attempt to reconnect."
  (lambda (&rest _)
    (when (buffer-live-p sse-buff)
      (message "SSE stream: %s closed, atempting reconnect." sse-buff)
      (with-current-buffer sse-buff
        (erase-buffer)
        (setq sse--passed-header nil)
        (run-at-time (/ sse--retry 1000.0) nil
                     (lambda ()
                       (when (buffer-live-p sse-buff)
                         (sse--start-retrieve sse-buff))))))))

(defun sse--start-retrieve (sse-buff)
  "Start a `url-retrieve' to get events for SSE-BUFF."
  (with-current-buffer sse-buff
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Cache-Control" . "no-cache")
              ,@(when sse--last-id
                  `(("Last-Event-ID" . ,sse--last-id)))))
           (callback (sse--make-closed-callback sse-buff))
           (retrieval-buff (url-retrieve sse--url callback)))
      (setq sse--retrieval-buff retrieval-buff)
      (with-current-buffer retrieval-buff
        (setq sse--buff sse-buff)
        (setq sse--read-point (point-min))))))

(defun sse-listener (url callback)
  "Listen to URL for SSEs, calling CALLBACK on each one.
Returns a buffer that you should kill when you are done with the stream.
Uses `url-retrive' internally."
  (let ((sse-buff (sse--make-sse-buff url callback)))
    (sse--start-retrieve sse-buff)
    sse-buff))

(defun sse--url-filter-advice (proc _)
  "Copy new data from PROC's buff to `sse--buff', if it exists."
  (when (process-buffer proc)
    (with-current-buffer (process-buffer proc)
      (when sse--buff
        (let ((data (prog1 (buffer-substring sse--read-point (point-max))
                      (setq sse--read-point (point-max-marker)))))
          (with-current-buffer sse--buff
            (save-excursion
              (goto-char (point-max))
              (insert data))))))))

(advice-add #'url-http-generic-filter :after #'sse--url-filter-advice)


(provide 'sse)

;;; sse.el ends here
