;;; sse.el --- SSE client library -*- lexical-binding: t; -*-

;; Author: Noah Evans <noah@nevans.me>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/clonex10100/sse.el

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

(require 'sse-curl)

(defvar-local sse--callback nil
  "Callback for sse buffer")
(defvar-local sse--retry 3000
  "Time in milliseconds to wait before trying to reopen closed SSE connection.")
(defvar-local sse--url nil
  "Url for SSE buffer's stream.")
(defvar-local sse--last-id nil
  "Id of last SSE recieved.")
(defvar-local sse--cookie nil
  "Cookie for sse stream auth.")
(defvar-local sse--extra-curl-args nil
  "Extra args for curl.")

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
        (when-let* ((sse-string (buffer-substring (point-min)
                                                  (point)))
                    (sse-event (sse--parse sse-string)))
          (when-let (retry (alist-get 'retry sse-event))
            (setq sse--retry retry))
          (when-let (id (alist-get 'id sse-event))
            (setq sse--last-id id))
          (funcall sse--callback sse-event))
        (delete-region (point-min) (point))))))

(defun sse--make-sse-buff (url callback &optional cookie extra-curl-args)
  "Return a new buffer for parsing SSEs from URL, calling CALLBACK for each one."
  (let ((sse-buff (generate-new-buffer "*sse*")))
    (with-current-buffer sse-buff
      (setq-local after-change-functions nil)
      (setq-local inhibit-modification-hooks nil)
      (setq sse--callback callback)
      (setq sse--url url)
      (setq sse--cookie cookie)
      (setq sse--extra-curl-args extra-curl-args)
      (add-to-list 'after-change-functions #'sse--on-change))
    sse-buff))

(defun sse--make-closed-callback (sse-buff)
  "Return a callback for SSE-BUFF's `url-retrieve' that will attempt to reconnect."
  (lambda (status)
    (message "%s stopped with status %s, attempting to reconnect." sse-buff status)
    (if (numberp status)
        (if (= status 204)
            (progn
              (message "%s recieved 204, stopping." sse-buff)
              (kill-buffer sse-buff))
          (run-at-time (/ sse--retry 1000.0) nil
                       (lambda ()
                         (when (buffer-live-p sse-buff)
                           (sse--start-retrieve sse-buff))))))))

(defun sse--start-retrieve (sse-buff)
  "Start sse-curl-retrive to get events in sse-buff"
  (with-current-buffer sse-buff
    (let ((extra-args (nconc (when sse--last-id
                               (list
                                (format "-HLast-Event-ID: %s"
                                        sse--last-id)))
                             sse--extra-curl-args)))
      (sse-curl-retrieve sse--url
                         sse-buff
                         (sse--make-closed-callback sse-buff)
                         sse--cookie
                         extra-args))))

(defun sse-listener (url callback &optional cookie extra-curl-args)
  "Listen to URL for SSEs, calling CALLBACK on each one.
Returns a buffer that you should kill when you are done with the stream.
Uses `url-retrive' internally."
  (let ((sse-buff (sse--make-sse-buff url callback
                                      cookie extra-curl-args)))
    (sse--start-retrieve sse-buff)
    sse-buff))


(provide 'sse)

;;; sse.el ends here
