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
(require 'seq)


(defun sse-parse (sse-string)
  "Parse SSE-STRING into an alist.
Return nil if it can't be parsed"
  (save-match-data
    ;; Strip leading and trailing newlines
    (let* ((sse-string (replace-regexp-in-string "\\(\n\\|\r\\)*\\'" "" sse-string))
           (sse-string (replace-regexp-in-string "\\`\\(\n\\|\r\\)*" "" sse-string)))
      (if (= (length sse-string) 0) nil
        ;;(message "SSE string: %s "sse-string)
        (delq nil
              (seq-map #'sse-parse-line
                       (split-string sse-string "\n\\|\r")))))))


(defun sse-parse-line (sse-line)
  "Parse SSE-LINE into a pair of name and data.
Return nil if it's a comment or can't be parsed."
  ;; If it starts with a colon it's a comment, ignore it
  (if (= (elt sse-line 0) ?:) nil
    (save-match-data
      (let ((match (string-match ".*: " sse-line)))
        (if (not match) nil
          (let ((name (substring sse-line 0 (- (match-end 0) 2)))
                (data (substring sse-line (match-end 0))))
            (cons name data)))))))


(defun sse-listener (url callback)
  "Listen to URL for SSEs, calling CALLBACK on each one.
Uses `url-retrive' internally, so the relevent variables apply"
  (let ((buff (url-retrieve url (lambda (&rest _) (message (concat url ": Stream ended"))))))
    (with-current-buffer buff
      (make-variable-buffer-local 'sse-handler)
      (let ((delim-regex ".\\(\\(\r\r\\)\\|\\(\n\n\\)\\|\\(\r\n\r\n\\)\\)")
            (passed-header nil) ;; The header isn't an event, we need to skip the first chunk
            (event-start (point-min)))
        (setq sse-handler
              (lambda ()
                (save-excursion
                  (save-match-data
                    (goto-char event-start)
                    (while (re-search-forward delim-regex nil t)
                      (if (not passed-header) (setq passed-header t)
                        (let ((sse-event (sse-parse (buffer-substring event-start (point)))))
                          (when sse-event
                            (funcall callback sse-event))))
                      (setq event-start (point)))))))))))


(defun url-filter-advice (proc data)
  "Advice for `url-http-generic-filter' that runs SSE handler code."
  (when (process-buffer proc)
    (with-current-buffer (process-buffer proc)
      (when (boundp 'sse-handler)
        (funcall sse-handler)))))


(advice-add #'url-http-generic-filter :after #'url-filter-advice)

(provide 'sse)

;;; sse.el ends here
