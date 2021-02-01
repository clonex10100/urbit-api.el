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
(provide 'subr-x)

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

(defun sse-listener (url callback)
  "Listen to URL for SSEs, calling CALLBACK on each one.
Uses `url-retrive' internally, so the relevent variables apply."
  (let* ((url-request-method "GET")
         ;; TODO: More robust stream end callback. Kill buffer etc.
         (buff (url-retrieve url (lambda (&rest _) (message (concat url ": Stream ended"))))))
    (with-current-buffer buff
      ;; Would a connection local variable work here?
      (setq-local sse-handler-function
                  (let ((delim-regex ".\\(\\(\r\r\\)\\|\\(\n\n\\)\\|\\(\r\n\r\n\\)\\)")
                        (passed-header nil) ;; The header isn't an event, so we need to skip the first chunk
                        (event-start (point-min)))
                    (lambda ()
                      (save-excursion
                        (save-match-data
                          (goto-char event-start)
                          (while (re-search-forward delim-regex nil t)
                            (if (not passed-header) (setq passed-header t)
                              (when-let ((sse-event (sse--parse (buffer-substring event-start (point)))))
                                (funcall callback sse-event)))
                            (setq event-start (point))))))))
      buff)))

(defun sse--url-filter-advice (proc _)
  "Run buffer local variable `sse-handler-function' if it is bound."
  (when (process-buffer proc)
    (with-current-buffer (process-buffer proc)
      (when (boundp 'sse-handler-function)
        (funcall sse-handler-function)))))

(advice-add #'url-http-generic-filter :after #'sse--url-filter-advice)


(provide 'sse)

;;; sse.el ends here
