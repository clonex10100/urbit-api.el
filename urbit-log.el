;;; urbit-log.el --- Logging for urbit-api.el -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(defconst urbit-log-buffer "*urbit-log*"
  "Buffer for urbit log messages.")

(defun urbit-log (&rest msg-args)
  "Log to `urbit-log-buffer'.  MSG-ARGS are passed to `format'."
  (with-current-buffer (get-buffer-create urbit-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format msg-args))
    (insert "\n\n")))

(provide 'urbit-log)

;;; urbit-log.el ends here
