;;; urbit.el --- An api for interacting with an urbit ship -*- lexical-binding: t -*-

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

;; An api for interacting with an urbit ship.

;;; Code:
(require 'urbit-http)
(require 'urbit-graph)

(defvar urbit-ship nil
  "Urbit ship name.")
(defconst urbit-log-buffer "*urbit-log*"
  "Buffer for urbit log messages.")

(defconst urbit-da-unix-epoch
  170141184475152167957503069145530368000)
(defconst urbit-da-second
  18446744073709551616)

(defun urbit-milli-time ()
  "Time since the unix epoch in millseconds."
  (string-to-number (format-time-string "%s%3N")))

(defun urbit-da-time ()
  "Gets the current time as a string in urbit's @da encoding."
  (number-to-string
   (+ (/ (* (urbit-milli-time) urbit-da-second)
         1000)
      urbit-da-unix-epoch)))

(defun urbit-desig (ship)
  "Remove the sig from a SHIP string."
  (if (and (> (length ship) 0)
           (eq (elt ship 0)
               ?~))
      (substring ship 1)
    ship))

(defun urbit-log (&rest msg-args)
  "Log to `urbit-log-buffer'.  MSG-ARGS are passed to `format'."
  (with-current-buffer (get-buffer-create urbit-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format msg-args))
    (insert "\n\n")))

(aio-defun urbit-launch (url code)
  "All in one intialization function to connect to ship at URL with CODE."
  (urbit-http-init url code)
  (aio-await (urbit-http-connect))
  (aio-await (urbit-http-poke "hood" "helm-hi" "Opening elisp airlock."))
  (urbit-http-start-sse)
  (aio-await (urbit-graph-init)))

(provide 'urbit)

;;; urbit.el ends here
