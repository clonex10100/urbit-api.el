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
(require 'urbit-metadata)

(aio-defun urbit-launch (url code)
  "All in one intialization function to connect to ship at URL with CODE."
  (urbit-http-init url code)
  (aio-await (urbit-http-connect))
  (aio-await (urbit-http-poke "hood" "helm-hi" "Opening elisp airlock."))
  (urbit-http-start-sse)
  (aio-await (urbit-graph-init))
  (aio-await (urbit-metadata-init)))

(provide 'urbit)

;;; urbit.el ends here
