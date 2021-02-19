;;; urbit-chat.el --- Library for interacting with urbit chats -*- lexical-binding: t -*-

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

;; Simple urbit chat application

;;; Code:
(require 'aio)
(require 'urbit)
(require 'urbit-graph)

(defgroup urbit-chat nil
  "Urbit chat client for Emacs."
  :prefix "urbit-chat-"
  :group 'applications)

(defcustom urbit-chat-initial-messages 15
  "How many past messages to load when opening a chat.")

;; TODO: parse urls and stuff
(defun urbit-chat-send-message (ship chat message)
   (urbit-graph-add-node
    ship
    chat
    (urbit-graph-make-node
     (urbit-graph-make-post `(((text . ,message)))))))

;; Example of a chat node:
;; (170141184504919577857035894011388936323
;;    (post
;;     (index . "/170141184504919577857035894011388936323")
;;     (author . "risruc-habteb")
;;     (time-sent . 1613694524117)
;;     (signatures .
;;                 [])
;;     (contents .
;;               [((text . "classic stuff"))])
;;     (hash))
;;    (children))
(defun urbit-chat-format-node (node)
  (let ((post (alist-get 'post node)))
    (let-alist post
      (concat .author ": "
              (urbit-chat-format-contents .contents)
              "\n"))))

(defun urbit-chat-format-contents (contents)
  (urbit-log "content: %s" contents)
  (string-join
   (mapcar (lambda (content)
             (pcase content
               ((and (app caar 'text)
                     (app cdar text))
                text)
               ((and (app caar 'url)
                     (app cdar url))
                (concat "*" url "*"))
               (x (urbit-log "Unknown type %s" x) "")))
           contents)))

(defun urbit-chat-handle-nodes (nodes buffer)
  (with-current-buffer buffer
      (dolist (node nodes)
        (urbit-chat-insert-message (urbit-chat-format-node node)))))

(defun urbit-chat-insert-message (message)
  (save-excursion
    (goto-char urbit-chat-prompt-start-marker)
    (let ((start (point))
          (inhibit-read-only t))
      (insert-before-markers message)
      (add-text-properties start (point)
                           '(read-only t)))))

(defun urbit-chat-self-insert-command (n)
  (interactive "p")
  (when (< (point) urbit-chat-prompt-end-marker)
    (goto-char (point-max)))
  (self-insert-command n))

(defun urbit-chat-return ()
  (interactive)
  (let ((msg (buffer-substring urbit-chat-prompt-end-marker
                               (point-max))))
    (delete-region urbit-chat-prompt-end-marker
                   (point-max))
    (urbit-chat-send-message urbit-chat-ship urbit-chat-chat msg)
    (point-max)))

(defvar urbit-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'urbit-chat-return)
    map))

(substitute-key-definition
 'self-insert-command
 'urbit-chat-self-insert-command
 urbit-chat-mode-map
 global-map)

(defvar urbit-chat-prompt-start-marker)
(defvar urbit-chat-prompt-end-marker)
(defvar urbit-chat-ship)
(defvar urbit-chat-chat)

(defun urbit-chat-update-prompt ()
  (save-excursion
    (let ((start (marker-position urbit-chat-prompt-start-marker))
          (inhibit-read-only t))
      (goto-char urbit-chat-prompt-end-marker)
      (insert-before-markers "Enter message: ")
      (set-marker urbit-chat-prompt-start-marker start)
      (add-text-properties urbit-chat-prompt-start-marker
                           urbit-chat-prompt-end-marker
                           '(read-only t
                             rear-nonsticky t)))))

(aio-defun urbit-chat-mode (ship name)
  (kill-all-local-variables)
  (use-local-map urbit-chat-mode-map)
  (setq-local urbit-chat-prompt-start-marker (point-max-marker))
  (setq-local urbit-chat-prompt-end-marker (point-max-marker))
  (setq-local urbit-chat-ship ship)
  (setq-local urbit-chat-chat name)

  (urbit-chat-update-prompt)

  (let ((buffer (current-buffer)))
    (let ((nodes ))
      (urbit-chat-handle-nodes
       (sort (aio-await
              (urbit-graph-get-newest ship
                                      name
                                      urbit-chat-initial-messages))
             ;; HACK: clients should probably not need to parse and sort data
             ;; recieved from urbit-graph
             (lambda (a b)
               (< (car (urbit-graph-index-symbol-to-list (car a)))
                  (car (urbit-graph-index-symbol-to-list (car b))))))
       buffer)
      (let ((resource
             (urbit-graph-watch ship
                                name
                                (lambda (nodes)
                                  (urbit-log "Hit the lambda")
                                  (urbit-chat-handle-nodes
                                   nodes
                                   buffer)))))
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (urbit-graph-stop-watch resource))
                  nil
                  'local-hook)))
    (run-mode-hooks 'urbit-chat-mode-hook)))


;; TODO: cache keys so that starting chat's doesn't take so long
(defun urbit-chat-start ()
  (interactive)
  (let ((read (completing-read "Choose a chat: "
                               (aio-wait-for (urbit-graph-get-keys)))))
    (let* ((resource (urbit-graph-symbol-to-resource (intern read)))
           (ship (car resource))
           (name (cdr resource))
           (buffer (concat "*urbit-" ship "/" name "-chat*")))
      (if (buffer-live-p buffer) (switch-to-buffer buffer)
        (progn
          (switch-to-buffer buffer)
          (urbit-chat-mode ship name))))))


(provide 'urbit-chat)
;;; urbit-chat.el ends here
