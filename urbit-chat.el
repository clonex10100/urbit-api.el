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
;; User interface code heavily inspired by weechat.el

;;; Code:
(require 'aio)
(require 'urbit)
(require 'urbit-graph)

(defgroup urbit-chat nil
  "Urbit chat client for Emacs."
  :prefix "urbit-chat-"
  :group 'applications)

(defcustom urbit-chat-url-color "blue"
  "Color for urls"
  :group 'urbit-chat)

(defcustom urbit-chat-your-patp-color "green"
  "Color for your patp"
  :group 'urbit-chat)

(defcustom urbit-chat-other-patp-colors '("DarkGoldenRod")
  "List of possible colors for patps.")

(defcustom urbit-chat-initial-messages 15
  "How many past messages to load when opening a chat.")

(defgroup urbit-chat-faces nil
  "Urbit chat faces"
  :group 'weechat
  :prefix "weechat-")

(defface urbit-chat-prompt-face '((t :inherit minibuffer-prompt))
  "Face used for prompt in urbit chat"
  :group 'urbit-faces)

;; Stolen from weechat.el
(defconst urbit-chat-url-regex
  "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)\\(//[-[:alnum:]_.]+:[0-9]*\\)?[-[:alnum:]_=!?#$@~`%&*+\\/:;.,()]+[-[:alnum:]_=#$@~`%&*+\\/()]"
  "Regex to recognize a url")

(defun urbit-chat-send-message (ship chat message)
  (urbit-graph-add-node
   ship
   chat
   (urbit-graph-make-node
    (urbit-graph-make-post (urbit-chat-tokenize-message message)))))

(defun urbit-chat-url-p (string)
  (save-match-data
    (if-let ((start (string-match urbit-chat-url-regex string)))
        (and (= start 0)
             (= (match-end 0)
                (length string))))))

(defun urbit-chat-patp-to-syls (string)
  (let ((string (replace-regexp-in-string "[\\^~-]" "" string))
        (result '()))
    (while (> (length string) 3)
      (push (substring string 0 3) result)
      (setq string (substring string 3)))
    (when (> (length string) 0)
      (push string result))
    (reverse result)))

;; TODO: real validation
(defun urbit-chat-patp-p (string)
  (and (> (length string) 0)
       (= (elt string 0) ?~)))

(defun urbit-chat-tokenize-message (message)
  (let* ((contents ())
         (text '())
         (push-text
          (lambda ()
            (when text
              (push `((text . ,(string-join text " ")))
                    contents)
              (setq text '())))))
    (dolist (word (split-string message "\s"))
      (pcase word
        ((pred urbit-chat-url-p)
         (funcall push-text)
         (push `((url . ,word))
               contents))
        ((pred urbit-chat-patp-p)
         (funcall push-text)
         (urbit-log "word: %s" word)
         (push `((mention . ,word))
               contents))
        (- (push word text))))
    (funcall push-text)
    (apply #'vector (reverse contents))))

(defun urbit-chat-add-string-properties (string properties)
  (add-text-properties 0 (length string)
                       properties
                       string)
  string)

(defun urbit-chat-color-patp (patp)
  (cond ((string= patp urbit-ship)
         (urbit-chat-add-string-properties patp `(face (:foreground ,urbit-chat-your-patp-color))))
        ((= (length urbit-chat-other-patp-colors) 0) patp)
        (t
         (urbit-chat-add-string-properties patp `(face
                                                  (:foreground
                                                   ,(elt urbit-chat-other-patp-colors
                                                         (% (sxhash-equal patp)
                                                            (length urbit-chat-other-patp-colors)))))))))

(defun urbit-chat-format-url (url)
  (add-text-properties 0 (length url)
                       (list 
                        'mouse-face t
                        'follow-link t
                        'mouse-face t
                        'keymap (let ((map (make-sparse-keymap)))
                                  (let ((a (lambda ()
                                             (interactive)
                                             (urbit-log "boo")
                                             (browse-url url))))
                                    (define-key map [mouse-2] a)
                                    (define-key map (kbd "RET") a))
                                  map)
                        'face `(:foreground
                                ,urbit-chat-url-color
                                :underline
                                t))
                       url)
  url)

(defun urbit-chat-format-contents (contents)
  (urbit-log "content: %s" contents)
  (replace-regexp-in-string
   "^ \\| $" ""
   (string-join
    (mapcar (lambda (content)
              (pcase content
                ((and (app caar 'text)
                      (app cdar text))
                 text)
                ((and (app caar 'url)
                      (app cdar url))
                 (concat " " (urbit-chat-format-url url) " "))
                ((and (app caar 'mention)
                      (app cdar name))
                 (concat " " (urbit-chat-color-patp name) " "))
                (x (urbit-log "Unknown type %s" x) "")))
            contents))))

(defun urbit-chat-format-node (node)
  (let ((post (alist-get 'post node)))
    (let-alist post
      (concat (urbit-chat-color-patp .author) ": "
              (urbit-chat-format-contents .contents)
              "\n"))))

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
                           (list 'read-only t
                                 'field t
                                 'face 'urbit-chat-prompt-face
                                 'rear-nonsticky t)))))

(aio-defun urbit-chat-mode (ship name)
  (kill-all-local-variables)
  (use-local-map urbit-chat-mode-map)
  (setq-local urbit-chat-prompt-start-marker (point-max-marker))
  (setq-local urbit-chat-prompt-end-marker (point-max-marker))
  (setq-local urbit-chat-ship ship)
  (setq-local urbit-chat-chat name)

  (urbit-chat-update-prompt)

  (let ((buffer (current-buffer)))
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
                                (urbit-chat-handle-nodes
                                 nodes
                                 buffer)))))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (urbit-graph-stop-watch resource))
                nil
                'local-hook)))
  (run-mode-hooks 'urbit-chat-mode-hook))


;; TODO: cache keys so that starting chat's doesn't take so long
;; TODO: find a way to show group names with keys, only show chat keys
(defun urbit-chat-start ()
  (interactive)
  (let ((read (completing-read "Choose a chat: "
                               (aio-wait-for (urbit-graph-get-keys)))))
    (let* ((resource (urbit-graph-symbol-to-resource (intern read)))
           (ship (car resource))
           (name (cdr resource))
           (buffer (concat "*urbit-chat-" ship "/" name)))
      (if (get-buffer buffer) (switch-to-buffer buffer)
        (progn
          (switch-to-buffer buffer)
          (urbit-chat-mode ship name))))))

;; TODO: Add a way to load more past messages

(provide 'urbit-chat)
;;; urbit-chat.el ends here
