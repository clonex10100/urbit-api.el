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
(require 'urbit-log)
(require 'urbit-http)
(require 'urbit-metadata)

(defgroup urbit-chat nil
  "Urbit chat client for Emacs."
  :prefix "urbit-chat-"
  :group 'applications)

(defcustom urbit-prompt "%n> "
  "Prompt string to use in urbit chat buffers."
  :type 'string
  :initialize 'custom-initialize-default)

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

(defface urbit-chat-prompt-face '()
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
         (not-backticks t)
         (push-text
          (lambda ()
            (when text
              (push `((text . ,(string-join (reverse text) " ")))
                    contents)
              (setq text '())))))
    (dolist (word (split-string message "\s"))
      (pcase word
        ((pred (string-prefix-p "`"))
         (setq not-backticks nil)
         (push word text))
        ((pred (string-suffix-p "`") )
         (setq not-backticks t)
         (push word text))
        (
         (pred (lambda (s) (and (urbit-chat-url-p s) not-backticks)))
         (funcall push-text)
         (push `((url . ,word))
               contents))
        ((pred (lambda (s) (and (urbit-chat-patp-p s) not-backticks)))
         (funcall push-text)
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
  (cond ((string= patp urbit-http-ship)
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
                                             (browse-url url))))
                                    (define-key map [mouse-2] a)
                                    (define-key map (kbd "<return>") a))
                                  map)
                        'face `(:foreground
                                ,urbit-chat-url-color
                                :underline
                                t))
                       url)
  url)

(defun urbit-chat-format-contents (contents)
  (replace-regexp-in-string
   "^ \\| $" ""
   (string-join
    (mapcar (lambda (content)
              (pcase content
                ((and (app caar 'text)
                      (app cdar text))
                 text)
                ((and (app caar 'reference)
                      (app cdar reference))
                 (let-alist reference
                   (let* ((resource
                           (urbit-graph-symbol-to-resource
                            (urbit-metadata-resource-to-graph-resource-symbol .graph.graph)))
                          (node (aio-wait-for
                                 (urbit-graph-get-node (car resource) (cdr resource) .graph.index)))
                          (kids (alist-get 'children (car node)))
                          (youngest (assoc 1 (alist-get 'children (car kids))))
                          (final-node (if youngest (assoc-default
                                                    (length youngest) youngest)
                                        (car node))
                                      )
                          )
(condition-case ex
    (urbit-chat-format-reference final-node)
    (error "!!REF-ERROR!!"))
)))
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

      (concat

       (urbit-chat-color-patp .author) ": "
       (urbit-chat-format-contents .contents)
       "\n"))))

(defun urbit-chat-format-reference (node)
  (let ((post (alist-get 'post node)))
    (let-alist post
      (concat
       (urbit-chat-add-string-properties
        "\n|-----> "
        `(face
          (:foreground "#83898d")))
       "("
       (urbit-chat-color-patp .author)
       ")\n"

       (urbit-chat-add-string-properties
        "|>>> "
        `(face
          (:foreground "#83898d")))
       (urbit-chat-format-contents .contents)
       (urbit-chat-add-string-properties
        "\n|<-----\n"
        `(face
          (:foreground "#83898d")))))))

(defun urbit-chat-handle-nodes (nodes buffer)
  (with-current-buffer buffer
    (dolist (node nodes)

      (condition-case err
          (if (urbit-helper-alist-get-chain 'contents 'post node)
              (urbit-chat-insert-message
               (urbit-chat-format-node node)))
        (error (progn
                 ;; (message err)
                 (urbit-log "deleted: %s" (prin1-to-string node))
                 (urbit-chat-insert-message
                  (concat (urbit-chat-color-patp "sampel-palnet") ": "
                          (urbit-chat-add-string-properties
                           "deleted"
                           `(face
                             (:foreground "red")))
                          "\n"))))))))


(defun urbit-chat-insert-message (message)
  (save-excursion
    (goto-char urbit-chat-prompt-start-marker)
    (let ((start (point))
          (inhibit-read-only t))
      (insert-before-markers message)
      (add-text-properties start (point)
                           '(read-only t)))))

(defun urbit-chat-self-insert-command (n &optional c)
  (interactive "p")
  (when (< (point) urbit-chat-prompt-end-marker)
    (goto-char (point-max)))
  (if c (self-insert-command n c)
    (self-insert-command n)))

(defun urbit-chat-return ()
  (interactive)
  (let ((msg (buffer-substring urbit-chat-prompt-end-marker
                               (point-max))))
    (when (> (length msg) 0)
      (delete-region urbit-chat-prompt-end-marker
                     (point-max))
      (urbit-chat-send-message urbit-chat-ship urbit-chat-chat msg)
      (point-max))))

(defvar urbit-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") (lambda ()
                                         (interactive)
                                         (urbit-chat-self-insert-command 1 ?\n)))
    (define-key map (kbd "<return>") #'urbit-chat-return)
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
          (prompt (or urbit-prompt ""))
          (inhibit-read-only t))
      (mapc (lambda (rep)
              (setq prompt
                    (replace-regexp-in-string (car rep) (cdr rep) prompt)))
            (list (cons "%n" urbit-http-ship)
                  (cons "%s" urbit-chat-chat)))
      (goto-char urbit-chat-prompt-end-marker)
      (insert-before-markers prompt)
      (set-marker urbit-chat-prompt-start-marker start)
      (add-text-properties urbit-chat-prompt-start-marker
                           urbit-chat-prompt-end-marker
                           '(read-only t
                             face urbit-chat-prompt-face
                             rear-nonsticky t)))))

(defun urbit-chat-start (&optional resource)
  (interactive)
  (let ((read
         (if resource resource
           (completing-read "Choose a chat: " (urbit-metadata-get-app-graphs "chat" t)))))
    (let* ((resource (urbit-graph-symbol-to-resource (intern read)))
           (ship (car resource))
           (name (cdr resource))
           (buffer (concat "*urbit-chat-" ship "/" name)))
      (if (get-buffer buffer) (switch-to-buffer buffer)
        (progn
          (switch-to-buffer buffer nil t)
          (urbit-chat-mode ship name))
        ))))


(aio-defun urbit-chat-mode (ship name)
  (kill-all-local-variables)
  (use-local-map urbit-chat-mode-map)
  (setq-local urbit-chat-prompt-start-marker (point-max-marker))
  (setq-local urbit-chat-prompt-end-marker (point-max-marker))
  (setq-local urbit-chat-ship ship)
  (setq-local urbit-chat-chat name)

  (let ((buffer (current-buffer))
        (nodes (aio-await
                (urbit-graph-get-newest ship
                                        name
                                        urbit-chat-initial-messages))))
    (urbit-chat-update-prompt)

    (urbit-chat-handle-nodes
     (sort nodes
           (lambda (a b)
             (< (car a)
                (car b))))
     buffer)

    (goto-char (point-max))
    (recenter -1)
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

  (defun ct/send-notification (title msg)
    (let ((notifier-path (executable-find "terminal-notifier")))
      (start-process
       "Message Alert"
       "*Message Alert*" ; use `nil` to not capture output; this captures output in background
       notifier-path
       "-message" msg
       "-title" title
       "-sender" "dev.hmiller.port"
       "-activate" "org.gnu.Emacs")))
  (defun ct/message-display-native (resource-string body)
    (ct/send-notification
     (format "Message in %s" resource-string) ; Title
     (format "%s" body)))

(defun urbit-message-update-handler (event)
  "Handle graph-update EVENT."
  (let ((graph-update (alist-get 'graph-update event)))
    (if (not graph-update)
        (urbit-log
         "Unknown graph event: %s"
         event)
      (let* ((update-type (caar graph-update))
             (update (cdar graph-update)))
        (if (or (eq update-type 'add-nodes)
                (eq update-type 'remove-nodes))
            (let-alist
                update
              (let* ((resource-symbol (urbit-graph-resource-to-symbol
                                       .resource))
                     (resource-name (symbol-name resource-symbol))
                     (ship .resource.ship)
                     (name .resource.name)
                     (buffer (concat "*urbit-chat-" ship "/" name))
                     (resource-string (urbit-graph-pretty-resource
                                       .resource)))
                (pcase
                    update-type
                  ('add-nodes
                   (let ((nodes (alist-get 'nodes update)))
                     (dolist (node nodes)
                       (let ((body (urbit-chat-format-node node)))
                         (message resource-name)
                         (if (member
                              resource-name
                              urbit-tracked-graphs)
                             (ct/message-display-native resource-name body))
                         (when (get-buffer buffer)
                             (condition-case err
                                 (if err
                                     (progn
                                       (urbit-messages "error for %s | %s" resource-string body)
                                       (print err))
                                   (with-current-buffer buffer
                                     (save-excursion
                                       (let ((inhibit-read-only t))
                                         (delete-region (point-min) (point-max)))
                                       (urbit-graph-stop-watch resource-symbol)
                                       (urbit-chat-mode ship name))
                                     (if
                                         (> 500 (car (page--count-lines-page)))
                                         (message (concat "re-watching " (prin1-to-string buffer))))))
                               (error resource-symbol)))
                         (setq unread-groups (cl-adjoin resource-name unread-groups))
                         (unless (member resource-name urbit-ignored-graphs)
                           (urbit-messages
                            "%s: %s"
                            resource-string
                            body))))))))))))))
;; TODO: cache keys so that starting chat's doesn't take so long
;; TODO: find a way to show group names with keys, only show chat keys

(defun urbit-chat-resource (resource)
  (interactive)
  (unless (not resource)
    (if (member (intern resource) (urbit-metadata-get-app-graphs "chat" t))
        (urbit-chat-start resource))))
;; TODO: Add a way to load more past messages

(provide 'urbit-chat)
;;; urbit-chat.el ends here
