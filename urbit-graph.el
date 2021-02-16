;; urbit-graph.el --- Urbit graph library -*- lexical-binding: t -*-

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

;;; Code for interacting with urbit graphs.

;;; Code:

(require 'request)
(require 'aio)
(require 'urbit-http)


(defvar urbit-graph-update-subscription nil
  "Urbit-http graph-store /update subscription")

(defvar urbit-graph-subscriptions '()
  "Alist of graphs to callback functions.")

(aio-defun urbit-graph-init ()
  (setq urbit-graph-subscriptions nil)
  (setq urbit-graph-update-subscription
        (aio-await
         (urbit-http-subscribe "graph-store"
                               "/updates"
                               #'urbit-graph-update-handler))))

(defun urbit-graph-parse-update (event)
  "Parse a graph-update EVENT into a pair of resource symbol and node list."
  (let ((object (alist-get
                 'add-nodes
                 (alist-get
                  'graph-update
                  event))))
    (cons (urbit-graph-resource-to-symbol
           (alist-get 'resource object))
          (alist-get 'nodes object))))

(defun urbit-graph-update-handler (event)
  (let* ((parsed-event (urbit-graph-parse-update event))
         (key (car parsed-event))
         (nodes (cdr parsed-event)))
    (let ((callback (alist-get key urbit-graph-subscriptions)))
      (when callback
        (funcall callback nodes)))))

(defun urbit-graph-resource-to-symbol (resource)
  "Turn a RESOURCE object into a symbol."
  (intern (concat (alist-get 'ship resource)
                  (alist-get 'name resource))))

(defun urbit-graph-make-post (contents)
  "Create a new post with CONTENTS.
CONTENTS is a vector or list of content objects."
  (let ((contents (if (vectorp contents)
                      contents
                    (vconcat contents))))
    `((index . ,(concat "/" (urbit-da-time)))
      (author . ,(concat "~" urbit-ship))
      (time-sent . ,(urbit-milli-time))
      (signatures . [])
      (contents . ,contents)
      (hash . nil))))

(defun urbit-graph-make-node (post &optional children)
  "Make an urbit graph node."
  `(,(alist-get 'index post)
    (post . ,post)
    (children . ,children)))

(defun urbit-graph-parse-add (object)
  "Parse a graph-add object into a graph."
  (urbit-log "update: %s" graph-update)
  (alist-get
   'graph
   (alist-get
    'add-graph
    (alist-get
     'graph-update
     (car object)))))

(aio-defun urbit-graph-get (ship name)
  "Get a graph at SHIP NAME."
  (urbit-graph-parse-add
   (aio-await (urbit-http-scry "graph-store"
                               (format "/graph/~%s/%s"
                                       ship
                                       name)))))

(aio-defun urbit-graph-add-node (ship name node)
  "To graph at SHIP NAME, add NODE."
  (aio-await (urbit-http-poke "graph-push-hook"
                              "graph-update"
                              `((add-nodes
                                 (resource (ship . ,ship)
                                           (name . ,name))
                                 (nodes ,node))))))

(aio-defun urbit-graph-subscribe (ship name callback)
  "Subscribe to a graph at SHIP and NAME, calling CALLBACK with a list of new nodes on each update."
  (add-to-list 'urbit-graph-subscriptions
               (cons
                (urbit-graph-resource-to-symbol `((ship . ,(urbit-desig ship))
                                                  (name . ,name)))
                callback)))

(provide 'urbit-graph)

;;; urbit-graph.el ends here
