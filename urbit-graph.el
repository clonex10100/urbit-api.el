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

;; TODO: Consider combining these into one alist
(defvar urbit-graph-graphs '()
  "Alist of resource symbolds to graphs.")

(defvar urbit-graph-hooks '()
  "Alist of resource symbols to functions that run on graph updates.")


(aio-defun urbit-graph-init ()
  (setq urbit-graph-subscriptions nil)
  (setq urbit-graph-update-subscription
        (aio-await
         (urbit-http-subscribe "graph-store"
                               "/updates"
                               #'urbit-graph-update-handler))))

(pcase-defmacro urbit-graph-match-key (key)
  "Matches if EXPVAL is an alist with KEY, and let binds val to the value of that key."
  `(and (pred (assoc ,key))
        (app (alist-get ,key) val)))

;;
;; Event handling
;;

(defun urbit-graph-index-symbol-to-list (symbol)
  (mapcar #'string-to-number
          (split-string (symbol-name symbol) "/" t)))

(defun urbit-graph-add-nodes-handler (data)
  "Handle add-nodes graph-update action."
  (urbit-log "Add nodes")
  (let-alist data
    (let* ((resource-symbol (urbit-graph-resource-to-symbol .resource))
           ;; Assoc to perserve mutability in case of empty graph
           (graph (assoc resource-symbol urbit-graph-graphs))
           (callback (alist-get resource-symbol urbit-graph-subscriptions)))
      (defun add-node (graph index post)
        (urbit-log "Adding node: %s" post)
        (urbit-log "Graph: %s" graph)
        (urbit-log "Index: %s" index)
        (if (= (length index) 1) (nconc graph (list (cons (car index) post)))
          (let ((parent (alist-get (car index) graph)))
            (if (not parent) (urbit-log "Parent not found for: %s" index)
              (add-node (assoc 'children parent)
                        (cdr index)
                        post)))))
      (dolist (node .nodes)
        (let ((index (urbit-graph-index-symbol-to-list (car node)))
              (post (cdr node)))
          (add-node graph index post))))))

(defun urbit-graph-add-graph-handler (data)
  (let-alist data
    (let ((resource-symbol (urbit-graph-resource-to-symbol .resource)))
      (when (assoc resource-symbol urbit-graph-graphs)
        (urbit-log "Add graph: Graph %s already exists." resource-symbol))
      ;; TODO: Do we need to store the mark and overwrite metadata?
      ;; TODO: Clean up the indexes (remove the slashes)
      (add-to-list 'urbit-graph-graphs (cons resource-symbol .graph)))))

(defun urbit-graph-update-handler (event)
  "Handle graph-update EVENT."
  (let ((graph-update (alist-get 'graph-update event)))
    (if (not graph-update) (urbit-log "Unknown graph event: %s" event)
      (pcase graph-update
        ((urbit-graph-match-key 'add-nodes) (urbit-graph-add-nodes-handler val))
        ((urbit-graph-match-key 'add-graph) (urbit-graph-add-graph-handler val))
        ((urbit-graph-match-key 'remove-graph) (urbit-log "Remove graph not implemented"))
        (- (urbit-log "Unkown graph-update: %s" graph-update))))))

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

;;
;; Actions
;;

(aio-defun urbit-graph-store-action (action &optional ok-callback err-callback)
  (urbit-http-poke "graph-store"
                   "graph-update"
                   action
                   ok-callback
                   err-callback))

(aio-defun urbit-graph-view-action (thread-name action)
  (urbit-http-poke "graph-view-action"
                   "json"
                   thread-name
                   action))

(aio-defun urbit-graph-hook-action (action &optional ok-callback err-callback)
  (urbit-http-poke "graph-push-hook"
                   "graph-update"
                   action
                   ok-callback
                   err-callback))

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

(aio-defun urbit-graph-get (ship name)
  "Get a graph at SHIP NAME."
  (urbit-graph-update-handler
   (car ;; Why do we need this?
    (aio-await (urbit-http-scry "graph-store"
                                (format "/graph/~%s/%s"
                                        ship
                                        name))))))

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
                (urbit-graph-resource-to-symbol `((ship . ,ship)
                                                  (name . ,name)))
                callback)))


(provide 'urbit-graph)

;;; urbit-graph.el ends here
