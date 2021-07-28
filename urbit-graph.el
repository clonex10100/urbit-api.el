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
(require 'urbit-log)
(require 'urbit-helper)



;;
;; Variables
;;
(defvar urbit-graph-update-subscription nil
  "Urbit-http graph-store /update subscription")

(defvar urbit-graph-hooks '()
  "Alist of resource symbols to hook objects for watching graphs.")

(defvar urbit-graph-keys ()
  "List of graph resource symbols.")

;;
;; Functinos
;;
(aio-defun urbit-graph-init ()
  (setq urbit-graph-hooks nil)
  (let ((subscription-promise
         (urbit-http-subscribe "graph-store"
                               "/updates"
                               #'urbit-graph-update-handler))
        (keys-promise (urbit-graph-get-keys)))
    (setq urbit-graph-update-subscription
          (aio-await subscription-promise))
    (setq urbit-graph-keys (aio-await keys-promise))))

(defun urbit-graph-index-symbol-to-list (symbol)
  (mapcar #'string-to-number
          (split-string (symbol-name symbol) "/" t)))

(defun urbit-graph-resource-to-symbol (resource)
  "Turn a RESOURCE object into a symbol."
  (intern (concat (alist-get 'ship resource)
                  "/"
                  (alist-get 'name resource))))

(defun urbit-graph-symbol-to-resource (symbol)
  (let ((split (split-string (symbol-name symbol) "/")))
    (cons (car split)
          (cadr split))))

(defun urbit-graph-fix-indexes (nodes)
  "Fix indexes of add-nodes node list by stripping the slashes and converting to numbers."
  (dolist (node nodes)
    (setf (car node)
          (car (last (urbit-graph-index-symbol-to-list (car node)))))
    (let ((children (alist-get 'children (cdr node))))
      (setf children (urbit-graph-fix-indexes children)))))

(defun urbit-graph-index-to-ud (index)
  "Convert all nums in INDEX to UDs."
  (string-join
   (mapcar (lambda (x)
             (if (= (length x) 0) x
               (urbit-helper-dec-to-ud
                x)))
           (split-string index
                         "/"))
   "/"))
;;
;; Event handling
;;
(defun urbit-graph-update-handler (event)
  "Handle graph-update EVENT."
  (let ((graph-update (alist-get 'graph-update event)))
    (if (not graph-update) (urbit-log "Unknown graph event: %s" event)
      (let* ((update-type (caar graph-update))
             (update (cdar graph-update)))
          (pcase update-type
            ((or 'add-nodes 'remove-nodes)
             (urbit-graph-update-nodes-handler update-type update))
            ('add-graph
             (push (urbit-graph-resource-to-symbol (alist-get 'resource update))
                   urbit-graph-keys))
            ('remove-graph
             (setq urbit-graph-keys
                   (remq (urbit-graph-resource-to-symbol
                          update)
                         urbit-graph-keys)))
            (- (urbit-log "Ignoring graph-update %s" update-type)))))))

(defun urbit-graph-update-nodes-handler (update-type update)
  (let-alist update
    (let* ((resource-symbol (urbit-graph-resource-to-symbol .resource))
           (hook (urbit-helper-alist-get-chain update-type
                                               resource-symbol
                                               urbit-graph-hooks)))
      (funcall
       hook
       (pcase update-type
         ('add-nodes
          (let ((nodes (alist-get 'nodes update)))
            (urbit-graph-fix-indexes nodes)
            nodes))
         ('remove-nodes
          (append (alist-get 'indices update) nil)))))))

(defun urbit-graph-watch (ship name add-callback &optional remove-callback)
  "Watch graph at SHIP NAME. When an add-nodes event is recieved, ADD-CALLBACK will be called with a list of nodes.
When a remove-nodes event is recieved, REMOVE-CALLBACK will be called with a list of indexes.
Return a resource symbol you can use with `urbit-graph-stop-watch'."
  (let ((resource (urbit-graph-resource-to-symbol `((ship . ,ship)
                                                    (name . ,name)))))
    (urbit-helper-let-if-nil ((remove-callback (lambda (indexes)
                                                 (urbit-log "Got remove-nodes for indexes: %s" indexes))))
      (add-to-list 'urbit-graph-hooks
                   `(,resource (add-nodes . ,add-callback)
                               (remove-nodes . ,remove-callback))))

    resource))

(defun urbit-graph-stop-watch (resource)
  "Stop watching RESOURCE symbol."
  (setq urbit-graph-hooks
        (assq-delete-all resource urbit-graph-hooks)))

;;
;; Constructors
;;
(defun urbit-graph-make-post (contents &optional parent-index index)
  "Create a new post with CONTENTS.
CONTENTS is a vector or list of content objects.
PARENT-INDEX is the index of its parent node
INDEX is the index of this node. If not passed, it will be auto-generated."
  (let ((contents (if (vectorp contents)
                      contents
                    (vconcat contents))))
    `((index . ,(concat (or parent-index "")
                        (or index
                            (concat "/" (urbit-helper-da-time)))))
      (author . ,(urbit-helper-ensig urbit-http-ship))
      (time-sent . ,(urbit-helper-milli-time))
      (signatures . [])
      (contents . ,contents)
      (hash . nil))))

(defun urbit-graph-index-children (nodes)
  "Convert all children nodes of a node into pairs of their index and node."
  (mapcar
   (lambda (node)
     (urbit-log "node: %s" node)
     (setf (alist-get 'children node)
           (urbit-graph-index-children (alist-get 'children node)))
     (cons (car (last (split-string (urbit-helper-alist-get-chain 'index
                                                                  'post
                                                                  node)
                                    "/")))
           node))
   nodes))

(defun urbit-graph-make-node (post &optional children)
  "Make an urbit graph node with POST and CHILDREN."
  `((post . ,post)
    (children . ,(urbit-graph-index-children children))))

(defun urbit-graph-make-resource (ship name)
  "Make a resource object from SHIP NAME."
  `(resource (ship . ,ship)
             (name . ,name)))

;;
;; Actions
;;
(defun urbit-graph-store-action (action &optional ok-callback err-callback)
  (urbit-http-poke "graph-store"
                   "graph-update-2"
                   action
                   ok-callback
                   err-callback))

(defun urbit-graph-view-action (thread-name action)
  (urbit-http-spider "graph-view-action"
                     "json"
                     thread-name
                     action))

(defun urbit-graph-hook-action (action &optional ok-callback err-callback)
  (urbit-http-poke "graph-push-hook"
                   "graph-update-2"
                   action
                   ok-callback
                   err-callback))

;;
;; View Actions
;;
(defun urbit-graph-join (ship name)
  "Join graph at SHIP NAME."
  (urbit-helper-let-resource
   (urbit-graph-view-action "graph-join"
                            `((join ,resource
                                    (ship . ,ship))))))

(defun urbit-graph-delete (name)
  "Delete graph at NAME from your ship."
  (let ((resource
         (urbit-graph-make-resource (urbit-helper-ensig urbit-http-ship)
                                    name)))
    (urbit-graph-view-action "graph-delete"
                             `((delete ,resource)))))

(defun urbit-graph-leave (ship name)
  "Leave graph at SHIP NAME."
  (urbit-helper-let-resource
   (urbit-graph-view-action "graph-leave"
                            `((leave ,resource)))))


;; TODO: what is to
(defun urbit-graph-groupify (ship name to-path)
  (urbit-helper-let-resource
   (urbit-graph-view-action "graph-groupify"
                            `((groupify ,resource (to . to))))))

;;
;; Store Actions
;;
(defun urbit-graph-add (ship name graph mark)
  "At SHIP NAME, add GRAPH with MARK."
  (urbit-helper-let-resource
   (urbit-graph-store-action
    `((add-graph ,resource (graph . ,graph) (mark . ,mark))))))

;;
;; Hook Actions
;;
(defun urbit-graph-add-nodes (ship name nodes)
  "To graph at SHIP NAME, add NODES."
  (urbit-log "adding nodes: %s" nodes)
  (urbit-helper-let-resource
   (urbit-graph-hook-action
    `((add-nodes ,resource (nodes . ,nodes))))))

(defun urbit-graph-add-node (ship name node)
  "To graph at SHIP NAME, add NODE."
  (urbit-graph-add-nodes ship name
                         (let ((index (intern
                                       (alist-get 'index
                                                  (alist-get 'post node)))))
                           `((,index . ,node)))))

(defun urbit-graph-remove-nodes (ship name indices)
  "INDICES is a list of indexes to be removed from graph at SHIP NAME."
  (urbit-helper-let-resource
   (urbit-graph-hook-action `((remove-nodes ,resource (indices . indices))))))

;;
;; Fetching
;;
(aio-defun urbit-graph-get-wrapper (path)
  "Scries graph-store at PATH, and feeds the result to `urbit-graph-update-handler'.
Returns a list of nodes"
  (let ((result (car
                 (aio-await
                  (urbit-http-scry "graph-store" path)))))
    (let* ((graph-update (cdar result))
           (update-type (caar graph-update))
           (update (cdar graph-update)))
      (let ((nodes 
             (pcase update-type
               ('add-nodes
                (alist-get 'nodes update))
               ('add-graph
                (alist-get 'graph update))
               (- (urbit-log "Get wrapper unkown update type %s" update-type)
                  '()))))
        (urbit-graph-fix-indexes nodes)
        nodes))))

(aio-defun urbit-graph-get-keys ()
  "Get a list of all graph store keys as resource symbols."
  (let ((keys
         (aio-await
          (urbit-http-scry "graph-store" "/keys"))))
    ;; Return a list of resource symbols
    (mapcar #'urbit-graph-resource-to-symbol
            (cdar (cdaar keys)))))

(defun urbit-graph-get (ship name)
  "Get a graph at SHIP NAME."
  (urbit-graph-get-wrapper
   (format "/graph/%s/%s"
           (urbit-helper-ensig ship)
           name)))

(defun urbit-graph-get-newest (ship name count &optional index)
  (urbit-helper-let-if-nil ((index ""))
    (urbit-graph-get-wrapper
     (format "/graph/%s/%s/node/siblings/newest/lone/%s%s"
             (urbit-helper-ensig ship)
             name
             count
             index))))

(defun urbit-graph-get-older-siblings (ship name count &optional index)
  (urbit-helper-let-if-nil ((index ""))
    (urbit-graph-get-wrapper
     (format "/graph/%s/%s/node/siblings/older/lone/%s%s"
             (urbit-helper-ensig ship)
             name
             count
             (urbit-graph-index-to-ud index)))))

(defun urbit-graph-get-younger-siblings (ship name count &optional index)
  (urbit-helper-let-if-nil ((index ""))
    (urbit-graph-get-wrapper
     (format "/graph/%s/%s/node/siblings/younger/lone/%s%s"
             (urbit-helper-ensig ship)
             name
             count
             (urbit-graph-index-to-ud index)))))

(defun urbit-graph-get-subset (ship name start end)
  (urbit-graph-get-wrapper
   (format "/graph-subset/%s/%s/%s/%s"
           ship
           name
           end
           start)))

(defun urbit-graph-get-node (ship name index)
  (urbit-graph-get-wrapper

   (format "/graph/%s/%s/node/index/kith%s"
           (urbit-helper-ensig ship)
           name
           (urbit-graph-index-to-ud index))))


(provide 'urbit-graph)

;;; urbit-graph.el ends here
