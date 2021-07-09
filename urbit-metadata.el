;;; urbit-metadata.el --- Library for interacting with urbit metadata -*- lexical-binding: t -*-

;; Author: Noah Evans <noa@nevans.me>

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

(require 'aio)
(require 'urbit-http)
(require 'urbit-graph)
(require 'urbit-helper)

(defvar urbit-metadata-all-subscription nil
  "Subscription id for metadata-store /all.")

(defvar urbit-metadata-associations ())

(aio-defun urbit-metadata-init ()
  (setq urbit-metadata-hooks nil)
  (setq urbit-metadata-update-subscription
        (aio-await
         (urbit-http-subscribe "metadata-store"
                               "/all"
                               #'urbit-metadata-update-handler))))

(defun urbit-metadata-update-handler (event)
  "Handle metadata-update EVENT."
  (let ((metadata-update (alist-get 'metadata-update event)))
    (if (not metadata-update) (urbit-log "Unknown metadata event: %s" event)
      (pcase metadata-update
        ((urbit-helper-match-key 'initial-group)
         (urbit-metadata-handle-associations
          (alist-get 'associations val)))
        ((urbit-helper-match-key 'associations)
         (urbit-metadata-handle-associations val))
        ((urbit-helper-match-key 'add)
         (urbit-log "Unhandled metadata event: add"))
        ((urbit-helper-match-key 'update-metadata)
         (urbit-log "Unhandled metadata event: update-metadata"))
        ((urbit-helper-match-key 'remove)
         (urbit-log "Unhandled metadata event: remove"))))))

(defun urbit-metadata-handle-associations (data)
  (dolist (association-pair data)
    (let* ((association (cdr association-pair))
           (app-name (intern (alist-get 'app-name association)))
           (rid (intern (alist-get 'resource association))))
      (push (cons rid association)
            (alist-get app-name urbit-metadata-associations)))))

(defun urbit-metadata-resource-to-graph-resource-symbol (resource)
  "Convert a metadata RESOURCE string or symbol to a graph resource symbol.
e.g. /ship/~zod/group -> zod/group"
  (let ((resource (if (symbolp resource)
                      (symbol-name resource)
                    resource)))
    (intern
     (substring
      resource
      (1+
       (seq-position resource ?~))))))

;;
;; Actions
;;
(defun urbit-metadata-action (action &optional ok-callback err-callback)
  (urbit-http-poke "metadata-push-hook"
                   "metadata-update"
                   action
                   ok-callback
                   err-callback))

;; This seems to be deprecated. New channels are added with graph-create and the metadata seems to come automatically
(defun urbit-metadata-add (app-name resource group title description date-created color module-name)
  (urbit-metadata-action
   `((add . ((group . ,group)
             (resource (resource . ,resource)
                       (app-name . ,app-name))))
     (metadata . ((title . ,title)
                  (description . ,description)
                  (color . ,color)
                  (date-created . ,date-created)
                  (creator . ,(urbit-helper-ensig urbit-http-ship))
                  (module . ,module-name)
                  (picture . "")
                  (preview . ,nil)
                  (vip . ""))))))

(defun urbit-metadata-remove (app-name resource group)
  "Remove metadata at APP-NAME RESOURCE GROUP. Often used for deleting channels."
  (urbit-metadata-action
   `((remove (group . ,group)
             (resource (resource . ,resource)
                       (app-name . ,app-name))))))

(defun urbit-metadata-update (association new-metadata)
  (let ((metadata (urbit-helper-assign
                   (alist-get 'metadata association)
                   new-metadata)))
    (setf (alist-get 'color metadata)
          (urbit-helper-ux-to-hex (alist-get 'color metadata)))
    (urbit-metadata-action
     `(,(assoc 'group association)
       (resource ,(assoc 'resource association)
                 ,(assoc 'app-name association))
       (metadata . ,metadata)))))

;;
;; urbit-metadata-associations queries
;;
(defun urbit-metadata-get-app-graphs (app &optional joined-only mine-only)
  (let ((resources
         (urbit-helper-filter-map
          (lambda (graph)
            (when (string=
                   (urbit-helper-alist-get-chain
                    'graph
                    'config
                    'metadata
                    graph)
                   "chat")
              (urbit-metadata-resource-to-graph-resource-symbol (alist-get 'resource graph))))
          (alist-get 'graph urbit-metadata-associations))))
    (if joined-only
        (seq-filter
         (lambda (resource)
           (memq resource urbit-graph-keys))
         resources)
      resources)
    (if mine-only
        (seq-filter
         (lambda (resource)
           (let* ((res (urbit-graph-symbol-to-resource resource))
                  (name (cdr res))
                  (ship (car res)))

             (and (not (s-prefix? "dm--" name)) (string= "littel-wolfur" ship))))
         resources)
      resources)))


(provide 'urbit-metadata)

;;; urbit-metadata.el ends here
