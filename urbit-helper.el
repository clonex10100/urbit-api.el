;;; urbit-helper.el --- Helper functions for urbit -*- lexical-binding: t -*-

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

(defconst urbit-helper-da-unix-epoch
  170141184475152167957503069145530368000)
(defconst urbit-helper-da-second
  18446744073709551616)

(defun urbit-helper-milli-time ()
  "Time since the unix epoch in millseconds."
  (string-to-number (format-time-string "%s%3N")))

(defun urbit-helper-da-time ()
  "Gets the current time as a string in urbit's @da encoding."
  (number-to-string
   (+ (/ (* (urbit-helper-milli-time) urbit-helper-da-second)
         1000)
      urbit-helper-da-unix-epoch)))

(defun urbit-helper-desig (ship)
  "Remove the sig from a SHIP string."
  (if (and (> (length ship) 0)
           (eq (elt ship 0)
               ?~))
      (substring ship 1)
    ship))

(defun urbit-helper-ensig (ship)
  "Add a sig to SHIP string if it doesn't have one"
  (if (and (> (length ship) 0)
           (eq (elt ship 0)
               ?~))
      ship
    (concat "~" ship)))

(defmacro urbit-helper-let-resource (&rest body)
  "Bind ship to ensigged ship, and create a resource."
  `(let* ((ship (urbit-helper-ensig ship))
          (resource (urbit-graph-make-resource ship name)))
     ,@body))

(defun urbit-helper-chunk (list size)
  (let* ((res
          (seq-reduce (lambda (memo x)
                        (let ((c (car memo))
                              (l (cdr memo)))
                          (push x c)
                          (when (= (length c)
                                   size)
                            (push (nreverse c)
                                  l)
                            (setf c nil))
                          (cons c l)))
                      list
                      '(nil . nil)))
         (leftover (car res))
         (list (cdr res)))
    (when leftover
      (push (nreverse
             leftover)
            list))
    (nreverse list)))

(defun urbit-helper-num-to-ud (index)
  (string-join
   (mapcar #'string-join
           (nreverse
            (urbit-helper-chunk
             (nreverse
              (split-string
               (number-to-string
                index)
               ""
               t))
             3)))
   "."))

(defmacro urbit-helper-let-if-nil (spec &rest body)
  "Bind variables according to SPEC only if they are nil, then evaluate BODY.
Useful for assigning defaults to optional args."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   (let ((sym (car s))
                         (else (cadr s)))
                     `(,sym (or ,sym ,else))))
                 spec)
     ,@body))


(provide 'urbit-helper)

;;; urbit-helper.el ends here
