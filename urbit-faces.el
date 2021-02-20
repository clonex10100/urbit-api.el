

(defface urbit-my-nick ;; font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Blue")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :inverse-video t :weight bold))
  "Urbit face for my messages.")

(defface urbit-other-nick ;; font-lock-variable-name-face
  '((((class grayscale) (background light))
     :foreground "Gray90" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "DimGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 88) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 8)) :foreground "yellow" :weight light)
    (t :weight bold :slant italic))
  "Urbit face for other users' messages.")

(defun urbit-add-face (start end name &optional object)
  "Add face NAME to the face text property of the text from START to END."
  (when name
    (let ((pos start)
  next prop)
      (while (< pos end)
        (setq prop (get-text-property pos 'font-lock-face object)
              next (next-single-property-change pos 'font-lock-face object end))
        (unless (member name (get-text-property pos 'font-lock-face object))
          (add-text-properties pos next
                               (list 'font-lock-face (cons name prop)) object))

        (setq pos next)))))

(defun urbit-facify (string face)
  "Return a copy of STRING with FACE property added."
  (let ((string (or string "")))
    (urbit-add-face 0 (length string) face string)
    string))

(defface urbit-prompt
  '((((min-colors 88) (background dark)) :foreground "cyan1")
    (((background dark)) :foreground "cyan")
    (t :foreground "dark blue"))
  "Urbit face for prompts.")

(provide 'urbit-faces)
;;; urbit-faces.el ends here
