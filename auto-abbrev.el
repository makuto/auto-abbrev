(require 'faces)

(defgroup auto-abbrev nil
  "Auto-Abbrev customization group."
  :group 'applications)

(defgroup auto-abbrev-faces nil
  "Faces for displaying Auto-Abbrev information."
  :group 'auto-abbrev)

(defface auto-abbrev-highlight-face
  '((t (:underline t)))
  "Face for abbreviation hints"
  :group 'auto-abbrev-faces)

;; Filled out by script later
(setq auto-abbrev-highlights nil)

(define-minor-mode auto-abbrev-mode
  "Minor mode for hinting auto-abbrev abbreviations."
  :lighter " Auto-Abbrev")
;; (font-lock-add-keywords nil auto-abbrev-highlights))

(defun auto-abbrev-on ()
  (interactive)
  (font-lock-add-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

(defun auto-abbrev-off ()
  (interactive)
  (font-lock-remove-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

(provide 'auto-abbrev)
