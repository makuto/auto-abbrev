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
  (abbrev-mode 1)
  (font-lock-add-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

(defun auto-abbrev-off ()
  (interactive)
  ;; TODO: This should only remove the auto-abbrevs, not necessarily turn abbrev-mode off
  (abbrev-mode 0)
  (font-lock-remove-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

;; TODO: What buffer will this run in?
(defun auto-abbrev-generation-finished ()
	(font-lock-remove-keywords nil auto-abbrev-highlights)
	(load-file "auto-abbrevs-for-buffer.el")
	(font-lock-add-keywords nil auto-abbrev-highlights)
	(abbrev-mode 1)
	(font-lock-fontify-buffer))

(defun auto-abbrev-refresh-current-buffer ()
  (interactive)
  ;; We need to remove keywords first, otherwise changed ones will stick around
  (let ((process (start-process "Auto-Abbrev" "*Auto-Abbrev*" "python3" "AbbrevsFromFile.py" buffer-file-name)))
	(set-process-sentinel process
						  (lambda (process _string)
							(auto-abbrev-generation-finished)))))
(provide 'auto-abbrev)
