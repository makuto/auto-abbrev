(require 'abbrev)

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

(defun auto-abbrev-turn-on ()
  "Add auto-abbrev font locks and abbrevs to the current buffer"
  (abbrev-mode 1)
  ;; Make sure we clear out the old keywords
  (clear-abbrev-table auto-abbrev-mode-abbrev-table)
  (font-lock-remove-keywords nil auto-abbrev-highlights)
  (load-file "/home/macoy/Development/code/repositories/auto-abbrev/.auto-abbrevs-for-buffer.el")
  (font-lock-add-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

(defun auto-abbrev-turn-off ()
  ;; Note that we Don't need to clear abbrev table because it will be disabled when the mode is off
  (font-lock-remove-keywords nil auto-abbrev-highlights)
  (font-lock-fontify-buffer))

(define-minor-mode auto-abbrev-mode
  "Minor mode for hinting auto-abbrev abbreviations."
  ;; :init-value t
  :lighter " Auto-Abbrev"
  (if auto-abbrev-mode
      (auto-abbrev-turn-on)
    (auto-abbrev-off)))

;; Filled out by script later
(setq auto-abbrev-highlights nil)
(define-abbrev-table 'auto-abbrev-mode-abbrev-table nil)

(add-to-list 'abbrev-minor-mode-table-alist `(auto-abbrev-mode ,auto-abbrev-mode-abbrev-table))

;; TODO: What buffer will this run in?
(defun auto-abbrev-generation-finished ()
  ;; Only refresh if auto-abbrev-mode is active
  (when auto-abbrev-mode
    (auto-abbrev-turn-on))
  (message "Auto-abbrevs updated"))

(defun auto-abbrev-refresh-current-buffer ()
  (interactive)
  ;; We need to remove keywords first, otherwise changed ones will stick around
  (let ((process (start-process "Auto-Abbrev" "*Auto-Abbrev*" "python3"
                                "/home/macoy/Development/code/repositories/auto-abbrev/AbbrevsFromFile.py" "--verbose"
                                buffer-file-name "-o"
                                "/home/macoy/Development/code/repositories/auto-abbrev/.auto-abbrevs-for-buffer.el")))
    (set-process-sentinel process
                          (lambda (process _string)
                            (auto-abbrev-generation-finished)))))
(provide 'auto-abbrev)
