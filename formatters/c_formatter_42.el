(defvar c-formatter-42-executable "$HOME/.local/bin/c_formatter_42"
  "The executable for the C formatter.")

(defvar c-formatter-42-format-on-save t
  "Whether to format C/C++ code automatically on save.")

(defvar c-formatter-42-in-progress nil
  "Flag to track whether the formatter is currently running.")

(defvar c-formatter-42-file-extensions "\\(\\.c\\|\\.cpp\\|\\.h\\|\\.cxx\\|\\.hxx\\|\\.hpp\\)\\'"
  "list of file extensions supported by c_formatter_42.")

(defun format-c-buffer-42 ()
  "Format the current C buffer with c_formatter_42."
  (interactive)
  (when (and buffer-file-name
             (string-match-p c-formatter-42-file-extensions buffer-file-name)
             (not c-formatter-42-in-progress))  ;; Check if formatter is in progress
    (save-buffer)  ; Ensure the file is saved before formatting
    (setq c-formatter-42-in-progress t)  ;; Set the flag to in-progress
    (let ((command (format "%s %s"
                           c-formatter-42-executable
                           (shell-quote-argument buffer-file-name))))
      (async-shell-command command "*c_formatter_42*"))
    ;; Revert buffer to see the formatted version
    (add-hook 'async-shell-command-finished-functions
              #'c-formatter-42-command-finished)))

(defun c-formatter-42-command-finished (process)
  "Callback function for when the async shell command finishes."
  (setq c-formatter-42-in-progress nil)  ;; Reset the flag when the command finishes
  (when (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
    (revert-buffer t t t)))  ;; Revert buffer after successful formatting

(defun enable-c-formatter-42 ()
  "Enable C formatter 42 and setup auto-format on save."
  (local-set-key (kbd "C-c C-f") 'format-c-buffer-42)
  (when c-formatter-42-format-on-save
    (add-hook 'before-save-hook #'format-c-buffer-42 nil t)))

(add-hook 'c-mode-hook 'enable-c-formatter-42)
(add-hook 'c++-mode-hook 'enable-c-formatter-42)

(provide 'c_formatter_42)
