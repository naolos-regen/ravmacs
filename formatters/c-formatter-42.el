;;; c-formatter-42.el --- C Formatter 42 integration for Emacs

;;; Commentary:
;; This package integrates the c_formatter_42 tool into Emacs for formatting C code.
;; It also allows running Norminette on C files for style checking.

;;; Code:

(defgroup c-formatter-42 nil
  "Customization group for c-formatter-42 integration."
  :prefix "c-formatter-42-"
  :group 'tools)

(defcustom c-formatter-42-exec "c_formatter_42"
  "The executable for the C formatter."
  :type 'string
  :group 'c-formatter-42)

(defcustom c-formatter-42-format-on-save 0
  "If non-zero, formats C files on save."
  :type 'integer
  :group 'c-formatter-42)

(defcustom norminette-command "norminette"
  "The command used to run Norminette for style checking."
  :type 'string
  :group 'c-formatter-42)

(defcustom c-formatter-42-keybinding "<f2>"
  "The Keybinding for triggering C code formatting with c_formatter_42."
  :type 'string
  :group 'c-formatter-42)

(defcustom norminette-keybinding "<f4>"
  "Keybinding to run Norminette on current C file."
  :type 'string
  :group 'c-formatter-42)

;;; Format the current buffer with c_formatter_42
(defun c-formatter-42 ()
  "Format the current buffer using the c_formatter_42 tool."
  (interactive)
  (let ((pos (point))
        (inhibit-read-only t)
        (deactivate-mark nil)
        (mark-active nil))
    (save-excursion
      (shell-command-on-region (point-min) (point-max) c-formatter-42-exec nil t))
    (goto-char pos)))

;;; Norminette on the current buffer
(defun norminette ()
  "Run Norminette on the current file and display the result in a new buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (let ((buf (get-buffer-create "*Norminette Result*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Norminette result for %s\n" current-file))
            (insert (make-string 80 ?-) "\n")
            (insert (shell-command-to-string (format "%s %s"
                                                     norminette-command
                                                     (shell-quote-argument current-file)))))
          (goto-char (point-min))
          (setq-local buffer-read-only t)
          (setq-local truncate-lines t))
        (display-buffer buf)))))

;;; After-save just calls the c-formatter-42 defun :-)
(add-hook 'before-save-hook
          (lambda ()
            (when (and (eq major-mode 'c-mode)
                       c-formatter-42-format-on-save)
              (c-formatter-42))))

(defvar c-formatter-42-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd c-formatter-42-keybinding) 'c-formatter-42)
    (define-key map (kbd norminette-keybinding) 'norminette)
    map)
  "Keymap for c-formatter-42 minor mode.")

;;;###autoload
(define-minor-mode c-formatter-42-mode
  "Minor mode for formatting C code with c_formatter_42."
  :init-value nil
  :lighter " C-Formatter-42"
  :keymap c-formatter-42-mode-map)

;;;###autoload
(add-hook 'c-mode-hook 'c-formatter-42-mode)
;;;###autoload
(add-hook 'c++-mode-hook 'c-formatter-42-mode)

(provide 'c-formatter-42)

;;; c-formatter-42.el ends here
