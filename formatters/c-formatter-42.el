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

(defcustom c-formatter-42-set-equalprg 0
  "If non-zero, sets the 'equalprg' variable to the C formatter executable."
  :type 'integer
  :group 'c-formatter-42)

(defcustom c-formatter-42-format-on-save 0
  "If non-zero, formats C files on save."
  :type 'integer
  :group 'c-formatter-42)

(defcustom norminette-command "norminette"
  "The command used to run Norminette for style checking."
  :type 'string
  :group 'c-formatter-42)

;(defun c-formatter-42-install ()
;  "Ensure that c_formatter_42 is installed."
;  (unless (executable-find c-formatter-42-exec)
;    (message "Installing c_formatter_42...")
;    (shell-command "pip3 install --user c-formatter-42")))

(defun c-formatter-42 ()
  "Format the current buffer using the c_formatter_42 tool."
  (interactive)
  (save-excursion
    (let ((equalprg-temp (executable-find "equalprg")))
      (setq-local equalprg c-formatter-42-exec)
      (shell-command-on-region (point-min) (point-max) c-formatter-42-exec nil t)
      (setq-local equalprg equalprg-temp))))

(defun norminette ()
  "Run Norminette on the current file and display the result in a new buffer."
  (interactive)
  (let ((current-file (expand-file-name (buffer-file-name))))
    (let ((buf (get-buffer-create "*Norminette Result*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "Norminette result for %s\n" current-file))
        (insert (make-string 80 ?-) "\n")
        (insert (shell-command-to-string (format "%s %s" norminette-command current-file)))
        (goto-char (point-min))
        (display-buffer buf)
        (setq-local buffer-read-only t)
        (setq-local truncate-lines t)))))

;;;###autoload
(add-hook 'before-save-hook
          (lambda ()
            (when (and (eq major-mode 'c-mode)
                       c-formatter-42-format-on-save)
              (c-formatter-42))))

(defvar c-formatter-42-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f2>") 'c-formatter-42)
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

(defun c-formatter-42-enable-equalprg ()
  "Enable equalprg with c_formatter_42 if configured."
  (when c-formatter-42-set-equalprg
    (setq-local equalprg c-formatter-42-exec)))

;;;###autoload
(c-formatter-42-enable-equalprg)

(provide 'c-formatter-42)

;;; c-formatter-42.el ends here
