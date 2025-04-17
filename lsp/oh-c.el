(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) . lsp)
  :config
  (setq ccls-executable "ccls"))

(defun disable-c-indentation ()
  (setq-local indent-tabs-mode         t)
  (setq-local tab-width                8)
  (setq-local c-basic-offset           8)
  (setq-local electric-indent-inhibit  t)
  (setq-local indent-line-function     'indent-relative)
  (c-set-style "bsd"))

(add-hook 'c-mode-hook   'disable-c-indentation)
(add-hook 'c++-mode-hook 'disable-c-indentation)

(provide 'oh-c)
