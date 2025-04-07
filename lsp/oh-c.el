(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) . lsp)
  :config
  (setq ccls-executable "ccls"))

(provide 'oh-c)
