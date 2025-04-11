;; oh-zig.el

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (zig-mode . lsp)
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("~/.local/bin/zls"))
      :major-modes '(zig-mode)
      :server-id 'zls))))

(provide 'oh-zig)
