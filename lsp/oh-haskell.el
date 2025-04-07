(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode)
	  (haskell-mode . lsp))
  :config
  (setq haskell-stylish-on-save t))


(provide 'oh-haskell)
